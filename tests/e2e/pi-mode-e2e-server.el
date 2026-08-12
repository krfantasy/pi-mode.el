;;; pi-mode-e2e-server.el --- OpenAI-compatible model endpoint for e2e tests -*- lexical-binding: t; -*-

;;; Commentary:
;; A real HTTP server (make-network-process) speaking just enough of the
;; OpenAI Chat Completions protocol for pi: POST /v1/chat/completions
;; (plain JSON and SSE streaming) and GET /v1/models.  The e2e suite
;; points pi's `models.json' at this endpoint, so the full loop
;; Emacs -> ghostel -> pi -> model HTTP -> pi -> ghostel -> Emacs runs
;; against real components with a deterministic canned reply.
;; Every request is recorded in `pi-mode-e2e-server-requests'.

;;; Code:

(require 'json)

(defvar pi-mode-e2e-server-process nil
  "The listening server process, or nil.")
(defvar pi-mode-e2e-server-requests nil
  "List of (METHOD PATH HEADERS BODY) recorded for each request.")
(defvar pi-mode-e2e-server-last-user-text nil
  "User message text from the most recent chat completion request.")
(defvar pi-mode-e2e-server-port nil
  "Port the server is listening on (nil when not running).")

(defconst pi-mode-e2e-server-model-id "e2e-model"
  "Model id pi is configured with for e2e tests.")

(defun pi-mode-e2e-server--reply-content (user-text)
  "Canned assistant reply echoing USER-TEXT."
  (format "E2E-REPLY: I received: %s" user-text))

(defun pi-mode-e2e-server--user-text-from-body (body)
  "Return the last user message text from an OpenAI chat request BODY."
  (condition-case nil
      (let* ((json (json-read-from-string body))
             (messages (cdr (assq 'messages json)))
             (last-user (car (last (cl-remove-if-not
                                    (lambda (m) (eq (cdr (assq 'role m)) 'user))
                                    messages)))))
        (if last-user
            (let ((content (cdr (assq 'content last-user))))
              (cond
               ((stringp content) content)
               ((listp content)   ; content parts array
                (mapconcat (lambda (part)
                             (let ((type (cdr (assq 'type part))))
                               (if (equal type "text")
                                   (or (cdr (assq 'text part)) "")
                                 "")))
                           content ""))
               (t "")))
          ""))
    (error "")))

(defun pi-mode-e2e-server--json-response (alist)
  "Encode ALIST as a JSON string for the chat completion response."
  (json-encode alist))

(defun pi-mode-e2e-server--chat-response (user-text stream-p)
  "Return the HTTP response body for a chat completion of USER-TEXT.
STREAM-P selects the SSE streaming format."
  (let ((content (pi-mode-e2e-server--reply-content user-text)))
    (if stream-p
        (concat
         (mapconcat
          (lambda (chunk)
            (format "data: %s\n\n"
                    (pi-mode-e2e-server--json-response
                     `((id . "chatcmpl-e2e")
                       (object . "chat.completion.chunk")
                       (created . 0)
                       (model . ,pi-mode-e2e-server-model-id)
                       (choices . [((index . 0)
                                    (delta . ,chunk)
                                    (finish_reason . :null))])))))
          (list `((role . "assistant") (content . ,content))
                '((finish_reason . "stop")))
          "")
         "data: [DONE]\n\n")
      (pi-mode-e2e-server--json-response
       `((id . "chatcmpl-e2e")
         (object . "chat.completion")
         (created . 0)
         (model . ,pi-mode-e2e-server-model-id)
         (choices . [((index . 0)
                      (message . ((role . "assistant") (content . ,content)))
                      (finish_reason . "stop"))])
         (usage . ((prompt_tokens . 1) (completion_tokens . 1) (total_tokens . 2))))))))

(defun pi-mode-e2e-server--models-response ()
  "HTTP response body for GET /v1/models."
  (pi-mode-e2e-server--json-response
   `((object . "list")
     (data . [((id . ,pi-mode-e2e-server-model-id) (object . "model"))]))))

(defun pi-mode-e2e-server--handle-request (proc method path headers body)
  "Handle one HTTP request from PROC and send the response."
  (push (list method path headers body) pi-mode-e2e-server-requests)
  (let* ((stream-p (and (string-match-p "chat/completions" path)
                        (string-match-p "\"stream\"[[:space:]]*:[[:space:]]*true" body)))
         (user-text (if (string-match-p "chat/completions" path)
                        (pi-mode-e2e-server--user-text-from-body body)
                      ""))
         (status (if (string-match-p "^/v1/" path) "200 OK" "404 Not Found"))
         (resp-body
          (cond
           ((and (string-match-p "^/v1/models" path) (equal method "GET"))
            (pi-mode-e2e-server--models-response))
           ((string-match-p "chat/completions" path)
            (setq pi-mode-e2e-server-last-user-text user-text)
            (pi-mode-e2e-server--chat-response user-text stream-p))
           (t "{\"error\":\"not found\"}")))
         (content-type (if (and (string-match-p "chat/completions" path) stream-p)
                           "text/event-stream"
                         "application/json")))
    (process-send-string
     proc
     (format "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
             status content-type (string-bytes resp-body) resp-body))
    ;; Flush the queued response before tearing the connection down.
    (run-at-time 1 nil
                 (lambda (p) (when (process-live-p p) (delete-process p)))
                 proc)))

(defun pi-mode-e2e-server--filter (proc output)
  "Accumulate the HTTP request from PROC; dispatch when complete."
  (let ((buf (concat (or (process-get proc :http-buf) "") output)))
    (process-put proc :http-buf buf)
    (when (string-match "\r\n\r\n" buf)
      (let* ((head (substring buf 0 (match-beginning 0)))
             (body-start (match-end 0))
             (clen 0))
        (dolist (line (split-string head "\r\n"))
          (when (string-match "^Content-Length:[[:space:]]*\\([0-9]+\\)" line)
            (setq clen (string-to-number (match-string 1 line)))))
        (when (and (> clen 0) (>= (- (length buf) body-start) clen))
          (process-put proc :http-buf nil)
          (let* ((lines (split-string head "\r\n"))
                 (request-line (split-string (car lines) " "))
                 (headers nil))
            (dolist (line (cdr lines))
              (when (string-match "^\\([^:]*\\):[[:space:]]*\\(.*\\)$" line)
                (push (cons (downcase (match-string 1 line))
                            (match-string 2 line))
                      headers)))
            (pi-mode-e2e-server--handle-request
             proc (nth 0 request-line) (nth 1 request-line)
             headers (substring buf body-start (+ body-start clen)))))))))

(defun pi-mode-e2e-server-start (&optional host)
  "Start the e2e model server on HOST (default 127.0.0.1).
Returns the port; the process is stored in `pi-mode-e2e-server-process'."
  (unless (and pi-mode-e2e-server-process
               (process-live-p pi-mode-e2e-server-process))
    (setq pi-mode-e2e-server-requests nil)
    (setq pi-mode-e2e-server-process
          (make-network-process
           :name "pi-mode-e2e-server"
           :server t
           :host (or host "127.0.0.1")
           :service 0
           :family 'ipv4
           :filter #'pi-mode-e2e-server--filter
           :sentinel (lambda (proc _event) (when (process-live-p proc) nil))))
    (setq pi-mode-e2e-server-port
          (process-contact pi-mode-e2e-server-process :service)))
  pi-mode-e2e-server-port)

(defun pi-mode-e2e-server-stop ()
  "Stop the e2e model server."
  (when (and pi-mode-e2e-server-process
             (process-live-p pi-mode-e2e-server-process))
    (delete-process pi-mode-e2e-server-process))
  (setq pi-mode-e2e-server-process nil)
  (setq pi-mode-e2e-server-port nil))

(defun pi-mode-e2e-server-models-json ()
  "Return the models.json content pointing at this server."
  (format (concat "{\n"
                  "  \"providers\": {\n"
                  "    \"e2e\": {\n"
                  "      \"baseUrl\": \"http://127.0.0.1:%d/v1\",\n"
                  "      \"api\": \"openai-completions\",\n"
                  "      \"apiKey\": \"e2e-dummy-key\",\n"
                  "      \"compat\": { \"supportsDeveloperRole\": false, \"supportsReasoningEffort\": false },\n"
                  "      \"models\": [ { \"id\": \"%s\" } ]\n"
                  "    }\n"
                  "  }\n"
                  "}\n")
          pi-mode-e2e-server-port pi-mode-e2e-server-model-id))

(provide 'pi-mode-e2e-server)
;;; pi-mode-e2e-server.el ends here
