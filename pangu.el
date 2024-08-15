;;; pangu.el --- A plugin for python code generating with pangu ai
;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 TX Mao
;;
;; Author: TX Mao <mtianxiang@gmail.com>
;; Maintainer: TX Mao <mtianxiang@gmail.com>
;; Created: August 06, 2024
;; Modified: August 06, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/worker/pangu
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; balabala
;;
;;; Code:

(require 'request)

(provide 'pangu)

;; set it in config.el
;; (setq pangu-conn (pangu-make-conn "localhost" 29001))

                                        ;(setq pangu-conn (pangu-make-conn "localhost" 29001))

                                        ;(defvar pangu-conn '())

                                        ;(defvar pangu-conn (pangu-make-conn "localhost" 29001))

;;;###autoload
(defun pangu-make-conn (host port)
  ;; construct function for pangu object
  `(
    (host . ,host)
    (port . ,port)
    (msg . nil)
    (buf-name . "*ai-workspace*")
    (chat-buffer . "*ai-chat*")
    (saved-buffer . nil)
    (current-id . "")
    (lastbuffer . nil) ; last buffer
    ))

(defun txmao/--get-prop (data prop)
  "usage:
     (txmao/--get-prop (txmao/make-http-header) 'tag)"
  (condition-case err
      (cdr (assoc prop data))
    (error nil)))

(defun pangu-host (p)
  (cdr (assoc 'host p)))

(defun pangu-port (p)
  (cdr (assoc 'port p)))

(defun pangu-msg (p)
  (cdr (assoc 'msg p)))

(defun pangu-buffer-name (p)
  (cdr (assoc 'buf-name p)))

(defun pangu-saved-buffer (p)
  (cdr (assoc 'saved-buffer p)))

(defun pangu-last-buffer (p)
  (cdr (assoc 'lastbuffer p)))

(defun pangu-set-last-buffer (p buf)
  (setf (cdr (assoc 'lastbuffer p)) buf))

(defun pangu-current-id (p)
  (cdr (assoc 'current-id p)))

(defun pangu-set-current-id (p id)
  (setf (cdr (assoc 'current-id p)) id))

(defun pangu-set-msg (p msg)
  (setf (cdr (assoc 'msg p)) msg))

(defun pangu-buffer (p)
  (let ((buf (pangu-buffer-name p)))
    (if (null (get-buffer buf))
        (progn
          (get-buffer-create buf)
          (with-current-buffer buf
            (progn
              (insert "# This buffer is generated by pangu ai for python coding.\n# Author: pangu ai & mtianxiang@gmail.com\n")
              (python-mode)
              (pangu-set-workspace-edit-mode 0)))
          (get-buffer buf))
      (get-buffer buf))))

(defun pangu-window (p)
  (let ((w (get-buffer-window (pangu-buffer p))))
    (cond ((null w) (let ((new-w (split-window-right)))
                      (set-window-buffer new-w (pangu-buffer p))
                      new-w))
          (w))))

;; ------------------------------

(defun pangu-request-response-parse (response tag)
  (cdr (assoc tag (request-response-data response))))

(defun pangu-request (p signature)
  (let ((response
         (request (format "http://%s:%s" (pangu-host p) (pangu-port p))
           :type "POST"
           :data (json-encode `(("id" . "") ("msg" . ,signature)))
           :headers '(("Content-Type" . "application/json"))
           :parser 'json-read
           :sync t
           :complete (cl-function
                      (lambda (&key response &allow-other-keys)
                        (pangu-request-response-parse response 'msg))))))
    response))


(defun pangu-get-region-content ()
  "Return the content of the current region."
  (interactive)
  (with-current-buffer (current-buffer)
    (buffer-substring (region-beginning) (region-end))))


(defun pangu-coder-py ()
  "Aifly command to replace selected text based on rules."
  (interactive)
  (let* ((selected-text (pangu-get-region-content))
         (p pangu-conn)
         (response (pangu-request p selected-text))
         (code (pangu-request-response-parse response 'msg))
         (id (pangu-request-response-parse response 'id))
         (text (format "\n#>>> %s\n%s\n#--- human modified ---\n\n#<<<" id code)))
    (message "generate code by ai.pangu.datalab...")
    (select-window (pangu-window p))
    (goto-char (point-max))
    (pangu-set-workspace-edit-mode 1)
    (insert text)
    (pangu-set-workspace-edit-mode 0)))

(defun pangu-buffer-get-region-by-id (&optional id)
  (if (null id) (setq id "") id)
  (with-current-buffer (pangu-buffer pangu-conn)
    (goto-char (point-min))
    (let* (
           (prefix (format "#>>> %s" id))
           (_ (re-search-forward prefix))
           (beg (match-beginning 0))
           (_ (re-search-forward "#<<<"))
           (end (match-end 0)))
      `(,beg ,end))))

(defun pangu-buffer-get-region-by-point ()
  (if
      (not (string= (pangu-buffer-name pangu-conn) (buffer-name (current-buffer))))
      (error (format
              "Must be called in buffer %s"
              (pangu-buffer-name pangu-conn)))
    (with-current-buffer (pangu-buffer pangu-conn)
      (let* (
             (prefix "#>>> \\(.+\\)\n")
             (_ (re-search-backward prefix))
             (id-beg (match-beginning 1))
             (id-end (match-end 1))
             (id (buffer-substring id-beg id-end))
             (beg (match-beginning 0))
             (_ (re-search-forward "#<<<"))
             (end (match-end 0)))
        (pangu-set-current-id pangu-conn id)
        (message (format "%s: %s -> %s " id beg end))
        `(,beg ,end)))))

(defun pangu-coder-py-add-id (id)
                                        ; (let* ((beg (python-nav-backward-defun)))
  (let* ((beg (re-search-backward "def ")))
    (progn
      (re-search-backward "\\@compile\\(\(id=.+\)\\)?" nil t 1)
      (if (null (match-string 1))
          (replace-match (format "@compile(id=%s)" id) )
        (replace-match (format "#%s\n@compile(id=%s)" (match-string 0) id))))))

(defun pangu-check-in-buffer (buf)
  (string= (buffer-name buf) (buffer-name (current-buffer))))


;; ---------------------
;; key map and minor mode for interactive command

(defun pangu-window-open ()
  (interactive)
  (when (not (pangu-check-in-buffer (pangu-buffer pangu-conn)))
    (pangu-set-last-buffer pangu-conn (current-buffer)))
  (select-window (pangu-window pangu-conn))
  (pangu-set-workspace-edit-mode 0))

(defun pangu-window-close ()
  (interactive)
  (pangu-set-workspace-edit-mode 0)
  (delete-window (pangu-window pangu-conn)))

(defun pangu-window-edit ()
  (interactive)
  (select-window (pangu-window pangu-conn))
  (pangu-set-workspace-edit-mode 1))

;; ---
(defun pangu-buffer-save ()
  "Save the human created code"
  (interactive)
  (message "buffer saved...")
  (pangu-set-workspace-edit-mode 0))


(defun pangu-code-accept ()
  (interactive)
  (when (not (pangu-check-in-buffer (pangu-buffer pangu-conn)))
    (error (format "Must be called in buffer %s" (pangu-buffer-name pangu-conn))))
  (let ((beg-end (pangu-buffer-get-region-by-point))
        (id (pangu-current-id pangu-conn)))
    (pangu-workspace-edit-mode 1)
    (eval `(delete-region ,@beg-end))
    (pangu-workspace-edit-mode 0)
    (with-current-buffer (pangu-last-buffer pangu-conn)
      (pangu-coder-py-add-id id))
    (select-window (get-buffer-window (pangu-last-buffer pangu-conn)))))


(defun pangu-code-modify ()
  (interactive)
  (message "buffer saved..."))

;; ---

(defun pangu-set-workspace-edit-mode (enable)
  (with-current-buffer (pangu-buffer pangu-conn)
    (if (= enable 0)
        (progn (read-only-mode 1) (pangu-mode 1))
      (progn (read-only-mode 0) (pangu-mode 0)))))


(define-minor-mode pangu-mode
  "Get your foos in the right places."
  :lighter " PanguAI"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-p w") 'pangu-window-open)
            (define-key map (kbd "M-p k") 'pangu-window-close)
            (define-key map (kbd "M-p e") 'pangu-window-edit)
            (define-key map (kbd "M-p s") 'pangu-buffer-save)
            (define-key map (kbd "M-p a") 'pangu-code-accept)
            map))

(global-set-key (kbd "M-p w") 'pangu-window-open)
(global-set-key (kbd "M-p p") 'pangu-coder-py)

;; for chat mode

(defun pangu-chat-buffer-name (p)
  (txmao/--get-prop p 'chat-buffer))


(defun pangu-chat-buffer (p)
  (let ((buf (pangu-chat-buffer-name p)))
    (if (null (get-buffer buf))
        (progn
          (get-buffer-create buf)
          (with-current-buffer buf
            (progn
              (insert "\n---\n# <Task 1>\n")
              (markdown-mode)
              (pangu-chat-mode 1)))
          (get-buffer buf))
      (get-buffer buf))))


(defun pangu-chat-window (p)
  (let ((w (get-buffer-window (pangu-chat-buffer p))))
    (cond ((null w) (let ((new-w (split-window-right)))
                      (set-window-buffer new-w (pangu-chat-buffer p))
                      new-w))
          (w))))

(defun pangu-chat-request-callback (p response)
  (with-current-buffer (pangu-chat-buffer p)
    (goto-char (point-max))
    (let ((msg (txmao/--get-prop (request-response-dataa response) 'msg)))
      (insert (format "\nAssistant:\n\n%s\n\nUser:\n\n" msg))
      )))

(defun pangu-chat-request (p question)
  (request (format "http://%s:%s/chat" (pangu-host p) (pangu-port p))
    :type "POST"
    :data (json-encode `(("id" . "") ("msg" . ,question)))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (pangu-chat-request-callback p response))))
  )


(defun pangu--extract-task-number (p)
  "Extract the number from the first match of '<Task N>' in the backward search."
  (with-current-buffer (pangu-chat-buffer p)
    (goto-char (point-max))
    (let* ((_ (re-search-backward "# <Task \\([0-9]+\\)>"))
           (beg (match-beginning 1))
           (end (match-end 1)))
      (string-to-number (buffer-substring-no-properties beg end)))))

(defun pangu--extract-chat-history (p)
  "Extract the number from the first match of '<Task N>' in the backward search."
  (with-current-buffer (pangu-chat-buffer p)
    (goto-char (point-max))
    (let* ((_ (re-search-backward "# <Task \\([0-9]+\\)>\n"))
           (end (match-end 0)))
      (buffer-substring-no-properties end (point-max)))))

;; command map

(defun pangu-chat-window-open ()
  (interactive)
  (pangu-chat-window pangu-conn)
  (select-window (pangu-chat-window pangu-conn))
  )

(defun pangu-chat-new ()
  (interactive)
  (with-current-buffer (pangu-chat-buffer pangu-conn)
    (let* ((task-num (+ 1 (pangu--extract-task-number pangu-conn)))
           (header (format "\n---\n# <Task %d>\n" task-num)))
      (goto-char (point-max))
      (insert header)
      (insert "\nUser:\n")
      )))

(defun pangu-chat-call ()
  (interactive)
  (with-current-buffer (pangu-chat-buffer pangu-conn)
    (goto-char (point-max))
    (let ((history (pangu--extract-chat-history pangu-conn))
          (_ (goto-char (point-max))))
      (pangu-chat-request pangu-conn history))))


(define-minor-mode pangu-chat-mode
  "Get your foos in the right places."
  :lighter " PanguAI-Chat"
  :keymap (let ((map (make-sparse-keymap)))
            (global-set-key (kbd "M-p n") 'pangu-chat-new)
            (global-set-key (kbd "M-p c") 'pangu-chat-call)
            map))

(global-set-key (kbd "M-p c") 'pangu-chat-window-open)

(global-set-key (kbd "M-p n") 'pangu-chat-new)

                                        ;(global-set-key (kbd "M-p c") 'pangu-chat-call)


;;; pangu.el ends here
