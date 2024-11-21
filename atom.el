;;; atom.el --- a elisp package for create, modify and get pangu item data -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 TX Mao
;;
;; Author: TX Mao <mtianxiang@gmail.com>
;; Maintainer: TX Mao <mtianxiang@gmail.com>
;; Created: November 18, 2024
;; Modified: November 18, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/worker/atom
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  a elisp package for create, modify and get pangu item data
;;
;;; Code:

(require 'pangu)

;; set it in config.el

;;(defvar atom-conn (atom-make-conn "localhost" 38000))

(defun atom-make-conn (host port)
  ;; construct function for pangu object
  `(
    (host . ,host)
    (port . ,port)
    (msg . nil)
    (buf-name . "*pangu-atom*")
    (saved-buffer . nil)
    (current-id . "")
    (lastbuffer . nil) ; last buffer
    ))


(defun atom-buffer-name (p)
  (txmao/--get-prop p 'buf-name))

(defun atom-buffer (p)
  (let ((buf (atom-buffer-name p)))
    (if (null (get-buffer buf))
        (progn
          (get-buffer-create buf)
          (with-current-buffer buf
            (progn
              (pangu-atom-mode 1)
              ))
          (get-buffer buf))
      (get-buffer buf))))


(defun atom-window (p)
  (let ((w (get-buffer-window (atom-buffer p))))
    (cond ((null w) (let ((new-w (split-window-right)))
                      (set-window-buffer new-w (atom-buffer p))
                      new-w))
          (w))))

(defun atom-same-window (p)
  (display-buffer-same-window (atom-buffer p) nil)
  )


(defun txmao/--json-to-xml (p)
  ;; if no yq, run `go install github.com/mikefarah/yq/v4@latest`
  (with-current-buffer (atom-buffer p)
    (nxml-mode)
    (shell-command-on-region (point-min) (point-max) "yq 'sort_keys(.node) | sort_keys(.config) | sort_keys(.meta)' -p json -o xml" :REPLACE 't)
    (goto-char (point-min))
    (while (search-forward "&#34;" nil t)
      (replace-match "\""))
    ))


(defun txmao/--xml-to-json (p)
  ;; if no yq, run `go install github.com/mikefarah/yq/v4@latest`
  (let ((cmd "yq '.. |= omit([\"id\", \"hash_id\", \"config_id\", \"meta_id\"]) | sort_keys(..) | (  .. | select(tag==\"!!str\" and key != \"kwargs\" and key != \"template\" and key != \"description\")) |= from_json' -p xml -o json"))
    (with-current-buffer (atom-buffer p)
      (json-mode)
      (shell-command-on-region (point-min) (point-max) cmd :REPLACE 't)
      (json-mode-beautify (point-min) (point-max))
      )))



(defun atom-buffer-string (p)
  (with-current-buffer (atom-buffer p)
    (buffer-substring-no-properties (point-min) (point-max))))




(defun atom-request-callback (p response)
  (with-current-buffer (atom-buffer p)
    (erase-buffer)
    (insert (request-response-data response))
    (json-mode-beautify (point-min) (point-max))
    (txmao/--json-to-xml p)
    ))


(defun atom-request-get (p id)
  (request (format "http://%s:%s/atom/get" (pangu-host p) (pangu-port p))
    :type "POST"
    :data (json-encode `(("id" . ,id)))
    :headers '(("Content-Type" . "application/json"))
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (atom-request-callback atom-conn response)
                 )))
  )

(defun atom-request-new (p)
  (request (format "http://%s:%s/atom/new" (pangu-host p) (pangu-port p))
    :type "POST"
    :data (progn (txmao/--xml-to-json p) (atom-buffer-string p))
    :headers '(("Content-Type" . "application/json") ("accept" . "application/json"
                                                      ))
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (message
                  (txmao/--get-prop
                   (request-response-data response)
                   'status
                   )
                  )
                 )))
  )

;;;;;;;;;;;;;;
;; commands ;;
;;;;;;;;;;;;;;


(defun pangu-api-atom-create ()
  (interactive)
  (atom-request-new atom-conn))

(defun pangu-api-atom-get (id)
  ;; id example: "8116909f2711612"
  (interactive (list (read-string (format "node id: (%s)" (word-at-point 't)) nil nil (word-at-point 't))))

  ;; on the same window
                                        ;(display-buffer-same-window (atom-buffer atom-conn) nil)
  ;; to another window
  (when (not (pangu-check-in-buffer (pangu-buffer atom-conn)))
    (pangu-set-last-buffer atom-conn (current-buffer)))
  (select-window (pangu-window atom-conn))
  
  (atom-request-get  atom-conn id)
  (sleep-for 0.1)
  (with-current-buffer (atom-buffer atom-conn)
    (if (string= id "")
        (progn
          (txmao/--xml-to-json atom-conn)
          (txmao/--json-to-xml atom-conn))
      (evil-escape))))



(define-minor-mode pangu-atom-mode
  "Get your foos in the right places."
  :lighter " PanguAI-Atom"
  :keymap (let ((map (make-sparse-keymap)))
            (global-set-key (kbd "C-c n") 'pangu-api-atom-get)
            (global-set-key (kbd "C-c C-c") 'pangu-api-atom-create)
            map))




(provide 'atom)
;;; atom.el ends here
