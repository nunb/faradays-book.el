;;; frb.el -- Emacs client for faraday's book

;; Client to Prakash Raman's http://faradaysbook.com

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;; (require 'frb)
;; (setq frb-server "http://faradaysbook.com")
;; (setq frb-username "username")
;; (setq frb-password "password")

;; Isaac Praveen <icylisper@gmail.com> 

(eval-when-compile (require 'cl))
(require 'url)
(require 'md5)
(require 'json)
(require 'highline)

(defgroup frb nil "client for the faraday's book")

(defvar url-http-end-of-headers)

(defcustom frb-server "http://faradaysbook.com"
  "Base URL for the frb-server"
  :type 'string
  :group 'frb)
       
(defcustom frb-server "http://faradaysbook.com"
  "Base URL for the frb-server"
  :type 'string
  :group 'frb)

(defcustom frb-username nil
  "frb username, which is the email address"
  :type '(choice (string) (const :tag "Ask every time" nil))
  :group 'frb)

(defcustom frb-password nil
  "frb pasword"
  :type '(choice (string) (const :tag "Ask every time" nil))
  :group 'frb)

(defvar frb-stash)
(defvar frb-tags-current)

;; modes

(setq frb-keywords
      '(("[0-9]+\\/[0-9]+\\/[0-9]+ [0-9]+\\:[0-9]+\\:[0-9]+ \\+0000" . font-lock-variable-name-face)
        ("public\\:[0-9]+" . font-lock-constant-face)
        ("private\\:[0-9]+" . font-lock-comment-face)
        (" - [a-z]+" . font-lock-variable-name-face)
        (" ([0-9]+)" . font-lock-constant-face)))

(define-derived-mode frb-mode fundamental-mode
  (setq font-lock-defaults '(frb-keywords))
  (setq mode-name "frb"))

;; Key-bindings
(global-set-key (kbd "C-c C-f") 'frb-note-region)
(global-set-key (kbd "C-c C-o") 'frb-open-note-region)
(global-set-key [f2] 'frb-note)

;;; Commands/Interfaces
(defun frb-note-region (beg end)
  "Send the region to the frb server specified in `frb-server'"
  (interactive "r")
  (frb-post "note/new" (format "body=%s&privacy=%s"
                               (buffer-substring-no-properties beg end)
                               "private")))

(defun frb-note-buffer ()
  "Like frb-note-region but post the entire buffer"
  (interactive)
  (frb-note-region (point-min) (point-max)))

(defun frb-note-at-point ()
  "Like frb-note-region but post from current point"
  (interactive)
  (frb-note-region (point) (point-max)))

(defun frb-note ()
  (interactive)
  (frb-post "note/new" (format "body=%s&privacy=%s"
                               (read-from-minibuffer "Note: ")
                               "private")))

(defun frb-open-note-region (beg end)
  "Send the region to the frb server specified in `frb-server'"
  (interactive "r")
  (frb-post "note/new" (format "body=%s&privacy=%s"
                               (buffer-substring-no-properties beg end)
                               "public")))

(defun frb-open-note-buffer ()
  "Like frb-note-region but post the entire buffer"
  (interactive)
  (frb-open-note-region (point-min) (point-max)))

(defun frb-open-note ()
  (interactive)
  (frb-post "note/new" (format "body=%s&privacy=%s"
                               (read-from-minibuffer "Note: ")
                               "public")))

(defun frb-open-note-at-point ()
  "Like frb-note-at-point that is public"
  (interactive)
  (frb-note-region (point) (point-max)))

(defun frb-tags ()
  (interactive)
  (frb-get "tags"))

(defun frb-notes ()
  (interactive)
  (frb-get "notes"))

(defun frb-open-tags ()
  (interactive)
  (frb-open-get "opentags"))

(defun frb-open-notes ()
  (interactive)
  (frb-open-get "openbook"))

(defun frb-open-notes-tag ()
  "Get the notes for the given tag"
  (interactive)
  (frb-get "tag/openbook" (format "tag_name=%s" (read-from-minibuffer "Tag: "))))

(defun frb-open-note-id ()
  "Get the note for the given id"
  (interactive)
  (frb-get "opennote" (format "note_id=%s" (read-from-minibuffer "Note: "))))

(defun frb-notes-tag ()
  "Get the notes for the given tag"
  (interactive)
  (let ((tags (frb-tags-all)))
    (frb-get "tag/notes" (format "tag_name=%s" (completing-read "Tag: " tags)))))

(defun frb-notes-tag-debug ()
  "Get the notes for the given tag"
  (interactive)
  (frb-get "tag/notes" (format "tag_name=%s" (read-from-minibuffer "Tag: "))))

(defun frb-note-id ()
  "Get the note for the given id"
  (interactive)
  (frb-get "note" (format "note_id=%s" (read-from-minibuffer "Note: "))))


;;; Post-helpers

(defun frb-post (path query-string)
  (let* ((username (or frb-username (read-from-minibuffer "User: ")))
         (password (or frb-password (read-passwd "Password: ")))
         (uri (format "%s/api/1.0/%s" frb-server path))
         (url-request-method "POST")
         (qs (concat (format "auth_email=%s&auth_password=%s&"
                             username
                             (md5 password)) query-string))
         (url-request-data qs))
    (frb-save-creds username password)
    (url-retrieve uri 'kill-url-buffer)))

(defun kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer))
  (message "Added a faraday's note"))

(defun switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))

;; View-helpers

(defun frb-get (path &optional query-string)
  (let* ((username (or frb-username (read-from-minibuffer "User: ")))
         (password (or frb-password (read-passwd "Password: ")))
         (uri (format "%s/api/1.0/%s" frb-server path))
         (url-request-method "GET")
         (qs (if query-string
                 (concat uri (format "?auth_email=%s&auth_password=%s&"
                                     username (md5 password)) query-string)
               (concat uri (format "?auth_email=%s&auth_password=%s"
                                   username (md5 password))))))
    (frb-save-creds username password)
    (frb-viewer path (url-retrieve-synchronously qs))))

(defun frb-open-get (path &optional query-string)
  (let* ((uri (format "%s/api/1.0/%s" frb-server path))
         (url-request-method "GET")
         (qs (if query-string
                 (concat uri (format "?%s" query-string))
               uri)))
    (setq url-mime-encoding-string "identity")
    (frb-viewer path (url-retrieve-synchronously qs))))

(defun frb-data (path &optional query-string)
  (let* ((username (or frb-username (read-from-minibuffer "User: ")))
         (password (or frb-password (read-passwd "Password: ")))
         (uri (format "%s/api/1.0/%s" frb-server path))
         (url-request-method "GET")
         (qs (if query-string
                 (concat uri (format "?auth_email=%s&auth_password=%s&"
                                     username (md5 password)) query-string)
               (concat uri (format "?auth_email=%s&auth_password=%s"
                                   username (md5 password))))))
    (frb-save-creds username password)
    (frb-stasher (url-retrieve-synchronously qs))))

(defun frb-open-data (path &optional query-string)
  (let* ((uri (format "%s/api/1.0/%s" frb-server path))
         (url-request-method "GET")
         (qs (if query-string
                 (concat uri (format "?%s" query-string))
               uri)))
    (setq url-mime-encoding-string "identity")
    (frb-stasher (url-retrieve-synchronously qs))))

;; The frb stasher - to get data
(defun frb-stasher (frb-buffer)
  (with-current-buffer frb-buffer
    (progn
      (set-visited-file-name (format "frbdata" (random)))
      (delete-region (point-min) (point))
      (set-buffer-modified-p nil)
      (switch-to-buffer frb-buffer)
      (goto-line 10)
      (let* ((j (json-read-from-string
                (buffer-substring-no-properties (point) (point-max))))
             (k  (append j nil)))
        (setq frb-stash k))))
  (kill-buffer frb-buffer))

;;; The frb viewer: new(n) edit(e) show(s) delete(d)
(defun frb-viewer (fn-type frb-buffer)
  (funcall (intern-soft (format "frb-viewer-%s" fn-type)) frb-buffer))

(defun frb-viewer-tags (frb-buffer)
  (when (get-buffer "frb-tags")
    (kill-buffer "frb-tags"))
  (with-current-buffer frb-buffer
    (progn
      (set-visited-file-name "frb-tags")
      (delete-region (point-min) (point))
      (set-buffer-modified-p nil)
      (switch-to-buffer frb-buffer)
      (frb-mode)
      (goto-line 10))
    (let* ((j (json-read-from-string
               (buffer-substring-no-properties (point) (point-max))))
           (k  (append j nil)))
      (delete-region (point-min) (point-max))
      (format-tags k)
      (sort-columns nil (point-min) (point-max))
      (setq buffer-read-only t)
      (goto-line 1))))

(defun frb-viewer-notes (frb-buffer)
  (when (get-buffer "frb-notes")
    (kill-buffer "frb-notes"))
  (with-current-buffer frb-buffer
    (progn
      (set-visited-file-name "frb-notes")
      (switch-to-buffer frb-buffer)
      (frb-mode)
      (goto-line 10)
      (let* ((j (json-read-from-string
                 (buffer-substring-no-properties (point) (point-max))))
            (k  (append j nil)))
        
        (delete-region (point-min) (point-max))
        (format-notes k)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-line 1)))))

(defun frb-viewer-opentags (frb-buffer)
  (frb-viewer-tags frb-buffer))

(defun frb-viewer-openbook (frb-buffer)
  (frb-viewer-notes frb-buffer))

(defun frb-viewer-tag/notes (frb-buffer)
  (frb-viewer-notes frb-buffer))

(defun frb-viewer-tag/opennotes (frb-buffer)
  (frb-viewer-notes frb-buffer))

(defun frb-viewer-note (frb-buffer)
  (frb-viewer-notes frb-buffer))

(defun frb-viewer-opennote (frb-buffer)
  (frb-viewer-notes frb-buffer))

(defun format-tags (x)
  (dolist (p x) (insert (format " - %s (%s)\n" (cdr (car (cdr p))) (cdr (car p))))))

(defun format-notes (x)
  (dolist (p x)
    (progn
      (insert (format "%s %s:%s\n%s\n\n" (cdr (nth 3 p)) (cdr (nth 0 p)) (cdr (nth 2 p)) (cdr (nth 4 p)))))))

(defun frb-modeline ()
  "format of the modeline: contain count of notes/tags")

;; login and session

(defun frb-save-creds (username password)
  (or frb-username (setq frb-username username))
  (or frb-password (setq frb-password password)))

(defun frb-login ()
  (interactive)
  (let ((username (read-from-minibuffer "User: "))
        (password (read-passwd "Password: ")))
    (setq frb-username username)
    (setq frb-password password)))

;; data
(defun frb-tags-all ()
  "Get the notes for the given tag"
  (frb-data "tags")
  (let (value)
    (dolist (e frb-stash value)
      (setq value (cons (cdr (car (cdr e))) value)))))

(defun frb-notes-count ()
  "Get the notes for the given tag"
  (frb-data "count"))

(provide 'frb)
