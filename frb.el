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

;; Code components:
;;   commands : M-x frb-*
;;   modules  : frb-auth, frb-http, frb-data, frb-view, frb-util
;;   modes    : frb-mode, frb-tags-mode

(eval-when-compile (require 'cl))
(require 'url)
(require 'md5)
(require 'json)
(require 'highline)
(require 'mcomplete)

(mcomplete-mode 1)

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

(setq frb-notes-keywords
      '(("[0-9]+\\/[0-9]+\\/[0-9]+ [0-9]+\\:[0-9]+\\:[0-9]+ \\+0000" . font-lock-variable-name-face)
        ("public\\:[0-9]+" . font-lock-constant-face)
        ("private\\:[0-9]+" . font-lock-comment-face)))

(setq frb-tags-keywords
      '((" - [a-z]+" . font-lock-variable-name-face)
        (" ([0-9]+)" . font-lock-constant-face)))

(define-derived-mode frb-notes-mode fundamental-mode
  (setq font-lock-defaults '(frb-notes-keywords))
  (setq mode-name "frb-notes"))

(define-derived-mode frb-tags-mode fundamental-mode
  (setq font-lock-defaults '(frb-tags-keywords))
  (setq mode-name "frb-tags"))

;; Key-bindings

(global-set-key (kbd "C-c C-p") 'frb-note-region)
(global-set-key (kbd "C-c C-o") 'frb-open-note-region)
(global-set-key (kbd "C-c C-n") 'frb-notes)
(global-set-key (kbd "C-c C-n") 'frb-open-notes)
(global-set-key (kbd "C-c C-t") 'frb-tags)
(global-set-key [f2] 'frb-note)
(global-set-key [f5] 'frb-notes)
(define-key frb-tags-mode-map "?" 'frb-view/show-tag-help)
(define-key frb-tags-mode-map [return] 'frb-view/notes-at-point)
(define-key frb-notes-mode-map "?" 'frb-view/show-help)


;;; Commands/Interfaces
(defun frb-note-region (beg end)
  "Send the region to the frb server specified in `frb-server'"
  (interactive "r")
  (frb-http/post "note/new"
                 (format "body=%s&privacy=%s"
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
  (frb-http/post "note/new"
                 (format "body=%s&privacy=%s"
                         (read-from-minibuffer "Note: ")
                         "private")))

(defun frb-open-note-region (beg end)
  "Send the region to the frb server specified in `frb-server'"
  (interactive "r")
  (frb-http/post "note/new"
                 (format "body=%s&privacy=%s"
                         (buffer-substring-no-properties beg end)
                         "public")))

(defun frb-open-note-buffer ()
  "Like frb-note-region but post the entire buffer"
  (interactive)
  (frb-open-note-region (point-min) (point-max)))

(defun frb-open-note ()
  (interactive)
  (frb-http/post "note/new"
                 (format "body=%s&privacy=%s"
                         (read-from-minibuffer "Note: ")
                         "public")))

(defun frb-open-note-at-point ()
  "Like frb-note-at-point that is public"
  (interactive)
  (frb-note-region (point) (point-max)))

(defun frb-tags ()
  (interactive)
  (frb-http/get "tags"))

(defun frb-notes-all ()
  (interactive)
  (frb-http/get "notes"))

(defun frb-open-tags ()
  (interactive)
  (frb-http/open-get "opentags"))

(defun frb-open-notes-all ()
  (interactive)
  (frb-http/open-get "openbook"))

(defun frb-open-notes-tag ()
  "Get the notes for the given tag"
  (interactive)
  (frb-http/get "tag/openbook"
                (format "tag_name=%s" (read-from-minibuffer "Tag: "))))

(defun frb-open-note-id ()
  "Get the note for the given id"
  (interactive)
  (frb-http/get "opennote"
                (format "note_id=%s" (read-from-minibuffer "Note: "))))

(defun frb-notes ()
  "Get the notes for the given tag"
  (interactive)
  (let ((tags (frb-data/tags-all)))
    (frb-http/get "tag/notes"
                  (format "tag_name=%s" (completing-read "Get notes containing tag: " tags)))))

(defun frb-open-notes ()
  "Get the open notes for the given tag"
  (interactive)
  (let ((tags (frb-data/open-tags-all)))
    (frb-http/get "tag/openotes"
                  (format "tag_name=%s" (completing-read "Tag: " tags)))))

(defun frb-notes-tag-debug ()
  "Get the notes for the given tag"
  (interactive)
  (frb-http/get "tag/notes"
                (format "tag_name=%s" (read-from-minibuffer "Tag: "))))

(defun frb-note-id ()
  "Get the note for the given id"
  (interactive)
  (frb-http/get "note"
                (format "note_id=%s" (read-from-minibuffer "Note: "))))

(defun frb-login ()
  (interactive)
  (let ((username (read-from-minibuffer "User: "))
        (password (read-passwd "Password: ")))
    (setq frb-username username)
    (setq frb-password password)))

;; modules
(defun frb-http/uri (path)
  (format "%s/api/1.0/%s" frb-server path))

(defun frb-http/post (path query-string)
  (let* ((username (or frb-username (read-from-minibuffer "User: ")))
         (password (or frb-password (read-passwd "Password: ")))
         (uri (frb-http/uri path))
         (url-request-method "POST")
         (qs (concat (format "auth_email=%s&auth_password=%s&"
                             username
                             (md5 password)) query-string))
         (url-request-data qs))
    (frb-auth/save-creds username password)
    (url-retrieve uri 'frb-http/kill-buffer)))

(defun frb-http/kill-buffer (status)
  (kill-buffer (current-buffer))
  (message "Added a faraday's note"))

(defun frb-http/switch-buffer (status)
  (switch-to-buffer (current-buffer)))

(defun frb-http/get (path &optional query-string)
  (let* ((username (or frb-username (read-from-minibuffer "User: ")))
         (password (or frb-password (read-passwd "Password: ")))
         (uri (frb-http/uri path))
         (url-request-method "GET")
         (qs (if query-string
                 (concat uri (format "?auth_email=%s&auth_password=%s&"
                                     username (md5 password)) query-string)
               (concat uri (format "?auth_email=%s&auth_password=%s"
                                   username (md5 password))))))
    (frb-auth/save-creds username password)
    (frb-view/dispatcher path (url-retrieve-synchronously qs))))

(defun frb-http/open-get (path &optional query-string)
  (let* ((uri (frb-http/uri path))
         (url-request-method "GET")
         (qs (if query-string
                 (concat uri (format "?%s" query-string))
               uri)))
    (setq url-mime-encoding-string "identity")
    (frb-view/dispatcher path (url-retrieve-synchronously qs))))

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
    (frb-auth/save-creds username password)
    (frb-data/stash (url-retrieve-synchronously qs))))

(defun frb-data/open (path &optional query-string)
  (let* ((uri (format "%s/api/1.0/%s" frb-server path))
         (url-request-method "GET")
         (qs (if query-string
                 (concat uri (format "?%s" query-string))
               uri)))
    (setq url-mime-encoding-string "identity")
    (frb-data/stash (url-retrieve-synchronously qs))))

;; The frb stasher - to get data
(defun frb-data/stash (frb-buffer)
  (frb-util/kill-buffer "frb-data")
  (with-current-buffer frb-buffer
    (frb-util/set-buffer "frb-data" frb-buffer)
    (let* ((j (json-read-from-string
               (buffer-substring-no-properties (point) (point-max))))
           (k  (append j nil)))
      (setq frb-stash k))
    (set-buffer-modified-p nil))
  (kill-buffer frb-buffer))

(defun frb-data/tags-all ()
  "Get the notes for the given tag"
  (frb-data "tags")
  (let (value)
    (dolist (e frb-stash value)
      (setq value (cons (cdr (car (cdr e))) value)))))

(defun frb-data/open-tags-all ()
  "Get the notes for the given tag"
  (frb-data/open "opentags")
  (let (value)
    (dolist (e frb-stash value)
      (setq value (cons (cdr (car (cdr e))) value)))))

(defun frb-data/notes-count ()
  "Get the notes for the given tag"
  (frb-data "count"))

;;; The frb viewer: new(n) edit(e) show(s) delete(d)
(defun frb-view/dispatcher (fn-type frb-buffer)
  (funcall (intern-soft (format "frb-view/%s" fn-type)) frb-buffer))

(defun frb-view/tags (frb-buffer)
  (frb-util/kill-buffer "frb-tags")
  (with-current-buffer frb-buffer
    (frb-util/set-buffer "frb-tags" frb-buffer)
    (frb-tags-mode)
    (let* ((j (json-read-from-string
               (buffer-substring-no-properties (point) (point-max))))
           (k  (append j nil)))
      (delete-region (point-min) (point-max))
      (frb-view/format-tags k)
      (sort-columns nil (point-min) (point-max))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-line 1))))

(defun frb-view/notes (frb-buffer)
  (frb-util/kill-buffer "frb-notes")
  (with-current-buffer frb-buffer
    (frb-util/set-buffer "frb-notes" frb-buffer)
    (frb-notes-mode)
    (let* ((j (json-read-from-string
               (buffer-substring-no-properties (point) (point-max))))
           (k  (append j nil)))
      (delete-region (point-min) (point-max))
      (frb-view/format-notes k)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-line 1))))

(defun frb-view/opentags (frb-buffer)
  (frb-view/tags frb-buffer))

(defun frb-view/openbook (frb-buffer)
  (frb-view/notes frb-buffer))

(defun frb-view/tag/notes (frb-buffer)
  (frb-view/notes frb-buffer))

(defun frb-view/tag/opennotes (frb-buffer)
  (frb-view/notes frb-buffer))

(defun frb-view/note (frb-buffer)
  (frb-view/notes frb-buffer))

(defun frb-view/opennote (frb-buffer)
  (frb-view/notes frb-buffer))

(defun frb-view/format-tags (x)
  (dolist (p x)
    (insert
     (format " - %s (%s)\n" (cdr (car (cdr p))) (cdr (car p))))))

(defun frb-view/format-notes (x)
  (dolist (p x)
    (progn
      (insert
       (format "%s %s:%s\n%s\n\n"
               (cdr (nth 3 p)) (cdr (nth 0 p))
               (cdr (nth 2 p)) (cdr (nth 4 p)))))))

(defun frb-view/show-tag-help ()
  (interactive)
  (message "help: Notes (return)"))

(defun frb-view/show-help ()
  (interactive)
  (message "help: Edit(e)  Delete(d)  OpenNote(o)"))

(defun frb-view/notes-at-point ()
  (interactive)
  (message "help: Edit(e)  Delete(d)  OpenNote(o)"))

(defun frb-auth/save-creds (username password)
  (or frb-username (setq frb-username username))
  (or frb-password (setq frb-password password)))

(defun frb-util/set-buffer (name buffer)
  (progn
    (set-visited-file-name name)
    (switch-to-buffer buffer)
    (goto-line 10)))

(defun frb-util/kill-buffer (name)
  (when (get-buffer name)
    (kill-buffer name)))

(provide 'frb)
