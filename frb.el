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
  "frb username"
  :type '(choice (string) (const :tag "Ask every time" nil))
  :group 'frb)

(defcustom frb-password nil
  "frb pasword"
  :type '(choice (string) (const :tag "Ask every time" nil))
  :group 'frb)

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

;;; The frb viewer: new(n) edit(e) show(s) delete(d)
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

(defun frb-open-get (path &optional query-string)
  (let* ((uri (format "%s/api/1.0/%s" frb-server path))
         (url-request-method "GET")
         (qs (if query-string
                 (concat uri (format "?%s" query-string))
               uri)))
    (setq url-mime-encoding-string "identity")
    (frb-viewer path (url-retrieve-synchronously qs))))

(defun frb-viewer (fn-type frb-buffer)
  (funcall (intern-soft (format "frb-viewer-%s" fn-type)) frb-buffer))

(defun frb-viewer-tags (frb-buffer)
  (with-current-buffer frb-buffer
    (progn
      (set-visited-file-name (format "frb #%s" (random)))
      (delete-region (point-min) (point))
      (set-buffer-modified-p nil)
      (switch-to-buffer frb-buffer)
       (goto-line 10)
      (let* ((j (json-read-from-string
                 (buffer-substring-no-properties (point) (point-max))))
             (k  (append j nil)))
        (delete-region (point-min) (point-max))
        (format-tags k)
        (sort-columns nil (point-min) (point-max))
        (goto-line 1)))))

(defun frb-viewer-notes (frb-buffer)
  (with-current-buffer frb-buffer
    (progn
      (set-visited-file-name (format "frb #%s" (random)))
      (delete-region (point-min) (point))
      (set-buffer-modified-p nil)
      (switch-to-buffer frb-buffer)
      (goto-line 10)
      (let* ((j (json-read-from-string
                 (buffer-substring-no-properties (point) (point-max))))
             (k  (append j nil)))
        
        (delete-region (point-min) (point-max))
        (format-notes k)
        (goto-line 1)))))

(defun frb-viewer-opentags (frb-buffer)
  (frb-viewer-tags frb-buffer))

(defun frb-viewer-openbook (frb-buffer)
  (frb-viewer-notes frb-buffer))

;;; The openbook viewer: show(s)

(defun kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(defun switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (print status)
  (switch-to-buffer (current-buffer)))

(defun format-tags (x)
  (dolist (p x) (insert (format "  %s    %s\n" (cdr (car (cdr p))) (cdr (car p))))))

(defun format-notes (x)
  (dolist (p x)
    (progn
      (insert (format "----\n%s\n%s\n" (cdr (nth 3 p)) (cdr (nth 4 p)))))))

(defun frb-save-creds (username password)
  (or frb-username (set-variable frb-username (format "%s" username))) 
  (or frb-username (set-variable frb-password (format "%s" password))))

(global-set-key (kbd "C-c C-f") 'frb-note-region)

(provide 'frb)
