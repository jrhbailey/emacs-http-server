;;; httpd-buffers.el -- example servlets for the Emacs web server
;;
;; Copyright (c) 2009 Chunye Wang
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defun httpd/list-buffers (uri-query req uri-path)
  "List all buffers as a web page."
  (httpd-generate-html
   '(html
     (head
      (title "emacs-httpd: list-buffers"))
     (body
      (h1 "buffer list")
      (ul
       (elisp
	(mapcar
	 #'(lambda (b)
	     `(li ((a (href .
			    ,(format "/show-buffer?buffer=%s"
				     (url-hexify-string (buffer-name b)))))
		   ,(buffer-name b))))
	 (buffer-list))))))))

(defun httpd/show-buffer (uri-query req uri-path)
  "Output the contents of the requested buffer inside HTML."
  (let ((buffer (get-buffer (cadr (assoc "buffer" uri-query)))))
    (insert-buffer-substring buffer)
    "text/plain"))

(defun httpd/switch-buffer(uri-query req uri-path)
  "Switch to the requested buffer."
  (let ((buffer (get-buffer (cadar uri-query)))
	(cb (current-buffer)))
    (switch-to-buffer buffer)
    (set-buffer cb)
    (httpd/list-buffers uri-query req uri-path)))
