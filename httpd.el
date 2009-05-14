;;; httpd.el -- HTTP/1.0 web server for emacs
;;
;; Copyright (c) 2009 Christopher Wellons <mosquitopsu@gmail.com>
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
;;
;;; Commentary:
;;
;; Load this file and run httpd-start to start the web server. The
;; variable httpd-root changes the server's root folder, and
;; httpd-port adjusts the port.
;;
;;; TODO:
;;
;; * Directory listing
;; * / -> /index.html
;; * .htaccess.el
;; * Dynamic .el

(defvar httpd-port 8080
  "Web server port.")

(defvar httpd-root "~/public_html"
  "Web server file root.")

(defvar httpd-mime-types
  '(("png"  . "image/png")
    ("gif"  . "image/gif")
    ("jpg"  . "image/jpg")
    ("css"  . "text/css")
    ("htm"  . "text/html")
    ("html" . "text/html")
    ("txt"  . "text/plain"))
  "MIME types for headers")

(defvar httpd-indexes
  '("index.html"
    "index.htm")
  "File served by default when accessing a directory.")

(defvar httpd-status-codes
  '((202 . "OK")
    (403 . "Forbidden")
    (500 . "Internal Server Error")
    (404 . "Not Found"))
  "HTTP status codes")

(defvar httpd-html
  '((404 . "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>404 Not Found</title>
</head><body>
<h1>Not Found</h1>
<p>The requested URL was not found on this server.</p>
</body></html>")
    (403 . "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>403 Forbidden</title>
</head><body>
<h1>Forbidden</h1>
<p>The requested URL is forbidden.</p>
</body></html>"))
  "HTML for various errors.")

(defun httpd-start ()
  "Start the emacs web server."
  (interactive)
  (httpd-stop)
  (httpd-clear-log)
  (httpd-log-string "'(log)\n")
  (httpd-log-alist `(start ,(current-time-string)))
  (make-network-process
   :name     "httpd"
   :service  httpd-port
   :server   t
   :family   'ipv4
   :filter   'httpd-filter))

(defun httpd-stop ()
  "Stop the emacs web server."
  (interactive)
  (if (process-status "httpd") (delete-process "httpd"))
  (httpd-log-alist `(stop ,(current-time-string))))

(defun httpd-log-string (string)
  "Add string to the web server log."
  (with-current-buffer (get-buffer-create "*httpd*")
    (goto-char (point-max))
    (insert string)))

(defun httpd-log-alist (item &optional sp)
  "Add alist to the log."
  (if (not sp) (setq sp 2))
  (with-current-buffer (get-buffer-create "*httpd*")
    (goto-char (- (point-max) 2))
    (insert "\n" (make-string sp 32))
    (if (atom (cadr item)) (insert (format "%S" item))
      (insert "(" (symbol-name (car item)))
      (dolist (el (cdr item))
	(httpd-log-alist el (+ 1 sp)))
      (insert ")"))))

(defun httpd-clear-log ()
  "Clear the web server log."
  (with-current-buffer (get-buffer-create "*httpd*")
    (erase-buffer)))

(defun httpd-filter (proc string)
  "Runs each time client makes a request."
  (let* ((log '(connection))
	 (req (httpd-parse string))
	 (get (cadr (assoc "GET" req)))
	 (path (httpd-gen-path get))
	 (status (httpd-status path)))
    (setq log (list 'connection
		    `(date ,(current-time-string))
		    `(address ,(car (process-contact proc)))
		    `(get ,get)
		    (append '(req) req)
		    `(path ,path)
		    `(status ,status)))
    (httpd-log-alist log)
    (if (not (= status 200)) (httpd-error proc status)
      (httpd-send-header proc (httpd-get-mime (httpd-get-ext path)) status)
      (httpd-send-file proc path))))

(defun httpd-parse (string)
  "Parse client http header into alist."
  (let* ((lines (split-string string "\n\r?"))
	 (req (list (split-string (car lines)))))
    (dolist (line (cdr lines))
      (push (list (car (split-string line ": "))
		  (mapconcat 'identity
			     (cdr (split-string line ": ")) ": ")) req))
    (cddr req)))

(defun httpd-status (path)
  "Determine status code for the path."
  (cond
   ((not (file-exists-p path))   404)
   ((not (file-readable-p path)) 403)
   ((file-directory-p path)      403)
   (200)))

(defun httpd-gen-path (path)
  "Translate GET to secure path in httpd-root."
  (let ((path (httpd-clean-path (concat httpd-root path)))
	(indexes (copy-list httpd-indexes))
	(testpath nil))
    (if (not (file-directory-p path)) path
      (while (not (or (null indexes)
		      (and testpath (file-exists-p testpath))))
	(setq testpath (concat path "/" (pop indexes))))
      (if (file-exists-p testpath) testpath path))))

(defun httpd-send-file (proc path)
  "Serve file to the given client."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents path)
    (httpd-send-buffer proc (current-buffer))))

(defun httpd-clean-path (path)
  "Clean dangerous .. from the path."
  (mapconcat 'identity 
	     (delete ".." (split-string path "\\/")) "/"))

(defun httpd-get-ext (path)
  "Get extention from path to determine MIME type."
  (car (reverse (split-string path "\\."))))

(defun httpd-get-mime (ext)
  "Fetch MIME type given the file extention."
  (cdr (assoc ext httpd-mime-types)))

(defun httpd-send-header (proc mime status)
  "Send header with given MIME type."
  (let ((status-str (cdr (assq status httpd-status-codes))))
    (process-send-string
     proc (format "HTTP/1.0 %d %s\nContent-Type: %s\n\n"
		  status status-str mime))))

(defun httpd-error (proc status)
  "Handle an error situation."
  (httpd-send-header proc "text/html" status)
  (httpd-send-string proc (cdr (assq status httpd-html))))

(defun httpd-send-string (proc string)
  "Send string to client."
  (process-send-string proc string)
  (process-send-eof proc))

(defun httpd-send-buffer (proc buffer)
  "Send buffer to client."
  (with-current-buffer buffer  
    (httpd-send-string proc (buffer-substring (point-min)
					      (point-max)))))
