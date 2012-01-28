;;; httpd.el --- HTTP/1.0 web server for emacs

;; Copyright (C) 2011 Joe Schafer <joe@jschaf.com>

;; This file is not part of GNU Emacs.

;; Foobar is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Foobar is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Load this file and run httpd-start to start the web server. The
;; variable httpd-root changes the server's root folder, and
;; httpd-port adjusts the port.

;;; TODO:
;; * Directory listing

;;; Code:

(require 'cl)

(defgroup httpd nil 
  "Emacs Web Server." 
  :group 'comm)

(defcustom httpd-port 8080
  "Web server port."
  :group 'httpd
  :type 'integer)

(defcustom httpd-root "~/public_html"
  "Web server file root."
  :group 'httpd
  :type 'directory)

;; Causes error during eval: custom-declare-variable: Symbol's function definition is void: choice
;; (defcustom httpd-host 'local
;;   "The host to connect to.  `httpd-host' should be a host name or
;;   IP address or the symbol `local' for the the local
;;   host."
;;   :group 'httpd
;;   :type (choice (string :tag "Host name or IP address")
;;                 (const 'local :tag "Local host")))

(defvar httpd-mime-types
  '(("png"  . "image/png")
    ("gif"  . "image/gif")
    ("jpg"  . "image/jpg")
    ("css"  . "text/css")
    ("htm"  . "text/html")
    ("html" . "text/html")
    ("xml"  . "text/xml")
    ("txt"  . "text/plain"))
  "MIME types for headers")

(defvar httpd-indexes
  '("index.html"
    "index.htm")
  "File served by default when accessing a directory.")

(defvar httpd-htaccess-name ".htaccess.el"
  "Filename for hypertext access files.")

(defvar httpd-status-codes
  '((200 . "OK")
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
</body></html>")
    (500 . "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>500 Internal Error</title>
</head><body>
<h1>500 Internal Error</h1>
<p>Internal error when handling this request.</p>
</body></html>"))
  "HTML for various errors.")

(defun httpd-start ()
  "Start the emacs web server."
  (interactive)
  (httpd-stop)
  (httpd-clear-log)
  (httpd-log-string "'(log)\n")
  (httpd-log-alist `(start ,(current-time-string)))
  (let
      ((proc (make-network-process
	      :name     "httpd"
	      :service  httpd-port
	      :server   t
	      :family   'ipv4
	      :filter   'httpd-filter)))
    (process-put proc :httpd-root httpd-root)
    proc))


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
  (let* ((httpd-root (process-get proc :httpd-root))
	 (log '(connection))
	 (req (httpd-parse string))
	 (uri (cadr (assoc "GET" req)))
	 (parsed-uri (httpd-parse-uri uri))
	 (uri-path (nth 0 parsed-uri))
	 (uri-query (nth 1 parsed-uri))
	 (uri-target (nth 2 parsed-uri))
	 (servlet (httpd-get-servlet uri-path))
	 (path (httpd-gen-path uri-path))
	 (status (httpd-status path)))
    (setq log (list 'connection
		    `(date ,(current-time-string))
		    `(address ,(car (process-contact proc)))
		    `(get ,uri-path)
		    (append '(req) req)
		    `(servlet ,(format "%S" servlet))
		    `(path ,path)
		    `(status ,status)))
    (httpd-log-alist log)
    (cond
     (servlet (httpd-handle-servlet proc servlet uri-query req uri-path))
     ((not (= status 200)) (httpd-error proc status))
     (t (httpd-send-header proc (httpd-get-mime (httpd-get-ext path)) status)
	(httpd-send-file proc path)))))

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
  (let ((path (httpd-clean-path (concat httpd-root path))))
    (httpd-htaccess (httpd-path-base path))
    (let ((indexes httpd-indexes)
	  (testpath nil))
      (if (not (file-directory-p path)) path
	(while (not (or (null indexes)
			(and testpath (file-exists-p testpath))))
	  (setq testpath (concat path "/" (pop indexes))))
	(if (file-exists-p testpath) testpath path)))))

(defun httpd-path-base (path)
  "Return the directory base of the path."
  (if (file-directory-p path) path
    (let ((pathlist (split-string path "\\/")))
      (mapconcat 'identity (butlast pathlist) "/"))))

(defun httpd-htaccess (path)
  "Load a hypertext access file."
  (let ((htaccess (concat path "/" httpd-htaccess-name)))
    (if (and (file-exists-p htaccess) (file-readable-p htaccess))
	(load-file htaccess))))

(defun httpd-send-file (proc path)
  "Serve file to the given client."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents path)
    (httpd-send-buffer proc (current-buffer))))

(defun httpd-clean-path (path)
  "Clean dangerous .. from the path."
  (mapconcat 'identity
	     (delete ".." (split-string (url-unhex-string path) "\\/")) "/"))

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

(defun httpd-parse-uri (uri)
  "Split a URI into it's components. In the return, the first
element is the script path, the second is an alist of
variable/value pairs, and the third is the target."
  (let ((p1 (string-match (regexp-quote "?") uri))
	(p2 (string-match (regexp-quote "#") uri))
	retval)
    (push (if p2 (url-unhex-string (substring uri (1+ p2))))
	  retval)
    (push (if p1 (mapcar #'(lambda (str)
			     (mapcar 'url-unhex-string (split-string str "=")))
			 (split-string (substring uri (1+ p1) p2) "&")))
	  retval)
    (push (substring uri 0 (or p1 p2))
	  retval)))

(defun httpd-get-servlet (uri-path)
  "Return function for given path if it exists."
  (let ((func (intern-soft (concat "httpd" uri-path))))
    (if (fboundp func) func)))

(defun httpd-handle-servlet (proc servlet uri-query req uri-path)
  "Execute the given servlet and handle any errors."
  (with-temp-buffer
    (condition-case nil
	(let (mimetype)
	  (setq mimetype (funcall servlet uri-query req uri-path))
	  (set-buffer-multibyte nil)
	  (httpd-send-header proc mimetype 200)
	  (httpd-send-buffer proc (current-buffer)))
      (error (httpd-error proc 500)))))

(defun httpd-generate-html (sexp)
  "Generate HTML from the given sexp. Tags are based on symbol
names, like 'html, 'head. The elisp symbol begins a section of
lisp to be executed, and the results used to generate
HTML. Strings are passed literally."
  (let ((tag (if (consp sexp) (car sexp))))
    (cond
     ((stringp sexp)
      (insert sexp))
     ((eq tag 'elisp)
      (mapcar 'httpd-generate-html (eval (cadr sexp))))
     ((symbolp tag)
      (insert (format "<%s>\n" tag))
      (mapc 'httpd-generate-html (cdr sexp))
      (insert (format "</%s>\n" tag)))
     ((listp tag)
      (insert (format "<%s " (car tag))
	      (mapconcat
	       #'(lambda (pair)
		   (format "%s=\"%s\"" (car pair) (cdr pair)))
	       (cdr tag) " ") ">")
      (mapc 'httpd-generate-html (cdr sexp))
      (insert (format "</%s>\n" (car tag))))))
  "text/html")


; ---------
; some file utility functions
; used by httpd/list-files

(defun httpd-file-p (dir item)
  "Is /dir/item a file?"
  (not (file-directory-p (concat dir item))))

(defun httpd-last-ch (str)
  "Return last char in string"
  (aref str (1- (length str)))
  )

(defun httpd-dir-needs-term-p (dir)
  "Does dir path need trailing '/'?"
  (not (char-equal ?/ (httpd-last-ch dir)))
  )

(defun httpd-keep-files (dir dir-list)
  "Process dir-list, return list of just files"
  (cond
	; end of list signal
   ((not dir-list) nil)

	; if file keep and recurse
   ((httpd-file-p dir (car dir-list))  
	(cons (car dir-list) (httpd-keep-files dir (cdr dir-list))))

	; default - don't keep folders
   (t (httpd-keep-files dir (cdr dir-list)))
   ) ;end cond scope
  )

(defun httpd-file-list (dir)
  "Return list of files in dir"
  (if (httpd-dir-needs-term-p dir) ; add trailing '/' if needed
	  (setq dir (file-name-as-directory dir)))

  (let ((dir-list (directory-files dir nil nil t)))
	(httpd-keep-files dir dir-list)
	))


(provide 'httpd)
