;; These are an example servlets for the Emacs web server.

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
  (let ((buffer (get-buffer (cadar uri-query))))
    (insert-buffer-substring buffer)
    "text/plain"))

(defun httpd/switch-buffer(uri-query req uri-path)
  "Switch to the requested buffer."
  (let ((buffer (get-buffer (cadar uri-query)))
	(cb (current-buffer)))
    (switch-to-buffer buffer)
    (set-buffer cb)
    (httpd/list-buffers uri-query req uri-path)))
