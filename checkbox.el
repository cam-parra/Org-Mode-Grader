(require 'dash)
(require 'org-element)

(defun check-hook-fn ()
  (when (-contains? (org-element-property
		     :attr_org
		     (org-element-property :parent (org-element-context)))
		    ":radio")
    (save-excursion
      (loop for el in (org-element-property :structure (org-element-context))
	    do
	    (goto-char (car el))
	    (when (re-search-forward "\\[X\\]" (line-end-position) t)
	      (replace-match "[ ]"))))
    (forward-char)
    (insert "X")
    (delete-char 1)))

(add-hook 'org-checkbox-statistics-hook 'check-hook-fn)

(defun org-get-plain-list (name)
  "Get the org-element representation of a plain-list with NAME."
  (catch 'found
    (org-element-map
	(org-element-parse-buffer)
	'plain-list
      (lambda (plain-list)
	(when
	    (string= name (org-element-property :name plain-list))
	  (throw 'found plain-list))))))

(defun get-radio-list-value (list-name)
  "Return the value of the checked item in a radio list."
  (save-excursion
    (loop for el in (org-element-property
		     :structure
		     (org-get-plain-list list-name))
	  if (string= (nth 4 el) "[X]")
	  collect (progn
		   (let ((item (buffer-substring (car el) (car (last el)))))
		     (string-match "\\[X\\]\\(.*\\)$" item)
		     (match-string 1 item))))))

(get-radio-list-value "test")
