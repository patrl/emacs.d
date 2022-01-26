(defun patrl/avy-indent (char)
  (interactive (list (read-char "char: " t)))
  (save-excursion
    (beginning-of-line 0)
    (let*
	((pos
	 (avy-with avy-goto-char
	   (avy-jump
	    (regexp-quote (string char))
	    :beg (line-beginning-position)
	    :end (line-end-position))))
	 (l (length (buffer-substring (line-beginning-position) (point)))))
      (message "indent by %s" l) ;; print the number of white spaces to insert
      (when pos
	(beginning-of-line 2)
	(re-search-forward "[^\s]" nil t)
	(backward-char)
	(delete-region (line-beginning-position) (point))

	(insert (mapconcat #'identity (make-list l " ") "")))
      )
    ))
