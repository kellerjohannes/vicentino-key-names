(defun jk/extract-pdf-coordinates (body-string)
  (let* ((left-par (cl-search "(" body-string))
         (right-par (cl-search ")" body-string))
         (space (cl-search " " body-string :start2 left-par))
         (cons-dot (cl-search " . " body-string :start2 left-par))
         (pdf-page (substring body-string (1+ left-par) space))
         (pdf-position (if (= space cons-dot)
                           (substring body-string (+ 3 cons-dot) right-par)
                         (format "(%s)" (substring body-string (1+ space) right-par)))))
    (cons pdf-page pdf-position)))

(defun jk/extract-comment (body-string)
  (if (cl-search ":PROPERTIES:" body-string)
      (substring body-string (+ 6 (cl-search ":END:" body-string)))
    body-string))

(defun jk/extract-interval-info (header-line)
  (let* ((direction-position (or (cl-search "➚" header-line)
                                 (cl-search "➘" header-line)))
         (departure (substring header-line (cl-search " " header-line
                                                      :start2 direction-position
                                                      :from-end t)
                               direction-position))
         (destination (substring header-line (1+ direction-position)))
         (direction (substring header-line direction-position (1+ direction-position))))
    (list departure direction destination)))

(defun jk/insert-vicentino-interval (index header-line body-string)
  (let ((interval-info (jk/extract-interval-info header-line))
        (pdf-coordinates (jk/extract-pdf-coordinates body-string))
        (interval-name (substring header-line 0 (cl-search ", " header-line))))
    (insert "(:id " index
            "\n:item-type :interval"
            "\n:interval-name " interval-name
            "\n:interval-group-identity :X"
            "\n:departure " (car interval-info)
            "\n:destination " (caddr interval-info)
            "\n:direction " (cadr interval-info)
            "\n:pdf-page " (car pdf-coordinates)
            "\n:pdf-position " (cdr pdf-coordinates)
            "\n:tag-list (:diplomatic)"
            "\n:comment \"" (jk/extract-comment body-string) "\")\n")))

(defun jk/insert-vicentino-keyname (index header-line body-string)
  (let ((pdf-coordinates (jk/extract-pdf-coordinates body-string)))
    (insert "(:id " index
            "\n:item-type :key "
            "\n:key-name \"" header-line "\" "
            "\n:root-letter :X"
            "\n:ordine X"
            "\n:pdf-page " (car pdf-coordinates)
            "\n:pdf-position " (cdr pdf-coordinates)
            "\n:tag-list (:diplomatic) "
            "\n:comment \"" (jk/extract-comment body-string) "\")\n")))

(defun jk/insert-vicentino-notename (index header-line body-string)
  (insert (format "\nindex: %s\nheader-line: %s\nentry-string: %s\n\n"
                  index
                  header-line
                  body-string)))

(defun jk/convert-vicentino-item ()
  (interactive)
  (beginning-of-line)
  (let ((original-position (point))
        origin-position index header-line first-line-position first-line body-string)
    (forward-word)
    (setq index (thing-at-point 'word))
    (forward-char)
    (setq origin-position (point))
    (setq header-line (buffer-substring (point) (line-end-position)))
    (next-line)
    (setq first-line-position (line-beginning-position))
    (setq first-line (buffer-substring first-line-position (line-end-position)))
    (search-forward "***")
    (beginning-of-line)
    (setq body-string (buffer-substring first-line-position (1- (point))))
    (end-of-buffer)

    (let ((title-contains-arrow-p (or (cl-search "➚" header-line)
                                      (cl-search "➘" header-line)))
          (properties-exist-p (cl-search ":PROPERTIES:" first-line)))

      (cond (title-contains-arrow-p (jk/insert-vicentino-interval index header-line body-string))
            (properties-exist-p (jk/insert-vicentino-keyname index header-line body-string))
            (t (jk/insert-vicentino-notename index header-line body-string))))

    (goto-char original-position)
    ))
