(setq jk/current-book 5)
(setq jk/current-chapter 9)

(defun jk/convert-key-name ()
  (interactive)
  (beginning-of-line)
  (let ((original-position (point)))
    (forward-word)
    (let ((index (thing-at-point 'number)))
      ;;(forward-word)
      (forward-char)
      (let ((key-name (buffer-substring (point) (line-end-position)))
            (root-letter (progn (beginning-of-line)
                                (forward-line)
                                (buffer-substring (point) (line-end-position))))
            (ordine (progn (beginning-of-line)
                           (forward-line)
                           (thing-at-point 'number))))
        (beginning-of-line)
        (forward-line)
        (forward-line)
        (search-forward "(")
        (let ((pdf-position (buffer-substring (1- (point)) (line-end-position)))
              (pdf-page (progn (forward-char)
                               (thing-at-point 'number))))
          (beginning-of-line)
          (forward-line)
          (forward-line)
          (forward-char 2)
          (let ((tags (buffer-substring (point) (line-end-position)))
                (comment (progn
                           (beginning-of-line)
                           (forward-line)
                           (buffer-substring (point) (line-end-position)))))
            (end-of-line)
            (kill-region (point) original-position)
            (insert "(:id " (format "%d" index)
                    "\n:item-type :key "
                    "\n:key-name \"" key-name "\" "
                    "\n:root-letter " (format ":%s " root-letter)
                    ":ordine " (format "%d " ordine)
                    "\n:pdf-page " (format "%s" pdf-page) " "
                    "\n:pdf-position " pdf-position " "
                    "\n:tag-list (" tags ") "
                    "\n:comment \"" comment "\")")
            (indent-region original-position (point))))))))

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

(defun jk/insert-vicentino-interval (index header-line body-string)
  (let* ())
  (insert "(:id " index
          "\n:item-type :interval"
          "\n:interval-name " interval-name
          "\n:interval-group-identity :X"
          "\n:departure " departure
          "\n:destination " destination
          "\n:direction " direction
          "\n:pdf-page " pdf-page
          "\n:pdf-position " pdf-position
          "\n:tag-list (:diplomatic)"
          "\n:comment"))

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
            "\n:comment \"" (substring body-string (+ 6 (cl-search ":END:" body-string))) "\")")))

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
