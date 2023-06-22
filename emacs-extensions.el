(setq jk/current-book 5)
(setq jk/current-chapter 8)

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



 (:id 10  :category :key      :libro 5 :chapter 8 :folio (106 . :recto) :original-name "Dsolre secondo" :root :d :ordine 2 :flag :diplomatic :comment "")


(:id 11  :category :interval :libro 5 :chapter 8 :folio (106 . :recto) :original-name "sesta maggiore" :departure 1 :destination 12 :direction :down :group :sesta :flag :diplomatic :comment "")
