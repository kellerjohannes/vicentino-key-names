(in-package :key-names)

(read-db)

(format t "~&Databases loaded.")



;; Lists of keys, intervals and notes

(defun generate-lists ()
  (let ((critical-keys (distill-reading *keys* '(:recommended-correction
                                                 :obvious-correction
                                                 :omitted-text
                                                 :diplomatic))))
    (write-list "liste-komplett.tex"
                "Komplettes Inventar"
                "Sämtliche Tasten, Intervalle und Noten in allen Lesarten"
                *keys*
                *keys*)
    (write-list "liste-eingriffe.tex"
                "Inventar sämtlicher Probanden mit alternativen Lesarten"
                "Sämtliche Tasten, Intervalle und Noten, die nicht ausschliesslich eine Lesart haben."
                (remove-unique-items *keys*)
                (remove-unique-items *keys*))
    (write-list "liste-kritisch.tex"
                "Kritisches Inventar"
                "Sämtliche Tasten, Intervalle und Noten in kritischer Lesart (mit dem \\typesetTag{:diplomatic}-tag)."
                critical-keys
                critical-keys
                :resolve-intervals t)
    (write-list "liste-verkuerzungen.tex"
                "Inventar der \\typesetTag{shorthand-Notation}"
                "Sämtliche Probanden mit dem \\typesetTag{:regular-shorthand}-tag."
                (select critical-keys (where #'contains :tag-list :regular-shorthand))
                critical-keys)
    (write-list "liste-verkuerzungen-vergleich.tex"
                "Inventar aller Probanden, die \\typesetTag{shorthand}-Notation kennen"
                "Sämtliche Tastennamen mit \\typesetTag{:note-name} B♯, E♯, Ċ, Ḟ und Cʼ, sortiert nach \\typesetTag{:note-name} und Vorhandensein von \\typesetTag{:regular-shorthand}."
                (sort (unselect (select critical-keys
                                        (where #'member :note-name '(:B♯ :E♯ :Ċ :Ḟ :Cʼ)))
                                (where #'eq :item-type :note))
                      (lambda (a b)
                        (let ((name-a (symbol-name (car a)))
                              (name-b (symbol-name (car b)))
                              (tag-a (symbol-name (cdr a)))
                              (tag-b (symbol-name (cdr b))))
                          (if (string= name-a name-b)
                              (string< tag-a tag-b)
                              (string< name-a name-b))))
                      :key (lambda (item)
                             (cons (getf item :note-name)
                                   (if (member :regular-shorthand (getf item :tag-list))
                                       :b :a))))
                critical-keys)))


;; Spreadsheets of interval sizes

(defun generate-spreadsheets ()
  (let ((critical-keys (distill-reading *keys* '(:recommended-correction
                                                 :obvious-correction
                                                 :omitted-text
                                                 :diplomatic))))
    (write-spreadsheet "tab-tuning1-alle-intervalle-nach-groesse.tex"
                       "Inventar aller Intervalle"
                       "Sämtliche Intervalle, nach ihrer Grösse in \\typesetTag{:tuning1} sortiert."
                       :tuning1
                       (sort (select critical-keys (where #'eq :item-type :interval))
                             (lambda (a b)
                               (let ((size-a (get-interval-size a :tuning1 critical-keys))
                                     (size-b (get-interval-size b :tuning1 critical-keys))
                                     (direction-a (getf a :direction))
                                     (direction-b (getf b :direction)))
                                 (cond ((= size-a size-b) (string> (symbol-name direction-a)
                                                                   (symbol-name direction-b)))
                                       (t (< size-a size-b))))))
                       critical-keys)
    (write-spreadsheet "tab-tuning1-alle-terzen.tex"
                       "Inventar aller Intervalle in der Gruppe \\typesetTag{:terza}"
                       "Sämtliche Intervalle, die die Gruppenzugehörigkeit \\typesetTag{:terza} haben, nach Grösse in
Stimmung \\typesetTag{:tuning1} und nach Richtung sortiert."
                       :tuning1
                       (sort (select critical-keys (where #'eq :interval-group-identity :terza))
                             (lambda (a b)
                               (let ((size-a (get-interval-size a :tuning1 critical-keys))
                                     (size-b (get-interval-size b :tuning1 critical-keys))
                                     (id-a (getf a :id))
                                     (id-b (getf b :id))
                                     (direction-a (getf a :direction))
                                     (direction-b (getf b :direction)))
                                 (cond ((and (= size-a size-b)
                                             (eq direction-a direction-b))
                                        (< id-a id-b))
                                       ((= size-a size-b)
                                        (string> (symbol-name direction-a) (symbol-name direction-b)))
                                       (t (< size-a size-b))))))
                       critical-keys)
    (write-spreadsheet "tab-tuning1-alle-quinten.tex"
                       "Inventar aller Intervalle in der Gruppe \\typesetTag{:quinta}"
                       "Sämtliche Intervalle, die die Gruppenzugehörigkeit \\typesetTag{:quinta} haben, nach Grösse in
Stimmung \\typesetTag{:tuning1} und nach Richtung sortiert."
                       :tuning1
                       (sort (select critical-keys (where #'eq :interval-group-identity :quinta))
                             (lambda (a b)
                               (let ((size-a (get-interval-size a :tuning1 critical-keys))
                                     (size-b (get-interval-size b :tuning1 critical-keys))
                                     (id-a (getf a :id))
                                     (id-b (getf b :id))
                                     (direction-a (getf a :direction))
                                     (direction-b (getf b :direction)))
                                 (cond ((and (= size-a size-b)
                                             (eq direction-a direction-b))
                                        (< id-a id-b))
                                       ((= size-a size-b)
                                        (string> (symbol-name direction-a) (symbol-name direction-b)))
                                       (t (< size-a size-b))))))
                       critical-keys)
    (write-spreadsheet "tab-tuning1-alle-sexten.tex"
                       "Inventar aller Intervalle in der Gruppe \\typesetTag{:sesta}"
                       "Sämtliche Intervalle, die die Gruppenzugehörigkeit \\typesetTag{:sesta} haben, nach Grösse in
Stimmung \\typesetTag{:tuning1} und nach Richtung sortiert."
                       :tuning1
                       (sort (select critical-keys (where #'eq :interval-group-identity :sesta))
                             (lambda (a b)
                               (let ((size-a (get-interval-size a :tuning1 critical-keys))
                                     (size-b (get-interval-size b :tuning1 critical-keys))
                                     (id-a (getf a :id))
                                     (id-b (getf b :id))
                                     (direction-a (getf a :direction))
                                     (direction-b (getf b :direction)))
                                 (cond ((and (= size-a size-b)
                                             (eq direction-a direction-b))
                                        (< id-a id-b))
                                       ((= size-a size-b)
                                        (string> (symbol-name direction-a) (symbol-name direction-b)))
                                       (t (< size-a size-b))))))
                       critical-keys)
    (write-spreadsheet "tab-tuning1-alle-oktaven.tex"
                       "Inventar aller Intervalle in der Gruppe \\typesetTag{:ottava}"
                       "Sämtliche Intervalle, die die Gruppenzugehörigkeit \\typesetTag{:ottava} haben, nach Grösse in
Stimmung \\typesetTag{:tuning1} und nach Richtung sortiert."
                       :tuning1
                       (sort (select critical-keys (where #'eq :interval-group-identity :ottava))
                             (lambda (a b)
                               (let ((size-a (get-interval-size a :tuning1 critical-keys))
                                     (size-b (get-interval-size b :tuning1 critical-keys))
                                     (id-a (getf a :id))
                                     (id-b (getf b :id))
                                     (direction-a (getf a :direction))
                                     (direction-b (getf b :direction)))
                                 (cond ((and (= size-a size-b)
                                             (eq direction-a direction-b))
                                        (< id-a id-b))
                                       ((= size-a size-b)
                                        (string> (symbol-name direction-a) (symbol-name direction-b)))
                                       (t (< size-a size-b))))))
                       critical-keys)
    (write-spreadsheet "tab-tuning1-alle-semitoni-und-kleiner.tex"
                       "Inventar aller Intervalle in der Gruppe \\typesetTag{:semitono} und \\typesetTag{:diesis}"
                       "Sämtliche Intervalle, die die Gruppenzugehörigkeit \\typesetTag{:semitono} oder \\typesetTag{:diesis} haben, nach Grösse in
Stimmung \\typesetTag{:tuning1} und nach Richtung sortiert."
                       :tuning1
                       (sort (select critical-keys (where #'member
                                                          :interval-group-identity
                                                          '(:semitono :diesis)))
                             (lambda (a b)
                               (let ((size-a (get-interval-size a :tuning1 critical-keys))
                                     (size-b (get-interval-size b :tuning1 critical-keys))
                                     (id-a (getf a :id))
                                     (id-b (getf b :id))
                                     (direction-a (getf a :direction))
                                     (direction-b (getf b :direction)))
                                 (cond ((and (= size-a size-b)
                                             (eq direction-a direction-b))
                                        (< id-a id-b))
                                       ((= size-a size-b)
                                        (string> (symbol-name direction-a) (symbol-name direction-b)))
                                       (t (< size-a size-b))))))
                       critical-keys)

    ;; Analoge liste mit :recommended-correction anlegen
    ))




;; Entry point

(defun generate-all ()
  (read-db)
  (generate-lists)
  (generate-spreadsheets))
