(in-package :vicentino-key-names)

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
                "Sämtliche Tasten, Intervalle und Noten, die nicht ausschliesslich eine einzige Lesart haben. Dies reflektiert keine Entscheidungen, sondern zeigt alle denkbaren Alternativen."
                (remove-unique-items *keys*)
                *keys*)
    (write-list "liste-kritisch.tex"
                "Kritisches Inventar"
                "Sämtliche Tasten, Intervalle und Noten in kritischer Lesart (mit den Tags \\typesetTag{:diplomatic}, \\typesetTag{:obvious-correction}, \\typesetTag{:recommended-correction} und \\typesetTag{:omitted-text})."
                critical-keys
                critical-keys
                :resolve-intervals t)
    (write-list "liste-kritische-eingriffe.tex"
                "Kritisches Inventar aller Eingriffe"
                "Sämtliche Tasten, Intervalle und Noten, die in der kritischen Lesart korrigiert werden (mit den Tags \\typesetTag{:obvious-correction}, \\typesetTag{:recommended-correction} und \\typesetTag{:omitted-text})."
                (unselect critical-keys (where #'contains :tag-list :diplomatic))
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
Stimmung \\typesetTag{:tuning1} und nach Grösse sortiert."
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
    (write-spreadsheet "tab-tuning3-alle-intervalle-nach-groesse.tex"
                       "Inventar aller Intervalle"
                       "Sämtliche Intervalle, nach ihrer Grösse in \\typesetTag{:tuning3} sortiert."
                       :tuning3
                       (sort (select critical-keys (where #'eq :item-type :interval))
                             (lambda (a b)
                               (let ((size-a (get-interval-size a :tuning3 critical-keys))
                                     (size-b (get-interval-size b :tuning3 critical-keys))
                                     (direction-a (getf a :direction))
                                     (direction-b (getf b :direction)))
                                 (cond ((= size-a size-b) (string> (symbol-name direction-a)
                                                                   (symbol-name direction-b)))
                                       (t (< size-a size-b))))))
                       critical-keys)
    (write-spreadsheet "tab-tuning3-alle-terzen.tex"
                       "Inventar aller Intervalle in der Gruppe\\typesetTag{:terza}"
                       "Sämtliche Intervalle, die die Gruppenzugehörigkeit \\typesetTag{:terza} haben, nach Grösse in Stimmung \\typesetTag{:tuning3} und nach Grösse sortiert."
                       :tuning3
                       (sort (select critical-keys (where #'eq :interval-group-identity :terza))
                             (lambda (a b)
                               (let ((size-a (get-interval-size a :tuning3 critical-keys))
                                     (size-b (get-interval-size b :tuning3 critical-keys))
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

    ;; TODO
    ;; - extract all combinations of propinqua/propinquissima, put it in one table and one spreadsheet
    ;; - extract all /quinte perfette/, for Quintenschaukel and for tuning1
    ))



;; New idea: algorithmically list all conceivable interval names; correlate them with all intervals
;; that can be found on the keyboard. Then compare this complete list of playable intervals with the
;; intervals that are specifically mentioned in "L'antica musica", cap. V.8-V.38. Main insight: how
;; complete is Vicentino's interval list, is there a pattern for missing intervals, did he ignore a
;; whole class of intervals?

;; TODO: generate a list of conceivable interval sizes, with unique technical names and Vicentino's
;; own (partly ambiguous) names.

;; TODO: define a data format to represent all available keys (with multiple versions, and
;; extensions, depending on the various interpretations of Vicentino's text.

;; TODO: define a function to cycle through all 2-key-combinations.

;; TODO: define a search algorithm that matches the 2-key-combinations with specific tuning
;; variations and compares these sizes with the complete list of conceivable intervals, using a
;; definable precision window.


;; Need for manually listing all 'propinqui', since they can't be generated algorithmically for
;; intervals smaller than the 'quarta'. All intervals withing the octave need to be listed, because
;; Vicentino includes the 'seste' in his lists of consonances.  To deduce the size of a specific
;; interval, examples of note names need to be given, since they might differ in various tuning
;; versions. This results in a band of possible interval sizes for a given interval name. The
;; selection of note name examples is crucial and might be problematic for specific tuning
;; systems. Therefore it's important to define the first example as the musically most evident
;; occurrance of an interval (for example: for the 'diesis enarmonico minore', C➚Ċ is much more
;; common in Vicentino's text and music than D♯➚E♭).

(defparameter *stem-intervals*
  '((diesis (:c :up :ċ) (:c♯ :up :d♭) (:ḋ♭ :up :d))
    (semitono-minore (:d :up :d♯) (:ė :up :f))
    (semitono-maggiore )))

(defun generate-interval-list (size-of-comma)
  )





;; Entry point

(defun generate-all ()
  (read-db)
  (generate-lists)
  (generate-spreadsheets))
