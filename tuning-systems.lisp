(in-package :key-names)

;; TODO load :vicentino-tunings and import necessary functions

(defparameter *tuning-1* (vicentino-tunings::pitch-fun (vicentino-tunings::meantone -1/4) :wolf-Ė-Ḃ♮))
(defparameter *tuning-2* (vicentino-tunings::pitch-fun (vicentino-tunings::meantone -1/3) :wolf-Ė-Ḃ♮))

(defun calculate-interval-size (departure-name destination-name direction pitch-fun)
  (let ((departure-interval (pitch pitch-fun departure-name))
        (destination-interval (pitch pitch-fun destination-name)))
    (expt (cond ((eq direction :up)
            (if (> destination-interval departure-interval)
                (simplify (/ destination-interval departure-interval))
                (simplify (/ (* 2/1 destination-interval departure-interval)))))
           ((eq direction :down)
            (if (< destination-interval departure-interval)
                (simplify (/ destination-interval departure-interval))
                (simplify (/ (* 1/2 destination-interval) departure-interval))))
           (t 1/1))
          (cond ((eq direction :up) 1)
                ((eq direction :down) -1)
                (t 0)))))
