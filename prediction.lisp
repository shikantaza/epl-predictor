(in-package #:epl-predictor)

(defun generate-predictors (ht at &optional params)

  (assert (and (not (null ht)) (not (null at))))

  (if (not (null params))
      (loop for x in params collect (cond ((equal x "A") (get-record ht 'home))
                                          ((equal x "B") (get-record at 'away))
                                          ((equal x "C") (get-record ht))
                                          ((equal x "D") (get-record at))
                                          ((equal x "E") (get-record ht 'all 3))
                                          ((equal x "F") (get-record at 'all 3))
                                          ((equal x "G") (- (get-goals ht 'for) (get-goals ht 'against)))
                                          ((equal x "H") (- (get-goals at 'for) (get-goals at 'against)))
                                          ((equal x "I") (get-rank ht))
                                          ((equal x "J") (get-rank at))
					  (t (error (format nil "Unknown parameter: ~A" x)))))
    (list (get-record ht 'home)
          (get-record at 'away)
          (get-record ht)
          (get-record at)
          (get-record ht 'all 3)
          (get-record at 'all 3)
          (- (get-goals ht 'for) (get-goals ht 'against))
          (- (get-goals at 'for) (get-goals at 'against))
          (get-rank ht)
          (get-rank at))
    ))

(defun convert-response-to-result (y)
  (cond (                 (<= y 0.333)  1)
        ((and (> y 0.333) (<= y 0.667)) 2)
        (     (> y 0.667)               3)))

(defun display-prediction (prediction)
  (let* ((ht-name (get-team-name (first (first prediction))))
         (at-name (get-team-name (second (first prediction)))) 
         (result (cond ((eq (second prediction) 1) ht-name)
                       ((eq (second prediction) 2) "Draw")
                       ((eq (second prediction) 3) at-name))))
    (format t "~a vs ~a -- ~a~%" ht-name at-name result)))

(defun display-predictions (predictions)
  (dolist (prediction predictions)
    (display-prediction prediction)))

(defun predictions-for-week (week fn &rest args)
  (loop for x in (nth (1- week) fixtures) collect (list x (apply fn (append (list x) args)))))

;TODO -- doesn't work for cases where generate-training-data needs to 
;be passed the optional boolean parameter
(defun predict-for-week (week fn &rest args)
  (let ((tr))
    (unwind-protect
        (progn
          (load-all-data)
          (setf tr (generate-training-data 4 (1- week)))
          (load-matches-upto (1- week))
          (display-predictions (apply #'predictions-for-week (append (list week fn) args (list tr)))))
      (clean-all-data))))

(defun match-result (m)
  (cond ((>  (get-home-goals m) (get-away-goals m)) 1)
        ((eq (get-home-goals m) (get-away-goals m)) 2)
        (t                                          3)))
 
(defun accuracy-of-predictions (predictions)
  (let ((correct-predictions 0))
    (dolist (prediction predictions)
      (let ((ht (first (first prediction))) (at (second (first prediction))))
        (if (eq (match-result (find-match (list ht at))) (second prediction))
            (setf correct-predictions (1+ correct-predictions)))))
    (/ correct-predictions (length predictions) 1.0))) ;1.0 to force evaluation as float

;predict using random numbers
(defun predict-random (training-data fixture)
  (convert-response-to-result (random 1.0)))

;homegrown prediction based on home/away, complete form and recent form
(defun predict (fixture training-data)
  (let* ((ht-score 1) ;home advantage
         (at-score 0)
         (ht (first fixture))
         (at (second fixture))
         (ht-full-stats (get-stats ht))
         (at-full-stats (get-stats at))
         (ht-recent-stats (get-stats ht nil 3))
         (at-recent-stats (get-stats at nil 3)))
    (if (> (nth 8 ht-full-stats) (nth 8 at-full-stats))
        (setf ht-score (1+ ht-score))
      (if (< (nth 8 ht-full-stats) (nth 8 at-full-stats))
          (setf at-score (1+ at-score))))
    (if (> (nth 8 ht-recent-stats) (nth 8 at-recent-stats))
        (setf ht-score (1+ ht-score))
      (if (< (nth 8 ht-recent-stats) (nth 8 at-recent-stats))
          (setf at-score (1+ at-score))))
    (cond ((> ht-score at-score) 1)
          ((< ht-score at-score) 3)
          (t                     4))))

;prediction using linear regression model
(defun predict-glm (fixture training-data &optional predictor-params)
  (let ((predictors (generate-predictors (first fixture) (second fixture) predictor-params))
        (glm-coeffs))
    (multiple-value-bind (x y)
        (extract-predictors-and-responses training-data)
      (setf glm-coeffs (flatten (glm x y))))
    (convert-response-to-result (+ (nth 0 glm-coeffs) (dot-product predictors (rest glm-coeffs))))))

;prediction using k-nearest-neighbours algorithm
(defun predict-knn (fixture k average-p training-data &optional predictor-params)
  (if average-p
      (convert-response-to-result (classify-knn-avg (generate-predictors (first fixture) (second fixture) predictor-params) training-data k))
    (classify-knn (generate-predictors (first fixture) (second fixture) predictor-params) training-data k)))

;homegrown prediction that uses the historical (current season only)
;fractions of home wins, draws and away wins
(defun predict-breakup-fractions (fixture training-data)
  (let ((h (breakup-fraction 'home-wins))
        (d (breakup-fraction 'draws))
        (r (random 1.0)))
    (cond ((<= r h)                        1)
          ((and (> r h) (<= r (+ h d)))    2)
          ((> r (+ h d))                   3))))

