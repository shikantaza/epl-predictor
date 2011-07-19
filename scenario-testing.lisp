(in-package #:epl-predictor)

(defmacro my-unwind-protect (&body body)
  `(unwind-protect (progn (load-all-data) ,@body) (clean-all-data)))

(defun test (prediction-week training-data-start-week training-data-end-week k print-p &optional classes-p)
  
  (let ((training-data1) (training-data2) 
        (predictions-homegrown1) (predictions-homegrown2) (predictions-random) (predictions-glm) (predictions-knn) 
        (p1) (p2) (p3) (p4) (p5))
    
    (unwind-protect
        (progn
    
          (load-all-data)
    
          (setf training-data1 (generate-training-data training-data-start-week training-data-end-week ))
          (setf training-data2 (generate-training-data training-data-start-week training-data-end-week classes-p))
    
          (load-matches-upto (1- prediction-week))
    
          (setf predictions-homegrown1 (predictions-for-week prediction-week #'predict training-data1))
          (setf predictions-homegrown2 (predictions-for-week prediction-week #'predict-breakup-fractions training-data1))          
          (setf predictions-random (predictions-for-week prediction-week #'predict-random training-data1))
          (setf predictions-glm (predictions-for-week prediction-week #'predict-glm training-data1))
    
          (setf predictions-knn (predictions-for-week prediction-week #'predict-knn k (not classes-p) training-data2))
    
          (load-matches prediction-week)
    
          (setq p1 (accuracy-of-predictions predictions-homegrown1)
              p2 (accuracy-of-predictions predictions-homegrown2)
              p3 (accuracy-of-predictions predictions-random)
              p4 (accuracy-of-predictions predictions-glm)
              p5 (accuracy-of-predictions predictions-knn))
          
          (if print-p
              (progn
                (format t "Accuracy of predictions:~%")
                (format t "Homegrown algorithm (1): ~f~%" p1)
                (format t "Homegrown algorithm (2): ~f~%" p2)
                (format t "Random                 : ~f~%" p3)
                (format t "Linear model           : ~f~%" p4)
                (if classes-p (format t "KNN (vote)             :") (format t "KNN (average)          :"))
                (format t " ~f~%" p5)))
      
          (list p1 p2 p3 p4 p5))
      
      (clean-all-data))))

;determine the correlation between
;some ranking function and match result
;e.g. the ranking function could be total number of points,
;recent record, total number of goals scored, etc.
(defun correlate-parameter (week fn &rest args)
  (let ((count 0) (wins 0))  
    (my-unwind-protect
     (loop for i from 2 to week
         do 
           (load-matches (1- i))		;load previous week's matches
           (dolist (m (nth (1- i) match-data))
             (let* ((ht (nth 0 m)) (at (nth 1 m)) (hg (nth 2 m)) (ag (nth 3 m))
                    (ht-rank (apply fn (append (list ht) args))) (at-rank (apply fn (append (list at) args))))
               (cond ((and (> ht-rank at-rank) (> hg ag)) (incf wins))
                     ((and (< ht-rank at-rank) (< hg ag)) (incf wins)))
               (incf count)))))
    (/ wins count 1.0)))

;determine the percentage of
;all matches in which the home team won,
;the away team won, the result was a draw.
(defun breakup-fraction (result)
  (let ((op (cond ((eq result 'home-wins) #'>)
                  ((eq result 'draws)     #'=)
                  ((eq result 'away-wins) #'<)
                  (t (error "Invalid result parameter"))))
        (count 0) (hits 0))
    (dolist (m1 match-data)
      (dolist (m m1)
	(let ((hg (nth 2 m)) (ag (nth 3 m)))
	  (if (funcall op hg ag)
	      (incf hits))
	  (incf count))))
  (/ hits count 1.0)))

;figure out which combination of parameters produces the most
;accurate predictions (for both GLM and KNN)
(defun identify-best-parameters (prediction-start-week prediction-end-week tr-start-week tr-end-week n)
  (with-open-file (stream "epl-predictor-params.txt" :direction :output :if-exists :supersede)

    (progn

      (let* ((params '(a b c d e f g h i j))
	     (param-combinations (combinations params n))
	     (tr-data1) (tr-data2) 
	     (predictions-glm) (best-param-combination-glm nil) (highest-average-accuracy-glm 0)
	     (predictions-knn) (best-param-combination-knn nil) (highest-average-accuracy-knn 0))


	;;(dolist (sub-combinations param-combinations)
      
	  (dolist (p param-combinations)
        
	    (format stream "Evaluating combination: ~A~%" p)
        
	    (load-all-data)
        
	    (setf tr-data1 (generate-training-data tr-start-week tr-end-week nil p))
	    (setf tr-data2 (generate-training-data tr-start-week tr-end-week nil p))        
    
	    (let ((sum-glm 0) (sum-knn 0))

	      (loop for prediction-week from prediction-start-week to prediction-end-week do

		   (progn 
		     (load-matches-upto (1- prediction-week))
      
		     (setf predictions-glm (predictions-for-week prediction-week #'predict-glm tr-data1 p))
		     (setf predictions-knn (predictions-for-week prediction-week #'predict-knn 3 t tr-data2 p))
	       
		     (load-matches prediction-week)
      
		     (incf sum-glm (accuracy-of-predictions predictions-glm))
		     (incf sum-knn (accuracy-of-predictions predictions-knn))))

	      (let ((accuracy (/ sum-glm (1+ (- prediction-end-week prediction-start-week)))))
		(if (> accuracy highest-average-accuracy-glm)
		    (setq best-param-combination-glm p highest-average-accuracy-glm accuracy)))
    
	      (let ((accuracy (/ sum-knn (1+ (- prediction-end-week prediction-start-week)))))
		(if (> accuracy highest-average-accuracy-knn)
		    (setq best-param-combination-knn p highest-average-accuracy-knn accuracy)))))

	(format stream "Linear regression:~%")
	(format stream "Highest average accuracy achieved: ~f~%" highest-average-accuracy-glm)
	(format stream "Corresponding parameters: ~A~%"  best-param-combination-glm)
	
	(format stream "KNN (average):~%")
	(format stream "Highest average accuracy achieved: ~f~%" highest-average-accuracy-knn)
	(format stream "Corresponding parameters: ~A~%" best-param-combination-knn))))

    (clean-all-data))
         
;average success rates for the different
;prediction methods, at the end of the specified number
;of weeks of the season
(defun average-success-rates (week)
  (let ((result (loop for i from 8 to week collect (test i 4 (1- i) 3 nil))))
    (loop for i from 0 to 4 do (print (average #'+ (loop for x in result collect (nth i x)))))))        
	
