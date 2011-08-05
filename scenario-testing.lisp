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

        
;average success rates for the different
;prediction methods, at the end of the specified number
;of weeks of the season
(defun average-success-rates (week)
  (let ((result (loop for i from 8 to week collect (test i 4 (1- i) 3 nil))))
    (loop for i from 0 to 4 do (print (average #'+ (loop for x in result collect (nth i x)))))))        

;figure out which combination of parameters produces the most
;accurate predictions (for both GLM and KNN)	
(defun identify-best-parameters (prediction-start-week 
				 prediction-end-week
				 tr-start-week
				 tr-end-week
				 nof-params 
				 prediction-method
				 &optional
				 k)

  (if (and (not (equal prediction-method "GLM"))
	   (null k))
      (error "Parameter 'k' should be specified for KNN-CLASSES and KNN-AVERAGE"))

  (with-open-file (stream "epl-predictor-params.txt" :direction :output :if-exists :supersede)

    (progn
#|
      Parameter descriptions

      A	Home record of home team
      B	Away record of away team
      C	Total record of home team
      D	Total record of away team
      E	Most recent record (last three matches) of home team
      F	Most recent record (last three matches) of away team
      G	Goal difference of home team
      H	Goal difference of away team
      I	Rank of home team in points table
      J	Rank of away team in points table
|#

      (let* ((full-param-list '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J"))
	     (param-combinations (combinations full-param-list nof-params))
	     (tr-data)
	     (predictions) (best-param-combination) (highest-average-accuracy 0))

	(load-all-data)
      
	(dolist (param-combination param-combinations)
        
	  (format stream "Evaluating combination: ~A~%" param-combination)
        

        
	  (setf tr-data (generate-training-data tr-start-week 
						tr-end-week 
						(equal prediction-method "KNN-CLASSES")
						param-combination))        

	  (let ((sum 0))

	    (loop for prediction-week from prediction-start-week to prediction-end-week do

		 (if (>= (length (nth (1- prediction-week) match-data)) 5)
		     
		     (progn 

		       (setq matches nil)

		       (load-matches-upto (1- prediction-week))
      
		       (setf predictions (cond ((equal prediction-method "GLM")
						(predictions-for-week prediction-week 
								      #'predict-glm tr-data param-combination))
					       ((equal prediction-method "KNN-CLASSES")
						(predictions-for-week prediction-week 
								      #'predict-knn k nil tr-data param-combination))
					       ((equal prediction-method "KNN-AVERAGE")
						(predictions-for-week prediction-week
								      #'predict-knn k t tr-data param-combination))
					       (t (error "Unknown method"))))

		       (load-matches prediction-week)
      
		       (incf sum (accuracy-of-predictions predictions)))))

	    (let ((accuracy (/ sum (1+ (- prediction-end-week prediction-start-week)))))
	      (if (> accuracy highest-average-accuracy)
		  (setq best-param-combination param-combination highest-average-accuracy accuracy)))))

	(format stream "~A:~%" prediction-method)	
	(format stream "Highest average accuracy achieved: ~f~%" highest-average-accuracy)
	(format stream "Corresponding parameters: ~A~%" best-param-combination))))
  
  (clean-all-data))

;;computes average and median accuracy for a method
(defun report-accuracy (pred-start-week 
			pred-end-week
			tr-start-week 
			tr-end-week 
			params
			prediction-method 
			&optional
			k)

  (if (not (or (equal prediction-method "KNN-CLASSES")
	       (equal prediction-method "KNN-AVERAGE")
	       (equal prediction-method "GLM")))
      (error "Prediction method should be GLM, KNN-CLASSES or KNN-AVERAGE"))

  (if (and (not (equal prediction-method "GLM"))
	   (null k))
      (error "Parameter 'k' should be specified for KNN-CLASSES and KNN-AVERAGE"))

  (let ((accuracies-list) (tr-data) (predictions))

    (load-all-data)
        
    (setq tr-data (generate-training-data tr-start-week 
					  tr-end-week 
					  (equal prediction-method "KNN-CLASSES")
					  params))        

    (loop for prediction-week from pred-start-week to pred-end-week do

	 ;;to filter out those match weeks with a small number
	 ;;of matches, which may skew the accuracy
	 (if (>= (length (nth (1- prediction-week) match-data)) 5)

	     (progn 

	       (setq matches nil)

	       (load-matches-upto (1- prediction-week))
      
	       (setq predictions (cond ((equal prediction-method "GLM")
					(predictions-for-week prediction-week 
							      #'predict-glm tr-data params))
				       ((equal prediction-method "KNN-CLASSES")
					(predictions-for-week prediction-week 
							      #'predict-knn k nil tr-data params))
				       ((equal prediction-method "KNN-AVERAGE")
					(predictions-for-week prediction-week
							      #'predict-knn k t tr-data params))
				       (t (error "Unknown method"))))

	       (load-matches prediction-week)
      
	       (setf accuracies-list (nconc accuracies-list (list (accuracy-of-predictions predictions)))))))

    (format t "Prediction method: ~A:~%" prediction-method)
    (format t "Parameters: ~A~%" params)
    (format t "~A~%" accuracies-list)
    (format t "Average accuracy achieved: ~f~%" (average #'+ accuracies-list))
    (format t "Median accuracy achieved: ~f~%" (median accuracies-list))
    (format t "Max accuracy achieved: ~f~%" (apply #'max accuracies-list))
    (format t "Min accuracy achieved: ~f~%" (apply #'min accuracies-list)))

  
  (clean-all-data))
			