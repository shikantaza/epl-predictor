(in-package #:epl-predictor)

(defun clean-all-data ()
  (setq teams nil)
  (setq matches nil)
  (setq match-data nil)
  (setq fixtures nil))

(defun load-all-data ()
  (clean-all-data)
  (load-team-data)
  (load-match-data)
  (load-fixtures))

(defun load-matches (week)
  (dolist (data (nth (1- week) match-data))
    (add-match (nth 0 data) (nth 1 data) (nth 2 data) (nth 3 data))))

(defun load-matches-upto (week)
  (dotimes (i week)
    (load-matches (1+ i))))

(defun snapshot-till (week)
  (load-all-data)
  (load-matches-upto week))


;load team data
(defun load-team-data ()
  (let ((team-list (list "Arsenal"            ;1
                         "Aston Villa"        ;2
                         "Birmingham City"    ;3
                         "Blackburn Rovers"   ;4
                         "Blackpool"          ;5
                         "Bolton Wanderers"   ;6
                         "Chelsea"            ;7
                         "Everton"            ;8
                         "Fulham"             ;9
                         "Liverpool"          ;10
                         "Manchester City"    ;11
                         "Manchester United"  ;12
                         "Newcastle United"   ;13
                         "Stoke City"         ;14
                         "Sunderland"         ;15
                         "Tottenham Hotspur"  ;16
                         "West Brom"          ;17
                         "West Ham"           ;18
                         "Wigan Athletic"     ;19
                         "Wolverhampton"))    ;20
        (i 1)
        (t1 ()))
    (setq teams (make-hash-table))
    (dolist (x team-list)
      (setf t1 (make-instance 'team :name x))
      ;(setf (slot-value t1 'name) x)
      (setf (gethash i teams) t1)
      (setf i (1+ i)))))
  
(defun load-match-data ()

  (setq match-data (nconc match-data (list (list '(16 11 0 0) '(2 18 3 0) '(4 8 1 0) '(6 9 0 0) '(15 3 2 2) 
                                                 '(19 5 0 4) '(20 14 2 1) '(7 17 6 0) '(10 1 1 1) '(12 13 3 0)))))

  (setq match-data (nconc match-data (list (list '(1 5 6 0) '(3 4 2 1) '(8 20 1 1) '(14 16 1 2) '(17 15 1 0) 
                                                 '(18 6 1 3) '(19 7 0 6)'(13 2 6 0) '(9 12 2 2) '(11 10 3 0)))))
                   
  (setq match-data (nconc match-data (list (list '(4 1 1 2) '(5 9 2 2) '(7 14 2 0) '(16 19 0 1) '(20 13 1 1)
                                                 '(12 18 3 0) '(6 3 2 2) '(10 17 1 0) '(15 11 1 0) '(2 8 1 0)))))
                   
  (setq match-data (nconc match-data (list (list '(8 12 3 3) '(1 6 4 1) '(9 20 2 1) '(11 4 1 1) '(13 5 0 2)
                                                 '(17 16 1 1) '(18 7 1 3) '(19 15 1 1) '(3 10 0 0) '(14 2 2 1)))))
                                      
  (setq match-data (nconc match-data (list (list '(14 18 1 1) '(2 6 1 1) '(4 9 1 1) '(8 13 0 1) '(16 20 3 1)
                                                 '(17 3 3 1) '(15 1 1 1) '(12 10 3 2) '(19 11 0 2) '(7 5 4 0)))))
                   
  (setq match-data (nconc match-data (list (list '(11 7 1 0) '(1 17 2 3) '(3 19 0 0) '(5 4 1 2) '(9 8 0 0)
                                                 '(10 15 2 2) '(18 16 1 0) '(6 12 2 2) '(20 2 1 2) '(13 14 1 2)))))
                   
  (setq match-data (nconc match-data (list (list '(19 20 2 0) '(3 8 0 2) '(14 4 1 0) '(15 12 0 0) '(16 2 2 1)
                                                 '(17 6 1 1) '(18 9 1 1) '(11 13 2 1) '(10 5 1 2) '(7 1 2 0)))))

  (setq match-data (nconc match-data (list (list '(1 3 2 1) '(6 14 2 1) '(9 16 1 2) '(12 17 2 2) '(13 19 2 2) 
                                                 '(20 18 1 1) '(2 7 0 0) '(8 10 2 0) '(5 11 2 3) '(4 15 0 0)))))
  
  (setq match-data (nconc match-data (list (list '(16 8 1 1) '(3 5 2 0) '(7 20 2 0) '(15 2 1 0) '(17 9 2 1) 
						 '(19 6 1 1) '(18 13 1 2) '(14 12 1 2) '(10 4 2 1) '(11 1 0 3)))))
  
  (setq match-data (nconc match-data (list (list '(1 18 1 0) '(4 7 1 2) '(8 14 1 0) '(9 19 2 0) '(20 11 2 1) 
                                             '(12 16 2 0) '(2 3 0 0) '(13 15 5 1) '(6 10 0 1) '(5 17 2 1)))))

  (setq match-data (nconc match-data (list (list '(6 16 4 2) '(3 18 2 2) '(4 19 2 1) '(5 8 2 2) '(9 2 1 1) 
                                             '(12 20 2 1) '(15 14 2 0) '(1 13 0 1) '(17 11 0 2) '(10 7 2 0)))))  
  
  (setq match-data (nconc match-data (list (list '(14 3 3 2) '(16 15 1 1) '(2 5 3 2) '(7 9 1 0) '(13 4 1 2) 
                                             '(18 17 2 2) '(19 10 1 1) '(8 6 1 1) '(11 12 0 0) '(20 1 0 2)))))  
  
  (setq match-data (nconc match-data (list (list '(2 12 2 2) '(11 3 0 0) '(13 9 0 0) '(16 4 4 2) '(18 5 0 0) 
                                             '(19 17 1 0) '(20 6 2 3) '(14 10 2 0) '(8 1 1 2) '(7 15 0 3)))))  
  
  (setq match-data (nconc match-data (list (list '(1 16 2 3) '(3 7 1 0) '(5 20 2 1) '(6 13 5 1) '(12 19 2 0) 
                                             '(17 14 0 3) '(10 18 3 0) '(4 2 2 0) '(9 11 1 4) '(15 8 2 2)))))  

  (setq match-data (nconc match-data (list (list '(2 1 2 4) '(6 5 2 2) '(8 17 1 4) '(9 3 1 1) '(12 4 7 1) 
                                             '(14 11 1 1) '(18 19 3 1) '(20 15 3 2) '(13 7 1 1) '(16 10 2 1)))))  

  ;;Blackpool v Man United postponed; hence only nine matches
  (setq match-data (nconc match-data (list (list '(1 9 2 1) '(3 16 1 1) '(4 20 3 0) '(7 8 1 1) '(11 6 1 0) 
                                             '(19 14 2 2) '(17 13 3 1) '(15 18 1 0) '(10 2 3 0) ))))  

  (setq match-data (nconc match-data (list (list '(2 17 2 1) '(8 19 0 0) '(9 15 0 0) '(14 5 0 1) '(18 11 1 3)
					     '(13 10 3 1) '(6 4 2 1) '(20 3 1 0) '(16 7 1 1) '(12 1 1 0)))))

  ;;snowed out
  (setq match-data (nconc match-data (list (list '(15 6 1 0) '(4 18 1 1) '(11 8 1 2)))))

  ;;two matches snowed out
  (setq match-data (nconc match-data (list (list '(9 18 1 3) '(4 14 0 2) '(6 17 2 0) '(12 15 2 0) '(13 11 1 3) 
                                             '(20 19 1 2) '(2 16 1 2) '(1 7 3 1) 
					     '(11 2 4 0) '(14 9 0 2) '(15 5 0 2)
					     '(16 13 2 0) '(17 4 1 3) '(18 8 1 1) '(3 12 1 1) '(7 6 1 0) '(19 1 2 2) '(10 20 0 1)
					     ))))  

  (setq match-data (nconc match-data (list (list '(17 12 1 2) '(10 6 2 1) '(11 5 1 0) '(14 8 2 0) '(15 4 3 0) '(16 9 1 0) '(18 20 2 0) '(3 1 0 3) '(7 2 3 3)
					     '(19 13 0 1) '(5 3 1 2) '(9 17 3 0) '(12 14 2 1) '(1 11 0 0) '(2 15 0 1) '(13 18 5 0) '(20 7 1 0) '(4 10 3 1)
					     '(6 19 1 1) '(8 16 2 1)))))

  (setq match-data (nconc match-data (list (list '(5 10 2 1)))))

  (setq match-data (nconc match-data (list (list '(7 4 2 0) '(11 20 4 3) '(14 6 2 0) '(17 5 3 2) '(19 9 1 1) '(18 1 0 3) 
						 '(3 2 1 1) '(15 13 1 1) '(10 8 2 2) '(16 12 0 0)))))

  (setq match-data (nconc match-data (list (list '(20 10 0 3) '(1 19 3 0) '(5 15 1 2) '(8 18 2 2) '(9 14 2 0) 
						 '(12 3 5 0) '(13 16 1 1) '(2 11 1 0) '(4 17 2 0)
						 '(6 7 0 4) '(5 12 2 3) '(19 2 1 2) '(10 9 1 0)))))

  (setq match-data (nconc match-data (list (list '(1 8 2 1) '(15 7 2 4) '(12 2 3 1) '(17 19 2 2) '(3 11 2 2) 
					     '(4 16 0 1) '(5 18 1 3) '(6 20 1 0) '(9 13 1 0) '(10 14 2 0)))))

  (setq match-data (nconc match-data (list (list '(14 15 3 2) '(2 9 2 2) '(8 5 5 3) '(11 17 3 0) '(13 1 4 4)
					     '(16 6 2 1) '(19 4 4 3) '(20 12 2 1) '(18 3 0 1) '(7 10 0 1)))))

  (setq match-data (nconc match-data (list (list '(12 11 2 1) '(1 20 2 0) '(3 14 1 0) '(4 13 0 0) '(5 2 1 1) 
					     '(10 19 1 1) '(17 18 3 3) '(15 16 1 2) '(6 8 2 0) '(9 7 0 0) '(3 13 0 2)))))

  (setq match-data (nconc match-data (list (list '(17 20 1 1) '(5 16 3 1) '(1 14 1 0)))))

  (setq match-data (nconc match-data (list (list '(2 4 4 1) '(8 15 2 0) '(13 6 1 1) '(19 12 0 4) '(20 5 4 0) 
						 '(18 10 3 1) '(11 9 1 1) '(14 17 1 1) '(7 12 2 1)))))

  (setq match-data (nconc match-data (list (list '(3 17 1 3) '(1 15 0 0) '(6 2 3 2) '(9 4 3 2) '(13 8 1 2) '(18 14 3 0)
						 '(11 19 1 0) '(10 12 3 1) '(20 16 3 3) '(5 7 1 3) '(8 3 1 1)))))

  (setq match-data (nconc match-data (list (list '(16 18 0 0) '(2 20 0 1) '(4 5 2 2) '(12 6 1 0) '(14 13 4 0) 
						 '(17 1 2 2) '(19 3 2 1) '(8 9 2 1) '(15 10 0 2) '(7 11 2 0)))))

  (setq match-data (nconc match-data (list (list '(18 12 2 4) '(3 6 2 1) '(8 2 2 2) '(13 20 4 1) '(14 7 1 1) 
						 '(17 10 2 1) '(19 16 0 0) '(1 4 0 0) '(9 5 3 0) '(11 15 5 0)))))

  (setq match-data (nconc match-data (list (list '(20 8 0 3) '(4 3 1 1) '(6 18 3 0) '(7 19 1 0) '(12 9 2 0) 
						 '(15 17 2 3) '(16 14 3 2) '(5 1 1 3) '(2 13 1 0) '(10 11 3 0)))))

  (setq match-data (nconc match-data (list (list '(3 15 2 0) '(5 19 1 3) '(8 4 2 0) '(17 7 1 3) '(18 2 1 2) 
						 '(1 10 1 1) '(13 12 0 0) '(7 3 3 1) '(16 1 3 3)))))

  (setq match-data (nconc match-data (list (list '(12 8 1 0) '(2 14 1 1) '(5 13 1 1) '(10 3 5 0) '(15 19 4 2) '(16 17 2 2) 
					     '(20 9 1 1) '(7 18 3 0) '(6 1 2 1) '(4 11 0 1) '(14 20 3 0) '(9 6 3 0)))))

  (setq match-data (nconc match-data (list (list '(4 6 1 0) '(5 14 0 0) '(15 9 0 3) '(17 2 2 1) '(19 8 1 1)
						 '(7 16 2 1) '(3 20 1 1) '(10 13 3 0) '(1 12 1 0) '(11 18 2 1)))))

  (setq match-data (nconc match-data (list (list '(2 19 1 1) '(6 15 1 2) '(8 11 2 1) '(13 3 2 1) '(18 4 1 1)
						 '(16 5 1 1) '(20 17 3 1) '(14 1 3 1) '(12 7 2 1) '(9 10 2 5) '(11 16 1 0)))))

  (setq match-data (nconc match-data (list (list '(4 12 1 1) '(5 6 4 3) '(15 20 1 3) '(17 8 1 0) '(7 13 2 2)
						 '(1 2 1 2) '(3 9 0 2) '(10 16 0 2) '(19 18 3 2) '(11 14 3 0)))))

  (setq match-data (nconc match-data (list (list '(2 10 1 0) '(6 11 0 2) '(8 7 1 0) '(9 1 2 2) '(12 5 4 2)
						 '(13 17 3 3) '(14 19 0 1) '(16 3 2 1) '(18 15 0 3) '(20 4 2 3)))))


  )

(defun load-fixtures ()
  (setq fixtures (nconc fixtures (list (list '(16 11) '(2 18) '(4 8) '(6 9) '(15 3) 
                                             '(19 5) '(20 14) '(7 17) '(10 1) '(12 13)))))

  (setq fixtures (nconc fixtures (list (list '(1 5) '(3 4) '(8 20) '(14 16) '(17 15) 
                                             '(18 6) '(19 7) '(13 2) '(9 12) '(11 10)))))

  (setq fixtures (nconc fixtures (list (list '(4 1) '(5 9) '(7 14) '(16 19) '(20 13) 
                                             '(12 18) '(6 3) '(10 17) '(15 11) '(2 8)))))

  (setq fixtures (nconc fixtures (list (list '(8 12) '(1 6) '(9 20) '(11 4) '(13 5) 
                                             '(17 16) '(18 7) '(19 15) '(3 10) '(14 2)))))

  (setq fixtures (nconc fixtures (list (list '(14 18) '(2 6) '(4 9) '(8 13) '(16 20) 
                                             '(17 3) '(15 1) '(12 10) '(19 11) '(7 5)))))

  (setq fixtures (nconc fixtures (list (list '(11 7) '(1 17) '(3 19) '(5 4) '(9 8) 
                                             '(10 15) '(18 16) '(6 12) '(20 2) '(13 14)))))

  (setq fixtures (nconc fixtures (list (list '(19 20) '(3 8) '(14 4) '(15 12) '(16 2) 
                                             '(17 6) '(18 9) '(11 13) '(10 5) '(7 1)))))
  
  (setq fixtures (nconc fixtures (list (list '(1 3) '(6 14) '(9 16) '(12 17) '(13 19) 
                                             '(20 18) '(2 7) '(8 10) '(5 11) '(4 15)))))
  
  (setq fixtures (nconc fixtures (list (list '(16 8) '(3 5) '(7 20) '(15 2) '(17 9) 
                                             '(19 6) '(18 13) '(14 12) '(10 4) '(11 1)))))

  (setq fixtures (nconc fixtures (list (list '(1 18) '(4 7) '(8 14) '(9 19) '(20 11) 
                                             '(12 16) '(2 3) '(13 15) '(6 10) '(5 17)))))

  (setq fixtures (nconc fixtures (list (list '(6 16) '(3 18) '(4 19) '(5 8) '(9 2) 
                                             '(12 20) '(15 14) '(1 13) '(17 11) '(10 7)))))  
  
  (setq fixtures (nconc fixtures (list (list '(14 3) '(16 15) '(2 5) '(7 9) '(13 4) 
                                             '(18 17) '(19 10) '(8 6) '(11 12) '(20 1)))))  
  
  (setq fixtures (nconc fixtures (list (list '(2 12) '(11 3) '(13 9) '(16 4) '(18 5) 
                                             '(19 17) '(20 6) '(14 10) '(8 1) '(7 15)))))  
  
  (setq fixtures (nconc fixtures (list (list '(1 16) '(3 7) '(5 20) '(6 13) '(12 19) 
                                             '(17 14) '(10 18) '(4 2) '(9 11) '(15 8)))))  

  (setq fixtures (nconc fixtures (list (list '(2 1) '(6 5) '(8 17) '(9 3) '(12 4) 
                                             '(14 11) '(18 19) '(20 15) '(13 7) '(16 10)))))  

  ;;Blackpool v Man United postponed; hence only nine matches
  (setq fixtures (nconc fixtures (list (list '(1 9) '(3 16) '(4 20) '(7 8) '(11 6) 
                                             '(19 14) '(17 13) '(15 18) '(10 2) ))))  

  (setq fixtures (nconc fixtures (list (list '(2 17) '(8 19) '(9 15) '(14 5) '(18 11)
					     '(13 10) '(6 4) '(20 3) '(16 7) '(12 1)))))

  ;;snowed out
  (setq fixtures (nconc fixtures (list (list '(15 6) '(4 18) '(11 8)))))

  ;;two matches snowed out
  (setq fixtures (nconc fixtures (list (list '(9 18) '(4 14) '(6 17) '(12 15) '(13 11) 
                                             '(20 19) '(2 16) '(1 7) '(11 2) '(14 9) '(15 5)
					     '(16 13) '(17 4) '(18 8) '(3 12) '(7 6) '(19 1) '(10 20)))))  

  (setq fixtures (nconc fixtures (list (list '(17 12) '(10 6) '(11 5) '(14 8) '(15 4) '(16 9) '(18 20) '(3 1) '(7 2)
					     '(19 13) '(5 3) '(9 17) '(12 14) '(1 11) '(2 15) '(13 18) '(20 7) '(4 10)
					     '(6 19) '(8 16)))))

  (setq fixtures (nconc fixtures (list (list '(5 10)))))

  (setq fixtures (nconc fixtures (list (list '(7 4) '(11 20) '(14 6) '(17 5) '(19 9) '(18 1) 
					     '(3 2) '(15 13) '(10 8) '(16 12)))))

  (setq fixtures (nconc fixtures (list (list '(20 10) '(1 19) '(5 15) '(8 18) '(9 14) '(12 3) 
					     '(13 16) '(2 11) '(4 17) '(6 7) '(5 12) '(19 2) '(10 9)))))

  (setq fixtures (nconc fixtures (list (list '(1 8) '(15 7) '(12 2) '(17 19) '(3 11) 
					     '(4 16) '(5 18) '(6 20) '(9 13) '(10 14)))))

  (setq fixtures (nconc fixtures (list (list '(14 15) '(2 9) '(8 5) '(11 17) '(13 1)
					     '(16 6) '(19 4) '(20 12) '(18 3) '(7 10)))))

  (setq fixtures (nconc fixtures (list (list '(12 11) '(1 20) '(3 14) '(4 13) '(5 2) 
					     '(10 19) '(17 18) '(15 16) '(6 8) '(9 7) '(3 13)))))

  (setq fixtures (nconc fixtures (list (list '(17 20) '(5 16) '(1 14)))))

  (setq fixtures (nconc fixtures (list (list '(2 4) '(8 15) '(13 6) '(19 12) '(20 5) 
					     '(18 10) '(11 9) '(14 17) '(7 12)))))

  (setq fixtures (nconc fixtures (list (list '(3 17) '(1 15) '(6 2) '(9 4) '(13 8) '(18 14 )
					     '(11 19) '(10 12) '(20 16) '(5 7) '(8 3)))))

  (setq fixtures (nconc fixtures (list (list '(16 18) '(2 20) '(4 5) '(12 6) '(14 13) 
					     '(17 1) '(19 3) '(8 9) '(15 10) '(7 11)))))

  (setq fixtures (nconc fixtures (list (list '(18 12) '(3 6) '(8 2) '(13 20) '(14 7) 
					     '(17 10) '(19 16) '(1 4) '(9 5) '(11 15)))))

  (setq fixtures (nconc fixtures (list (list '(20 8) '(4 3) '(6 18) '(7 19) '(12 9) 
					     '(15 17) '(16 14) '(5 1) '(2 13) '(10 11)))))

  (setq fixtures (nconc fixtures (list (list '(3 15) '(5 19) '(8 4) '(17 7) '(18 2) 
					     '(1 10) '(13 12) '(7 3) '(16 1)))))

  (setq fixtures (nconc fixtures (list (list '(12 8) '(2 14) '(5 13) '(10 3) '(15 19) '(16 17) 
					     '(20 9) '(7 18) '(6 1) '(4 11) '(14 20) '(9 6)))))

  (setq fixtures (nconc fixtures (list (list '(4 6) '(5 14) '(15 9) '(17 2) '(19 8)
					     '(7 16) '(3 20) '(10 13) '(1 12) '(11 18)))))

  (setq fixtures (nconc fixtures (list (list '(2 19) '(6 15) '(8 11) '(13 3) '(18 4)
					     '(16 5) '(20 17) '(14 1) '(12 7) '(9 10) '(11 16)))))

  (setq fixtures (nconc fixtures (list (list '(4 12) '(5 6) '(15 20) '(17 8) '(7 13)
					     '(1 2) '(3 9) '(10 16) '(19 18) '(12 14)))))

  (setq fixtures (nconc fixtures (list (list '(2 10) '(6 11) '(8 7) '(9 1) '(12 5)
					     '(13 17) '(14 19) '(16 3) '(18 15) '(20 4)))))
  )

(defun valid-team-number (val)
  (and (>= val 1) (<= val 20)))

(defmacro at (a i j)
  `(nth ,j (nth ,i ,a)))

;to ensure that fixtures and match-data are in sync, team numbers are within range,
;goals are non-negative, etc.
(defun sanity-check ()
  (load-all-data)
  (unwind-protect
      (progn
        ;;(loop for f in fixtures do
        ;;      (if (not (eq (length f) 10)) (error "Number of matches in each week (fixtures) should be 10")))
        ;;(loop for m in match-data do
        ;;      (if (not (eq (length m) 10)) (error "Number of matches in each week (match data) should be 10")))
        (loop for f in fixtures do
              (dotimes (i 10)
                (if (not (and (valid-team-number (at f i 0))
                              (valid-team-number (at f i 1))))
                    (error "Team numbers in fixtures should be between 1 and 20"))))
        (loop for m in match-data do
              (dotimes (i 10)
                (if (not (and (valid-team-number (at m i 0))
                              (valid-team-number (at m i 1))))
                    (error "Team numbers in match data should be between 1 and 20"))))
        (loop for m in match-data do
              (dotimes (i 10)
                (if (or (< (at m i 2) 0) (< (at m i 3) 0))
                    (error "Goals in match data should be greater than or equal to zero"))))
        ;;(loop for f in fixtures do 
        ;;      (if (not (eq (length (remove-duplicates (flatten f) :test 'eq)) 20))
        ;;          (error "Not all teams figure in a week's fixtures")))
        ;;(loop for m in match-data do 
        ;;      (if (not (eq (length (remove-duplicates (flatten (loop for match in m collect (list (nth 0 match) (nth 1 match)))) :test 'eq)) 20))
        ;;          (error "Not all teams figure in a week's match data")))
        (loop
          for m in match-data
          for f in fixtures
          do (dotimes (i 10)
               (if (not (and (eq (at m i 0) (at f i 0))
                             (eq (at m i 1) (at f i 1))))
                   (error "Mismatch between fixtures and match-data")))))
    (clean-all-data))
  "Sanity check passed")
  