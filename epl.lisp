(in-package #:epl-predictor)

(defvar teams ())

(defvar matches ())

(defvar match-data ())

(defvar fixtures ())

(defclass team ()
  ((name :initarg :name))) ; go with a class here in case we need to add more attributes

(defclass match ()
  ((home-team :accessor get-home-team :initarg :home-team)
   (away-team :accessor get-away-team :initarg :away-team)
   (home-goals :accessor get-home-goals :initarg :home-goals)
   (away-goals :accessor get-away-goals :initarg :away-goals)))

(defun add-match (ht at hg ag)
  (my-append matches (make-instance 'match 
                       :home-team ht 
                       :away-team at 
                       :home-goals hg 
                       :away-goals ag)))

(defun get-team-name (id)
  (slot-value (gethash id teams) 'name))

;get the last n home/away/both matches for a team
(defun get-matches (a-team match-type &optional n)
  (cond ((eq match-type 'home) (get-home-matches a-team n))
        ((eq match-type 'away) (get-away-matches a-team n))
        (t                     (get-all-matches  a-team n))))

;get the last n matches for a team
(defun get-all-matches (a-team &optional n)
    (let ((ms (remove-if-not (lambda (m) (or (eq (get-home-team m) a-team) (eq (get-away-team m) a-team))) matches)))
      (if (numberp n) (last ms n) ms)))

;get the last n home matches for a team
(defun get-home-matches (a-team &optional n)
  (let ((ms (remove-if-not (lambda (m) (eq (get-home-team m) a-team)) matches)))
    (if (numberp n) (last ms n) ms)))

;get the last n away matches for a team
(defun get-away-matches (a-team &optional n)
  (let ((ms (remove-if-not (lambda (m) (eq (get-away-team m) a-team)) matches)))
    (if (numberp n) (last ms n) ms)))

(defun display-match (m)
  (format t "~a ~d : ~d ~a~%" (get-team-name (get-home-team m)) (get-home-goals m) (get-away-goals m) (get-team-name (get-away-team m))))

;generic function for getting number of wins/losses/draws
;for a team for home/away/both for the last n matches
(defun get-count (a-team result-type &optional match-type n)
  (let ((operator) (played-matches (get-matches a-team match-type n)) (count 0))
    (cond ((eq result-type 'wins)   (setf operator #'>))
          ((eq result-type 'losses) (setf operator #'<))
          ((eq result-type 'draws)  (setf operator #'eq)))
    (if (numberp n)
        (setf played-matches (last played-matches n)))
    (dolist (m played-matches)
      (if (or (and (eq (get-home-team m) a-team)
                   (funcall operator (get-home-goals m) (get-away-goals m)))
              (and (eq (get-away-team m) a-team)
                   (funcall operator (get-away-goals m) (get-home-goals m))))
          (setf count (1+ count))))
    count))

;generic function for getting goals for and against
;for a team for home/away/both for the last n matches
(defun get-goals (a-team for-against &optional match-type n)
  (let ((played-matches (get-matches a-team match-type)) (goals 0))
    (if (numberp n)
        (setf played-matches (last played-matches n)))
    (dolist (m played-matches)
      (if (eq (get-home-team m) a-team)
          (setf goals (+ goals (cond ((eq for-against 'for)     (get-home-goals m))
                                     ((eq for-against 'against) (get-away-goals m))
                                     (t                         0)))))
      (if (eq (get-away-team m) a-team)
          (setf goals (+ goals (cond ((eq for-against 'for)     (get-away-goals m))
                                     ((eq for-against 'against) (get-home-goals m))
                                     (t                         0))))))
    goals))

(defun points-table ()
  (let ((results-list ()))
    (dotimes (i 20)
      (my-append results-list (get-stats (1+ i))))
    (sort results-list #'compare)))

(defun show-points-table ()
  (let ((i 0))
    (format t "NO ~20a PL WN DR LO GF GA  GD PT~%" "TEAM")
    (format t "------------------------------------------------~%")
    (dolist (l (points-table))
      (format t "~2d ~20a ~2d ~2d ~2d ~2d ~2d ~2d ~3d ~2d~%" (incf i) (nth 1 l) (nth 2 l) (nth 3 l) 
        (nth 4 l) (nth 5 l) (nth 6 l) (nth 7 l) (nth 8 l) (nth 9 l)))))

;played/won/drawn/lost/gf/ga/gd/points for a team
;for home/away/both for the last n matches
(defun get-stats (a-team &optional match-type n)
  (let ((played-matches (get-matches a-team match-type)) (p) (w) (l) (d) (gf) (ga))
    (setf p (if (null n) (length played-matches) (length (last played-matches n))))
    (setf w (get-count a-team 'wins match-type n)) 
    (setf l (get-count a-team 'losses match-type n)) 
    (setf d (- p w l))
    (setf gf (get-goals a-team 'for match-type n))
    (setf ga (get-goals a-team 'against match-type n))
    (list a-team (get-team-name a-team) p w d l gf ga (- gf ga) (+ (* 3 w) d))))

;used to rank teams in the points table
(defun compare (r1 r2)
  (let ((p1 (nth 9 r1))  (p2 (nth 9 r2))  (gd1 (nth 8 r1)) 
        (gd2 (nth 8 r2)) (gf1 (nth 6 r1)) (gf2 (nth 6 r2)))
    (cond ((> p1 p2)                                  t)
          ((and (eq p1 p2) (> gd1 gd2))               t)
          ((and (eq p1 p2) (eq gd1 gd2) (> gf1 gf2))  t)
          (t                               nil))))

;how a team fared in a match: W, D or L
(defun result (a-team m)
  (cond ((or (and (> (get-home-goals m) (get-away-goals m))
                  (eq (get-home-team m) a-team))
             (and (< (get-home-goals m) (get-away-goals m))
                  (eq (get-away-team m) a-team)))            'W)
        ((eq (get-home-goals m) (get-away-goals m))          'D)
        (t                                                   'L)))

(defun find-match (fixture)
  (find-if (lambda (m) (and (eq (get-home-team m) (first fixture))
                            (eq (get-away-team m) (second fixture)))) 
           matches))

;short version of a team's form: W D D L ...
;for a team for home/away/both for the last n matches
(defun form-summary (a-team &optional match-type n)
  (let ((matches-considered (get-matches a-team match-type)))
    (if (numberp n)
        (setf matches-considered (last matches-considered n)))
    (mapcar (curry #'result a-team) matches-considered)))

;details of the matches for a team
;for home/away/both for the last n matche
(defun results-for-team (a-team &optional match-type n)
  (let ((matches-considered (get-matches a-team match-type)))
    (if (numberp n)
        (setf matches-considered (last matches-considered n)))
    (mapcan #'display-match matches-considered)))

;how many points a team scored out of the maximum possible, for home/away/both for the last n matches
(defun get-record (a-team &optional match-type n)
  (let ((wins (get-count a-team 'wins match-type n)) (draws (get-count a-team 'draws match-type n)))
        (/ (+ (* 3.0 wins) draws) (* 3.0 (length (get-matches a-team match-type n))))))

;get a team's position in the points table
(defun get-rank (a-team)
  (labels ((get-rank-internal (rank table)
	     (if (eq a-team (caar table))
		 rank
	       (get-rank-internal (1+ rank) (cdr table)))))
    (get-rank-internal 1 (points-table))))

(defun display-fixtures (week)
  (dolist (f (nth (1- week) fixtures))
    (format t "~a vs ~a~%" (get-team-name (first f)) (get-team-name (second f)))))

