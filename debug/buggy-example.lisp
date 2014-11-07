
(defun make-position (line column)
  (list line colunm)) ; <-- typo

(defun position-line (position)
  (caar position))    ; <-- should use 'car'

(defun position-column (position)
  (caddr position))   ; <-- should use 'cadr'

(defun make-positions ()
  (list))  ; <-- should be (list 'end)

(defun join-position (position positions)
  (cons position positions))

(defun make-board ()
  (list (list) (list) (list)))

(defun join-queen (position board)
  (let ((l (position-line position))
        (c (position-column position)))
    (list (cons c (car board))
          (cons (+ l c) (cadr board)) (cons (- l c) (caddr board)))))

(defun attacked-queen-p (position board)
  (let ((l (position-line position))
        (c (position-column position)))
    (or (member c (car board))
        (member (+ l c) (cadr board))
        (member (- l c) (caddr board)))))

(defun queens (n)
  (place-queens n n n (make-board)))

(defun place-queens (n i j board)
  (cond ((= i 0)
         (make-positions))
        ((= j 0) nil)
        ((attacked-queen (make-position i j) board) ; <-- should use 'attacked-queen-p'
         (place-queens n i (1- j) board))
        (t
         (let ((result (place-queens n
                                     (1- i)
                                     n
                                     (join-queen (make-position i j)
                                                 board))))
           (if result
               (join-position (make-position i j) result)
               (place-queens n i (1- j) board))))))
