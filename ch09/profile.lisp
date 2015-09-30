;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH9; Base: 10 -*-

;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991, 1996 Peter Norvig

;;; File profile.lisp: Profiling code from older version of PAIP.

(in-package #:ch9)

(defvar *profiled-functions* nil
  "Function names that are currently profiled")

(defmacro profile (&rest fn-names)
  "Profile FN-NAMES. With no args, list profiled functions."
  `(mapcar #'profile1
           (setf *profiled-functions*
                 (union *profiled-functions* ',fn-names))))

(defmacro unprofile (&rest fn-names)
  "Stop profiling FN-NAMES. With no args, stop all profiling."
  `(progn
     (mapcar #'unprofile1
             ,(if fn-names `',fn-names '*profiled-functions*)) ; equivalent to '(quote ,fn-names). See `kwote'
     (setf *profiled-functions*
           ,(if (null fn-names)
                nil
                `(set-difference *profiled-functions*
                                 ',fn-names)))))

;; The 'quote' is constant and the variable X is evaluated.
(defun kwote (x)
  (list 'quote x))

(defun profile1 (fn-name)
  "Make the function count how often it is called"
  ;; First save away the old, unprofiled function
  ;; Then make the name be a new function that increments
  ;; a counter and then calls the original function
  (let ((fn (symbol-function fn-name)))
    (unless (eq fn (get fn-name 'profiled-fn))
      (let ((new-fn (profiled-fn fn-name fn)))
        (setf (symbol-function fn-name) new-fn
              (get fn-name 'profiled-fn) new-fn
              (get fn-name 'unprofiled-fn) fn
              (get fn-name 'profile-time) 0
              (get fn-name 'profile-count) 0))))
  fn-name)

(defun unprofile1 (fn-name)
  "Make the function stop counting how often it is called."
  (setf (get fn-name 'profile-time) 0)
  (setf (get fn-name 'profile-count) 0)
  (when (eq (symbol-function fn-name) (get fn-name 'profiled-fn))
    ;; normal case: restore unprofiled version
    (setf (symbol-function fn-name)
          (get fn-name 'unprofiled-fn)))
  fn-name)

(defun get-fast-time ()
  "Return the elapsed time. This may wrap around;
   use `fast-time-difference' to compare."
  (get-internal-real-time))

(defun fast-time-difference (end start)
  "Subtract two time points."
  (- end start))

(defun fast-time->seconds (time)
  "Convert a fast-time interval into seconds."
  (/ time internal-time-units-per-second))

(defun fast-time->minutes (time)
  "Convert a fast-time interval into minutes."
  (/ (/ time internal-time-units-per-second) 60))

;; Inline expansion is a compiler optimization technique that reduces the overhead of a
;; function call by simply not doing the call: instead, the compiler effectively rewrites
;; the program to appear as though the definition of the called function was inserted at
;; each call site.

(proclaim '(inline profile-enter profile-exit inc-profile-time))

(defvar *profile-call-stack* nil)

(defun inc-profile-time (entry fn-name)
  (incf (get fn-name 'profile-time)
        (fast-time-difference (get-fast-time) (cdr entry))))

(defun profile-enter (fn-name)
  (incf (get fn-name 'profile-count))
  (unless (null *profile-call-stack*)
    ;; Time charged against the calling function:
    (inc-profile-time (first *profile-call-stack*)
                      (car (first *profile-call-stack*))))
  ;; Put a new entry on the stack
  (push (cons fn-name (get-fast-time))
        *profile-call-stack*))

(defun profile-exit (fn-name)
  ;; Time charged against the current function:
  (inc-profile-time (pop *profile-call-stack*)
                    fn-name)
  ;; Change the top entry to reflect current time
  (unless (null *profile-call-stack*)
    (setf (cdr (first *profile-call-stack*))
          (get-fast-time))))

(defun profiled-fn (fn-name fn)
  "Return a function that increments the count, and times."
  #'(lambda (&rest args)
      (profile-enter fn-name)
      (multiple-value-prog1
          (apply fn args)
        (profile-exit fn-name))))

(defun profile-count (fn-name) (get fn-name 'profile-count))

(defvar *inf-count*)

(defun profile-report (&optional
                         (fn-names (copy-list *profiled-functions*))
                         (key #'profile-count))
  "Report profiling statistics on given functions."
  (let ((total-time (reduce #'+ (mapcar #'profile-time fn-names))))
    (unless (null key)
      (setf fn-names (sort fn-names #'> :key key)))
    (format t "~%Total elapsed time: ~,2F seconds."
            (fast-time->seconds total-time))
    (format t "~&  Count     Secs   Mins Time% Name")
    (dolist (name fn-names)
      (format t "~&~7D ~8,2F ~6,2F  ~3d% ~A"
              ;; a convenient side effect:
              (setf *inf-count* (profile-count name))
              (fast-time->seconds (profile-time name))
              (fast-time->minutes (profile-time name))
              (round (/ (profile-time name) total-time) .01)
              name))
    (terpri)))

(defun profile-time (fn-name) (get fn-name 'profile-time))

;; Note the use of `unwind-protect' to produce the report and call `unprofile' even if the
;; computation is aborted

(defmacro with-profiling (fn-names &rest body)
  `(progn
     (unprofile . ,fn-names)
     (profile . ,fn-names)
     (setf *profile-call-stack* nil)
     (unwind-protect
          (progn . ,body)
       (profile-report ',fn-names)
       (unprofile . ,fn-names))))

(defun assert-equal (x y)
  (assert (equal x y) (x y)
	  "Expected ~a to be equal to ~a" x y))
