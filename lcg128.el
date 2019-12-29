;;; lcg128-el --- 128-bit truncated LCG bignum PRNG -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; See README.md.

;;; Code:

(require 'cl-lib)

(defvar lcg128--state 
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (set-buffer-multibyte nil)
      (prin1 (random))
      (prin1 (cl-random 1.0))
      (prin1 (emacs-pid))
      (dotimes (_ 256)
        (prin1 (time-convert nil t)))
      (ignore-errors
        (call-process "dd" nil t nil "if=/dev/urandom" "bs=16" "count=1"))
      (list (string-to-number (md5 (current-buffer)) 16))))
  "Global random state used by default when no state is supplied.")

(defun lcg128-create (&optional seed)
  "Return a new PRNG state, randomly seeded if no seed is provided.
The seed may be any lisp object of any type, except nil."
  (list
   (cl-typecase seed
     (integer seed)
     (null (lcg128-bits 128 lcg128--state))
     (string (string-to-number (md5 seed) 16))
     (buffer (string-to-number (md5 seed) 16))
     (otherwise (string-to-number (md5 (prin1-to-string seed)) 16)))))

(defsubst lcg128-p (object)
  "Return non-nil of OBJECT is an lcg128 state."
  (and (consp object)
       (integerp (car object))
       (null (cdr object))))

(defsubst lcg128-copy (state)
  "Create an exact copy of STATE."
  (list (car state)))

(cl-defun lcg128--next (&optional (state lcg128--state))
  "Advance the PRNG to the next state and return it as an integer.

DO NOT use more than the high 64 bits of the return value. It is
returned unshifted for efficiency."
  (let ((m #x86cbe851ccd8e971cdd864f4f5fd99b5)
        (a #xc3385d20aa58ba6d70f12e993960a383)
        (n #xffffffffffffffffffffffffffffffff)
        (s (car state)))
    (setf (car state) (logand n (+ a (* s m))))))

(cl-defun lcg128-uniform (&optional (state lcg128--state))
  "Return a random float in [0.0 – 1.0)."
  (ldexp (lsh (lcg128--next state) -75) -53))

(cl-defun lcg128-bits (nbits &optional (state lcg128--state))
  "Return the next NBITS bits of the generator as an integer."
  (let ((accum 0)
        (rem (% nbits 64)))
    (dotimes (_ (/ nbits 64))
      (setf accum (logior (lsh accum 64)
                          (lsh (lcg128--next state) -64))))
    (if (zerop rem)
        accum
      (logior (lsh accum rem)
              (lsh (lcg128--next state) (- rem 128))))))

(cl-defun lcg128-range (limit &optional (state lcg128--state))
  "Return a random integer in [0,limit) without bias."
  (let ((nbits (1+ (max 0 (logb (1- limit))))))
    (cl-loop for result = (lcg128-bits nbits state)
             then (lcg128-bits nbits state)
             until (< result limit)
             finally return result)))

(cl-defun lcg128-shuffle (seq &optional (state lcg128--state))
  "Destructively shuffle the elements of SEQ, optionally using STATE."
  (cond ((or (stringp seq) (vectorp seq))
         (cl-loop for i from (length seq) downto 2
                  for j = (lcg128-range i state)
                  do (cl-rotatef (aref seq (1- i)) (aref seq j))
                  finally return seq))
        ((listp seq)
         (let ((length (length seq)))
           (if (< length 1)
               seq
             (let ((copy (make-vector length nil))
                   (head seq))
               ;; Fill vector with the cons cells (not the elements)
               (dotimes (i length)
                 (setf (aref copy i) head
                       head (cdr head)))
               ;; Shuffle the cons cells
               (lcg128-shuffle copy state)
               ;; Reconnect cons cells in the new order
               (prog1 (aref copy 0)
                 (dotimes (i (1- length))
                   (setf (cdr (aref copy i)) (aref copy (1+ i))))
                 (setf (cdr (aref copy (1- length))) nil))))))
        ((signal 'wrong-type-argument (list 'sequencep seq)))))

(cl-defun lcg128-normal (&optional (state lcg128--state))
  "Returns a pair of random numbers from the normal distribution.

Mean is 0 and standard deviation is 1. The values are returned as
the car and cdr of a cons cell."
  (let ((x (lcg128-uniform state))
        (y (lcg128-uniform state)))
    (cons (* (sqrt (* -2 (log x))) (cos (* 2 pi y)))
          (* (sqrt (* -2 (log x))) (sin (* 2 pi y))))))

(provide 'lcg128)

;;; lcg128.el ends here
