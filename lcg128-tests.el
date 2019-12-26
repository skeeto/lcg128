;;; lcg128-tests.el --- tests for lcg128 -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Benchmarks and ERT tests for the lcg128 package.

;;; Code:

(require 'ert)
(require 'lcg128)

(defun lcg128--benchmark ()
  (let ((names '("lcg128" "cl-random" "random"))
        (results (list (benchmark-run 10
                         (dotimes (_ 100000)
                           (lcg128-range #x400)))
                       (benchmark-run 10
                         (dotimes (_ 100000)
                           (cl-random #x400)))
                       (benchmark-run 10
                         (dotimes (_ 100000)
                           (random #x400))))))
    (cl-loop for name in names
             for result in results
             do (princ (format "%-10s[small] %S\n" name result))))
  (let ((names '("lcg128" "cl-random" "random"))
        (results (list (benchmark-run 10
                         (dotimes (_ 100000)
                           (lcg128-range #x1000000)))
                       (benchmark-run 10
                         (dotimes (_ 100000)
                           (cl-random #x1000000)))
                       (benchmark-run 10
                         (dotimes (_ 100000)
                           (random #x1000000))))))
    (cl-loop for name in names
             for result in results
             do (princ (format "%-10s[large] %S\n" name result)))))

(ert-deftest lcg128-valid ()
  "Verifies that the algorithm is working as intended.

The table of expected values was generated from this a Python
implementation of the same generator:

s = 0
for _ in range(21):
    s *= 0x86cbe851ccd8e971cdd864f4f5fd99b5 
    s += 0xc3385d20aa58ba6d70f12e993960a383
    s &= 0xffffffffffffffffffffffffffffffff
    print(f'#x{s >> 64:016x}')"
  (let ((state (lcg128-create 0))
        (expect '(#xc3385d20aa58ba6d #x7abac485a110db03 #x447abdcd11a7814c
                  #xd682b6e5489123aa #xe8da74646e02dfab #xa0620ca19e4398fb
                  #x58b553a184dc3a45 #x8129c4a1520b5645 #x2bdd0e5222abf095
                  #xf3ad737c085f794f #xcf75d67d279b76d2 #x66d2a77412fae671
                  #xd2386a8ef0f5a13f #xe8f72a6f89f5e81d #x80a8190834fad237
                  #x15c98afbd6b42aad #x2fe95ef594719530 #xb95d0700ea40ddfc
                  #x72ade5d3c828a4ee #x847f879778a30c83 #xaa3fc383ec12d542)))
    (dolist (n expect)
      (should (eql n (lcg128-bits 64 state))))))

(ert-deftest lcg128-seed ()
  "Tests that using the same seed results in the same stream."
  (let* ((seed "foobarbaz")
         (state-a (lcg128-create seed))
         (state-b (lcg128-create seed))
         (n (lsh 1 261)))
    (dotimes (_ 1000)
      (should (eql (lcg128-range n state-a)
                   (lcg128-range n state-b))))))

(ert-deftest lcg128-copy ()
  "Tests that `lcg128-copy' produces a perfect copy of the generator."
  (let* ((state-a (lcg128-create :copy-test))
         (state-b (progn
                    (while (not (zerop (lcg128-range 1000 state-a))))
                    (lcg128-copy state-a)))
         (n (lsh 1 261)))
    (should (equal state-a state-b))
    (dotimes (_ 1000)
      (should (eql (lcg128-range n state-a)
                   (lcg128-range n state-b))))))

(ert-deftest lcg128-range ()
  "Tests that `lcg128-range' produces only values in range."
  (let* ((state (lcg128-create 'lcg128-range))
         (max 300)
         (draws 100000)
         (hist (make-vector max 0)))
    (dotimes (_ draws)
      (let ((n (lcg128-range max state)))
        (should (and (>= n 0) (< n max)))
        (cl-incf (aref hist n))))
    (let ((expect (/ draws (float max))))
      (dotimes (i (length hist))
        (should (< (abs (- (aref hist i) expect))
                   (* expect 0.20)))))))

(ert-deftest lcg128-shuffle ()
  "Test `lcg128-shuffle' on strings, vectors, and lists, including empty."
  (should (equal (lcg128-shuffle (string)) ""))
  (should (equal (lcg128-shuffle (string ?1)) "1"))
  (dotimes (_ 100)
    (let ((result (lcg128-shuffle (string ?1 ?2))))
      (should (or (equal result "12") (equal result "21")))))
  (should (equal (lcg128-shuffle (vector)) []))
  (should (equal (lcg128-shuffle (vector 1)) [1]))
  (dotimes (_ 100)
    (let ((result (lcg128-shuffle (vector 1 2))))
      (should (or (equal result [1 2]) (equal result [2 1])))))
  (should (equal (lcg128-shuffle (list)) '()))
  (should (equal (lcg128-shuffle (list 1)) '(1)))
  (dotimes (_ 100))
  (let ((result (lcg128-shuffle (list 1 2))))
    (should (or (equal result '(1 2)) (equal result '(2 1))))))

;;; lcg128-tests.el ends here
