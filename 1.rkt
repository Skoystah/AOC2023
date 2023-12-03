#lang r7rs

(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (scheme cxr)
        (scheme char))

(define (char->digit ch)
  (- (char->integer ch) 48))

(define (calibration)
  (call-with-input-file "1input.txt"
    (lambda (in)
      (let loop ((ch (read-char in))
                 (first #f)
                 (last #f)
                 (result 0))
        (cond 
          ((eof-object? ch) result)
          ((char=? ch #\newline) (display result)(display #\space)(display first)(display last)
                                 (display #\newline)
                                 (loop (read-char in) #f #f (if last
                                                                 (+ result (+ (* first 10) last))
                                                                 (+ result (+ (* first 10) first)))))
          ((char-numeric? ch)(if (not first)
                                 (loop (read-char in) (char->digit ch) last result)
                                 (loop (read-char in) first (char->digit ch) result)))
          (else (loop (read-char in) first last result)))))))
