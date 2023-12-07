#lang r7rs

(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (scheme cxr)
        (scheme char))

(define enum-numb
  '(("1" 1)
    ("2" 2)
    ("3" 3)
    ("4" 4)
    ("5" 5)
    ("6" 6)
    ("7" 7)
    ("8" 8)
    ("9" 9)
    ("one" 1)
    ("two" 2)
    ("three" 3)
    ("four" 4)
    ("five" 5)
    ("six" 6)
    ("seven" 7)
    ("eight" 8)
    ("nine" 9)))

(define (super-match t)
  (define n-t (string-length t))
  (let loop
    ((i-t 0)
     (str t)
     (match-pair #f))         
    (cond
      ((> i-t (- n-t 1))
        '())
      ((let ((result (match str)))
         (set! match-pair result)
         (pair? result))
       (cons (cadr match-pair)
             (loop (+ i-t 1)
                   (substring str 1)
                   #f)))
      (else
       (loop (+ i-t 1)
             (substring str 1)
             #f)))))

(define (match s)
  (let loop
    ((enums enum-numb))
    (cond
      ((null? enums) #f)
      ((let* ((str (caar enums))
              (len-str (string-length str))
              (len-t (string-length s)))
         (and (<= len-str len-t)
              (string-ci=? str (substring s 0 len-str))))
       (car enums))
      (else (loop (cdr enums))))))

(define (last lst)
  (cond
    ((null? (cdr lst)) (car lst))
    (else (last (cdr lst)))))

(define (calibration)
  (call-with-input-file "1input.txt"
    (lambda (in)
      (let loop ((str (read-line in))
                 (result 0)
                 (n 1))
        (cond 
          ((eof-object? str) result)
          (else (let ((line-res (super-match str)))
                  (display n)(display " ")(display result)(display " ")(display line-res)(display #\newline)
                  (loop (read-line in)
                        (+ result (+ (* (car line-res) 10)
                                     (last line-res)))
                        (+ n 1)))))))))
                  

