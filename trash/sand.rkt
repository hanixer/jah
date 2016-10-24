#lang racket

(define IN "jump.rkt")

(define (reading)
  (define str "")
  (do ([line (read-line) (read-line)])
      ((eof-object? line) str)
    (print line)
    (when (> (string-length line) 2)
      (set! str
        (string-append str (substring line 0 2))))))

(struct lexem ([type #:mutable] [val #:mutable]) #:transparent)

(struct buffer ([str #:mutable]  [pos #:mutable]) #:transparent)

(define (nondigit? ch)
  (list?
    (member ch 
            '(#\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                  #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                  #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                  #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))))




(define (read-lexem [in (current-input-port)])
  (define ch (peek-char in))
  (cond
    [(eof-object? ch) eof]
    [(nondigit? ch) (read-identifier in)]
    ;[(non
    [else 1]))

(define (read-identifier in)
  (define str (string (read-char in)))
  (do ([ch (read-char in) (read-char in)])
      ((or 
        (eof-object? ch)
        (and 
         (not (nondigit? ch))
         (not (char-numeric? ch))))
       (lexem 'identifier str))
    (set! str (~a str ch))))

(define (t)
  (define in-p (open-input-string "kkk+kk"))
  (read-identifier in-p))
       
  



