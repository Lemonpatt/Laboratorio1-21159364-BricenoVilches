#lang racket


;Constructor
;
;


;Constructor
;
;
(define (crear-user name)
  (list name
        '() ;Lista para el ChatHistory del usuario
        ))


;Selector
;Dom:
;Rec:

(define (getuser-name user)
  (car user))


(define(find-user list-users username)
  (if (null? (filter (lambda (user) (string=? username (car user))) list-users))
      '()
      (car (filter (lambda (user) (string=? username (car user))) list-users))))



(provide crear-user getuser-name find-user)