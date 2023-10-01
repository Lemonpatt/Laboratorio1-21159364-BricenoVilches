#lang racket

(require "TDAuser_21159364_BricenoVilches.rkt")

(define (getsystem-name system)
  (car system))

(define (getsystem-inicbcodelink system)
  (cadr system))


(define (getsystem-users system)
  (caddr system))


(define (getsystem-loggeduser system)
  (cadddr system))

(define (getsystem-chatbots system)
  (cadddr (cdr system)))

(define (juntar-system-chatbot system chatbot)
  (reverse(cons chatbot (reverse (getsystem-chatbots system)))))

(define (juntar-system-users system user)
  (reverse(cons user (reverse (getsystem-users system)))))

(define (juntar-loginuser-user loginuser users)
  (cons loginuser (cons users null)))


(define (añadir-chatbot-system system chatbot)
  (cons (getsystem-name system)
        (cons (getsystem-inicbcodelink system)
              (cons (getsystem-users system)
                    (cons (getsystem-loggeduser system)
                          (cons (juntar-system-chatbot system chatbot) null))))))

(define (añadir-user-system system user)
  (cons (getsystem-name system)
        (cons (getsystem-inicbcodelink system)
              (cons (juntar-system-users system user)
                    (cons (getsystem-loggeduser system)
                          (cons (getsystem-chatbots system) null))))))

(define (eliminar-user-logged list-users logged-user)
  (if (eq? (car (car list-users)) (car logged-user))
           (cons logged-user (cdr list-users))
           (cons (car list-users) (eliminar-user-logged (cdr list-users) logged-user))))

(define (logged? system)
    (not(eq? (getsystem-loggeduser system) '() )))

(define (user-presente? system user)
  (not(member (car user) (map car (getsystem-users system)))))


(define (logear user system)
   (cons (getsystem-name system)
        (cons (getsystem-inicbcodelink system)
              (cons (getsystem-users system)
                    (cons user
                          (cons (getsystem-chatbots system) null))))))

(define (logout-user system)
  (if (eq? (getsystem-loggeduser system) '())
      system
      (list (getsystem-name system)
            (getsystem-inicbcodelink system)
            (eliminar-user-logged (getsystem-users system) (getsystem-loggeduser system) )
            '()
            (getsystem-chatbots system))))

(define (getchatbot-initial system)
  (let ((ini-code (getsystem-inicbcodelink system))
    (chatbots (getsystem-chatbots system)))
  (define (getchatbot-initial2 ini-code chatbots)
    (if (eq? (car (car chatbots))  ini-code)
        (car chatbots)
        (getchatbot-initial2 ini-code (cdr chatbots))))
    (getchatbot-initial2 ini-code chatbots)))


(provide getsystem-chatbots getsystem-users añadir-chatbot-system añadir-user-system logged? logear user-presente? logout-user getchatbot-initial)