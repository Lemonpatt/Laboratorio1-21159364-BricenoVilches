#lang racket



;Constructor
;Crea una lista de usuario que contiene su nombre y un espacio para su historial de chat
;Dom: name (string)
;Rec: usuario (list)
(define (crear-user name)
  (list name
        '() ;Lista para el ChatHistory del usuario
        ))


;Selector
;Obtiene el nombre de un usuario
;Dom: user (list)
;Rec: nombre (string)

(define (getuser-name user)
  (car user))


;Selector
;Revisa si el nombre de un usuario se encuentra dentro de la lista de usuarios de un sistema y lo retorna 
;Dom: lista usuarios X username (string)
;Rec: usuario (list)

(define(find-user list-users username)
  (if (null? (filter (lambda (user) (string=? username (getuser-name user))) list-users))
      '()
      (car (filter (lambda (user) (string=? username (getuser-name user))) list-users))))



(provide crear-user getuser-name find-user)