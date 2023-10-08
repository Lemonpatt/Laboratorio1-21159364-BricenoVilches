#lang racket

(require "TDAoption_21159364_BricenoVilches.rkt")



;Capa selector


;Retorna la lista de opciones dentro de un flow
;Dominio: flow (list)
;Recorrido: options (list)

(define (getflow-options flow)
   (caddr flow))


;Retorna la id de un flow
;Dominio: flow (list)
;Recorrido: id (int)

(define (getflow-id flow)
  (car flow))


;Retorna la lista de opciones dentro de un flow
;Dominio: flow (list)
;Recorrido: options (list)

(define (getflow-namemsg flow)
   (cadr flow))



;Capa Modificador


;Toma un flow y una opción y crea un nuevo flow con la opcion añadida a la lista de opciones
;Dominio: option (list)
;Recorrido: code (int)

(define (añadir-opcion-flow flow option)
  (cons (getflow-id flow)
        (cons (getflow-namemsg flow)
              (cons (juntar-flow-op flow option) null))))


;Capa pertenencia

;Revisa si el primer elemento de una lista se encuentra dentro de una lista de listas en la misma posición
;Dominio: elem (list) X list-elem (list)
;Recorrido: Boolean

(define (no-duplicado? elem list-elem)
   (not (member (car elem) (map car list-elem))))



;Otros

;Función auxiliar para añadir la opcion a la lista de opciones de un flow
;Dominio: flow (list) X option (list)
;Recorrido: option (list)

(define (juntar-flow-op flow option)
  (sort (reverse(cons option (reverse (getflow-options flow)))) < #:key get-option-code))



;Muestra las opciones de un flow elegido por el chatbot al conversar con este
;Dominio: lista de opciones
;Recorrido: Muestra string en consola 

(define (show-options list-options)
  (if (null? (cdr list-options))
      (begin
        (display (string-append (number->string( car (car list-options))) ")" (cadr(car list-options)) "\n\n"))
        )
      (begin
        (display (string-append (number->string (car (car list-options))) ")" (cadr(car list-options)) "\n\n"))
        (show-options (cdr list-options)))))


;Lo mismo que show-options pero de manera no recursiva para el requisito funcional 13
;Dominio: lista de opciones
;Recorrido: Muestra string en consola
(define (show-options-norec list-options)
  (map
   (lambda (option)
     (display (string-append (number->string (car option)) ")" (cadr option) "\n\n")))
   list-options))


;Toma todo lo importante dentro de cada opción dentro del flow y lo guarda como un string para posteriormente ser guardado en el historial del chat
;Dominio: lista de opciones
;Recorrido: string

(define (save-options list-options)
  (if (null? (cdr list-options))
        (string-append (number->string( car (car list-options))) ")" (cadr(car list-options)) "\n\n")
        (string-append (number->string( car (car list-options))) ")" (cadr(car list-options)) "\n\n"
                       (save-options (cdr list-options)))))


;Lo mismo que save-options pero de manera no recursiva para el requisito funcional 13
;Dominio: lista de opciones
;Recorrido: string

(define (save-options-norec list-options)
  (apply string-append
   (map
    (lambda (option)
      (string-append
       (number->string (car option))
       ")" (cadr option) "\n\n"))
    list-options)))



(provide añadir-opcion-flow getflow-options getflow-id no-duplicado? show-options save-options show-options-norec save-options-norec)