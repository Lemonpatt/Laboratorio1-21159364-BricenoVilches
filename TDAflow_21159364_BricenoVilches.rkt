#lang racket

(require "TDAoption_21159364_BricenoVilches.rkt")

;RF 3 Función Flow - constructor
;Función crea una lista con los datos de un flow con ninguna o varias opciones sin repetir basandonos en su id usando una llave
;Dominio: id (int) X name-msg (String) X Option* (0 o más opciones)
;Recorrido: flow (list)

(define (flow id name-msg . Option)
  (list id name-msg (remove-duplicates Option #:key getflowoption-code)))


;Función Flow - selector option code
;Función que toma el código de una opción para despues ser usada como llave en la revisión de duplicados
;Dominio: option (list)
;Recorrido: code (int)
(define (getflowoption-code Option)
  (car Option))


(define (juntar-flow-op flow option)
  (reverse(cons option (reverse (getflow-options flow)))))

(define (añadir-opcion-flow flow option)
  (cons (getflow-id flow) (cons (getflow-namemsg flow) (cons (juntar-flow-op flow option) null))))

(define (no-opcion-duplicada? option list-opciones)
   (not (member (car option) (map car list-opciones))))

(define (getflow-options flow)
   (caddr flow))

(define (getflow-id flow)
  (car flow))

(define (getflow-namemsg flow)
   (cadr flow))

(provide getflowoption-code getflow-id añadir-opcion-flow getflow-options no-opcion-duplicada? juntar-flow-op all-defined-out)