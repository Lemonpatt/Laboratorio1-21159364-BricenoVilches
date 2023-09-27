#lang racket

(require "TDAoption_21159364_BricenoVilches.rkt")

;RF 3 Función Flow - constructor
;Función crea una lista con los datos de un flow con ninguna o varias opciones sin repetir basandonos en su id usando una llave
;Dominio: id (int) X name-msg (String) X Option* (0 o más opciones)
;Recorrido: flow (list)

(define (flow id name-msg . Option)
  (list id name-msg (remove-duplicates Option #:key getoption-code)))


;Función Flow - selector option code
;Función que toma el código de una opción para despues ser usada como llave en la revisión de duplicados
;Dominio: option (list)
;Recorrido: code (int)
(define (getoption-code Option)
  (car Option))


; 
;(define (añadir-opcion-flow flow opcion)
; (reverse(cons option (reverse flow)))


(define op1(option 1 "Que quiere saber?" 1 1 "viajar" "descuentos" "sobre nosotros" ))
(define op11 (option 1 "Empresa" 2 3 "sobre nosotros" ))
(define op2 (option 2 "Buscas noticias?" 1 1 "noticias" "quiero ver noticias" "Busco noticias"))
(define op3 (option 3 "O quieres saber sobre nuevas ofertas en supermercados?" 1 1 "ofertas"))
(define f1 (flow 1 "Flujo1: prueba" op1 op1 op11 op2 op1 op11 op2 op3 op3))
f1