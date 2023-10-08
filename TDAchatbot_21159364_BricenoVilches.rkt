#lang racket

(require "TDAflow_21159364_BricenoVilches.rkt")


;Capa selector


;Retorna el id de un chatbot
;Dominio: chatbot (list)
;Recorrido: id (int)

(define (getchatbot-id chatbot)
  (car chatbot))


;Retorna el nombre de un chatbot
;Dominio: chatbot (list)
;Recorrido: name (string)

(define (getchatbot-name chatbot)
  (cadr chatbot))


;Retorna el mensaje de un chatbot
;Dominio: chatbot (list)
;Recorrido: message chatbot (string)

(define (getchatbot-msg chatbot)
  (caddr chatbot))


;Retorna el startflowid de un chatbot
;Dominio: chatbot (list)
;Recorrido: startflowid (int)

(define (getchatbot-startflowid chatbot)
  (cadddr chatbot))


;Retorna los flows de un chatbot
;Dominio: chatbot (list)
;Recorrido: flows (list)

(define (getchatbot-flows chatbot)
  (cadddr (cdr chatbot)))


;Retorna el id de un flow de la lista de flows de un chatbot/ Usada para revisar flows repetidos dentro de un chatbot
;Dominio: lista flows (list)
;Recorrido: id (int)

(define (getchatbotflows-code flows)
  (car flows))


;Encuentra y retorna el flow inicial de un chatbot haciendo uso de recursión natural para hacer una comparación de los elementos de la lista de flows y ver si se encuentra una id igual
;Dominio: lista flows (list) X idFlowinitial (int)
;Recorrido: Flow inicial (list)

(define (iniflowops-chatbot idflow-chatbot flows-chatbot)
  (if (eq? idflow-chatbot (car (car flows-chatbot) ))
      (car flows-chatbot)
      (iniflowops-chatbot idflow-chatbot (cdr flows-chatbot))))


;Encuentra y retorna el flow inicial de un chatbot de forma no recursiva
;Dominio: lista flows (list) X idFlowinitial (int)
;Recorrido: Flow inicial (list)

(define (iniflowops-chatbot-norec idflow-chatbot flows-chatbot)
  (let ((flow-matches-ini-code?
         (lambda (flow)
           (eq? (car flow) idflow-chatbot))))
  (car (filter flow-matches-ini-code? flows-chatbot))))


;Capa Modificador


;Añade un flow nuevo a un chatbot
;Dominio: chatbot (list) X flow (list)
;Recorrido: chatbot (list)

(define (añadir-flow-chatbot chatbot flow)
  (cons (getchatbot-id chatbot) (cons (getchatbot-name chatbot) (cons (getchatbot-msg chatbot) (cons (getchatbot-startflowid chatbot) (cons (juntar-chatbot-flow chatbot flow) null))))))


;Función auxiliar que contiene la recursion de cola pedida para juntar el flow nuevo al final de la lista de flows
;Dominio: chatbot (list) X flow (list)
;Recorrido: flows (list)

(define (juntar-chatbot-flow chatbot flow)
  (define (juntar-chatbot-flow-tail list-flows flow acc)
    (if (null? list-flows) (append acc (list flow))
        (juntar-chatbot-flow-tail (cdr list-flows) flow (append acc (list(car list-flows))))))
  (juntar-chatbot-flow-tail (getchatbot-flows chatbot) flow null))


;Actualiza un chatbot que ahora tendrá un flow inicial nuevo
;Dominio: chatbot (list) X newstartflow-id (int)
;Recorrido: chatbot (list)
(define (chatbot-startflow-update chatbot newstartflow-id)
  (cons (getchatbot-id chatbot)
        (cons (getchatbot-name chatbot)
              (cons (getchatbot-msg chatbot)
                    (cons newstartflow-id
                          (cons (getchatbot-flows chatbot) null))))))


(provide getchatbot-id getchatbot-flows getchatbotflows-code añadir-flow-chatbot iniflowops-chatbot chatbot-startflow-update iniflowops-chatbot-norec)