#lang racket

(require "TDAflow_21159364_BricenoVilches.rkt")

;RF 5 Función Chatbot - constructor
;Función crea un chatbot con una ID única, un nombre, mensaje, ID del flujo con el cual comienza y 0 o muchos flujos
;Dominio: chatbotID (int) X name (String) X welcomeMessage (String) X startFlowId(int) X  flows* (0 o más flujos)
;Recorrido: chatbot (list)

(define (chatbot chatbotID name welcomeMessage startFlowId . flows)
  (list chatbotID name welcomeMessage startFlowId flows))



(define (getchatbot-id chatbot)
  (car chatbot))


(define (getchatbot-name chatbot)
  (cadr chatbot))


(define (getchatbot-msg chatbot)
  (caddr chatbot))


(define (getchatbot-startflowid chatbot)
  (cadddr chatbot))

(define (getchatbot-flows chatbot)
  (cadddr (cdr chatbot)))

(define (getchatbotflows-code flows)
  (car flows))

(define (añadir-flow-chatbot chatbot flow)
  (cons (getchatbot-id chatbot) (cons (getchatbot-name chatbot) (cons (getchatbot-msg chatbot) (cons (getchatbot-startflowid chatbot) (cons (juntar-chatbot-flow chatbot flow) null))))))

(define (juntar-chatbot-flow chatbot flow)
  (reverse(cons flow (reverse (getchatbot-flows chatbot)))))

(provide getchatbot-id getchatbot-flows getchatbotflows-code añadir-flow-chatbot)