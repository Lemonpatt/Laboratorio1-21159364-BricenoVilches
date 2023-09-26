#lang racket


;RF 2 Función Option - constructor
;Función crea una opción con un código único para un flow de un chatbot
;Dominio: code (Int) X message (String) X ChatbotCodeLink (Int) X InitialFlowCodeLink (Int) X Keyword* (0 o más strings)
;Recorrido: option (list)

(define (option code message ChatbotCodeLink InitialFlowCodeLink . Keyword)
  (list code message ChatbotCodeLink InitialFlowCodeLink Keyword))
