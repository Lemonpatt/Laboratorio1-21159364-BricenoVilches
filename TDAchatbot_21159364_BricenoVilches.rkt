#lang racket


;RF 5 Función Chatbot - constructor
;Función crea un chatbot con una ID única, un nombre, mensaje, ID del flujo con el cual comienza y 0 o muchos flujos
;Dominio: chatbotID (int) X name (String) X welcomeMessage (String) X startFlowId(int) X  flows* (0 o más flujos)
;Recorrido: chatbot (list)

(define (chatbot chatbotID name welcomeMessage startFlowId . flow)
  (list chatbotID name welcomeMessage startFlowId flow))