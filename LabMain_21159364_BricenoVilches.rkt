#lang racket

(require "TDAoption_21159364_BricenoVilches.rkt")
(require "TDAflow_21159364_BricenoVilches.rkt")
(require "TDAchatbot_21159364_BricenoVilches.rkt")
(require "TDAuser_21159364_BricenoVilches.rkt")
(require "TDAsystem_21159364_BricenoVilches.rkt")


;RF 2 TDA Option - constructor
;Función crea una opción con un código único para un flow de un chatbot.
;Dominio: code (Int) X message (String) X ChatbotCodeLink (Int) X InitialFlowCodeLink (Int) X Keyword* (0 o más strings)
;Recorrido: option (list)

(define (option code message ChatbotCodeLink InitialFlowCodeLink . Keyword)
  (list code message ChatbotCodeLink InitialFlowCodeLink Keyword))


;RF 3 TDA Flow - constructor
;Función crea una lista con los datos de un flow con ninguna o varias opciones sin repetir basandonos en su id usando una llave.
;Dominio: id (int) X name-msg (String) X Option* (0 o más opciones)
;Recorrido: flow (list)

(define (flow id name-msg . Option)
  (list id name-msg (remove-duplicates Option #:key getflowoption-code)))


;RF 4 TDA Flow - modificador
;Función añade una opción a un flujo solamente si la ID de esta no está ya presente en las opciones del flujo.
;Dominio: flow (list) X option (list)
;Recorrido: flow (list)

(define (flow-add-option flow option)
  (if (no-duplicado? option (getflow-options flow))
      (añadir-opcion-flow flow option)
      flow))


;RF 5 TDA Chatbot - constructor
;Función crea un chatbot con una ID única, un nombre, mensaje, ID del flujo con el cual comienza y 0 o muchos flujos.
;Dominio: chatbotID (int) X name (String) X welcomeMessage (String) X startFlowId(int) X  flows* (0 o más flujos)
;Recorrido: chatbot (list)

(define (chatbot chatbotID name welcomeMessage startFlowId . flows)
  (list chatbotID name welcomeMessage startFlowId (remove-duplicates flows #:key getchatbotflows-code)))


;RF 6 TDA Chatbot - modificador
;Función que añade un flujo a un chatbot solo si este no está repetido en base a su ID único
;Dominio: chatbot (list) X flow (list)
;Recorrido: chatbot (list)


(define (chatbot-add-flow chatbot flow)
  (if (no-duplicado? flow (getchatbot-flows chatbot))
      (añadir-flow-chatbot chatbot flow)
      chatbot))


;RF 7 TDA system - constructor
;Función que crea un sistema que contiene chatbots
;Dominio:  name (string) X InitialChatbotCodeLink (Int) X chatbot* (0 o más chatbots)
;Recorrido: system (list)

(define (system name InitialChatbotCodeLink users . chatbot)
  (list name InitialChatbotCodeLink users chatbot))


;RF 8 TDA system - modificador
;Función modificadora para añadir chatbots a un sistema
;Dominio: system (list) X chatbot (list)
;Recorrido: system (list)

(define (system-add-chatbot system chatbot)
  (if (no-duplicado? chatbot (getsystem-chatbots system))
      (añadir-chatbot-system system chatbot)
      system))

;TDA usuario define (usuario nombre chatHistory)?

(define op1(option 1 "Que quiere saber?" 1 1 "viajar" "descuentos" "sobre nosotros" ))
(define op11 (option 1 "Empresa" 2 3 "sobre nosotros" ))
(define op2 (option 2 "Buscas noticias?" 1 1 "noticias" "quiero ver noticias" "Busco noticias"))
(define op3 (option 3 "O quieres saber sobre nuevas ofertas en supermercados?" 1 1 "ofertas"))
(define f1 (flow 1 "Flujo1: prueba" op1 op1 op11 op2 op1 op11 op2))
(define f1-new (flow-add-option f1 op3))
(define a (getflow-options f1))
(define f10 (flow 3 "Flujo1: mensaje de prueba"))
(define f11 (flow-add-option f10 op1))
(define f12 (flow-add-option f11 op2))
(define cb10 (chatbot 0 "Asistente" "Bienvenido\n¿Qué te gustaría hacer?" 1))
(define cbtest (chatbot 2 "Asistente" "Bienvenido\n¿Qué te gustaría hacer?" 1))
f1
f12
cb10

(define cb11 (chatbot-add-flow cb10 f1))
(define cb12 (chatbot-add-flow cb11 f1-new))
(define cb13 (chatbot-add-flow cb12 f12))
(define s1 (system "NewSystem" 2 '() cb13))
(define user1 (user "admin" '()))
(define s11(system-add-chatbot s1 cbtest))

