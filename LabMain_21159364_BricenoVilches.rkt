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
  (list id name-msg (sort (remove-duplicates Option #:key getflowoption-code) < #:key getflowoption-code))) ;Para que las opciones queden ordenadas por su id


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

(define (system name InitialChatbotCodeLink . chatbots)
  (list name InitialChatbotCodeLink
        '() ;Lista usuarios vacía /(El system siempre se crea sin usuarios añadidos a este)
        '() ;parámetro con la lista del usuario con su nombre y chathistory
        (remove-duplicates chatbots #:key getchatbot-id)))


;RF 8 TDA system - modificador
;Función modificadora para añadir chatbots a un sistema
;Dominio: system (list) X chatbot (list)
;Recorrido: system (list)

(define (system-add-chatbot system chatbot)
  (if (no-duplicado? chatbot (getsystem-chatbots system))
      (añadir-chatbot-system system chatbot)
      system))



;RF 9 TDA system - modificador
;Función modificadora para añadir usuarios a un sistema
;Dominio: system (list) X user (list)
;Recorrido: system (list)

(define (system-add-user system username)
  (let ((user (crear-user username)))
  (if (no-duplicado? user (getsystem-users system))
      (añadir-user-system system user)
      system)))


;RF 10 TDA system - modificador
;Función que añade un usuario al parámetro de usuario-logeado del sistema
;Dominio: system (list) X user (list)
;Recorrido: system (list) o texto de error (string)

(define (system-login system username)
  (let ((user (find-user (getsystem-users system) username)))
    (if (null? user)
        system
        (if (logged? system)
            system
            (if (user-presente? system user)
                system
                (logear user system))))))


;RF 11 TDA system - modificador
;Función que crea una sesión abierta dentro del system
;Dominio: system (list)
;Recorrido: system (list)

(define (system-logout system)
  (logout-user system))




(define op1(option 1 "Que quiere saber?" 0 3 "viajar" "descuentos" "sobre nosotros" ))
(define op11 (option 4 "Empresa" 0 3 "sobre nosotros" ))
(define op2 (option 2 "Buscas noticias?" 1 1 "noticias" "quiero ver noticias" "Busco noticias"))
(define op3 (option 3 "O quieres saber sobre nuevas ofertas en supermercados?" 1 1 "ofertas"))
(define f1 (flow 1 "Flujo1: prueba" op1 op1 op11 op2 op1 op11 op2))
(define f1-new (flow-add-option f1 op3))
(define a (getflow-options f1))
(define f10 (flow 3 "Flujo1 Principal Chatbot1"))
(define f11 (flow-add-option f10 op1))
(define f12 (flow-add-option f11 op2))
(define cb10 (chatbot 0 "Asistente" "Bienvenido. ¿Qué te gustaría hacer?" 1))
(define cbtest (chatbot 2 "Asistente" "Bienvenido. ¿Qué te gustaría hacer?" 1))

(define cb11 (chatbot-add-flow cb10 f1))
(define cb12 (chatbot-add-flow cb11 f1-new))
(define cb13 (chatbot-add-flow cb12 f12))
(define s1 (system "NewSystem" 0 cb13))
(define s11(system-add-chatbot s1 cbtest))
(define s-user(system-add-user s11 "admin"))
(define s-user1(system-add-user s-user "user1"))
(define s-user2(system-add-user s-user1 "user2"))
(define re(getsystem-chatbots s-user))
(define usuario-log (system-login s-user1 "user1"));añade usuario admin a la lista de logeados
usuario-log
(define sss(getsystem-users usuario-log))
(define usuario-log1 (system-login usuario-log "user1")) ;falla ya que hay alguien loggeado ya
(define usuario-log2 (system-login s-user "user2")) ;falla ya que no encuentra al usuario en el sistema
(define logout (system-logout usuario-log))



(provide flow flow-add-option chatbot chatbot-add-flow system system-add-chatbot system-add-user system-login system-logout system-talk-rec system-talk-norec system-synthesis system-simulate)