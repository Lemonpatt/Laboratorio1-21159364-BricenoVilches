#lang racket

(require "TDAoption_21159364_BricenoVilches.rkt")
(require "TDAchatbot_21159364_BricenoVilches.rkt")
(require "TDAflow_21159364_BricenoVilches.rkt")
(require "TDAchathistory_21159364_BricenoVilches.rkt")


;Capa selector

;Retorna el nombre del sistema
;Dom: system (list)
;Rec: name (string)

(define (getsystem-name system)
  (car system))


;Retorna el id del chatbot inicial del sistema
;Dom: system (list)
;Rec: initialChatbotCodeLink (int)

(define (getsystem-inicbcodelink system)
  (cadr system))


;Retorna la lista de usuarios registrados en el sistema
;Dom: system (list)
;Rec: usuarios (list)

(define (getsystem-users system)
  (caddr system))


;Retorna al usuario que este logeado en el sistema
;Dom: system (list)
;Rec: usuario logeado (list)

(define (getsystem-loggeduser system)
  (cadddr system))


;Retorna los chatbots dentro del sistema
;Dom: system (list)
;Rec: chatbots (list)

(define (getsystem-chatbots system)
  (cadddr (cdr system)))


;Retorna el chatbot inicial de un sistema, esto dado por la id de su chatbot inicial
;Usa recursion de cola ya que así podemos guardar el scope del LET sin que estos se vean por una recursión natural
;Dom: system (list)
;Rec: chatbot inicial (list)

(define (getchatbot-initial system)
  (let ((ini-code (getsystem-inicbcodelink system))
        (chatbots (getsystem-chatbots system)))
    (define (getchatbot-initial2 ini-code chatbots)
      (if (eq? (car (car chatbots))  ini-code)
          (car chatbots)
          (getchatbot-initial2 ini-code (cdr chatbots))))
    (getchatbot-initial2 ini-code chatbots)))


;Retorna el chatbot inicial de un sistema, esto dado por la id de su chatbot inicial
;Usa recursion de cola ya que así podemos guardar el scope del LET sin que estos se vean por una recursión natural
;Dom: system (list)
;Rec: chatbot inicial (list)

(define (getchatbot-initial-norec system)
  (let ((ini-code (getsystem-inicbcodelink system))
        (chatbot-matches-ini-code?
         (lambda (chatbot)
           (eq? (car chatbot) (getsystem-inicbcodelink system))))
        (chatbots (getsystem-chatbots system)))
    (car(filter chatbot-matches-ini-code? chatbots))))


;Capa modificador

;Añade un chatbot a la lista de chatbots del sistema
;Dom: system (list) X chatbot (list)
;Rec: chatbots (list)

(define (juntar-system-chatbot system chatbot)
  (reverse(cons chatbot (reverse (getsystem-chatbots system)))))


;Añade un usuario a la lista de usuarios de un sistema
;Dom: system (list) X user (list)
;Rec: users (list)

(define (juntar-system-users system user)
  (reverse(cons user (reverse (getsystem-users system)))))


;Actualiza un sistema para tener un chatbot nuevo en la lista de chatbots
;Dom: system (list) X chatbot (list)
;Rec: system (list)

(define (añadir-chatbot-system system chatbot)
  (cons (getsystem-name system)
        (cons (getsystem-inicbcodelink system)
              (cons (getsystem-users system)
                    (cons (getsystem-loggeduser system)
                          (cons (juntar-system-chatbot system chatbot) null))))))


;Actualiza un sistema para tener un usuario nuevo dentro de la lista de usuarios en el sistema
;Dom: system (list) X user (list)
;Rec: system (list)

(define (añadir-user-system system user)
  (cons (getsystem-name system)
        (cons (getsystem-inicbcodelink system)
              (cons (juntar-system-users system user)
                    (cons (getsystem-loggeduser system)
                          (cons (getsystem-chatbots system) null))))))


;Modifica una lista de usuarios, añadiendo al usuario logeado dentro de la lista de usuarios eliminando su anterior lista para sustituirla con la actualizada con su historial haciendo uso de recursion natural para revisar uno por uno cada usuario y compararlo con el usuario logeado
;Dom: list-users (list) X logged-user (list)
;Rec: users (list)

(define (eliminar-user-logged list-users logged-user)
  (if (eq? (car (car list-users)) (car logged-user))
           (cons logged-user (cdr list-users))
           (cons (car list-users) (eliminar-user-logged (cdr list-users) logged-user))))


;Modifica un sistema para ahora tener a un usuario en el parametro de logeado
;Dom: user (list) X system (list)
;Rec: system (list)

(define (logear user system)
   (cons (getsystem-name system)
        (cons (getsystem-inicbcodelink system)
              (cons (getsystem-users system)
                    (cons user
                          (cons (getsystem-chatbots system) null))))))


;Modifica un sistema con alguien logeado para ahora vaciar ese parametro y devolver el usuario a la lista de usuarios ahora, este tambien actualizado
;Dom: system (list)
;Rec: system (list)

(define (logout-user system)
  (if (eq? (getsystem-loggeduser system) '())
      system
      (list (getsystem-name system)
            (getsystem-inicbcodelink system)
            (eliminar-user-logged (getsystem-users system) (getsystem-loggeduser system) )
            '()
            (getsystem-chatbots system))))


;retorna un sistema que solo actualiza el chathistory del usuario logeado cuando el mensaje no encontro nada indicativo de una opcion elegida
;Dom: system (list) X msg (string)
;Rec: system (list)

(define (update-talk-rec-sin-interaccion system msg)
  (let ((chatbot (getchatbot-initial system)))
    (cons (getsystem-name system)
          (cons (getsystem-inicbcodelink system)
                (cons (getsystem-users system)
                      (cons (update-chathistory system msg chatbot)
                            (cons (getsystem-chatbots system) null)))))))


;Lo mismo que su version de arriba pero ahora ya no usa funciones que ocupan recursion
;Dom: system (list) X msg (string)
;Rec: system (list)

(define (update-talk-norec-sin-interaccion system msg)
  (let ((chatbot (getchatbot-initial-norec system)))
    (cons (getsystem-name system)
          (cons (getsystem-inicbcodelink system)
                (cons (getsystem-users system)
                      (cons (update-chathistory-norec system msg chatbot)
                            (cons (getsystem-chatbots system) null)))))))


;retorna un sistema que actualiza el chathistory del usuario logeado y tambien cambia los valores de chatbot y flow inicial dados por la opcion elegida
;Dom: system (list) X msg (string)
;Rec: system (list)

(define (update-talk-rec system msg)
  (let ((chatbot (getchatbot-initial system))
        (idflow-chatbot (list-ref (getchatbot-initial system) 3))
        (flows-chatbot (list-ref (getchatbot-initial system) 4)))
    (cons (getsystem-name system)
          (cons (list-ref (get-eleccion-op (list-ref(iniflowops-chatbot idflow-chatbot flows-chatbot) 2) msg) 2)
                (cons (getsystem-users system)
                      (cons (update-chathistory system msg chatbot)
                            (cons (chatbotid-updated (getsystem-chatbots system)
                                                     (list-ref (get-eleccion-op (list-ref(iniflowops-chatbot idflow-chatbot flows-chatbot) 2) msg) 3)
                                                     (list-ref (get-eleccion-op (list-ref(iniflowops-chatbot idflow-chatbot flows-chatbot) 2) msg) 2)) null)))))))


;Lo mismo que arriba pero ahora usa funciones que no usan recursion
;Dom: system (list) X msg (string)
;Rec: system (list)

(define (update-talk-norec system msg)
  (let ((chatbot (getchatbot-initial-norec system))
        (idflow-chatbot (list-ref (getchatbot-initial-norec system) 3))
        (flows-chatbot (list-ref (getchatbot-initial-norec system) 4)))
    (cons (getsystem-name system)
          (cons (list-ref (get-eleccion-op-norec (list-ref(iniflowops-chatbot-norec idflow-chatbot flows-chatbot) 2) msg) 2)
                (cons (getsystem-users system)
                      (cons (update-chathistory-norec system msg chatbot)
                            (cons (chatbotid-updated-norec (getsystem-chatbots system) (getchatbot-initial-norec system)
                                                     (list-ref (get-eleccion-op-norec (list-ref(iniflowops-chatbot-norec idflow-chatbot flows-chatbot) 2) msg) 3)
                                                     (list-ref (get-eleccion-op-norec (list-ref(iniflowops-chatbot-norec idflow-chatbot flows-chatbot) 2) msg) 2)) null)))))))


;Modifica el chatbot inicial nuevo de un sistema dado por la opcion elegida dandole su flow incial haciendo uso de recursion natural para así revisar uno por uno los chatbots y encontrar cual es el inicial nuevo dado por la opcion
;Dom: list-chatbots (list) X idflow-ini (int) X idchatbot-update (int)
;Rec: chatbots (list)

(define (chatbotid-updated list-chatbots idflow-ini idchatbot-update)
  (if (null? list-chatbots)
  '()
  (if (eq? (car(car list-chatbots)) idchatbot-update )
      (cons (chatbot-startflow-update (car list-chatbots) idflow-ini) (cdr list-chatbots))
      (cons (car list-chatbots) (chatbotid-updated (cdr list-chatbots) idflow-ini idchatbot-update)))))


;Lo mismo que la version de arriba pero sin el uso de recursión
;Dom: list-chatbots (list) X idflow-ini (int) X idchatbot-update (int)
;Rec: chatbots (list)

(define (chatbotid-updated-norec list-chatbots chatbotinicial idflow-ini idchatbot-update)
  (if (null? list-chatbots) '()
      (sort (remove-duplicates (cons (chatbot-startflow-update chatbotinicial idflow-ini) (remove chatbotinicial list-chatbots)) #:key getchatbot-id) < #:key getchatbot-id)))


;Capa pertenencia

;Revisa que no haya nadie logeado dentro del sistema
;Dom: system (list)
;Rec: boolean

(define (logged? system)
    (not(eq? (getsystem-loggeduser system) '() )))


;Revisa que el usuario dado no este presente dentro de la lista de usuarios basandonos en su nombre
;Dom: system (list) X user (list)
;Rec: boolean

(define (user-presente? system user)
  (not(member (string-downcase (car user)) (map string-downcase(map car (getsystem-users system))))))



;Capa Otros

;Esta funcion muestra la conversacion que el usuario esta teniendo en consola con el sistema /Solo usada cuando se habilita tambien la forma recursiva para hablar con este ya que ayuda a ver las opciones disponibles
;Dom: system (list) X msg (string)
;Rec: Muestra string en consola

(define (show-conversation system msg)
  (let ((chatbot (getchatbot-initial system))
        (idflow-chatbot (list-ref (getchatbot-initial system) 3))
        (flows-chatbot (list-ref (getchatbot-initial system) 4)))
    (begin
      (display (string-append (number->string (current-seconds))
                              " - " (car(cadddr system)) ": "
                              msg "\n"))
      (display (string-append (number->string (current-seconds))
                              " - Chatbot-" (cadr chatbot) ": "
                              (list-ref(iniflowops-chatbot idflow-chatbot flows-chatbot) 1)
                              "\n\n"))
      (show-options (list-ref(iniflowops-chatbot idflow-chatbot flows-chatbot) 2)))))


;Lo mismo que arriba pero ahora usa funciones que no usan recursion
;Dom: system (list) X msg (string)
;Rec: Muestra string en consola

(define (show-conversation-norec system msg)
  (let ((chatbot (getchatbot-initial-norec system))
        (idflow-chatbot (list-ref (getchatbot-initial-norec system) 3))
        (flows-chatbot (list-ref (getchatbot-initial-norec system) 4)))
    (begin
      (display (string-append (number->string (current-seconds))
                              " - " (car(cadddr system)) ": "
                              msg "\n"))
      (display (string-append (number->string (current-seconds))
                              " - Chatbot-" (cadr chatbot) ": "
                              (list-ref(iniflowops-chatbot idflow-chatbot flows-chatbot) 1)
                              "\n\n"))
      (show-options-norec (list-ref(iniflowops-chatbot idflow-chatbot flows-chatbot) 2)))))


(provide getsystem-chatbots getsystem-users getsystem-loggeduser añadir-chatbot-system añadir-user-system logged? logear user-presente? logout-user getchatbot-initial update-talk-norec update-talk-rec show-conversation update-talk-rec-sin-interaccion getchatbot-initial-norec show-conversation-norec update-talk-norec-sin-interaccion)