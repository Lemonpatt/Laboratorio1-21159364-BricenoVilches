#lang racket


;RF 1 TDAS
;Aquí se pueden observar los 6 TDAs pedidos más un archivo extra que contiene funciones desligadas de un TDA
(require "TDAoption_21159364_BricenoVilches.rkt")
(require "TDAflow_21159364_BricenoVilches.rkt")
(require "TDAchatbot_21159364_BricenoVilches.rkt")
(require "TDAuser_21159364_BricenoVilches.rkt")
(require "TDAsystem_21159364_BricenoVilches.rkt")
(require "FuncionesSystemSimulate_21159364_BricenoVilches.rkt")


;RF 2 TDA Option - constructor
;Función crea una opción con un código único para un flow de un chatbot.
;Dominio: code (Int) X message (String) X ChatbotCodeLink (Int) X InitialFlowCodeLink (Int) X Keyword* (0 o más strings)
;Recorrido: option (list)

(define (option code message ChatbotCodeLink InitialFlowCodeLink . Keyword)
  (list code message ChatbotCodeLink InitialFlowCodeLink Keyword))


;RF 3 TDA Flow - constructor
;Función crea una lista con los datos de un flow con ninguna o varias opciones sin repetir basandonos en su id usando una llave.
;Dominio: id (int) X name-msg (String) X Option* (0 o más opciones (listas))
;Recorrido: flow (list)

(define (flow id name-msg . Option)
  (list id name-msg (sort (remove-duplicates Option #:key get-option-code) < #:key get-option-code))) ;Para que las opciones queden ordenadas por su id


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
;Función modificadora para añadir chatbots a un sistema, su recursion usada es de cola, dentro de funciones internas
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
    (if (user-presente? system user)
        (añadir-user-system system user)
        system)))


;RF 10 TDA system - modificador
;Función que añade un usuario al parámetro de usuario-logeado del sistema
;Dominio: system (list) X user (list)
;Recorrido: system (list)

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


;RF 12 TDA system - otro
;Función que permite interactuar con un chatbot de manera recursiva usando muchas funciones con recursiones
;Dominio: system (list)
;Recorrido: system (list) o texto de error (string)


(define (system-talk-rec system msg)
  (let ((chatbot (getchatbot-initial system))
        (idflow-chatbot (if (null? (getchatbot-initial system))null (list-ref (getchatbot-initial system) 3)))
        (flows-chatbot (if (null? (getchatbot-initial system))null(list-ref (getchatbot-initial system) 4))))
    (if (logged? system)  ;Hace que funcione solo si hay alguien logeado
        (if (not(null? chatbot))
            (if (not(null?  flows-chatbot))
            
                (if (null? (get-eleccion-op (list-ref(iniflowops-chatbot idflow-chatbot flows-chatbot) 2) msg))
                    (begin
                      ;(show-conversation system msg) ;Habilitar en caso de querer ver un display de la interacción
                      ;(system-talk-rec system (read-line)))  Recursión de la función eliminada ya que no permitía al sistema nuevo definido guardar el historial de Chat completo
                      (update-talk-rec-sin-interaccion system msg))
                    (begin
                      ;(show-conversation (update-talk-rec system msg) msg) ;Habilitar en caso de querer ver un display de la interacción
                      ;(system-talk-rec (update-talk-rec system msg) (read-line)))) Parte de la razón descrita arriba
                      (update-talk-rec system msg)))
                "No se encuentra Flow\n")
            "No se encuentra Chatbot\n")
        "No hay Usuario logeado\n")))


;RF 13 TDA system - otro
;Función que permite interactuar con un chatbot de manera totalmente declarativa sin ninguna recursión explícita en sus funciones usadas
;Dominio: system (list) X msg (string)
;Recorrido: system (list) o texto de error (string)



(define (system-talk-norec system msg)
  (let ((chatbot (getchatbot-initial-norec system))
        (idflow-chatbot (if (null? (getchatbot-initial-norec system))null(list-ref (getchatbot-initial-norec system) 3)))
        (flows-chatbot (if (null? (getchatbot-initial-norec system))null(list-ref (getchatbot-initial-norec system) 4))))
    (if (logged? system)  ;Hace que funcione solo si hay alguien logeado
        (if (not(null? chatbot))
        (if (not(null? flows-chatbot))
            (if (null? (get-eleccion-op-norec (list-ref(iniflowops-chatbot-norec idflow-chatbot flows-chatbot) 2) msg))
                (begin
                  ;(show-conversation-norec system msg) ;Habilitar en caso de querer ver un display de la interacción
                  (update-talk-norec-sin-interaccion system msg))
                (begin
                  ;(show-conversation-norec (update-talk-norec system msg) msg) ;Habilitar en caso de querer ver un display de la interacción
                  (update-talk-norec system msg)))
            "No se encuentra Flow\n")
        "No se encuentra Chatbot\n")
        "No hay Usuario logeado\n")))

;RF 14 TDA system - otro
;Función que muestra una síntesis del chatbot para un usuario particular a partir de chatHistory contenido dentro del sistema
;Dominio: system (list) X usuario (string)
;Recorrido: chathistory o texto de error (string)


(define (system-synthesis system usuario)
  (let ((user (find-user (getsystem-users system) usuario)))
    (if (user-presente? system user)
        "Usuario no se encuentra en el sistema\n"
        (if (and (logged? system) (string-ci=? (car (getsystem-loggeduser system)) usuario))
            (if (null? (cadr (getsystem-loggeduser system)))
                "Historial vacío\n"
                (cadr (getsystem-loggeduser system)))
            (if (null? (cadr user))  ;Busca que esté ahora guardado en su instancia de usuario logeado ya que solo se guarda al deslogear en la lista
                "Historial vacío\n"
                (cadr user))))))

;RF 15 TDA system - otro
;Función que simula una conversación con el sistema a través de una función que crea números pseudo aleatorios tomando el primer digito de este y usándolo como una elección en el chatbot
;Usa recursion natural dentro de otra funcion definida dentro para asi poder guardar en nombre del usuario definido
;Dominio: system (list) X maxInteractions (int) X seed (int)
;Recorrido: system (list) o texto de error (string)

(define (system-simulate system maxInteractions seed)
  (let ((usuario-generado (string-append "user" (number->string seed))))
    (define (simulate-seed system maxInteractions seed)
    (if (string? system) system ;Esto es en caso de que algunas de las funciones al emular la conversacion retorne un string de error Ej:No hay usuario logeado
        (if (eq? maxInteractions 0)
            system
            (if (logged? system)
                (if (string=? (car (getsystem-loggeduser system)) usuario-generado)
                    (if (>= (get-primer-num seed) 6)  ;Agregado para reducir la cantidad de opciones muertas en la prueba como lo son dígitos muy altos
                        (simulate-seed (system-talk-rec system (number->string (-(get-primer-num seed) 5)))
                                         (- maxInteractions 1) (remove-primer-digit (myRandom seed)))
          
                        (simulate-seed (system-talk-rec system (number->string (get-primer-num seed) ))
                                         (- maxInteractions 1) (remove-primer-digit (myRandom seed))))
                    (simulate-seed (system-login (system-add-user (system-logout system) usuario-generado) usuario-generado) maxInteractions seed))
                (simulate-seed (system-login (system-add-user system usuario-generado) usuario-generado) maxInteractions seed)))))
  (simulate-seed system maxInteractions seed)))



(provide option flow flow-add-option chatbot chatbot-add-flow system system-add-chatbot system-add-user system-login system-logout system-talk-rec system-talk-norec system-synthesis system-simulate)