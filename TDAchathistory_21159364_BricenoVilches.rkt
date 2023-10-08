#lang racket

(require "TDAchatbot_21159364_BricenoVilches.rkt")
(require "TDAflow_21159364_BricenoVilches.rkt")


;Capa Modificador

;Funcion que actualiza el chat history de un usuario logeado y lo junta con el historial ya existente
;Dom: system (list) X msg (string) X chatbot inicial(list)
;Rec: logged user (list)

(define (update-chathistory system msg chatbot-inicial)
  (let* ((chathistory
          (string-append
           (number->string (current-seconds))
           " - " (car(cadddr system)) ": "
           msg "\n"
           (number->string (current-seconds))
           " - Chatbot-" (cadr chatbot-inicial) ": "
           (list-ref(iniflowops-chatbot (list-ref chatbot-inicial 3) (list-ref chatbot-inicial 4)) 1)
           "\n\n"
           (save-options (list-ref (iniflowops-chatbot (list-ref chatbot-inicial 3) (list-ref chatbot-inicial 4))2))))
         (log-user (cadddr system)))
    (if (null? (cadr log-user)) (list (car log-user) chathistory)
        (list (car log-user)
              (string-append (cadr log-user)
                             chathistory)))))


;Funcion que actualiza el chat history de un usuario logeado y lo junta con el historial ya existente usando funciones sin recursion
;Dom: system (list) X msg (string) X chatbot inicial(list)
;Rec: logged user (list)

(define (update-chathistory-norec system msg chatbot-inicial)
  (let* ((chathistory
          (string-append
           (number->string (current-seconds))
           " - " (car(cadddr system)) ": "
           msg "\n"
           (number->string (current-seconds))
           " - Chatbot-" (cadr chatbot-inicial) ": "
           (list-ref(iniflowops-chatbot-norec (list-ref chatbot-inicial 3) (list-ref chatbot-inicial 4)) 1)
           "\n\n"
           (save-options-norec (list-ref (iniflowops-chatbot-norec (list-ref chatbot-inicial 3) (list-ref chatbot-inicial 4))2))))
         (log-user (cadddr system)))
    (if (null? (cadr log-user)) (list (car log-user) chathistory)
        (list (car log-user)
              (string-append (cadr log-user)
                             chathistory)))))

(provide update-chathistory update-chathistory-norec)