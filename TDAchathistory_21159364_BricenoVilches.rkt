#lang racket

(require "TDAchatbot_21159364_BricenoVilches.rkt")
(require "TDAflow_21159364_BricenoVilches.rkt")

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
    (list (car log-user)
          (append (cadr log-user)
                (list chathistory)))))

(define (update-chathistory-norec system msg chatbot-inicial)
  (let* ((chathistory
          (string-append
                      (number->string (current-seconds))
                      " - " (car(cadddr system)) ": "
                      msg "\n"
                      (number->string (current-seconds))
                      " - Chatbot-" (cadr chatbot-inicial) ": "
                      (list-ref(iniflowops-chatbot (list-ref chatbot-inicial 3) (list-ref chatbot-inicial 4)) 1)
                      "\n\n"
                      (save-options-norec (list-ref (iniflowops-chatbot (list-ref chatbot-inicial 3) (list-ref chatbot-inicial 4))2))))
                     (log-user (cadddr system)))
    (list (car log-user)
          (append (cadr log-user)
                (list chathistory)))))

(provide update-chathistory update-chathistory-norec)