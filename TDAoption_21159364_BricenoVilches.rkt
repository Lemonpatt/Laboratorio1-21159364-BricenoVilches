#lang racket


;RF 2 Función Option - constructor
;Función crea una opción con un código único para un flow de un chatbot
;Dominio: code (Int) X message (String) X ChatbotCodeLink (Int) X InitialFlowCodeLink (Int) X Keyword* (0 o más strings)
;Recorrido: option (list)

(define (option code message ChatbotCodeLink InitialFlowCodeLink . Keyword)
  (list code message ChatbotCodeLink InitialFlowCodeLink Keyword))

(define (get-option-code option)
  (car option))

(define (get-option-cbcodelink option)
  (caddr option))

(define (get-eleccion-op list-options eleccion)
  (if (null? list-options)
      list-options
      (if
       (or
        (string=? eleccion (number->string (get-option-code (car list-options))))
        (find-keyword-ci (list-ref (car list-options) 4) eleccion))
       (car list-options)
       (get-eleccion-op (cdr list-options) eleccion))))

(define (find-keyword-ci list-keywords eleccion)
  (cond
   ((null? list-keywords) #f)
    ((string-ci=? (string-downcase eleccion) (string-downcase (car list-keywords))) #t)
    (else (find-keyword-ci (cdr list-keywords) eleccion))))

        
(provide option get-eleccion-op)