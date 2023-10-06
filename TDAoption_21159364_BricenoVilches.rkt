#lang racket


;Capa selector


;Retorna el codigo de una opción /Usado para revisar duplicados
;Dominio: option (list)
;Recorrido: code (int)

(define (get-option-code option)
  (car option))

;Retorna el id del chatbot inicial al que dirige una opción
;Dominio: option (list)
;Recorrido: ChatbotInitialCodeLink (int)

(define (get-option-cbcodelink option)
  (caddr option))


;Retorna una opcion dentro de una lista en donde un string dado contenga su code o keyword
;Dominio: lista de opciones X elección (string)
;Recorrido: Opción elegida (list)

(define (get-eleccion-op list-options eleccion)
  (if (null? list-options)
      list-options
      (if
       (or
        (string=? eleccion (number->string (get-option-code (car list-options))))
        (find-keyword-ci (list-ref (car list-options) 4) eleccion))
       (car list-options)
       (get-eleccion-op (cdr list-options) eleccion))))


;Función interna de get-eleccion-op que revisa si la elección se encuentra dentro de la lista de keywords
;Dominio: lista de keywords X elección (string)
;Recorrido: Boolean

(define (find-keyword-ci list-keywords eleccion)
  (cond
   ((null? list-keywords) #f)
    ((string-ci=? (string-downcase eleccion) (string-downcase (car list-keywords))) #t)
    (else (find-keyword-ci (cdr list-keywords) eleccion))))


;Versión sin recursión de get-eleccion-op para el requisito funcional 13
;Dominio: lista de opciones X elección (string)
;Recorrido: Opción elegida (list)

(define (get-eleccion-op-norec list-options eleccion)
  (let ((match-eleccion-op?
         (lambda (op)
           (or (string=? eleccion (number->string (get-option-code op)))
            (find-keyword-ci-norec (list-ref op 4) eleccion)))))
  (if (null? (filter 
           match-eleccion-op? list-options))
      '()
      (car(filter 
           match-eleccion-op? list-options)))))

;Versión sin recursión de la función interna de get-eleccion-op para el requisito funcional 13
;Dominio: lista de keywords X elección (string)
;Recorrido: Boolean

(define (find-keyword-ci-norec list-keywords eleccion)
  (let ((match-eleccion-keyword?
         (lambda (keyword)
           (string-ci=? (string-downcase eleccion) (string-downcase keyword)))))
  (cond
   ((null? (filter match-eleccion-keyword? list-keywords)) #f)
   ((not(null?(filter match-eleccion-keyword? list-keywords))) #t))))

(provide get-eleccion-op get-eleccion-op-norec get-option-code)