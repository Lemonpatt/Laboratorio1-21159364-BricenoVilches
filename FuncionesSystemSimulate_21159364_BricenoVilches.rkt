#lang racket

(provide (all-defined-out))

;Archivo creado para el uso de funciones internas de la función system-simulate ya que no es un TDA como tal

;Toma el primer dígito de un número usando recursión natural ya que así dividimos el numero varias veces hasta que tenga un digito entero
;Dom: number
;Rec: number

(define (get-primer-num number)
  (if (negative? number)
      (get-primer-num (- number)) ; Hace al número positivo
        (if (< number 10)
            number
            (get-primer-num (quotient number 10)))))

;Función para crear un número pseudoaleatorio
;Dom: Xn (int)
;Rec: Random number (int)

(define (myRandom Xn)
  (modulo (+ (* 1103515245 Xn) 12345) 2147483648))


;Toma un numero y lo devuelve con su primer dígito eliminado
;Dom: number
;Rec: number

(define (remove-primer-digit number)
  (if (negative? number)
      (string->number (substring (number->string (- number)) 1))
      (string->number (substring (number->string number) 1))))