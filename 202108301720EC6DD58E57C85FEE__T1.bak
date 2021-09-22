#lang play

#|
Complete sus datos personales :-)
NOMBRE Y APELLIDO: Benjamin Aguilar
RUT: 20.428.923-9
|#

;; Parte a)


#|
<SMatrix> ::= (null-slot)
           |  (slot <Int> <Int> <Number> <SMatrix>)

   Almacena coordenadas de elementos distintos a 0
   de una matriz dispersa
|#

(deftype SMatrix
  (null-slot)
  (slot r c v s))



;; Parte b)


;; sum :: SMatrix -> Number
;; Suma todos los elementos de una matriz dispersa

(define (sum SMatrix)
  (match SMatrix
    [(null-slot) 0]
    [(slot r c v s) (+ v (sum s))]))

;; Parte c)


;; dimension ::  SMatrix -> Pair
;; Devuelve un par con las dimensiones (filas, columnas) de la matriz dispersa

(define (dimension SMatrix)
  (match SMatrix
    [(null-slot) (cons 0 0)]
    [(slot r c v s) (cond [(< r (car (dimension s)))
                           (if (< c (cdr (dimension s)))
                               (cons (car (dimension s)) (cdr (dimension s)))
                               (cons (car (dimension s)) c))]
                          [else (if (< c (cdr (dimension s)))
                                    (cons r (cdr (dimension s)))
                                    (cons r c))])]))
;; Parte d)
;; fold :: (Number Number Number A -> A) A SMatrix -> A



;; Parte e)
;; new-sum :: SMatrix -> Number



;; Parte f)
;; map-sm :: (Number -> Number) SMatrix -> SMatrix



;; Parte g)
;; normalize :: SMatrix -> SMatrix



;; Parte h)
;; sumSM :: SMatrix SMatrix -> SMatrix



