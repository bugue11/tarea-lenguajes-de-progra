#lang br/quicklang
;------------------importar archivos---------------------------------------------------------
(require graph)
(require racket/string)
;------------------reader del archivo--------------------------------------------------------
(define (read-syntax path port)
  (define args (port->lines port))
  (define handle-datums (format-datums '(handle \"~a\") args))
  (define module-datum `(module stacker-mod "stacker.rkt"
                          ,@handle-datums))
  (datum->syntax #f module-datum))
(provide read-syntax)
;------------------expansor del archivo------------------------------------------------------
(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...))
(provide (rename-out [stacker-module-begin #%module-begin]))
;------------------declaracion de grafo------------------------------------------------------
(define G (weighted-graph/directed '()))
;------new-----------------------------------------------------------------------------------
(define (new arg)
  (cond
    [(equal? (substring arg 0 4) "graf") (graf (string-split (substring arg 5 (string-length arg))))]
    [(equal? (substring arg 0 4) "nodo") (nodo (string-split (substring arg 5 (string-length arg))))]
    [(equal? (substring arg 0 4) "aris") (aris (string-split (substring arg 5 (string-length arg))))]
    [else (error "error en new")])
  )
;------------------graf----------------------------------------------------------------------
(define (graf listaarg)
  (set! G (weighted-graph/directed '()))
  (cond
    [(and (= (length listaarg) 2) (equal? (list-ref listaarg 0) "completo") (integer? (string->number (list-ref listaarg 1))))
     (grafcompleto (string->number (list-ref listaarg 1)))]
    )
  )
;------------------grafcompleto--------------------------------------------------------------
(define (grafcompleto arg)
  (for ([i arg])
    (add-vertex! G (number->string i))
    )
  (for ([i arg])
    (for ([j arg])
      (cond
        [(not (= i j)) (add-edge! G (number->string i) (number->string j))])
      )
    )
  )
;------------------nodo----------------------------------------------------------------------
(define (nodo listaarg)
  (cond
    [(and (= (length listaarg) 1)) (add-vertex! G (list-ref listaarg 0))])
  )
;------------------aris----------------------------------------------------------------------
(define (aris listaarg)
  (cond
    [(and (= (length listaarg) 4) (equal? (list-ref listaarg 0) "!")) (add-directed-edge! G (list-ref listaarg 1) (list-ref listaarg 2) (list-ref listaarg 3))]
    [(and (= (length listaarg) 4) (equal? (list-ref listaarg 0) "#")) (add-edge! G (list-ref listaarg 1) (list-ref listaarg 2) (list-ref listaarg 3))]
    [(and (= (length listaarg) 3) (equal? (list-ref listaarg 0) "!")) (add-directed-edge! G (list-ref listaarg 1) (list-ref listaarg 2))]
    [(and (= (length listaarg) 3) (equal? (list-ref listaarg 0) "#")) (add-edge! G (list-ref listaarg 1) (list-ref listaarg 2))]
    )
  )
;------del-----------------------------------------------------------------------------------
(define (del arg)
  (cond
    [(equal? (substring arg 0 4) "nodo") (del_nodo (string-split (substring arg 5 (string-length arg))))]
    [(equal? (substring arg 0 4) "aris") (del_aris (string-split (substring arg 5 (string-length arg))))]
    [else (error "error en new")])
  )
;------------------nodo----------------------------------------------------------------------
(define (del_nodo listaarg)
  (cond
    [(and (= (length listaarg) 1)) (remove-vertex! G (list-ref listaarg 0))])
  )
;------------------aris----------------------------------------------------------------------
(define (del_aris listaarg)
  (cond
    [(and (= (length listaarg) 3) (equal? (list-ref listaarg 0) "!")) (remove-directed-edge! G (list-ref listaarg 1) (list-ref listaarg 2))]
    [(and (= (length listaarg) 3) (equal? (list-ref listaarg 0) "#")) (remove-edge! G (list-ref listaarg 1) (list-ref listaarg 2))]
    )
  )
;------handle--------------------------------------------------------------------------------
(define (handle [arg #f])
  (cond
    ;aqui elimina la primera linea
    [(equal? arg "") (display "")]
    ;aqui realiza la llamada a new 
    [(equal? (substring arg 0 3) "new") (new (substring arg 4 (string-length arg)))]
    [(equal? (substring arg 0 3) "del") (del (substring arg 4 (string-length arg)))]
    ;mas
    [(equal? arg "show graf") (get-vertices G)]
    [(equal? arg "show aris") (get-edges G)]
    [(equal? arg "prueba") (edge-weight G "s" "g")]
    [(equal? arg "prueba2") (edge-weight G "a" "s")]
    [(equal? arg "prueba3") (edge-weight G "g" "a")]
    [else (displayln "error")]))
(provide handle)