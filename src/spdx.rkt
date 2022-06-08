#lang racket/base

(require "spdx/get.rkt"
         net/url-string
         racket/symbol
         racket/string
         racket/match
         json)

(provide check-license-sexp)

(module+ test
  (require rackunit))

;; https://spdx.github.io/spdx-spec/SPDX-license-expressions/#d2-case-sensitivity
;;  - Operators (e.g. "WITH") are case-sensitive.
;;  - License and exception identifiers are NOT case-sensitive:
;;    the canonical case can matter for some purposes, but currently not ours.
;;    We convert them into case-folded symbols for comparison.

(define (parse-spdx-json pth list-key id-key url-key)
  (for/hasheq ([hsh (hash-ref (call-with-input-file* pth read-json) list-key)]
               #:unless (hash-ref hsh 'isDeprecatedLicenseId #f))
    (values (string->symbol (string-foldcase (hash-ref hsh id-key)))
            (let ([u (hash-ref hsh url-key)])
              (cond
                [(string-prefix? u "https://")
                 u]
                [(string-prefix? u "./")
                 (url->string (spdx-url (substring u 2)))]
                [else
                 (error 'parse-spdx-json "bad url: ~s" u)])))))

(define licenses
  (parse-spdx-json licenses.json 'licenses 'licenseId 'reference))

(define exceptions
  (parse-spdx-json exceptions.json 'exceptions 'licenseExceptionId 'detailsUrl))

(define custom-license-rx
  #px"^(?i:(?:DocumentRef-[-.[:alpha:][:digit:]]+:)?LicenseRef-[-.[:alpha:][:digit:]]+)$")

;; check-license-id : symbol? -> (values boolean? (listof xexpr/c))
(define (check-license-id sym)
  (define (check-sans-+ str)
    (cond
      [(hash-ref licenses (string->symbol (string-foldcase str)) #f)
       => (λ (url)
            (values #t `((a ([href ,url]) ,str))))]
      [(regexp-match? custom-license-rx str)
       (values #t (list str))]
      [else
       (values #f `((span () ,str)))]))
  (define str (symbol->immutable-string sym))
  (cond
    [(string-suffix? str "+")
     (define-values [valid? xs]
       (check-sans-+ (substring str 0 (sub1 (string-length str)))))
     (values valid? `(,@xs "+"))]
    [else
     (check-sans-+ str)]))


;; check-exception-id : symbol? -> (values boolean? (listof xexpr/c))
(define (check-exception-id sym)
  (define str (symbol->immutable-string sym))
  (cond
    [(hash-ref exceptions (string->symbol (string-foldcase str)) #f)
     => (λ (url)
          (values #t `((a ([href ,url]) ,str))))]
    [else
     (values #f `((span () ,str)))]))


;; check-license-sexp : any/c -> (values (or/c 'valid 'invalid 'ill-formed)
;;                                       (listof xexpr/c))
(define (check-license-sexp orig)
  (let/ec return
    (define-values [valid? xs]
      (let loop ([x orig])
        (match x
          [(? symbol?)
           (check-license-id x)]
          [`(,(? symbol? (app check-license-id license-valid? license-xs))
             WITH
             ,(? symbol? (app check-exception-id exception-valid? exception-xs)))
           (values (and license-valid? exception-valid?)
                   `("(" ,@license-xs " WITH " ,@exception-xs ")"))]
          [`(,(app loop lhs-valid? lhs-xs)
             ,(and (or 'AND 'OR) rator)
             ,(app loop rhs-valid? rhs-xs))
           (values (and lhs-valid? rhs-valid?)
                   `("(" ,@lhs-xs
                         " "
                         ,(symbol->immutable-string rator)
                         " "
                         ,@rhs-xs
                         ")"))]
          [_
           (return 'ill-formed (format "~s" orig))])))
    (values (if valid? 'valid 'invalid)
            xs)))

(module+ test
  (define-syntax-rule (status expr)
    (call-with-values (λ () expr)
      (λ (a b)
        a)))
  (check-eq?
   (status (check-license-sexp
            'SchemeReport))
   'valid)
  (check-eq?
   (status (check-license-sexp
            '(SchemeReport WITH Font-exception-2.0)))
   'valid)
  (check-eq?
   (status (check-license-sexp
            '((SchemeReport WITH Font-exception-2.0) OR MIT)))
   'valid)
  (check-eq?
   (status (check-license-sexp
            '((SchemeReport WITH Font-exception-2.0) OR (MIT AND XYZ))))
   'invalid)
  (check-eq?
   (status (check-license-sexp
            '((SchemeReport WITH Font-exception-2.0) OR (MIT AND LicenseRef-MIT-Style-1))))
   'valid)
  (check-eq?
   (status (check-license-sexp
            'DocumentRef-spdx-tool-1.2:LicenseRef-MIT-Style-2))
   'valid)
  (check-eq?
   (status (check-license-sexp
            '((SchemeReport WITH Font-exception-2.0) OR)))
   'ill-formed))
