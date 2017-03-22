#lang racket/base
(require brag/support (submod brag/rules/lexer lex-abbrevs) brag/support racket/match)
(provide color-brag)

(define brag-syntax-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   [(:or (from/to "'" "'") (from/to "\"" "\"")) (token 'LIT lexeme)]
   [(:or (char-set "()[]|+*:") hide-char splice-char) (token 'MISC lexeme)]
   [whitespace (return-without-srcloc (brag-syntax-lexer input-port))]
   [(:: (:or "#" ";")
        (complement (:: (:* any-char) NL (:* any-char)))
        (:or NL ""))
    (token 'COMMENT lexeme)]
   [id (token 'ID lexeme)]
   [(:: any-char) (token 'OTHER lexeme)]))


(define (handle-lexer-error exn)
  (define exn-srclocs (exn:fail:read-srclocs exn))
  (srcloc-token (token 'ERROR) (car exn-srclocs)))


(define (color-brag port)
  (define srcloc-tok (with-handlers ([exn:fail:read? handle-lexer-error])
                       (brag-syntax-lexer port)))
  (if (eof-object? srcloc-tok)
      (values srcloc-tok 'eof #f #f #f)
      (match-let* ([(srcloc-token
                     (token-struct type val _ _ _ _ _)
                     (srcloc _ _ _ posn span)) srcloc-tok]
                   [(cons start end) (cons posn (+ posn span))]
                   [(cons _ cat) (or (assq type
                                           '((ID . symbol)
                                             (LIT . string)
                                             (MISC . parenthesis)
                                             (COMMENT . comment)
                                             (ERROR . error)))
                                     (cons 'OTHER 'no-color))])
        (values val cat #f start end))))

(module+ main
  (require sugar/list)
  (define (apply-colorer str)
    (for/list ([annotation (in-port (λ (p)
                                      (let ([xs (values->list (color-brag p))])
                                        (if (eof-object? (car xs)) (car xs) xs)))
                                    (open-input-string str))])
      annotation))

  (apply-colorer "%"))