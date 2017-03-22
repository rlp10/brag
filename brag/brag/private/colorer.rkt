#lang racket/base
(require brag/rules/lexer brag/rules/parser racket/match br-parser-tools/private-lex/token)
(provide color-brag)

(define-tokens tokens (ERROR))

(define (color-brag port)
  (define (handle-lexer-error exn)
    (position-token (token-ERROR #f) (position 1 #f #f) (position 1 #f #f)))
  (define pos-tok (with-handlers ([exn:fail? handle-lexer-error])
                    (lex/1 port)))
  (cond
    [(eof-object? pos-tok) (values pos-tok 'eof #f #f #f)]
    [else
     (match-define (position-token tok (position start _ _) (position end _ _)) pos-tok)
     (match-define (list cat paren)
       (case (token-name tok)
         [(RULE_HEAD RULE_HEAD_SPLICED RULE_HEAD_HIDDEN) '(symbol #f)]
         [(LIT) '(string #f)]
         [(LBRACKET RBRACKET LPAREN RPAREN) (list 'parenthesis #f)]
         [(REPEAT PIPE HIDE SPLICE) '(parenthesis #f)]
         [else '(no-color #f)]))
     (values (token-value tok) cat paren start end)]))

(module+ main
  (require sugar/list)
  (define (apply-colorer str)
    (for/list ([annotation (in-port (λ (p)
                                      (let ([xs (values->list (color-brag p))])
                                        (if (eof-object? (car xs)) (car xs) xs)))
                                    (open-input-string str))])
              annotation))

  (define p (open-input-string "\n\n\nr:x"))
  (color-brag p)
  (color-brag p)
  (color-brag p)
  (color-brag p))