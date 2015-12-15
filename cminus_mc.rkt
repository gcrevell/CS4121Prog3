#lang racket

;(require web-server/servlet
;         web-server/servlet-env)
;
;(define (hello-servlet req)
;  (response
;   200                 ; response code
;   #"OK"               ; response message
;   (current-seconds)   ; timestamp
;   TEXT/HTML-MIME-TYPE ; MIME type for content
;   '()                 ; additional headers
;
;   ; the following parameter accepts the output port
;   ; to which the page should be rendered.
;   (λ (client-out)
;     (write-string "Hello, world!" client-out))))


  
; start serving:
;(serve/servlet hello-servlet
;               #:port 2112)

;; Import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; -------------------
;; Zero Page Locations
;; -------------------
;; Math Variables
(define MATH1LO #x10) ;; augend, subtrahend
(define MATH1HI #x11) 
(define MATH2LO #x12) ;; addend, minuend
(define MATH2HI #x13)
(define MATH3LO #x14) ;; sum, difference
(define MATH3HI #x15)
(define MATH4LO #x16)
(define MATH4HI #x17)
(define MATH5LO #x18)
(define MATH5HI #x19)
(define MATH6LO #x1A)
(define MATH6HI #x1B)
(define MATH7LO #x1C)
(define MATH7HI #x1D)
(define MATH8LO #x1E)
(define MATH8HI #x1F)

;; Work Space
(define WORK1LO #x20)
(define WORK1HI #x21)

;; Symbol Table
(define VARS #x30) ;$30 - $7F

;; Memory Mapped I/O
(define IOFROB #xFD)
(define IOINTL #xFE)
(define IOINTH #xFF)

;; ---------------
;; ROM Subroutines
;; ---------------
(define ADD #xF000)
(define SUBTRACT #xF00E)

(define-tokens value-tokens (NUM VAR FNCT))
(define-empty-tokens op-tokens (newline = OP CP + - * / ^ EOF NEG VOID MAIN OCB CCB INT SMC))

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (digit (:/ "0" "9")))

(define calcl
  (lexer
   [(eof) 'EOF]

   ["void" 'VOID] ;; Method return type
   ["main" 'MAIN] ;; main method
   ["{" 'OCB] ;; open brace around main
   ["}" 'CCB] ;; close brace aroudn main
   ["int" 'INT] ;; integer variable declaration
   [";" 'SMC] ;; semicolons to end your lines with
   
   ;; recursively call the lexer on the remaining input after a tab or space.
   ;; Returning the result of that operation.  
   ;; This effectively skips all whitespace.
   [(:or #\tab #\space) (calcl input-port)]
   ;; (token-newline) returns 'newline
   [#\newline (token-newline)]
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "=" "+" "-" "*" "/" "^") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   ["sin" (token-FNCT sin)]
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))

(define calcp
  (parser
   (start start)
   (end newline SMC EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))
   (precs (right =)
          (left - +)
          (left * /)
          (left NEG)
          (right ^))
   (grammar
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])    
    (exp [(NUM) (let-values ([(lo hi hexLo hexHi) (int->16bit $1)])
                  ;; Assembly Code
                  ;; Push number onto stack
                  (printf "A9 ~a 48 A9 ~a 48 " hexHi hexLo)
                  $1)
                ]
         [(VOID MAIN OP CP OCB) (begin
                  (display "D000")
                  (newline)
                  -1)] ;; Reutrn to tell us we just saw main
         [(CCB) #f] ;; ends the program
         [(VAR) (begin
                  ;; Assembly Code
                  ;; Retrieve variable value from Symbol Table
                  ;; Push value onto Stack
                  (printf "A2 ~a E8 E8 B5 ~a 48 CA B5 ~a 48 " 
                          (8bit->hex (symbol->var-lookup $1))
                          (8bit->hex VARS)
                          (8bit->hex VARS))
                  (hash-ref vars $1 (lambda () 0)))]
         [(VAR = exp) (let-values ([(lo hi hexLo hexHi) (int->16bit $3)])
                        ;; Assembly Code
                        ;; Pull right-hand expression off Stack
                        (printf "68 85 ~a 68 85 ~a " 
                                (8bit->hex WORK1LO)
                                (8bit->hex WORK1HI))
                        ;; Lookup variable location in Symbol Table
                        ;; Store expression value in variable
                        ;; Push value onto Stack
                        (printf "A2 ~a E8 E8 A5 ~a 95 ~a 48 CA A5 ~a 95 ~a 48 " 
                                (8bit->hex (symbol->var-lookup $1))
                                (8bit->hex WORK1HI)
                                (8bit->hex VARS)
                                (8bit->hex WORK1LO)
                                (8bit->hex VARS))
                        (hash-set! vars $1 $3)
                        $3)]
         [(FNCT OP exp CP) ($1 $3)]
         [(exp + exp) (let-values ([(lo hi hexLo hexHi) (int->16bit ADD)])
                        ;; Assembly Code
                        ;; Pull Right-hand expression off Stack
                        ;; Store in Addend workspace
                        (printf "68 85 ~a 68 85 ~a " 
                                (8bit->hex MATH2LO)
                                (8bit->hex MATH2HI))
                        ;; Pull Left-hand expression off Stack
                        (printf "68 85 ~a 68 85 ~a " 
                                (8bit->hex MATH1LO)
                                (8bit->hex MATH1HI))
                        (printf "20 ~a ~a " hexLo hexHi)
                        ;; Push sum onto Stack
                        (printf "A5 ~a 48 A5 ~a 48 " 
                                (8bit->hex MATH3HI)
                                (8bit->hex MATH3LO))
                        (+ $1 $3))
                      ]
         [(exp - exp) (let-values ([(lo hi hexLo hexHi) (int->16bit SUBTRACT)])
                        ;; Assembly Code
                        ;; Pull Right-hand expression off Stack
                        ;; Store in Minuend workspace
                        (printf "68 ")
                        (printf "85 ~a " (8bit->hex MATH2LO))
                        (printf "68 ")
                        (printf "85 ~a " (8bit->hex MATH2HI))
                        ;; Pull Left-hand expression off Stack
                        ;; Store in Subtrahend workspace
                        (printf "68 ")
                        (printf "85 ~a " (8bit->hex MATH1LO))
                        (printf "68 ")
                        (printf "85 ~a " (8bit->hex MATH1HI))
                        ;; Call SUBTRACT subroutine
                        (printf "20 ~a ~a " hexLo hexHi)
                        ;; Push difference onto Stack
                        (printf "A5 ~a " (8bit->hex MATH3HI))
                        (printf "48 ")
                        (printf "A5 ~a " (8bit->hex MATH3LO))
                        (printf "48 ")
                        (- $1 $3))
                      ]
         [(exp * exp) (* $1 $3)]
         [(exp / exp) (/ $1 $3)]
         [(- exp) (prec NEG) (- $2)]
         [(exp ^ exp) (expt $1 $3)]
         [(OP exp CP) $2]
         ;[(INT exp) $2]
         ))))

;; -------------------------------
;; DECIMAL / BINARY NUMBER FORMATS
;; -------------------------------
;; Convert an integer to the 6502 Lo/Hi-byte 16-bit Integer format.
;; Returns bytes and integers and hex strings.
(define (int->16bit arg)
  (let ((i (bitwise-and arg #xFFFF))
        (lo 0)
        (hi 0))
    (when (< i 0) 
      ;; Use Two's Compliment for negative numbers
      (set! i (+ (bitwise-xor i #xFFFF) 1)))
    (set! lo (bitwise-and i #x00FF))
    (set! hi (/ (bitwise-and i #xFF00) 256))
    (values lo hi 
            (8bit->hex lo)
            (8bit->hex hi))))

;; Convert 6502 Lo/Hi-bytes to an integer.
(define (16bit->int lo hi)
  (let ((i 0))
    (cond ((> (bitwise-and hi #x80) 0) 
           ;; Use Two's Compliment for negative numbers
           (set! i (- (+ (* (bitwise-xor hi #xFF) 256) (bitwise-xor lo #xFF) 1))))
          (#t (set! i (+ (* hi 256) lo))))
    i))

;; Converts an 8-bit integer (< 256) to a 2-digit hex string.
(define (8bit->hex number)
  (let ((padded (string-upcase (string-append "0" (number->string number 16)))))
    (substring padded (- (string-length padded) 2))))

;; Do the math locally to calculate index of variable in Symbol Table
(define (symbol->var-lookup var)
  (*
   (- (bytes-ref (string->bytes/utf-8 (string-upcase (symbol->string var))) 0)
      65)
   3))

;; run the calculator on the given input-port       
(define (calc ip)
  (port-count-lines! ip)
  (letrec ((one-line
            (lambda ()
              (let ((result (calcp (lambda () (calcl ip)))))
                (when result
                 ; (displayln (not (equal? (newline) result)))
                  ;; Assembly Code
                  ;; EOL pull last value off stack and print result
                  (cond [(not (equal? -1 result))
                  (printf "68 85 ~a 68 85 ~a 85 ~a " 
                          (8bit->hex IOINTL) 
                          (8bit->hex IOINTH)
                          (8bit->hex IOFROB))])
                  ;; Scheme result
                  (one-line))))))
    (one-line))
  (display "02") ;; Hex 02
  (newline)
  (display "FFFFFF") ;; Hex 02
  )

;; compile to object code
(define (compile-file source object)
  (with-output-to-file object	
    (lambda () (calc (open-input-file source)))    
    #:exists 'replace)
)

;; Some examples
(calc (open-input-string "void main(){\n int x = 10; int y = 100; x + (y - 20);}"))
;(calc (open-input-string "void main(){\n}"))
;(compile-file "input2" "output2")
