#lang racket

;; A very simple library to connect to a fadecandy server
;; See https://github.com/scanlime/fadecandy/


(require racket/tcp)

(provide opc-connect
         opc-close
         sendPixels
         chase
         black
         red
         blue
         green)

         
;; These are specific for MDX fadecandy installation in RG08/R101

(define MDX_IP "192.168.2.1")
(define MDX_PORT 7890)
(define NUMPIXELS 360)

(define portin empty)
(define portout empty)

;; Constant colour black
(define black 
  (for/list ([i (range NUMPIXELS)])
             (list 0 0 0)
    )
  )

;; What is says on the tin...
(define red 
  (for/list ([i (range NUMPIXELS)])
             (list 255 0 0)
    )
  )

;; What is says on the tin...
(define green 
  (for/list ([i (range NUMPIXELS)])
             (list 0 255 0)
    )
  )

;; What is says on the tin...
(define blue 
  (for/list ([i (range NUMPIXELS)])
             (list 0 0 255)
    )
  )

;; Open connection. Takes default parameters defined on top, or you
;; can provide your own
(define opc-connect (λ ([address MDX_IP] [port MDX_PORT])
                      (let-values ([(p-in p-out) (tcp-connect address port)])
                        (set! portin p-in)
                        (set! portout p-out)
                        )
                      ;; just sending two blacks
                      ;; Nick told me that this is required.
                      (sendPixels black)
                      (sendPixels black)
                      )
  )

;; Closes the connection (if present)
(define opc-close (λ ()
                    
                    (cond [(not (empty? portout))
                           (close-output-port portout)
                           (set! portout empty)]
                          )
                          
                    (cond [(not (empty? portin))
                           (close-input-port portin)
                           (set! portin empty)
                           ]
                    
                          )
                    )
  )

;; Send a list of pixels to be displayed.
;; The message format is: channel, command, high_bit, low_bit, R1 G1 B1 R2 G2 B2,
;; in bytes. channel and command fixed to 0; high_bit and low_bit computed depending
;; on the number of pins to be sent
(define sendPixels (λ (pxs) ;; pxs is a list of triples: ( (R1 G1 B1) (R2 G2 B2)...) 
                     ;; TODO: validate input!
                     (define highbit (quotient (* 3 (length pxs)) 256))
                     (define lowbit  (modulo (* 3 (length pxs)) 256))
                     (define header (bytes 0 0 highbit lowbit))
                     
                     ;; Nothing really complicated here, just send them :-)
                     (cond [ (not (empty? portout))
                               (write-bytes header portout)
                               (write-bytes (list->bytes (append* pxs)) portout)
                               (flush-output portout)
                              ]
                           )
                     )
  )


;; A simple demo function. Default value is 0 if
;; called without parameters

(define chase (λ ([num 0])
                
                ;; In the current strip we set only the pixel that
                ;; is equal to num, all the rest is 0
                (define cur-strip
                  (for/list ([i (range NUMPIXELS)])
                             (cond 
                               [ (= i num)
                                 (list 255 255 255)
                                 ]
                               [else (list 0 0 0)]
                               )
                             )
                  )
                (sendPixels cur-strip)
                (sleep 0.02)
                
                ;; Call recursively
                (cond [ (< num 360)
                        (chase (+ 1 num))
                        ]
                      [else (chase 0)]
                      )
                )
  )