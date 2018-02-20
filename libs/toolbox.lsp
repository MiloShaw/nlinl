;; @module toolbox.lsp 
;; @update 2018-02-12 15:47:06

;; @description modules manager of newlisp
;; @version 0.1 - common useful functions in my toolbox 
;; @author:  Milo Shaw   milo@newlisp.cn | milo@deepsupport.org

; make this module compatible with version less than 10.1.11
(when (< (sys-info -2) 10111)
	(constant (global 'term) name))

(when (< (sys-info -2) 10110)
	(constant (global '++) inc))


;; toolbox common functions list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (toolbox:get-remote-file remote-url save-name) (no save-name by default)
;; (toolbox:func-maker cmd left right option)
;; (toolbox:sort a-list flag) (no flag by default)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(context 'toolbox)

;; @add 2018-02-08 18:01:33
;; @desc get-remote-file: download remote file with url and save it on local.
;; @func (toolbox:get-remote-file remote-url save-name)
(define (toolbox:get-remote-file url save-name) 
 (if (null? url)
    (throw-error "please check your url again !")
    (begin
     ;; parse url and extract host port and target file, return a list about all of its.
      (set 'arguments (parse-url url))
      (if (and (list? arguments) (>= (length arguments) 3))
            (begin
                (set 'socket (net-connect (first arguments) (arguments 1)))
                (set 'target-file (join (2 arguments) "/"))
                (net-send socket (string "GET  /" target-file "\r\n\r\n"))
                (net-receive socket buffer 4096 "\n")
                (set 'total 0)
                (while (net-select socket "read" 500)
                 (if (null? save-name) 
                  (append-file (last arguments) buffer)
                  (append-file save-name buffer))
                 (inc total (length buffer))
                 (net-receive socket buffer 4096 "\n"))
                (println (string "received data:" total " bytes !"))
                (close socket)
            )
      ))
 ))

;; parese url and return a list about host name or ip , port and target file 
(define (parse-url url)
 (if (or (regex "^http.*//" url) (regex "^ftp.*//" url))
    (set 'tmp-url ((last (or (regex "http.*//" url) (regex "ftp.*//" url))) (length url) url))
    (set 'tmp-url url))
 ;; extract host name or ip 
 (set 'tmp-list (parse tmp-url "/"))
 (set 'host-port (parse (first tmp-list) ":"))
 (if (< (length host-port) 2)
  (push 80 host-port -1))
 (dolist (item (rest tmp-list))
  (push (string item) host-port -1))
 ;; return a empty list or result
 host-port)


;; @add 2018-02-11 15:33:39
;; @desc create string expression, and execute as a whole expression with eval-string
;; @func (func-maker: cmd left right option)
(define (toolbox:func-maker op x y z)
 (when (and (not (null? op)) (not (null? x)))
  (if (or (not (null? y)) (list? y))
   (if (and (list? x) (> (length x) 1))
    (if (= (length x) 1) 
      (if-not (null? z) 
        (string "(" op " " (type-check (last x)) " " (type-check y) " " z ")")
        (string "(" op " " (type-check (last x)) " " (type-check y) " )"))
      (if-not (null? z)
        (string "(" op " " (type-check (last x)) " " 
         (func-maker op (0 (- (length x) 1) x)  (type-check y)  z) " " z ")")
        (string "(" op " " (type-check (last x)) " " 
         (func-maker op (0 (- (length x) 1) x)  (type-check y)) ")")))
    (if-not (null? z) 
      (string "(" op " " (type-check x) " " (type-check y) " " z ")")
      (string "(" op " " (type-check x) " " (type-check y) ")")))
   (string "(" op " " (type-check x) ")") 
  )))

;; check arguments and do something for it.
(define (type-check arg)
 (if (or (not (null? arg)) (list? arg))
  (cond 
   ((list? arg) (string "'" arg))
   ((string? arg) (if (= (find {^\(.*\)$} arg 0) 0) arg (string "{" arg "}")))
   ((atom? arg) (eval arg))
   ((lambda? arg) (eval arg))
   ((number? arg) arg)
   ; ((symbol? arg) arg)
   )))

;; does newlisp copy all arguments into function ?
;; why does values of test-list not change in my sort function ?
;; 2018-02-07 21:50:18
;; yes, newlisp copy value in use-define function, not reference !

;; @add 2018-02-07 21:50:18
;; @desc sort with flag (0 or 1)
(define (toolbox:sort a-list flag)
 (define (sort-itor i l)
  (set 'alist-len (length a-list))
  (when (and (list? l) (< i alist-len))
   (define (sort-itor-item j)
    (when (< j alist-len)
     (if ((if (= flag 0) < >) (l i) (l j))
        (begin 
         (swap (l i) (l j)) 
         (set 'result l))
        )
     (sort-itor-item (++ j))
    ))
   (sort-itor-item i)
   (sort-itor (++ i) l)))
 (sort-itor 0 a-list)
 result)

; strings are limited with quotes
(define (is-string arg) 
  (regex {^".*"$} arg))

; numbers can be integers or floats in decimal or scientific notation 
(define (is-number arg)
  (regex {^([+-]?(0|[1-9]\d*)(\.\d*)?|\.\d+)([eE][+-]?\d+)?$} arg))

(context 'MAIN)

; eof ;
