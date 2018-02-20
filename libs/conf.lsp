;; @module config.lsp 
;; @update 2018-01-17 14:33:21
;; @description configure file operate interface routines
;; @version 0.1 - get and write values with search key.
;; @author:  Milo Shaw   milo@zionbsd.com | milo@deepsupport.org

; make this module compatible with version less than 10.1.11
(when (< (sys-info -2) 10111)
	(constant (global 'term) name))

(when (< (sys-info -2) 10110)
	(constant (global '++) inc))

(context 'conf)

(define (conf:get-value-with-key search-key key file-name)
 (if (and (not (null? search-key)) (not (null? key)) (file? file-name))
    (begin 
     (set 'file (open (string file-name) "read"))
     (while (read-line file)
      (if (and (find {search-key} (current-line) 1) (find {key} (current-line) 1))
       ;; get value from current-line 
       (println (current-line))))
     (close file))
    ;; throw warring information 
    (throw-error "search words can't be empty and configure file must exist !")
 ))

(context 'MAIN)

; eof ;
