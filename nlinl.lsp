;; @module  nlinl.lsp 
;; @update  2018-02-08 18:01:33
;; @desc    modules manager of newlisp project
;; @version 0.1 - search and install modules for newlisp project
;; @author: Milo Shaw   milo@newlisp.cn | milo@deepsupport.org

;; nlinl public functions list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (nlinl:import-module module-name)
;; (nlinl:registe-lib-path module-path)
;; (nlinl:unregiste-lib-path module-path)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make this module compatible with version less than 10.1.11
(when (< (sys-info -2) 10111)
	(constant (global 'term) name))

(when (< (sys-info -2) 10110)
	(constant (global '++) inc))

(context 'nlinl)

(define (nlinl-init)
 ;; url for get third-part module (library) of newlisp project 
 (set 'modules-site-url "http://www.newlisp.cn/modules?name=")
 ;; there is the collect vector to keep all modules path of project
 (set 'module-directories  '())

 ;; first of all, add default modules path. 
 (check-common-modules-dir)
 (registe-lib-path (append (env "NEWLISPDIR") "/" "modules"))
 ;; why common-modules-dir could be find ? it has bind in the function check-common-modules-dir. I am confused on
 ;; it because i am an old C programer. after i read article careful i understand it: dynamic scope ! 
 (registe-lib-path common-modules-dir))


;; @desc register module (three-part library) paths for current project 
(define (nlinl:registe-lib-path module-path)
 (when (file? module-path) 
  (if (null? module-directories)
   (push module-path module-directories)
   (begin 
        (if-not (find module-path module-directories)
         (push module-path module-directories -1))
    ))
 ))

;; @desc unregister modules (three-part library) paths from current  project
(define (nlinl:unregiste-lib-path module-path)
 (if (null? module-directories)
  (throw-error "Empty, nothing can remove !")
  (begin 
    (if (find module-path module-directories)
     (pop module-directories (find module-path module-directories))
     (throw-error (string module-path "don't exists !"))
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

;; get-remote-file: download remote file with url and save it on local.
(define (get-remote-file url save-name) 
 (if (null? url)
    (throw-error "please check your url again !")
    (begin
     ;; parse url and extract host port and target file with full path, return list about all of its.
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
 )
)

;; check third-part modules common directory of newlisp project, create it if not exists.
(define (check-common-modules-dir)
 (setq common-modules-dir (string (env "HOME") "/.newlisp/" "modules"))
 (setq tmp (append (env "HOME") "/" ".newlisp"))
 (if-not (file? tmp) (make-dir tmp))
 (if-not (file? common-modules-dir) (make-dir common-modules-dir)))


;; @update 2018-02-13 17:23:09 bug fixed for download module fail.
;; @desc public interface for import third-part module with name.
(define (nlinl:import-module module-name)
 (define (append-module-fullname dir-name) (append dir-name "/" module-name ".lsp"))
 (set 'local-modules-list (map append-module-fullname module-directories))
 (set 'local-module-file (local-modules-list (or 
                            (find true (map file? local-modules-list))
   (begin
        ;; if target-module not exists, donwload it from newlisp.cn office site.
        (check-common-modules-dir)
        (set 'download-module (append-module-fullname common-modules-dir))
        (if-not (null? modules-site-url)
            (begin 
             (get-remote-file (append  modules-site-url module-name) download-module) 
             (if (file? download-module) (load download-module)))) ))))
 ;; load target module 
 (if (file? local-module-file) 
  (if (> (file-info local-module-file 0) 0) 
   (load local-module-file)
   (begin (delete-file local-module-file) (throw-error 
    (string "download module " module-name " fail! try (nlinl:import-module \"" module-name "\") again."))))
  ))

;; lanching and init nlinl context 
(nlinl-init)

;; exit context nlinl and back to context main 
(context 'MAIN)

; eof ;
