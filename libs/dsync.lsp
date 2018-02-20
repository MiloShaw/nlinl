;; @module fmaker.lsp 
;; @update 2018-02-13 03:33:32 bug fixed for insert-target and get-level-list
;; @update 2018-02-10 18:40:14
;; @update 2018-02-11 02:46:49
;; @update 2018-02-11 11:49:45 
;; @update 2018-02-12 02:38:10 create-sync-tree is fine !

;; @desc it works as a service, received command and scanning whole target directory, 
;; make it as a list like this: (dir file file (sub-dir file file) file (sub-dir file))
;; @version 0.1 - scanning directory and create a tree for sync with remote require.
;; @author:  Milo Shaw   milo@newlisp.cn | milo@deepsupport.org

; make this module compatible with version less than 10.1.11
(when (< (sys-info -2) 10111)
	(constant (global 'term) name))

(when (< (sys-info -2) 10110)
	(constant (global '++) inc))

;; dsync public functions list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (dsync:get-sync-tree dir-name)
;; (dsync:clear-sync-tree dir-name)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new Tree 'dsync-tree)
(context 'dsync)

(nlinl:import-module "toolbox")

;; @update 2018-02-13 12:51:03 bug fixed 
;; @desc get whole sync tree with target directory full name
(define (dsync:get-sync-tree dir-name)
 (if (file? dir-name)
    (begin 
     (set 'key (last (parse (real-path dir-name) "/")))
     (if (null? (dsync-tree key)) (create-sync-tree dir-name))
     (string "'" (dsync-tree key)))
    (throw-error (string dir-name  "is not a exists directory")) 
 ))

;; @desc clear sync tree with target directory full name
(define (dsync:clear-sync-tree dir-name)
 (if (file? dir-name)
  (dsync-tree (last (parse (real-path dir-name) "/")) nil)
  (throw-error (string dir-name  "is not a exists directory")) ))


;; @desc create sync tree after scanning whole target directory
(define (create-sync-tree dir-name)
 (set 'top-dir (real-path dir-name))
 (define (sub-dir-itor sub-dir-name)
  (when (directory? sub-dir-name)
   (insert-target top-dir sub-dir-name dsync-tree)
   (dolist (item (directory sub-dir-name {[^.]}))
    (if (directory? (append sub-dir-name "/" item))
     (sub-dir-itor (append sub-dir-name "/" item))
     (insert-target top-dir (append sub-dir-name "/" item) dsync-tree) )) 
   ))
 (dsync-tree (last (parse top-dir "/")) '())
 (sub-dir-itor top-dir))


;; @update 2018-02-13 03:30:48 bug fixed
;; @desc push item into slice of whole directory tree 
(define (insert-target start-dir target-dir top-list)
 (set 'top-node (last (parse (real-path start-dir) "/")))
 (set 'level-list (get-level-list start-dir target-dir))
 (if (and (list? level-list) (null? level-list))
  (if (and (directory? target-dir) (> (length target-dir) (length start-dir)))
   (eval-string 
    (toolbox:func-maker "push" (toolbox:func-maker "push" (last (parse (real-path target-dir) "/")) '()) 
     (toolbox:func-maker "top-list" top-node) -1))
   (eval-string (toolbox:func-maker "push" (last (parse (real-path target-dir) "/")) (toolbox:func-maker "top-list" top-node) -1)))
  (if-not (null? level-list)
   (if (directory? target-dir)
    (eval-string 
     (toolbox:func-maker "push" (toolbox:func-maker "push" (last (parse (real-path target-dir) "/")) '())
      (toolbox:func-maker "assoc" level-list (toolbox:func-maker "top-list" top-node)) -1))
    (eval-string 
     (toolbox:func-maker "push" (last (parse (real-path target-dir) "/")) 
      (toolbox:func-maker "assoc" level-list (toolbox:func-maker "top-list" top-node)) -1)))
   )))

;; @update 2018-02-13 03:24:51 bug fixed 
;; @desc get a directory level list from start-dir to target-dir
(define (get-level-list start-dir target-dir) 
 (set 'root-dir (real-path start-dir))
 (set 'target-name (real-path target-dir))
 (if (and (file? root-dir) (not (null? target-name)) (file? target-name))
  (if (< (length root-dir) (length target-name)) 
   (if-not (null? (set 'regex-list (regex (append root-dir "/") target-name)))
    (0 -1 (parse ((last regex-list) (length target-name) target-name) "/")))
   '()) 
  ))

(context 'MAIN)

; eof ;
