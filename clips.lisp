;;;; the compiler

(defpackage #:clips
  (:use #:cl))
(in-package #:clips)

(defvar *label->string*) ; XXX this is dumb just pass it
(defun main (filename)
  "Main entry point, converts a single main function (in FILENAME) to mips on stdout"
  (let ((*label->string* (make-hash-table))
        (*gensym-counter* 0))
    (emit
     (with-open-file (f filename)
       (let ((ast (weave-c:parse-c-file f)))
         (loop
           for decl in ast
           ;; TODO handle variables, ignore prototypes
           collect (destructuring-bind (_definition _qualified_type
                                        (_ptr (_function name return-type)) nil
                                        (_block &rest body))
                       decl
                     (declare (ignore _definition _qualified_type _ptr _function _block))
                     return-type
                     (let ((ir (function->ir name body)))
                       (format t "translation of ~a~%~%" name)
                       (pprint-translation ir)))))))))

;;; ir to text

(defun emit (decls)
  (format t ".text~%")
  (loop for fn-ir in decls
        do (format t "~a:" (getf fn-ir :name))
           (emit-code fn-ir))
  (emit-data-segment))

(defun emit-data-segment ()
  (format t "~%.data~%")
  (let ((*print-case* :downcase))
    (maphash (lambda (k v)
               (format t "~a: .asciiz \"~a\"~%" k v))
             *label->string*)))

(defun replace-vars->regs (ir)
  (let ((vars (getf ir :vars)))
    (if (> (length vars) 10)
        (error "TOO MANY VARS: remind plisp to implement register spilling TODO")
        (loop for i below 10
              for sym in vars
              for reg = (concatenate 'string "$t" (princ-to-string i))
              do (setf (getf ir :code)
                       (nsubst reg sym (getf ir :code) :test 'equal))))))

(defvar *branch-insts* '(b bne beq bge ble blt bgt))
(defun emit-code (fn-ir)
  (replace-vars->regs fn-ir)
  (loop for inst in (getf fn-ir :code)
        do (let ((*print-case* :downcase)
                 (fn-name (getf fn-ir :name)))
             ;; TODO output comment containing var to register translations
             (case (first inst)
               (:comment (format t "~&	# ~a~&" (second inst)))
               (label (format t "~&~a_~a:~&" fn-name (second inst)))
               (t
                (when (member (first inst) *branch-insts*) ; 'fix' branch target name
                  (setf (alexandria:lastcar inst)
                        (format nil "~a_~a" fn-name (alexandria:lastcar inst))))
                (format t "~&	~8a ~{~a~^, ~}~&" (first inst) (rest inst)))))))

;;; compiler internals

(defvar *vars*)
(defun function->ir (name statements-decls)
  ;; TODO handle callconv
  ;; XXX doesn't know about lexical vars (assume no name shadowing in sample code)
  (let* ((*vars* (mapcan (lambda (decl)
                           (destructuring-bind (_declaration (type) &rest vars)
                               decl
                             (declare (ignore _declaration))
                             ;; TODO support other types? (calculate stack space here)
                             (assert (eq type 'weave-c::int))
                             (loop for x in vars
                                   collect (if (null (first x))
                                               (second x)
                                               (second (first x)))))) ; assignment
                         (traverse-match statements-decls 'weave-c::declaration)))
         (statements (translate-fn-def statements-decls name)))
    (list :name name
          :vars *vars*
          :code statements)))

(defun translate-fn-def (statements-decls name)
  "ignores declarations, printf/scanf/operators are converted to intrinsics
supports if/while/for/return"
  (format t "translating ~a..." name)
  (let ((statements (delete-if (lambda (s) (eq (first s) 'weave-c::declaration))
                               statements-decls)))
    (loop for statement in statements
          appending (translate-statement statement))))

(defun translate-if (statement end-label)
  (if (null statement)
      () ; no else
      (destructuring-bind (_statement (block/if &rest else-body))
          statement
        (declare (ignore _statement))
        (if (eq block/if 'block) ; reached end of chain
            `(,@(mapcan (lambda (l) (translate-statement l))
                        else-body))
            ;; translate condition
            ;; translate body
            ;; setup jump
            ;;
            (destructuring-bind (condition
                                 (_statement (_block &rest if-body))
                                 &optional else)
                else-body
              (declare (ignore _statement _block))
              (let ((else-label (gensym "IFELSEIF")))
                `(,@(translate-expression condition :false-branch else-label)
                  ,@(mapcan (lambda (l) (translate-statement l))
                            if-body)
                  (b ,end-label)
                  (label ,else-label)
                  ,@(translate-if else end-label))))))))

(defun translate-statement (statement)
  (let ((thing (second statement)))
    (case (first thing)
      (weave-c::if
       (destructuring-bind (condition
                            (_statement (_block &rest if-body))
                            &optional else)
           (rest thing)
         (declare (ignore _statement _block))
         ;;
         (let ((else-label (gensym "IFELSE"))
               (end-label (gensym "IFEXIT")))
           `(,@(translate-expression condition :false-branch else-label)
             ,@(mapcan (lambda (l) (translate-statement l))
                       if-body)
             (b ,end-label)
             (label ,else-label)
             ,@(translate-if else end-label)
             (label ,end-label)))))
      ;; XXX translate these directly, assume sample code has no goto
      (weave-c::while
       (destructuring-bind (condition
                            (_statement (_block &rest while-body)))
           (rest thing)
         (declare (ignore _statement _block))
         (let ((loop-label (gensym "WHILE"))
               (exit-label (gensym "WHILEEXIT")))
           `((label ,loop-label)
             ,@(translate-expression condition :false-branch exit-label)
             ,@(mapcan (lambda (l) (translate-statement l))
                       while-body)
             (b ,loop-label)
             (label ,exit-label)))))

      (weave-c::for
       (destructuring-bind (for-decl condition update-forms
                            (_statement (_block &rest for-body)))
           (rest thing)
         (declare (ignore _statement _block))
         (let ((loop-label (gensym "FOR"))
               (exit-label (gensym "FOREXIT")))
           ;; XXX single declaration for now. multiple looks like:
           ;; (_declaration (_type) &rest ((_ptr name) (_expression expr)))
           (destructuring-bind (_declaration _qualified_type
                                ((_ptr name) (_expression expr)))
               for-decl
             (declare (ignore _declaration _qualified_type _ptr _expression))
             `((:comment "for loop start")
               ,@(translate-expression `(= ,name ,expr))
               (label ,loop-label)
               ,@(translate-expression condition :false-branch exit-label)
               ,@(mapcan (lambda (l) (translate-statement l))
                         for-body)
               ,@(translate-expression update-forms)
               (b ,loop-label)
               (label ,exit-label))))))

      (weave-c::expression (translate-expression (second thing)))

      (weave-c::return '((li $v0 0)
                         (jr $ra)))

      (t (error "unimplemented, let plisp know")))))

(defmacro with-tempvar ((tmp str) &body body)
  `(let ((,tmp (gensym ,str)))
     (push ,tmp *vars*)
     ,@body))

(defmacro expand-comparision (inst)
  `(let ((code (list `(:comment ,(let ((*package* (find-package '#:weave-c))) ; XXX hack
                                   (format nil "~a ~a ~a"
                                           (second expr) (first expr) (third expr)))))))
     (assert false-branch)
     (when (listp (second expr))
       (with-tempvar (tmp "CMPTMP")
         (setf code (nconc code `(,@(translate-expression (second expr) :target tmp))))
         (setf (second expr) tmp)))
     ;;
     (when (listp (third expr))
       (with-tempvar (tmp "CMPTMP")
         (setf code (nconc code `(,@(translate-expression (third expr) :target tmp))))
         (setf (third expr) tmp)))
     ;; actual comparision, may use immediates
     (nconc code `((,',inst ,(second expr) ,(third expr) ,false-branch)))))

(defmacro expand-binary-op (inst)
  `(let ((code (list)))
     (assert target)
     (when (listp (second expr))
       (with-tempvar (tmp "TMP")
         (setf code (nconc code `(,@(translate-expression (second expr) :target tmp))))
         (setf (second expr) tmp)))
     ;;
     (when (listp (third expr))
       (with-tempvar (tmp "TMP")
         (setf code (nconc code `(,@(translate-expression (third expr) :target tmp))))
         (setf (third expr) tmp)))
     ;; actual comparision, may use immediates
     (nconc code `((,',inst ,target ,(second expr) ,(third expr))))))

(defun translate-expression (expr &key false-branch target)
  "Destination is either a exit/else jump label or a variable for storage"
  (if (not (listp expr))
      (warn "doing nothing for ~a" expr)
      (case (first expr)
        (apply (translate-call expr))
        ;; assignment
        (weave-c::=
         (assert (stringp (second expr)) nil "only var = * supported. todo ptr lvalues")
         (cond ((listp (third expr)) ; assignment to expression
                ;; tmp = (third expr)
                ;; (second expr) = tmp
                (with-tempvar (tmp "ASSIGNTMP")
                  `(,@(translate-expression (third expr) :target tmp)
                    (move ,(second expr) ,tmp))))
               ((integerp (third expr))
                (list `(li ,(second expr) ,(third expr))))
               ((stringp (third expr))
                (list `(move ,(second expr) ,(third expr))))
               (t (error "third arg of assignment ~a not supported" (third expr)))))
        ;; TODO unary ops, mostly - and ~ and ! (manual translate?)
        ;; TODO be careful with post++, the expression value is not the original, so
        ;; just manually convert for now
        ((weave-c::++) (list `(add ,(second expr) ,(second expr) 1)))
        ((weave-c::--) (list `(sub ,(second expr) ,(second expr) 1)))
        ;; compound assignment ops: XXX don't need the others
        ((weave-c::+=) (translate-expression
                        `(weave-c::= ,(second expr)
                                     (weave-c::+ ,(second expr) ,(third expr)))))
        ((weave-c::-=) (translate-expression
                        `(weave-c::= ,(second expr)
                                     (weave-c::- ,(second expr) ,(third expr)))))
        ((weave-c::*=) (translate-expression
                        `(weave-c::= ,(second expr)
                                     (weave-c::* ,(second expr) ,(third expr)))))
        ((weave-c::/=) (translate-expression
                        `(weave-c::= ,(second expr)
                                     (weave-c::/ ,(second expr) ,(third expr)))))
        ;; arithmetic
        ((weave-c::+)  (expand-binary-op add))
        ((weave-c::-)  (expand-binary-op sub))
        ((weave-c::*)  (expand-binary-op mul))
        ((weave-c::/)  (expand-binary-op div))
        ((weave-c::%)  (expand-binary-op rem))
        ((weave-c::&)  (expand-binary-op xor))
        ((weave-c::\|) (expand-binary-op or))
        ((weave-c::^)  (expand-binary-op xor))
        ((weave-c::<<) (expand-binary-op sllv))
        ((weave-c::>>) (expand-binary-op srav))
        ;; logical operators
        ;; XXX will not handle things that aren't comparisions e.g. while(curr) = bad style
        ;; XXX no branchfree shenanigans
        ((weave-c::&&)
         (assert false-branch)
         `(,@(translate-expression (second expr) :false-branch false-branch)
           ,@(translate-expression (third expr)  :false-branch false-branch)))
        ;; careful optimizing, this is subtle. compilers like to fall thru on last branch
        ((weave-c::\|\|)
         (assert false-branch)
         (let ((tmp (gensym "ORTRUE")))
           `(,@(translate-expression (second expr) :false-branch tmp)
             ,@(translate-expression (third expr)  :false-branch tmp)
             (b ,false-branch)
             (label ,tmp))))
        ;; comparisions
        ((weave-c::==) (expand-comparision bne))
        ((weave-c::!=) (expand-comparision beq))
        ((weave-c::<)  (expand-comparision bge))
        ((weave-c::>)  (expand-comparision ble))
        ((weave-c::>=) (expand-comparision blt))
        ((weave-c::<=) (expand-comparision bgt))
        (t
         (warn "operator ~a unimplemented, knock on plisp's window TODO" (first expr))
         (list :todo expr :thing)))))

;; TODO handle calling convention
(defun translate-call (thing)
  ;; parse looks smth like "(APPLY "scanf" ((STRING "%d") (WEAVE-C::& "number")))"
  (let ((name (second thing))
        (args (third thing)))
    (cond ((string= name "printf")
           ;; XXX assume string constant
           (assert (and (listp (first args))
                        (eq (first (first args)) 'string)))
           (loop for s in (tokenizef (second (first args)))
                 with params = (rest args)
                 appending (if (stringp s)
                               (let ((label (gensym "PRINTFARG")))
                                 (setf (gethash label *label->string*) s)
                                 (list `(:comment ,(format nil "print \"~a\"" s))
                                       `(la $a0 ,label)
                                       `(li $v0 4)
                                       '(syscall)))
                               (case s
                                 (:d
                                  (let ((param (pop params)))
                                    (if (stringp param)
                                        (list `(:comment ,(format nil "print \"~a\"" param))
                                              `(move $a0 ,param)
                                              `(li $v0 1)
                                              '(syscall))
                                        ;; constant number
                                        (list `(li $a0 ,param)
                                              `(li $v0 1)
                                              '(syscall)))))))))
          ;; XXX assume only single integers and chars for now
          ((string= name "scanf")
           (assert (and (listp (first args))
                        (eq (first (first args)) 'string)))
           (loop for s in (tokenizef (second (first args)))
                 with params = (rest args)
                 appending (case s
                             (:d ; (& "var")
                              (let ((param (second (pop params))))
                                (list `(:comment ,(format nil "scan int into ~a" param))
                                      `(li $v0 5)
                                      '(syscall)
                                      `(move ,param $v0))))
                             (t (error "not implemented, poke plisp")))))
          (t (error "function ~a not implemented, tell plisp about it" name)))))

;;; utils, don't look

(defun traverse-match (list symbol)
  (labels ((rec (list symbol res)
             (when (not (atom list))
               (if (eq (first list) symbol)
                   (push list res)
                   (mapcar (lambda (l) (setf res (rec l symbol res)))
                           list)))
             res))
    (rec list symbol (list))))

(defun tokenizef (fmt)
  (loop with res = (list)
        with i = 0
        until (= i (length fmt))
        do (case (char fmt i)
             (#\%
              (case (char fmt (incf i))
                (#\d
                 (push :d res)
                 (incf i))))
             (t
              (if (stringp (first res))
                  (setf (first res) (uiop:strcat (first res) (char fmt i)))
                  (push (string (char fmt i)) res))
              (incf i)))
        finally (return (nreverse res))))

(defun pprint-translation (ir)
  (loop for x in (getf ir :statements)
        do (princ x) (terpri))
  ir)
