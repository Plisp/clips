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
           ;; TODO handle static variables, ignore prototypes
           collect (destructuring-bind (_definition _qualified_type
                                        (_ptr (_function name return-type)) nil
                                        (_block &rest body))
                       decl
                     (declare (ignore _definition _qualified_type _ptr _function _block
                                      return-type))
                     (function->ir name body))))))))

;;; ir to text

(defun emit (decls)
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

;; Hacky I know but I'm not working on scope as this is a simple toy.
(defparameter *regs*
  '($t0 $t1 $t2 $t3 $t4 $t5 $t6 $t6 $t7 $t8 $t9 #|$a0 used by io|# $a1 $a2 $a3
    $s0 $s1 $s2 $s4 $s5)
  "TODO saved registers should be saved and restored by the prologue. right now just hack")

(defun replace-vars (code vars)
  (if (> (length vars) (length *regs*))
      (error "TOO MANY VARS: remind plisp to implement register spilling TODO")
      (loop for i below (length *regs*)
            for sym in vars
            for reg in *regs*
            do (setf code (nsubst reg sym code :test 'equal))
            finally (return code))))

(defparameter *branch-insts* '(b bne beq bge ble blt bgt))
(defun emit-code (fn-ir)
  (let ((code
          (replace-vars (append (getf fn-ir :prologue) (getf fn-ir :code))
                        (getf fn-ir :vars))))
    (loop for inst in code
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
                  (format t "~&	~8a ~{~a~^, ~}~&" (first inst) (rest inst))))))))

;;; compiler internals

(defvar *vars*)
(defun function->ir (name statements-decls)
  ;; TODO handle callconv
  ;; XXX doesn't know about lexical vars (assume no name shadowing in sample code)
  (let* ((stack-size 0) ; XXX I don't think I actually need to clean up the stack here
         (prologue (list))
         (*vars*
           (mapcan (lambda (decl)
                     (destructuring-bind (_declaration (type) &rest vars)
                         decl
                       (declare (ignore _declaration))
                       ;; TODO support other types (bool, char, double)
                       (assert (eq type 'weave-c::int))
                       (loop
                         for var in vars
                         collect (if (stringp (second var))
                                     ;; integer
                                     (if (null (first var))
                                         (second var)
                                         (second (first var)))
                                     ;; pointer (array N _qualifiers "var")
                                     (destructuring-bind (_array n _qualifiers name)
                                         (second (first var))
                                       (declare (ignore _array _qualifiers))
                                       (let ((init (gensym "INITSTART"))
                                             (init-end (gensym "INITEND")))
                                         (incf stack-size (* n 4))
                                         (setf prologue
                                               (nconc prologue
                                                      `((li $t0 0)
                                                        (label ,init)
                                                        (bge $t0 ,n ,init-end)
                                                        ;; store zero to stack and inc
                                                        (sw $zero ($sp))
                                                        (add $sp $sp -4)
                                                        (add $t0 $t0 1)
                                                        (b ,init)
                                                        (label ,init-end)
                                                        (move ,name $sp)
                                                        (:comment "body begins..."))))
                                         name))))))
                   (traverse-match statements-decls 'weave-c::declaration)))
         (statements (translate-fn-def statements-decls name)))
    (list :name name
          :vars (print *vars*)
          :prologue prologue
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
        ;; TODO XXX assign to array (= (aref "array" (+/- "index" immediate)) expr)
        (weave-c::=
         (if (stringp (second expr))
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
                   (t (error "third arg of assignment ~a not supported" (third expr))))
             ;; array! (= (AREF "numbers" (+ "i" 1)) "y")
             ;; XXX HACK should use pointers
             (let* ((value (third expr))
                    (expr (second expr))
                    (code))
               (with-tempvar (val "TMP")
                 (when (listp (third expr))
                   (with-tempvar (tmp "TMP")
                     (setf code
                           (nconc code `(,@(translate-expression (third expr) :target tmp))))
                     (setf (third expr) tmp)))
                 ;; we need to compute the pointer to load from
                 (with-tempvar (tmp "OFFSETTMP")
                   `(,@code
                     (mul ,tmp ,(third expr) 4) ; int
                     (add ,tmp ,(second expr) ,tmp)
                     (sw ,value (,tmp))))))))
        ;; TODO unary ops, mostly - and ~ and ! (manual translate?)
        ;; TODO be careful with post++, the expression value is not the original, so
        ;; just manually convert for now
        (weave-c::++ (list `(add ,(second expr) ,(second expr) 1)))
        (weave-c::-- (list `(sub ,(second expr) ,(second expr) 1)))
        ;; compound assignment ops: XXX don't need the others
        (weave-c::+= (translate-expression
                        `(weave-c::= ,(second expr)
                                     (weave-c::+ ,(second expr) ,(third expr)))))
        (weave-c::-= (translate-expression
                        `(weave-c::= ,(second expr)
                                     (weave-c::- ,(second expr) ,(third expr)))))
        (weave-c::*= (translate-expression
                        `(weave-c::= ,(second expr)
                                     (weave-c::* ,(second expr) ,(third expr)))))
        (weave-c::/= (translate-expression
                        `(weave-c::= ,(second expr)
                                     (weave-c::/ ,(second expr) ,(third expr)))))
        ;; arithmetic
        (weave-c::+  (expand-binary-op add))
        (weave-c::-  (expand-binary-op sub))
        (weave-c::*  (expand-binary-op mul))
        (weave-c::/  (expand-binary-op div))
        (weave-c::%  (expand-binary-op rem))
        (weave-c::&  (expand-binary-op xor))
        (weave-c::\| (expand-binary-op or))
        (weave-c::^  (expand-binary-op xor))
        (weave-c::<< (expand-binary-op sllv))
        (weave-c::>> (expand-binary-op srav))
        ;; logical operators
        ;; XXX will not handle things that aren't comparisions e.g. while(curr) = bad style
        ;; XXX no branchfree shenanigans
        (weave-c::&&
         (assert false-branch)
         `(,@(translate-expression (second expr) :false-branch false-branch)
           ,@(translate-expression (third expr)  :false-branch false-branch)))
        ;; careful optimizing, this is subtle. compilers like to fall thru on last branch
        (weave-c::\|\|
         (assert false-branch)
         (let ((tmp (gensym "ORTRUE")))
           `(,@(translate-expression (second expr) :false-branch tmp)
             ,@(translate-expression (third expr)  :false-branch tmp)
             (b ,false-branch)
             (label ,tmp))))
        ;; comparisions
        (weave-c::== (expand-comparision bne))
        (weave-c::!= (expand-comparision beq))
        (weave-c::<  (expand-comparision bge))
        (weave-c::>  (expand-comparision ble))
        (weave-c::>= (expand-comparision blt))
        (weave-c::<= (expand-comparision bgt))
        ;; XXX eww
        (weave-c::aref
         (let ((code
                 (list `(:comment ,(let ((*package* (find-package '#:weave-c))) ; XXX hack
                                     (format nil "~a ~a ~a"
                                             'aref (first expr) (third expr)))))))
           (with-tempvar (val "TMP")
             (when (listp (third expr))
               (with-tempvar (tmp "TMP")
                 (setf code
                       (nconc code `(,@(translate-expression (third expr) :target tmp))))
                 (setf (third expr) tmp)))
             ;; we need to compute the pointer to load from
             (with-tempvar (tmp "OFFSETTMP")
               `(,@code
                 (mul ,tmp ,(third expr) 4) ; int
                 (add ,tmp ,(second expr) ,tmp)
                 (lw ,target (,tmp)))))))
        (t
         (warn "operator ~a unimplemented, knock on plisp's window TODO" (first expr))
         (list :todo expr :thing)))))

;; TODO handle calling convention (caller)
(defun translate-call (thing)
  ;; parse looks smth like "(APPLY "scanf" ((STRING "%d") (WEAVE-C::& "number")))"
  (let ((name (second thing))
        (args (third thing)))
    (cond ((string= name "printf")
           ;; XXX assume string constant
           (assert (and (listp (first args))
                        (eq (first (first args)) 'string)))
           (loop
             for s in (tokenizef (second (first args)))
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
                                (cond ((stringp param)
                                       (list `(:comment
                                               ,(format nil "print \"~a\"" param))
                                             `(move $a0 ,param)
                                             '(li $v0 1)
                                              '(syscall)))
                                      ;; constant number
                                      ((integerp param)
                                       (list `(li $a0 ,param)
                                             '(li $v0 1)
                                              '(syscall)))
                                      ;; array index (aref "a" "test")
                                      (t
                                       (with-tempvar (tmp "OFFSETTMP")
                                         `((mul ,tmp ,(third param) 4)
                                           (add ,tmp ,(second param) ,tmp)
                                           (lw $a0 (,tmp))
                                           (li $v0 1)
                                           (syscall)))))))))))
          ;; XXX assume only single integers and chars for now
          ((string= name "scanf")
           (assert (and (listp (first args))
                        (eq (first (first args)) 'string)))
           (loop for s in (tokenizef (second (first args)))
                 with params = (rest args)
                 appending (case s
                             (:d ; (& "var")
                              (let ((param (second (pop params))))
                                (if (stringp param)
                                    `((:comment ,(format nil "scan int into ~a" param))
                                      (li $v0 5)
                                      (syscall)
                                      (move ,param $v0))
                                    ;; array index (aref "a" "test")
                                    (with-tempvar (tmp "OFFSETTMP")
                                      `((:comment ,(format nil "scan int into ~a" param))
                                        (li $v0 5)
                                        (syscall)
                                        (mul ,tmp ,(third param) 4)
                                        (add ,tmp ,(second param) ,tmp)
                                        (sw $v0 (,tmp)))))))
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
