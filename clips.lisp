;;;; the compiler

(defpackage #:clips
  (:use #:cl))
(in-package #:clips)

(defun main (filename)
  "Main entry point, converts a single main function (in FILENAME) to mips on stdout"
  (let ((*label->string* (make-hash-table)))
   (emit
    (with-open-file (f filename)
      (let ((ast (weave-c:parse-c-file f)))
        (loop for decl in ast
              ;; TODO handle variables, ignore prototypes
              collect (destructuring-bind (weave-c::definition (_int)
                                           (nil (_function name return-type)) nil
                                           (_block &rest body))
                          decl
                        (declare (ignore weave-c::definition _int _function _block))
                        return-type
                        (let ((ir (function->ir name body)))
                          (format t "translation of ~a~%~%" name)
                          (pprint-translation ir)))))))))

;;; ir to text

(defvar *label->string*)
(defun emit (decls)
  (format t ".text~%")
  (loop for fn-ir in decls
        do (format t "~a:~%" (getf fn-ir :name))
           (emit-code fn-ir))
  (emit-data-segment))

(defun emit-data-segment ()
  (format t "~% .data~%" )
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

(defun emit-code (fn-ir)
  (replace-vars->regs fn-ir)
  (loop for inst in (getf fn-ir :code)
        do (case (first inst)
             (:comment (format t "    # ~a~%" (second inst)))
             (label (let ((*print-case* :downcase))
                      (format t "~a:~%" (second inst))))
             (t
              (let ((*print-case* :downcase))
                (format t "    ~4a ~{~a~^, ~}~%" (first inst) (rest inst)))))))

;;; compiler internals

(defvar *vars*)
(defun function->ir (name statements-decls)
  ;; TODO handle calling convention
  ;; XXX doesn't know about lexical vars (assume no name shadowing in sample code)
  (let* ((*vars* (mapcan (lambda (decl)
                           (destructuring-bind (weave-c::declaration (type) &rest vars)
                               decl
                             (declare (ignore weave-c::declaration))
                             ;; TODO support other types?
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
          appending (translate-statement statement name))))

(defun translate-statement (statement fn-name)
  (assert (eq (first statement) 'weave-c::statement))
  (let ((thing (second statement)))
    (case (first thing)
      ;; TODO support else if
      (weave-c::if
       (destructuring-bind (condition
                            (weave-c::statement (weave-c::block &rest if-body))
                            (_statement (_block &rest else-body)))
           (rest thing)
         (declare (ignore weave-c::statement weave-c::block _statement _block))
         ;; TODO can leave off else
         (let ((else-label (gensym "IFELSE"))
               (end-label (gensym "IFEND")))
           `(,@(translate-expression condition else-label)
             ,@(mapcan (lambda (l) (translate-statement l fn-name))
                       if-body)
             (b ,end-label)
             (label ,else-label)
             ,@(mapcan (lambda (l) (translate-statement l fn-name))
                       else-body)
             (label ,end-label)))))
      ;; XXX translate these directly, assume sample code has no goto
      (weave-c::while
       (destructuring-bind (condition
                            (weave-c::statement
                             (_block &rest while-body)))
           (rest thing)
         (declare (ignore weave-c::statement _block))
         (let ((loop-label (gensym "WHILE"))
               (exit-label (gensym "WHILEEXIT")))
           `((label ,loop-label)
             ,@(translate-expression condition exit-label)
             ,@(mapcan (lambda (l) (translate-statement l fn-name))
                       while-body)
             (b ,loop-label)
             (label ,exit-label)))))

      (weave-c::for
       (destructuring-bind (for-decls condition update-forms
                            (weave-c::statement
                             (_block &rest for-body)))
           (rest thing)
         (declare (ignore for-decls weave-c::statement _block))
         (let ((loop-label (gensym "FOR"))
               (exit-label (gensym "FOREXIT")))
           ;; TODO for decls
           `((:comment "for loop start")
             ;; TODO initialization here
             (label ,loop-label)
             ,@(translate-expression condition exit-label)
             ,@(mapcan (lambda (l) (translate-statement l fn-name))
                       for-body)
             ,@(translate-expression update-forms)
             (b ,loop-label)
             (label ,exit-label)))))

      (weave-c::expression (translate-expression (second thing)))

      (weave-c::return '((li $v0 0)
                         (jr $ra)))

      (t (error "unimplemented")))))

(defmacro with-tempvar ((tmp str) &body body)
  `(let ((,tmp (gensym ,str)))
     (push ,tmp *vars*)
     ,@body))

(defmacro expand-comparision (op1 op2 inst1 inst2)
  (declare (ignore op2))
  `(let ((code (list `(:comment ,(let ((*package* (find-package '#:weave-c))) ; XXX hack
                                   (format nil "~a ~a ~a"
                                           (second expr) (first expr) (third expr)))))))
     (assert destination)
     (when (listp (second expr))
       (with-tempvar (tmp "CMPTEMP")
         (setf code (nconc code `(,@(translate-expression (second expr) tmp))))
         (setf (second expr) tmp)))
     ;;
     (when (listp (third expr))
       (with-tempvar (tmp "CMPTEMP")
         (setf code (nconc code `(,@(translate-expression (third expr) tmp))))
         (setf (third expr) tmp)))
     ;; actual comparision, may use immediates
     (nconc code `((,(if (eq (first expr) ',op1) ',inst1 ',inst2)
                    ,(second expr) ,(third expr) ,destination)))))

;; TODO conservatively allocate registers for subexpressions
(defun translate-expression (expr &optional destination)
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
                (with-tempvar (tmp "ASSIGNTEMP")
                  `(,@(translate-expression (third expr) tmp)
                    (move ,(second expr) ,tmp))))
               ((integerp (third expr))
                (list `(li ,(second expr) ,(third expr))))
               ((stringp (third expr))
                (list `(move ,(second expr) ,(third expr))))
               (t (error "third arg of assignment ~a not supported" (third expr)))))
        ;;((weave-c::post++))
        ;;((weave-c::+-*/=))

        ;; arithmetic
        ;; XXX assumes only third can be intermediates, needs a pass to eliminate constants
        ((weave-c::+)
         (assert destination)
         (list `(,(if (integerp (third expr)) 'addi 'add)
                 ,destination
                 ,(second expr)
                 ,(third expr))))
        ((weave-c::%)
         (assert destination)
         (list `(rem ,destination ,(second expr) ,(third expr))))
        ;;((weave-c::-))
        ;;((weave-c::/))

        ;; comparisions
        ;; XXX assumes a destination for now, no branchfree shenanigans
        ((weave-c::== weave-c::!=)
         (expand-comparision weave-c::== weave-c::!= beq bne))
        ((weave-c::< weave-c::>=)
         (expand-comparision weave-c::< weave-c::>= bge blt))
        ((weave-c::> weave-c::<=)
         (expand-comparision weave-c::> weave-c::<= ble bgt))
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
                                 (list `(:comment ,(format nil "print: ~a" s))
                                       `(la $a0 ,label)
                                       `(li $v0 4)
                                       '(syscall)))
                               (case s
                                 (:d
                                  (let ((param (pop params)))
                                    (if (stringp param)
                                        (list `(:comment ,(format nil "print: ~a" param))
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
                             (:d
                              (let ((param (pop params)))
                                (assert (and (listp param)
                                             (stringp (second param)))
                                        nil
                                        "scanf operates on vars! not ~a" param)
                                (setf param (second param))
                                (list `(:comment ,(format nil "scan int into ~a" param))
                                      `(li $v0 5)
                                      '(syscall)
                                      `(move ,param $v0)))))))
          (t (error "function ~a not implemented" name)))))

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
                  (setf (first res)
                        (concatenate 'string (first res) (string (char fmt i))))
                  (push (string (char fmt i)) res))
              (incf i)))
        finally (return (nreverse res))))

(defun pprint-translation (ir)
  (loop for x in (getf ir :statements)
        do (princ x) (terpri))
  ir)
