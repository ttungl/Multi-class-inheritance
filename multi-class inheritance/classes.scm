(module classes (lib "eopl.ss" "eopl")

  (require "store.scm")
  (require "lang.scm")

  ;; object interface
  (provide object object? new-object object->class-name object->fields object->field-names)

  ;; method interface
  (provide method method? a-method find-method)
  
  ;; class interface
  (provide lookup-class initialize-class-env! update-class-env! class->prototype class->field-names class->fields lookup-f-location)

;;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;

  ;; an object consists of a symbol denoting its class, and a list of
  ;; references representing the managed storage for the all the fields. 
  
  (define identifier? symbol?)

  (define-datatype object object? 
    (an-object
      (class-name identifier?)
      (field-names (list-of symbol?))
      (fields (list-of reference?))))

  ;; new-object : ClassName -> Obj
  ;; Page 340
  (define new-object                      
    (lambda (class-name)
      (let((f-lst
            (append 
               (map (lambda (field-name)
                         (newref (list 'uninitialized-field field-name)))
                         (class->field-names (lookup-class class-name)))
                    (construct-fields (lookup-class class-name) '())))
           (fn-lst
            (append (class->field-names (lookup-class class-name))
                    (construct-field-names (lookup-class class-name) '()))))
       (an-object class-name fn-lst f-lst)
        )))
      
  (define construct-field-names
    (lambda (class lst)
      (let ((proto (class->prototype class)))
        (cond 
          ((reference? proto) 
           (construct-field-names (deref proto)
                                  (append-field-names (class->field-names (deref proto)) 
                                                      lst)))
          (else lst)))))
  
  (define construct-fields
    (lambda (class lst)
      (let ((proto (class->prototype class)))
        (cond 
          ((reference? proto) (construct-fields (deref proto)
                                                (append-fields (class->fields (deref proto))
                                                               lst)))
          (else lst)))))
  
;;;;;;;;;;;;;;;; methods and method environments ;;;;;;;;;;;;;;;;

  (define-datatype method method?
    (a-method
      (vars (list-of symbol?))
      (body expression?)
      (field-names (list-of symbol?))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

  ;; a method environment looks like ((method-name method) ...)

  (define method-environment?
    (list-of 
      (lambda (p)
        (and 
          (pair? p)
          (symbol? (car p))
          (method? (cadr p))))))

  ;; method-env * id -> (maybe method)
  (define assq-method-env
    (lambda (m-env id)
      (cond
        ((assq id m-env) => cadr)
        (else #f))))

  ;; find-method : class * Sym -> Method
  ;; Page: 345
  (define find-method
    (lambda (class m-name)
      (let ((m-env (class->method-env class)))
        (let ((maybe-pair (assq m-name m-env)))
          (let ((proto (class->prototype class)))
          (cond
            ((pair? maybe-pair) (cadr maybe-pair)) ;;;; find-method
            ((reference? proto) (find-method (deref proto) m-name))
            (else
             (report-method-not-found m-name))))))))
  
  
  (define report-method-not-found
    (lambda (name)
      (eopl:error 'find-method "unknown method ~s" name)))
  
  ;; merge-method-envs : MethodEnv * MethodEnv -> MethodEnv
  ;; Page: 345
  (define merge-method-envs
    (lambda (prototype-m-env new-m-env)
      (append new-m-env prototype-m-env)))

  ;; method-decls->method-env :
  ;; Listof(MethodDecl) * ClassName * Listof(FieldName) -> MethodEnv
  ;; Page: 345
  (define method-decls->method-env
    (lambda (m-decls field-names)
      (map
        (lambda (m-decl)
          (cases method-decl m-decl
            (a-method-decl (method-name vars body)
              (list method-name
                (a-method vars body field-names)))))
        m-decls)))

  ;;;;;;;;;;;;;;;; classes ;;;;;;;;;;;;;;;;

  (define-datatype class class?
    (a-class
      (prototype (maybe reference?))
      (field-names (list-of symbol?))
      (fields (list-of reference?))
      (method-env method-environment?)))

  ;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

  ;; the-class-env will look like ((class-name class) ...)

  ;; the-class-env : ClassEnv
  ;; Page: 343
  (define the-class-env '())

  ;; add-to-class-env! : ClassName * Class -> Unspecified
  ;; Page: 343
  (define add-to-class-env!
    (lambda (class-name class)
      (let ((maybe-pair (assq class-name the-class-env)))
        (cond
          (maybe-pair (update-class-env (list-field-names maybe-pair the-class-env)
                                          (list class-name class)))
          (else
           (set! the-class-env
                 (cons
                  (list class-name class)
                  the-class-env)))))))
  
  ;; update class env
  (define update-class-env
    (lambda (index elem)
      (set! the-class-env
      (list-update-class-env the-class-env 
                             index elem))))

  (define (list-update-class-env lst countindex val)
    (cond ((null? lst) lst)
          (else
           (cons
            (cond ((zero? countindex) val)
                  (else (car lst)))
            (list-update-class-env (cdr lst) (- countindex 1) val)))))
  
  (define list-field-names
    (lambda (elem lst)
      (cond ((null? lst) -1)
            ((eq? (car lst) elem) 0)
            ((= (list-field-names elem (cdr lst)) -1) -1)
            (else
             (+ 1 (list-field-names elem (cdr lst)))))))
  
  ;; lookup-class : ClassName -> Class
  (define lookup-class                    
    (lambda (name)
      (let ((maybe-pair (assq name the-class-env)))
        (if maybe-pair (cadr maybe-pair)
          (report-unknown-class name)))))
  
  (define lookup-f-location
    (lambda (class field-name)
      (list-field-names field-name (class->field-names class))))

  (define report-unknown-class
    (lambda (name)
      (eopl:error 'lookup-class "Unknown class ~s" name)))
  
  ;; constructing classes

  ;; initialize-class-env! : Listof(ClassDecl) -> Unspecified
  ;; Page: 344
  (define initialize-class-env!
    (lambda (c-decls)
      (set! the-class-env 
        (list
          (list 'object (a-class #f '() '() '()))))
      (for-each initialize-class-decl! c-decls)))
  
  ;; update-class-env:
  (define update-class-env!
    (lambda (u-decls)
      (for-each update-class-decl! u-decls)))
  
  ;; update-class-decl:
  (define update-class-decl!
    (lambda (u-decl)
      (cases class-extend-merge u-decl
        (class-extend-merge-decl (merge-c3-name c2-name c1-name)
           (proto-merge merge-c3-name c2-name c1-name)))))

  ;; initialize-class-decl! : ClassDecl -> Unspecified
  (define initialize-class-decl!
    (lambda (c-decl)
      (cases class-decl c-decl
        (a-class-decl (class-name f-names m-decls)
           (add-to-class-env!
              class-name
              (a-class #f f-names 
                       (map
                        (lambda (field-name)
                          (newref (list 'uninitialized-field field-name)))
                        f-names)
                       (method-decls->method-env
                        m-decls f-names)))))))
  
  ;; proto-merge:
  ;; this procedure merges all the fields and methods of class 1 and class 2 and put into class 3 (merge-c3-name).
  (define proto-merge
    (lambda (merge-c3-name c2-name c1-name)
      (let ((s1-class (lookup-class c1-name))
            (s2-class (lookup-class c2-name)))
        (add-to-class-env!
           merge-c3-name
           (a-class (class->prototype s1-class)
                    (append-field-names (class->field-names s1-class)
                                        (class->field-names s2-class))
                    (append-fields (class->fields s1-class)
                                   (class->fields s2-class))
                    (merge-method-envs (class->method-env s1-class)
                                       (class->method-env s2-class)))))))
  

  ;; append-field-names :  Listof(FieldName) * Listof(FieldName) 
  ;;                       -> Listof(FieldName)
  ;; Page: 344
  ;; like append, except that any super-field that is shadowed by a
  ;; new-field is replaced by a gensym
  (define append-field-names
    (lambda (proto-fields new-fields)
      (cond
        ((null? proto-fields) new-fields)
        (else
         (cons 
           (if (memq (car proto-fields) new-fields)
             (fresh-identifier (car proto-fields))
             (car proto-fields))
           (append-field-names
             (cdr proto-fields) new-fields))))))

  ;; append-fields : Listof(Fields) * Listof(Fields) -> Listof(Fields) 
  (define append-fields
    (lambda (proto-fields new-fields)
      (append new-fields proto-fields)))
  
  
;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

  (define class->prototype
    (lambda (c-struct)
      (cases class c-struct
        (a-class (prototype field-names fields method-env)
          prototype))))

  (define class->field-names
    (lambda (c-struct)
      (cases class c-struct
        (a-class (super-name field-names fields  method-env)
          field-names))))
  
  (define class->fields
    (lambda (c-struct)
      (cases class c-struct
        (a-class (super-name field-names fields  method-env)
          fields))))

  (define class->method-env
    (lambda (c-struct)
      (cases class c-struct
        (a-class (super-name field-names fields method-env)
          method-env))))

  (define object->class-name
    (lambda (obj)
      (cases object obj
        (an-object (class-name field-names fields)
          class-name))))

  (define object->fields
    (lambda (obj)
      (cases object obj
        (an-object (class-decl field-names fields)
          fields))))
  
  (define object->field-names
    (lambda (obj)
      (cases object obj
        (an-object (class-decl field-names fields)
          field-names))))

  (define fresh-identifier
    (let ((sn 0))
      (lambda (identifier)  
        (set! sn (+ sn 1))
        (string->symbol
          (string-append
            (symbol->string identifier)
            "%"             ; this can't appear in an input identifier
            (number->string sn))))))

  (define maybe
    (lambda (pred)
      (lambda (v)
        (or (not v) (pred v)))))

  )