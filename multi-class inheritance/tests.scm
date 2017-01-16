(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
      (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)


      (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      (nested-procs2 "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
        -1)
      
       (y-combinator-1 "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)
      
       ;; simple letrecs
      (simple-letrec-1 "letrec f(x) = -(x,1) in (f 33)" 32)
      (simple-letrec-2
        "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)"
        8)

      (simple-letrec-3
        "let m = -5 
 in letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)"
        20)
      
;      (fact-of-6  "letrec
;  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
;in (fact 6)" 
;                  720)
      
      (HO-nested-letrecs
"letrec even(odd)  = proc(x) if zero?(x) then 1 else (odd -(x,1))
   in letrec  odd(x)  = if zero?(x) then 0 else ((even odd) -(x,1))
   in (odd 13)" 1)

      
      (begin-test-1
        "begin 1; 2; 3 end"
        3)

      ;; extremely primitive testing for mutable variables

      (assignment-test-1 "let x = 17
                          in begin set x = 27; x end"
        27)


      (gensym-test
"let g = let count = 0 in proc(d) 
                        let d = set count = -(count,-1)
                        in count
in -((g 11), (g 22))"
-1)

      (even-odd-via-set "
let x = 0
in letrec even(d) = if zero?(x) then 1 
                                  else let d = set x = -(x,1)
                                       in (odd d)
              odd(d)  = if zero?(x) then 0 
                                  else let d = set x = -(x,1)
                                       in (even d)
   in let d = set x = 13 in (odd -99)" 1)

      (example-for-book-1 "
let f = proc (x) proc (y) 
                  begin
                   set x = -(x,-1);
                   -(x,y)
                  end
in ((f 44) 33)"
	12)

      ;; multiple arguments
     (nested-procs2 "let f = proc(x,y) -(x,y) in (f -(10,5) 6)"
        -1)
            

    (twice-cps "
      let twice = proc(f, x, k)
                    (f x  proc (z) (f z k))
      in (twice 
          proc (x, k) (k -(x,1))
          11
          proc(z) z)"
      9)

    (cps-neither-basic "
      let f = proc (x) proc (y) -(x, y)
          g = proc (z) -(z, 1)
      in ((f 27) (g 11))"
      17)
    
    ;; class tests
    (create-empty-class
  "class c1 3" 3)
    
    (create-class-with-method "
class c1
  field y 
  method gety() y 33 "
33)
    
    (create-object "
class c1
 method initialize() 0 
let o1 = new c1() in 11
" 11)
    
    (send-msg-1 "
class c1
  field s 
  method initialize()set s = 44
  method gets()s
  method sets(v)set s = v
  
let o1 = new c1() in send o1 gets()
"
44)
    
    (send-msg-2 "
class c1
  field s 
  method initialize()set s = 44
  method gets()s
  method sets(v)set s = v
  
let o1 = new c1() 
    t1 = 0
    t2 = 0 
in begin
     set t1 = send o1 gets();
     send o1 sets(33);
     set t2 = send o1 gets();
     list(t1, t2)
  end
"
(44 33))
    
    (test-self-1 "
class c
  field s
  method initialize(v)set s = v
  method sets(v)set s = v
  method gets()s
  method testit()send self sets(13)
  
let o = new c (11)
       t1 = 0
       t2 = 0
   in begin 
       set t1 = send o gets();
       send o testit();
       set t2 = send o gets();
       list(t1,t2)
      end" (11 13))
  
    
    (chris-1 "
class aclass
  field i
  method initialize(x) set i = x
  method m(y) -(i,-(0,y))
  
let o1 = new aclass(3)
in send o1 m(2)"                        
5)
    
    (for-book-1 "
class c1
  field i
  field j
  method initialize(x) begin set i = x; set j = -(0,x) end
  method countup(d) begin set i = -(i,-(0,d)); set j = -(j,d) end
  method getstate()list(i,j)
  
let o1 = new c1(3)
    t1 = 0
    t2 = 0
in begin
    set t1 = send o1 getstate();
    send o1 countup(2);
    set t2 = send o1 getstate();
    list(t1,t2)
   end"
((3 -3) (5 -5)))
    
    (odd-even-via-self "
class oddeven
  method initialize()1
  method even(n)if zero?(n) then 1 else send self odd (-(n,1))
  method odd(n) if zero?(n) then 0 else send self even (-(n,1))
  
let o1 = new oddeven() in send o1 odd(13)"
1)

   
   (test-class-extend-merge-1
    "
class c1
   field f1
   field f3
   method initialize() set f1 = 1
   method getf3() f3

class c2
   field f2
   field f3
   method initialize() set f2 = 2
   method setf1(n)set f1 = n
   method setf2(n)set f2 = n
   method setf3(n)set f3 = n
   method getf1() f1
   method getf2() f2

classmerge c3 = c1 & c2

let o = new c3()
t1=0
t2=0
in begin
   setfield c1 f1 = 5;
   send o setf1(20);
   set t2 = getfield c1 f1;
   set t1 = send o getf1();
   list(t1,t2)
end
    "
   (20 5) )

   
  (test-class-extend-merge-2
    "
class c1
   field f1
   field f2
   field f4
   method initialize() set f1 = 1
   method getf4() f4

class c2
   field f3
   field f4
   method initialize() set f3 = 2
   method setf3(n)set f3 = n
   method setf4(n)set f4 = n
   method getf2() f2
   method getf3() f3

class c3

classmerge c4 = c2 & c3

let o = new c4()
 t1=0
 t2=0
in begin
   setfield c2 f3 = 5;
   send o setf3(20);
   set t2 = getfield c2 f3;
   set t1 = send o getf3();
   list(t1,t2)
end
    "
   (20 5) )   
   
   
  (test-class-extend-merge-3
    "
class c1
   field f1
   field f2
   field f4
   method initialize() set f1 = 1
   method getf4() f4

class c2
   field f3
   field f4
   method initialize() set f2 = 2
   method setf1(n)set f1 = n
   method setf2(n)set f2 = n
   method setf3(n)set f3 = n
   method setf4(n)set f4 = n
   method getf1() f1
   method getf2() f2
   method getf3() f3

classmerge c4 = c1 & c2

let o = new c4()
 t1=0
 t2=0
 t3=0 
 t4=0
in begin
   setfield c1 f1 = 5;
   send o setf1(20);
   set t2 = getfield c1 f1;
   set t1 = send o getf1();
   send o setf4(30);
   setfield c2 f4 =15;
   set t3 = getfield c2 f4;
   set t4 = send o getf4();
   list(t1,t2,t3,t4)
end
    "
   (20 5 15 30) ) 
   
  
  )))

