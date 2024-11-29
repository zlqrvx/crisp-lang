(defpackage crisp/tests/main
  (:use :cl
        :crisp
	:crisp-user
        :rove))
(in-package :crisp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :crisp)' in your Lisp.


(defun parse (parser str)
  (caar (smug:run parser str)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  crisp::.symbol  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.symbol-1
  (testing "parse symbol: cl_user:foobar"
    (ok (equal
	 'common-lisp-user::foobar
	 (parse (crisp::.symbol) "cl_user:foobar")))))

(deftest test-.symbol-2
  (testing "parse symbol: foobar"
    (ok (equal
	 'crisp-user::foobar
	 (parse (crisp::.symbol) "foobar")))))

(deftest test-.symbol-3
  (testing "parse symbol: :foobar"
    (ok (equal
	 nil
	 (parse (crisp::.symbol) ":foobar")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;  crisp::.quoted-symbol  ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.quoted-symbol-1
  (testing "parse quoted-symbol: 'foobar"
    (ok (equal
	 ''crisp-user::foobar
	 (parse (crisp::.quoted-symbol) "'foobar")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  crisp::.string  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.string-1
    (testing "parse string \"839\""
	     (ok (string-equal
		  "839"
		  (parse (crisp::.string) "\"839\"")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  crisp::.comment  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.comment-1
  (testing "parse comment: # This is a comment
# This is another comment
# Yet another comment
3 * 2"
    (ok (equal
	 '(PROGN (* 3 2))
	 (crisp::crisp-compile "# This is a comment
# This is another comment
# Yet another comment
3 * 2")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  crisp::.number  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.number-1
  (testing "parse number 8.39"
    (ok (equal
	 8.39
	 (parse (crisp::.number) "8.39")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  crisp::.tuple  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.tuple-1
  (testing "parse tuple (a,b, c,sin(d), e + f)"
    (ok (equal
	 '(CRISP-USER::A CRISP-USER::B CRISP-USER::C (SIN CRISP-USER::D)
    (+ CRISP-USER::E CRISP-USER::F))
	 (parse (crisp::.tuple) "(a,b, c,sin(d), e + f)")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;  crisp::.list  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.list-1
  (testing "parse list [a,b, c,sin(d), e + f]"
    (ok (equal
	 '(LIST CRISP-USER::A CRISP-USER::B CRISP-USER::C (SIN CRISP-USER::D)
    (+ CRISP-USER::E CRISP-USER::F))
	 (parse (crisp::.list) "[a,b, c,sin(d), e + f]")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  crisp::.matlab-array  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun array-equal (A B)
  (and
   (= (array-total-size A) (array-total-size B))
   (equal (array-dimensions A) (array-dimensions B))
   (reduce (lambda (x y) (and x y))
	   (loop for i below (array-total-size A)
		 collect (= (row-major-aref A i)
			    (row-major-aref B i))))))

(deftest test-.matlab-array-1
  (testing "parse matlab array: [|1 3 ; 4 5; 6 7|]"
    (ok (array-equal
	 #2A((1 3) (4 5) (6 7))
	 (parse (crisp::.matlab-array) "[|1 3 ; 4 5; 6 7|]")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  crisp::.paren  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.paren-1
    (testing "parse paren (e + f)"
	     (ok (equal
		  '(+ CRISP-USER::E CRISP-USER::F)
		  (parse (crisp::.paren) "(e + f)")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  crisp::.funcall  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.funcall-1
  (testing "parse funcall: myfun(3, _another_fun(4, 5 - 1, myarg=4))"
    (ok (equal
	 '(CRISP-USER::MYFUN 3 (CRISP-USER::-ANOTHER-FUN 4 (- 5 1) :MYARG 4))
	 (parse (crisp::.funcall) "myfun(3, _another_fun(4, 5 - 1, myarg=4))")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  crisp::.index  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.index-1
  (testing "parse index my_list[3]"
    (ok (equal
	 '(NTH 3 CRISP-USER::MY-LIST)
	 (parse (crisp::.index) "my_list[3]")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  crisp::.infix  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.infix-1
  (testing "parse infix 8 - 2"
    (ok (equal
	 '(- 8 2)
	 (parse (crisp::.infix) "8 - 2")))))

(deftest test-.infix-2
  (testing "parse infix 8!=2"
    (ok (equal
	 '(crisp-user::not-equal 8 2)
	 (parse (crisp::.infix) "8!=2")))))

(deftest test-.infix-3
  (testing "parse infix a = 8 - 2*9"
    (ok (equal
	 '(CRISP-USER::SETQ CRISP-USER::A (- 8 (* 2 9)))
	 (parse (crisp::.infix) "a = 8 - 2*9")))))

(deftest test-.infix-4
  (testing "parse infix: print $ 4*2"
    (ok (equal
	 '(CRISP-USER::RIGHT-ASSOCIATIVE-APPLY PRINT (* 4 2))
	 (parse (crisp::.infix) "print $ 4*2")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  crisp::.crisp  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-.crisp-1
    (testing "parse crisp: 2 * mylist[3] + x"
	     (ok (equal
		  '(PROGN (+ (* 2 (NTH 3 CRISP-USER::MYLIST)) CRISP-USER::X))
		  (parse (crisp::.crisp) "2 * mylist[3] + x")))))

(deftest test-.crisp-2
  (testing "parse crisp: myfun(x,y) * another_fun(a,b)"
    (ok (equal
	 '(PROGN (CRISP-USER::* (CRISP-USER::MYFUN CRISP-USER::X CRISP-USER::Y)
                   (CRISP-USER::ANOTHER-FUN CRISP-USER::A CRISP-USER::B)))
	 (parse (crisp::.crisp) "myfun(x,y) * another_fun(a,b)")))))

