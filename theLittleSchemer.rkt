#lang racket


(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)) )))

(atom? (quote ()))

(atom? (car '(atom,tim)))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(lat? '((Jack) Sprat could eat no chicken fat))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(member? 'meat '(mashed potataoes and meat gravy))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat)
                          (rember a (cdr lat)))))))))

(define rember1
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (rember1 a (cdr lat)))
      (else (cons (car lat)
            (rember1 a
                    (cdr lat)))))))

(rember 'bacon '(lettuce bacon and tomato))
(rember1 'bacon '(lettuce bacon and tomato bacon))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
             (firsts (cdr l)))))))

(firsts '((five plums)
          (four)
          (eleven green oranges)))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else
       (cons (car lat) (insertR new old (cdr lat)))))))

(insertR 'e 'a '(a b c d f g h i))

(atom? -3)

(define add1
  (lambda (n)
    (+ n 1)))

(add1 68)

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else
       (cond
         ((member? (car lat) (cdr lat)) #f)
         (else (set? (cdr lat))))))))

(set? '(a a))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else
       (cons (car lat)
             (makeset (cdr lat)))))))

(makeset '(apple peach pear peach plum apple lemon peach))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (cond
              ((member? (car set1) set2) (subset? (cdr set1) set2))
              (else #f))))))

(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

(subset? '(5 chicken wings) '(5 hambugers 2 pieces fried chicken and light duckling wings))


(define subset1?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2)
            (subset? (cdr set1) set2))))))
(subset1? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

(define eqset?
  (lambda (set1 set2)
    (and
     (subset1? set1 set2)
     (subset1? set2 set1))))

(eqset? '(a) '(b))
(eqset? '(a) '(a))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else
       (cons (car set1)
             (union (cdr set1) set2))))))
(union '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define complement
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (complement (cdr set1) set2))
      (else
       (cons (car set1)
             (complement (cdr set1) set2))))))

(complement '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
       (else
        (intersect (car l-set)
                   (intersectall (cdr l-set)))))))

(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)))

(define a-pair?
  (lambda (x)
    (cond
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(a-pair? '(3 7))
(a-pair? '(full (house)))

(define first
  (lambda (p) (car p)))

(define second
  (lambda (p) (car (cdr p))))

(define build
  (lambda (s1 s2) (cons s1 (cons s2 (quote ())))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((d 4) (b 0) (c 0) (e 5) (g 4)))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else
       (cons (build (second (car rel))
                    (first (car rel)))
             (revrel (cdr rel)))))))

;(revrel '())
(revrel '((8 a) (pumpkin pie) (got sick)))


;(define fullfun?
 ; (lambda (fun)
  ;  (set? (seconds fun))))


(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eq? s1 s2))
      ((atom? s1) #f)
      ((atom? s2) #f)
      (else
       (eqlist? s1 s2)))))
(equal? '(a) '(a))

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      (else
       (cond
         ((test? (car l) a) (cdr l))
         (else (cons (car l) (rember-f test? a (cdr l)))))))))

(rember-f equal? '() '())
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))

(define rember-f1
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l))
        (else
         (cons (car l) (rember-f1 test? a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else
         (cons (car l)
               ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else
         (cons (car l)
               ((insertR-f test?) new old (cdr l))))))))

((insertL-f eq?) 'hashmap 'list '(car list lisp))
(define insert-g
  (lambda (test?)
    (lambda (left new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
         (cond
           ((eq? left #t) (cons new (cons old (cdr l))))
           (else
             (cons old (cons new (cdr l))))))
        (else
         (cons (car l)
               ((insert-g test?) left new old (cdr l))))))))
((insert-g eq?) '#f 'hashmap 'list '(car list lisp))

(define update
  (lambda (operate)
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((eq? (car l) old)
       (operate new old (cdr l)))
      (else
       (cons (car l)
             ((update operate) new old (cdr l))))))))

(define subst
  (lambda (new old l)
    (cons new l)))

((update subst) 'hashmap 'list '(car cdr list array))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? a (car lat))
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

(define multirember-eq? (multirember-f eq?))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))
