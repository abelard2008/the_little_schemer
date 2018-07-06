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
