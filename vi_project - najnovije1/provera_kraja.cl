(defun proveri_petXOsnovna(el br n l)
              (cond((null l) '())
                    ((and (equal br '4) (equal (car l) 'X)) t) ;uslov za izlazak t
                    ((and (equal br '0) (equal (- n el) '4)) '()) ;uslov da je u zadnja 4 i br je 0 pa ne moze nista
                    ((< el 2) (proveri_petXOsnovna (1+ el) br n (cdr l))) ;uslov za prva 2
                    ((equal (car l) 'X) (if (equal (car l) (cadr l)) (proveri_petXOsnovna (1+ el) (1+ br) n (cdr l)) (proveri_petXOsnovna (1+ el) '0 n (cdr l)))) ;glavni uslov
                    (t (proveri_petXOsnovna (1+ el) '0 n (cdr l)))))

(defun proveri_petX(n l)
  (cond((null l) '())
        (t (or (proveri_petXOsnovna '0 '0 n (car l)) (proveri_petX n (cdr l))))))

(defun proveri_petOOsnovna(el br n l)
              (cond((null l) '())
                    ((and (equal br '4) (equal (car l) 'O)) t) ;uslov za izlazak t
                    ((and (equal br '0) (equal (- n el) '6)) '()) ;uslov da je u zadnja 6 i br je 0 pa ne moze nista(4 + 2 koja su njegov pocetni red)
                    ((equal (car l) 'O) (if (equal (car l) (cadr l)) (proveri_petOOsnovna (1+ el) (1+ br) n (cdr l)) (proveri_petOOsnovna (1+ el) '0 n (cdr l)))) ;glavni uslov
                    (t (proveri_petOOsnovna (1+ el) '0 n (cdr l)))))

(defun proveri_petO(n l)
  (cond((null l) '())
        (t (or (proveri_petOOsnovna '0 '0 n (car l)) (proveri_petO n (cdr l))))))

(defun figure_cetiriO(o)
               (cond((null o) '())
                    ((> o '4) '())
                     (t t)))

(defun figure_cetiriX(x)
               (cond((null x) '())
                    ((> x '4) '())
                     (t t)))

; s1 je objekat strukture "stanje"
(defun proveriKrajX (s1)
  (or (figure_cetiriX (stanje-_x s1)) (proveri_petX _n (transpon (stanje-_matrix s1))) (proveri_petX _n (transpon (getMainDiags (stanje-_matrix s1) _n))) (proveri_petX _n (transpon (getSideDiags (stanje-_matrix s1) _n)))))

; s1 je objekat strukture "stanje"
(defun proveriKrajO (s1)
  (or (figure_cetiriO (stanje-_o s1)) (proveri_petO _n (transpon (stanje-_matrix s1))) (proveri_petO _n (transpon (getMainDiags (stanje-_matrix s1) _n))) (proveri_petO _n (transpon (getSideDiags (stanje-_matrix s1) _n)))))

; s1 je objekat strukture "stanje"
(defun proveriKraj (s1)
  (cond ((proveriKrajX s1) 'X)
        ((proveriKrajO s1) 'O)
        (t '())))