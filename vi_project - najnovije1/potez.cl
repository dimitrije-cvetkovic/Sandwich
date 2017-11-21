; s1 je objekat strukture "stanje"
(defun odigraj (x1 y1 x2 y2 x matrica s1) ;(x1 y1)- pocetna poziciju (x2 y2)- krajnja pocizicija x-trenutna vrsta matrice koja se obilazi, y- trenutna kolona koja se obilazi
	(if (< x _n)
		(if (not (or (= x1 x) (= x2 x))) ;provera da li ima promene u vrsti do koje se stiglo
			(cons (car matrica) (odigraj x1 y1 x2 y2 (1+ x) (cdr matrica) s1)); u tekucoj vrsti matrice nama promena
				  (cons  (if (= x1 x2) (if (= x2 x) (ubaci y2 0 (izbaci y1 0 (car matrica)) s1))
				   (if (= x1 x) (izbaci y1 0 (car matrica)) (if (= x2 x) (ubaci y2 0 (car matrica) s1)))) (odigraj x1 y1 x2 y2 (1+ x) (cdr matrica) s1)		
		)
	)
))

; s1 je objekat strukture "stanje"
(defun ubaci (y2 y lista s1); na poziciji y2 u listi umesto '- stavlja 'x' ili 'o' u zavisnosti od toga ko je na potezu
	(
	cond
		((= y2 y) (cons (stanje-_na_potezu s1) (cdr lista)) )
		(t (cons (car lista) (ubaci y2 (1+ y) (cdr lista) s1) ))
	)
)

(defun izbaci (y1 y lista) ;na poziciji y1 u listi umesto znaka 'x' ili 'o' ubaci '-'
	(
	cond
		((= y1 y) (cons '- (cdr lista)) )
		(t (cons (car lista) (izbaci y1 (1+ y) (cdr lista)) ))
	)
)


;funkcija u kojoj se igra potez
; s1 je objekat strukture "stanje"
(defun odigrajPotez (lista s1)  ;treba uneseni indeksi da se iz konvertuju u [0, n] opseg
	(
		if (validacija lista s1) (odigraj (1- (caar lista)) (1- (cadar lista)) (1- (caadr lista)) (1- (cadadr lista)) 0 (stanje-_matrix s1) s1) '() 
	)
)

;f-ja koja racuna nakon odigranog poteza koliko je figura preostalo svakom od igraca
; s1 je objekat strukture "stanje"
(defun vrednostiXO (s1)
	(setf (stanje-_x s1) 0)
	(setf (stanje-_o s1) 0)
	(mapcar (lambda (x) (mapcar (lambda (y) (cond ((equal y 'x) (setf (stanje-_x s1) (1+ (stanje-_x s1)))) (t (if(equal y 'o) (setf (stanje-_o s1) (1+ (stanje-_o s1))))) )) x )) (stanje-_matrix s1) )
)


