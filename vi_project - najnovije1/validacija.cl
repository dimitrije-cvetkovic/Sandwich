;preduslov za funkciju validacijaFiguraIzmedju je da je validan potez na horizontali ili dijagonali i da se ne stavlja na mesto sa kojeg se krece

;prihvata dve liste 
;mi uvek radimo sa indeksima od 0, samo za stampanje i unos su brojevi od 1 zbog korisnika	
;el- element u nizu
;red- vrsta/kolona u matrici za koju proveramo da li ima izmedju neko
;ind- trenutni elem u listi
(defun validacijaFiguraIzmedjuPoHorizontali(el1 el2 ind red)
              (cond((= ind el2) (if (equal (car red) '-) t '()))
                    ((<= ind el1) (validacijaFiguraIzmedjuPoHorizontali el1 el2 (1+ ind) (cdr red)))	 
                    ((equal (car red) '-) (validacijaFiguraIzmedjuPoHorizontali el1 el2 (1+ ind) (cdr red)))
                    (t (if (= el2 (+ el1 2)) (if (equal (cadr red) '-) t '()) '()))))
					
					;; ovaj deo je za igru u slucaju da nije dozvoljeno da se preskace svoja figura za jedno mesto. Inicijalno je i to dozvoljeno
					 ;(t (if (equalp _na_potezu 'X) (if (equal (car red) 'X) '() (if (= el2 (+ el1 2)) t '())) (if (equal (car red) 'O) '() (if (= el2 (+ el1 2)) t '()))))))
		 
		 
;treba da se ispita prvo da li je slobodna pozicija na koju se igra pre moje funkcije jer sam taj slucaj izostavio namerno	
; ako je el2<el1 onda je ind = n 	
;poziv n-el2 n-el1 _n (reverse red) 
; s1 je objekat strukture "stanje" (stanje-_matrix s1)
(defun validacijaFiguraIzmedju(l1 l2 s1)
	(cond((equal (car l1) (car l2)) (if (< (cadr l1) (cadr l2)) 
		                          (validacijaFiguraIzmedjuPoHorizontali (- (cadr l1) 1) (- (cadr l2) 1) '0 (nth (- (car l1) 1) (stanje-_matrix s1)))
                                          (validacijaFiguraIzmedjuPoHorizontali (- _n (cadr l1)) (- _n (cadr l2)) '0 (reverse(nth (- (car l1) 1) (stanje-_matrix s1)))) ))
		 (t (if (< (car l1) (car l2)) 
				(validacijaFiguraIzmedjuPoHorizontali (- (car l1) 1) (- (car l2) 1) '0 (nth (- (cadr l1) 1) (transpon (stanje-_matrix s1))))
				(validacijaFiguraIzmedjuPoHorizontali (- _n  (car l1)) (- _n (car l2)) '0 (reverse(nth (- (cadr l1) 1) (transpon (stanje-_matrix s1)))))) )))


;funkcija za ispitivanje da li se pokusava sa kretnjom koja nije po vrsti i koloni t-dozvoljeno '()- nije dozvoljeno
(defun validacijaVrstaKolona (lista1 lista2)
	(
	cond
		((= (- (car lista1) (car lista2)) 0) t )
		((= (- (cadr lista1) (cadr lista2)) 0) t)
		(t '())
	)
)


;funkcija koja tesitra da li se pokusava stati na vec zauzeto polje  t-polje nije zauzeto '()-polje je zauzeto
; s1 je objekat strukture "stanje"
(defun validacijaZauzetoPolje (lista s1) ;parametar lista je (x y) ciljno polje
	(
	cond
	( (equal (nth (1- (cadr lista)) (nth (1- (car lista)) (stanje-_matrix s1)) ) 'x) '() )
	( (equal (nth (1- (cadr lista)) (nth (1- (car lista)) (stanje-_matrix s1)) ) 'o) '() )
	('t 't)
	)
)

;funkcija koja proverava da li su parametri za promenu pozicije striktno u formatu lista sa dve podliste od dva elementa
(defun validacijaUnosa (lista)
	(
	cond
		((and (equal (length lista) 2) (equal (length (car lista)) 2) (equal (length (cadr lista)) 2) (validacijaBrojeva lista) ) t)
		(t '())
	)
)
 
;funkcija koja pri zadavanju pocetnog i krajnjeg polja u potezu proverava da li su zadati indexi u opsegu od [1, n]
(defun validacijaBrojeva (lista)
	(
	cond 
		((or (<= (caar lista) 0) (> (caar lista) _n)) '())
		((or (<= (caadr lista) 0) (> (caadr lista) _n)) '())
		((or (<= (cadar lista) 0) (> (cadar lista) _n)) '())
		((or (<= (cadadr lista) 0) (> (cadadr lista) _n)) '())
		('t 't)
	)
)

;(setq _matrix '( (o x o o) (- - o x) (- - o o) (- x - o) ))


; s1 je objekat strukture "stanje" (stanje-_na_potezu s1) setf
(defun proveriSvojinuPolja (list s1) ; provera da li se na unetom pocetnom polju nalazi moja figura (X ili O)
  (if (equal (nth (1- (cadr list)) (nth (1- (car list)) (stanje-_matrix s1))) (stanje-_na_potezu s1)) T '()))

;funkcija koja vrsi kompletnu validaciju poteza
; s1 je objekat strukture "stanje"
(defun validacija (lista s1)
	(and 
		(validacijaUnosa lista)
		(validacijaVrstaKolona (car lista) (cadr lista))
                (validacijaZauzetoPolje (cadr lista) s1)
                (proveriSvojinuPolja (car lista) s1)
		(validacijaFiguraIzmedju (car lista) (cadr lista) s1)
	)
)
