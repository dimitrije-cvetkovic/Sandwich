 (defun svaMogucaStanja (s1 i j) 
                        (if (>= i _n)
                          '()
                           (if (>= j _n)
                              (svaMogucaStanja s1 (+ 1 i) 0) 
                              (if (equal (nth j (nth i (stanje-_matrix s1))) (stanje-_na_potezu s1) )  
											(append (generisiStanja s1 i j) (svaMogucaStanja s1 i (+ 1 j))) 
											(svaMogucaStanja s1 i (+ 1 j))
							  )
                          ) 
                        )
                    )
					
					
					
(defun generisiStanja (s1 i j)
                       (let ((listaPozicija (mogucePozicije s1 i j) ))
                            (stanjaZaListu i j listaPozicija s1) 
                       )
                  )
				  
(defun stanjaZaListu (i j listaPozicija s1)
		(cond 
			((null listaPozicija) '())		
			(t (cons (let* ((str (make-stanje))) (napraviStanje i j listaPozicija s1 str) ) (stanjaZaListu i j (cdr listaPozicija) s1) ))			
		)
)


(defun napraviStanje (i j listaPozicija s1 str)
	(progn 		
		(setf (stanje-_matrix str) (odigraj i j (caar listaPozicija) (cadar listaPozicija) 0 (stanje-_matrix s1) s1))
		(setf (stanje-_matrix str) (eliminisiSendviceIzMatriceX (caar listaPozicija) (cadar listaPozicija) (stanje-_matrix str)))
		(setf (stanje-_matrix str) (eliminisiSendviceIzMatriceO (caar listaPozicija) (cadar listaPozicija) (stanje-_matrix str)))
		(setf  (stanje-_kraj str)(proveriKraj str))
		(vrednostiXO str)
		(setf (stanje-_na_potezu str) (if (equal (stanje-_na_potezu s1) 'x) 'o 'x))
		str 
	)
)

;i i j od 0 do n-1				  
(defun mogucePozicije(s1 i j)
	(append (proveriPozicijeKolona s1 i j '0) (proveriPozicijeVrsta s1 i j '0)))

;brojac inicijalno 0
(defun proveriPozicijeVrsta(s1 i j brojac)
		(cond((>= brojac _n) '())
			 ((= j brojac) (proveriPozicijeVrsta s1 i j (1+ brojac)))
			 (t (let ((p (proveriPoziciju s1 (list i j) (list i brojac)))) (if (not (null p)) 
																				(cons p (proveriPozicijeVrsta s1 i j (1+ brojac))) 
																				(proveriPozicijeVrsta s1 i j (1+ brojac)) 
																			)
																			))))
			 
(defun proveriPozicijeKolona(s1 i j brojac)
		(cond((>= brojac _n) '())
			 ((= i brojac) (proveriPozicijeKolona s1 i j (1+ brojac)))
			 (t (let ((p (proveriPoziciju s1 (list i j) (list brojac j)))) (if (not (null p))  
																				(cons p (proveriPozicijeKolona s1 i j (1+ brojac))) 
																				(proveriPozicijeKolona s1 i j (1+ brojac)) 
																			)
																			))))
			 
;p1 i p2 su one koordinate koje korisnik unosi(jer u odigrajPotez se umanjuje za po 1 indeks)
(defun ProveriPoziciju(s1 p1 p2)
	(if (odigrajPotez (list (list (1+ (car p1)) (1+ (cadr p1))) (list (1+ (car p2)) (1+ (cadr p2))) ) s1) p2 '()))
