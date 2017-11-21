;;kvazi main
(progn 
  (format t "~% - APLIKACIJA POKRENUTA - ~%")
  (format t "U svrsi testiranja se igra prikazuje kao da igraju dva coveka, prvo jedan pa drugi~%")
  (format t "~%Unesite dimenziju tabele~%(broj; ako se unese manji od 9, veci od 20 ili ne-broj; 9 je po default-u)~%")
  (defparameter _n (read))
  
  (cond ((equal (type-of _n) 'fixnum) (if (< _n 9) (setf _n '9)) (if (> _n 20) (setf _n '9)))
        (t (setf _n '9))
        )       
  
  (defparameter _s (make-stanje))
  (setf (stanje-_o _s) (* 2 _n))
  (setf (stanje-_x _s) (* 2 _n))
  (setf (stanje-_matrix _s) (generisiMatricu))
  (setf (stanje-_na_potezu _s) 'X)
  (setf (stanje-_kraj _s) '())
  (setf (stanje-_vrednost _s) '())
  
  (format t "~%Da li zelite da igrate prvi? (da/ne)~%(default NE)~%")
  (defparameter _covek (read))
  (cond ((equal _covek 'da) (setf _covek 'X))
        (t (setf _covek 'O)))
  
  ;;za izbaciti - ovo je samo za testiranje
  (setf (stanje-_na_potezu _s) _covek)
  ;;
  
  (stampajStanje _s)
   
  (defparameter _potez '())
  (defparameter _validnost_poteza '())
  
  (format t "~%IGRA: ~a~%Unesite potez koji zelite odigrati~%(format ((i1 j1) (i2 j2)); EXIT za izlaz)~%" (stanje-_na_potezu _s))
  (setf _potez (read))
  
  ; (stanje-_kraj _s)
  (loop (progn
          (if (equal _potez 'exit) (return))
          (setf _validnost_poteza (odigrajPotez _potez _s))
          (if (not (null _validnost_poteza)) (setf (stanje-_matrix _s) _validnost_poteza))
                    
          (setf (stanje-_matrix _s) (eliminisiSendviceIzMatriceX (1- (caadr _potez)) (1- (cadadr _potez)) (stanje-_matrix _s)))
          (setf (stanje-_matrix _s) (eliminisiSendviceIzMatriceO (1- (caadr _potez)) (1- (cadadr _potez)) (stanje-_matrix _s)))
          
          (vrednostiXO _s)
          
          (setf (stanje-_kraj _s) (proveriKraj _s))
          (if (not (null (stanje-_kraj _s))) (progn (stampajStanje _s) (format t "~%~% - IGRA ZAVRSENA. POBEDNIK JE ~a - ~%~%" (stanje-_kraj _s)) (return)))
          
          (cond ((null (car _validnost_poteza)) (format t "~%-- GRESKA U UNETOM POTEZU. IGRAJTE PONOVO --"))
                (t 
                 ;;za izbaciti - ovo je samo za testiranje
                 (if (equal (stanje-_na_potezu _s) 'O) (setf (stanje-_na_potezu _s) 'X) (setf (stanje-_na_potezu _s) 'O))
                 (setf _covek (stanje-_na_potezu _s))
                 ;;
                 ))
          
          (stampajStanje _s)          
          (format t "~%IGRA: ~a~%Unesite potez koji zelite odigrati~%(format ((i1 j1) (i2 j2)); EXIT za izlaz)~%" (stanje-_na_potezu _s))
          (setf _potez (read))
          
          )
        )
   (format t "~% - APLIKACIJA ZATVORENA - ~%")

)