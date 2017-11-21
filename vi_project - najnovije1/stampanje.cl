(defun printListOfLists (lol lob)
  (cond ((null (car lol)) '())
        ((< (car lob) 10) (format t " ~a ~a~%" (car lob) (car lol)))
        (t (format t "~a ~a~%" (car lob) (car lol)))
        )
  (cond ((null (cdr lol)) '())
        (t (printListOfLists (cdr lol) (cdr lob)))
        )
  )

(defun generisiNizRastucihBrojeva (n)
  (cond ((= n 0) '())
        (t (append (generisiNizRastucihBrojeva (1- n)) (list n)))
        )
  )

(defun generisiNizOpadajucihBrojeva (n)
  (cond ((= n 0) '())
        (t (cons n (generisiNizOpadajucihBrojeva (1- n))))
        )
  )

(defun printMatrixForO (mat n)
  (let ((list_of_br (generisiNizRastucihBrojeva n)))
    (format t "~%   ~a~%" list_of_br)
    (printListOfLists mat list_of_br)
    )
  )

(defun printReverseListOfReverseLists (lol lob)
  (cond ((null (car lol)) '())
        ((< (car lob) 10) (format t " ~a ~a~%" (car (last lob)) (reverse (car (last lol)))))
        (t (format t "~a ~a~%" (car (last lob)) (reverse (car (last lol)))))
        )
  (cond ((null (cdr lol)) '())
        (t (printReverseListOfReverseLists (butlast lol) (butlast lob)))
        )
  )

(defun printMatrixForX (mat n)
  (let ((list_of_br (generisiNizRastucihBrojeva n)))
    (format t "~%   ~a~%" (reverse list_of_br))
    (printReverseListOfReverseLists mat list_of_br)
    )
  )

;(defun stampajStanje ()
  ;(format t "~%-Stanje table-~%X ima: ~a figura~%O ima: ~a figura" _x _o)
  ;(cond ((equal _covek 'X) (printMatrixForX _matrix _n))
        ;(t (printMatrixForO _matrix _n))))

; STAMPANJE ZA DEBUG
; s1 je struktura objekat strukture "stanje"
; stampa i vrednost u sturkturi s1
(defun stampajStanjeStruct (s1)
  (format t "~%-Stanje table-~%X ima: ~a figura~%O ima: ~a figura" (stanje-_x s1) (stanje-_o s1))
  (format t "~%Vrednost stanja: ~a" (stanje-_vrednost s1))
  (cond ((equal _covek 'X) (printMatrixForX (stanje-_matrix s1) _n))
        (t (printMatrixForO (stanje-_matrix s1) _n))))

; STAMPANJE ZA PLAY
; s1 je struktura objekat strukture "stanje"
(defun stampajStanje (s1)
  (format t "~%-Stanje table-~%X ima: ~a figura~%O ima: ~a figura" (stanje-_x s1) (stanje-_o s1))
  ;(format t "~%Vrednost stanja: ~a" (stanje-_vrednost s1))
  (cond ((equal _covek 'X) (printMatrixForX (stanje-_matrix s1) _n))
        (t (printMatrixForO (stanje-_matrix s1) _n))))

(defun printMat1 (matrix)
  (cond ((null (car matrix)) '())
        (t (format t "~%~a" (car matrix)) (printMat1 (cdr matrix)))
        )
  )

(defun stampajStanje1 (s1)
  (format t "~%- no numbering -~%")
  (printMat1 (stanje-_matrix s1))
  )



