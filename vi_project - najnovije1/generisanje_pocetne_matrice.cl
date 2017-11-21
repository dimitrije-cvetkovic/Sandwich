(defstruct stanje _o _x  _na_potezu _kraj _matrix _vrednost)

(defun generisiListu (simbol n)
  (cond ((= n 0) '())
        (t (cons simbol (generisiListu simbol (- n 1))))
        )
  )

(defun generisiMatricuOriginal (k n_const)
  (cond ((= k 0) '())
        ((> k (- n_const 2)) (cons (generisiListu 'X n_const) (generisiMatricuOriginal (1- k) n_const)))
        ((< k 3) (cons (generisiListu 'O n_const) (generisiMatricuOriginal (1- k) n_const)))
        (t (cons (generisiListu '- n_const) (generisiMatricuOriginal (1- k) n_const)))
        )
  )

(defun generisiMatricu ()
  (generisiMatricuOriginal _n _n)
  )