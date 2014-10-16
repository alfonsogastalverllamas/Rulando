;;;;;;;;;;;;;;;;;;;; GRAFOS DE PLANIFICACION ;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;      EJEMPLOS PDDL      ;;;;;;;;;;;;;;;;;;;;

;; EJEMPLO 1: El Problema de Mike.

 ;;Acciones
(defvar *cargar-arma* (list (list (list "-" "arma-cargada") (list "+" "tener-balas")) (list (list "+" "arma-cargada") (list "-" "tener-balas")) "cargar-arma"))
(defvar *matar-zombie* (list (list (list "-" "zombie-muerto") (list "+" "arma-cargada")) (list (list "-" "arma-cargada") (list "+" "zombie-muerto")) "matar-zombie"))

 ;;Persistencias
(defvar *arma-cargada* (list (list (list "+" "arma-cargada")) (list (list "+" "arma-cargada")) "persiste-arma-cargada"))
(defvar *arma-no-cargada* (list (list (list "-" "arma-cargada")) (list (list "-" "arma-cargada")) "persiste-arma-no-cargada"))
(defvar *tener-balas* (list (list (list "+" "tener-balas")) (list (list "+" "tener-balas")) "persiste-tener-balas"))
(defvar *no-tener-balas* (list (list (list "-" "tener-balas")) (list (list "-" "tener-balas")) "persiste-no-tener-balas"))
(defvar *zombie-muerto* (list (list (list "+" "zombie-muerto")) (list (list "+" "zombie-muerto")) "persiste-zombie-muerto"))
(defvar *zombie-no-muerto* (list (list (list "-" "zombie-muerto")) (list (list "-" "zombie-muerto")) "persiste-zombie-no-muerto"))

 ;;Variables
(defvar *estado-inicial1* (list (list "-" "arma-cargada") (list "+" "tener-balas") (list "-" "zombie-muerto")))
(defvar *objetivo1* (list (list "+" "zombie-muerto")))
(defvar *operadores1* (list *cargar-arma* *matar-zombie* *arma-cargada* *arma-no-cargada* *tener-balas* *no-tener-balas* *zombie-muerto* *zombie-no-muerto*))

;; EJEMPLO 2: PROBLEMA DEL SALCHICHON

 ;;Acciones
(defvar *alimentar-cerdo* (list (list (list "+" "tener-cerdo") (list "-" "cerdo-alimentado") (list "-" "cerdo-muerto") ) (list (list "+" "cerdo-alimentado")) "alimentar-cerdo"))
(defvar *matar-cerdo* (list (list (list "+" "tener-cerdo") (list "+" "cerdo-alimentado") (list "-" "cerdo-muerto") ) (list (list "+" "cerdo-muerto") (list "-" "tener-cerdo") (list "-" "cerdo-alimentado")) "matar-cerdo"))
(defvar *fabricar-salchichon* (list (list (list "+" "cerdo-muerto")) (list (list "-" "cerdo-muerto") (list "+" "salchichon")) "fabricar-salchichon"))


 ;;Persistencias
(defvar *tener-cerdo* (list (list (list "+" "tener-cerdo")) (list (list "+" "tener-cerdo")) "persiste-tener-cerdo"))
(defvar *no-tener-cerdo* (list (list (list "-" "tener-cerdo")) (list (list "-" "tener-cerdo")) "persiste-no-tener-cerdo"))
(defvar *cerdo-alimentado* (list (list (list "+" "cerdo-alimentado")) (list (list "+" "cerdo-alimentado")) "persiste-cerdo-alimentado"))
(defvar *cerdo-no-alimentado* (list (list (list "-" "cerdo-alimentado")) (list (list "-" "cerdo-alimentado")) "persiste-cerdo-no-alimentado"))
(defvar *cerdo-muerto* (list (list (list "+" "cerdo-muerto")) (list (list "+" "cerdo-muerto")) "persiste-cerdo-muerto"))
(defvar *cerdo-no-muerto* (list (list (list "-" "cerdo-muerto")) (list (list "-" "cerdo-muerto")) "persiste-cerdo-no-muerto"))
(defvar *salchichon* (list (list (list "+" "salchichon")) (list (list "+" "salchichon")) "persiste-salchichon"))
(defvar *no-salchichon* (list (list (list "-" "salchichon")) (list (list "-" "salchichon")) "persiste-no-salchichon"))

 ;;Variables
(defvar *estado-inicial2* (list (list "+" "tener-cerdo") (list "-" "cerdo-muerto") (list "-" "salchichon") (list "-" "cerdo-alimentado")))
(defvar *objetivo2* (list (list "+" "salchichon")))
(defvar *operadores2* (list *alimentar-cerdo* *matar-cerdo* *fabricar-salchichon* *tener-cerdo* *no-tener-cerdo* *cerdo-alimentado* *cerdo-no-alimentado* *cerdo-muerto* *cerdo-no-muerto* *salchichon* *no-salchichon*))

;; EJEMPLO 3: El Problema de los ladrones.

  ;; Acciones
(defvar *ir-coche* (list (list (list "-" "estan-en-coche") (list "-" "estan-en-caja-fuerte")) (list (list "+" "estan-en-coche") (list "-" "estan-en-ventana") (list "-" "estan-en-alarma")) "ir-coche"))
(defvar *ir-ventana* (list (list (list "-" "estan-en-ventana")) (list (list "+" "estan-en-ventana") (list "-" "estan-en-coche") (list "-" "estan-en-alarma") (list "-" "estan-en-caja-fuerte")) "ir-ventana"))
(defvar *ir-alarma* (list (list (list "-" "estan-en-alarma") (list "-" "estan-en-caja-fuerte")) (list (list "+" "estan-en-alarma") (list "-" "estan-en-coche") (list "-" "estan-en-ventana")) "ir-alarma"))
(defvar *ir-caja-fuerte* (list (list (list "+" "ventana-abierta") (list "-" "estan-en-caja-fuerte") (list "-" "estan-en-coche") (list "-" "estan-en-alarma")) (list (list "+" "estan-en-caja-fuerte") (list "-" "estan-en-ventana")) "ir-caja-fuerte"))

(defvar *desactivar-alarma* (list (list (list "-" "alarma-desactivada") (list "+" "estan-en-alarma")) (list (list "+" "alarma-desactivada")) "desactivar-alarma"))
(defvar *abrir-ventana* (list (list (list "-" "ventana-abierta") (list "+" "alarma-desactivada") (list "+" "estan-en-ventana")) (list (list "+" "ventana-abierta")) "abrir-ventana"))
(defvar *abrir-caja-fuerte* (list (list (list "-" "caja-fuerte-abierta") (list "+" "estan-en-caja-fuerte")) (list (list "+" "caja-fuerte-abierta")) "abrir-caja-fuerte"))
(defvar *robar-dinero* (list (list (list "-" "dinero-robado") (list "+" "estan-en-caja-fuerte") (list "+" "caja-fuerte-abierta")) (list (list "+" "dinero-robado")) "robar-dinero"))
  
  ;; Persistencias
(defvar *estan-en-coche* (list (list (list "+" "estan-en-coche")) (list (list "+" "estan-en-coche")) "persiste-coche"))
(defvar *no-estan-en-coche* (list (list (list "-" "estan-en-coche")) (list (list "-" "estan-en-coche")) "persiste-no-coche"))
(defvar *estan-en-alarma* (list (list (list "+" "estan-en-alarma")) (list (list "+" "estan-en-alarma")) "persiste-alarma"))
(defvar *no-estan-en-alarma* (list (list (list "-" "estan-en-alarma")) (list (list "-" "estan-en-alarma")) "persiste-no-alarma"))
(defvar *estan-en-ventana* (list (list (list "+" "estan-en-ventana")) (list (list "+" "estan-en-ventana")) "persiste-ventana"))
(defvar *no-estan-en-ventana* (list (list (list "-" "estan-en-ventana")) (list (list "-" "estan-en-ventana")) "persiste-no-ventana"))
(defvar *estan-en-caja-fuerte* (list (list (list "+" "estan-en-caja-fuerte")) (list (list "+" "estan-en-caja-fuerte")) "persiste-caja-fuerte"))
(defvar *no-estan-en-caja-fuerte* (list (list (list "-" "estan-en-caja-fuerte")) (list (list "-" "estan-en-caja-fuerte")) "persiste-no-caja-fuerte"))
 
 
(defvar *alarma-desactivada* (list (list (list "+" "alarma-desactivada")) (list (list "+" "alarma-desactivada")) "persiste-alarma-desactivada"))
(defvar *alarma-no-desactivada* (list (list (list "-" "alarma-desactivada")) (list (list "-" "alarma-desactivada")) "persiste-alarma-no-desactivada"))
(defvar *ventana-abierta* (list (list (list "+" "ventana-abierta")) (list (list "+" "ventana-abierta")) "persiste-ventana-abierta"))
(defvar *ventana-no-abierta* (list (list (list "-" "ventana-abierta")) (list (list "-" "ventana-abierta")) "persiste-ventana-no-abierta"))
(defvar *caja-fuerte-abierta* (list (list (list "+" "caja-fuerte-abierta")) (list (list "+" "caja-fuerte-abierta")) "persiste-caja-fuerte-abierta"))
(defvar *caja-fuerte-no-abierta* (list (list (list "-" "caja-fuerte-abierta")) (list (list "-" "caja-fuerte-abierta")) "persiste-caja-fuerte-no-abierta"))
(defvar *dinero-robado* (list (list (list "+" "dinero-robado")) (list (list "+" "dinero-robado")) "persiste-dinero-robado"))
(defvar *dinero-no-robado* (list (list (list "-" "dinero-robado")) (list (list "-" "dinero-robado")) "persiste-dinero-no-robado"))
  
  ;; Variables
(defvar *estado-inicial3* (list (list "+" "estan-en-coche") (list "-" "estan-en-alarma") (list "-" "estan-en-ventana") (list "-" "estan-en-caja-fuerte") (list "-" "alarma-desactivada") (list "-" "ventana-abierta")  (list "-" "caja-fuerte-abierta") (list "-" "dinero-robado")))
(defvar *objetivo3* (list (list "+" "estan-en-coche") (list "+" "dinero-robado")))
(defvar *operadores3* (list *ir-coche* *ir-ventana* *ir-alarma* *ir-caja-fuerte* *desactivar-alarma* *abrir-ventana* *abrir-caja-fuerte* *robar-dinero* *estan-en-coche* *no-estan-en-coche* *estan-en-alarma* *no-estan-en-alarma* *estan-en-ventana* *no-estan-en-ventana* *estan-en-caja-fuerte* *no-estan-en-caja-fuerte* *alarma-desactivada* *alarma-no-desactivada* *ventana-abierta* *ventana-no-abierta* *caja-fuerte-abierta* *caja-fuerte-no-abierta* *dinero-robado* *dinero-no-robado*))
	
;;;;;;;;;;;;;;;;;;;;   FUNCIONES GENERICAS   ;;;;;;;;;;;;;;;;;;;;

(defun contiene-todo-en (lista1 lista2)
	(let* ((resultado T))
		(loop for e1 in lista1 when (equal (member e1 lista2 :test #'equal) NIL) do (setf resultado NIL))
		resultado))

(defun literal-contrario (literal)
  (list (if (equal (first literal) "+") "-" "+") (second literal)))

(defun contiene-mutex-en (elem1 elem2 lista)
  (or (member (list elem1 elem2) lista :test #'equal) (member (list elem2 elem1) lista :test #'equal)))
		
;;;;;;;;;;;;;;;;;;;;   FUNCIONES AUXILIARES  ;;;;;;;;;;;;;;;;;;;;

;; Comprobar Objetivo es una funcion que comprueba si los literales de la capa actual cumplen el objetivo y no tienen enlaces mutex entre ellos.
(defun comprobar-objetivo (literales objetivo mutex)
	(equal (length (loop for ob in objetivo when (and (not (literal-en-mutex ob objetivo mutex)) (member ob literales :test #'equal)) collect ob)) (length objetivo)))

;; Funcion basica para Comprobar-Objetivo.
(defun literal-en-mutex (literal objetivo mutex)
  (let* ((resultado NIL))
    (loop for ob in objetivo do
	 (loop for mut in (first mutex) when (contiene-mutex-en literal ob mut) do
	      (setf resultado T))
	 (loop for mut in (second mutex) when (contiene-mutex-en literal ob mut) do
	      (setf resultado T)))
    resultado))

;; Extrae-Plan devolvera el plan que deberemos tomar para obtener el objetivo desde el estado inicial. 		
(defun extrae-plan (grafo-planificacion objetivo)
  (elimina-persistencias (extrae-solucion objetivo (- (length grafo-planificacion) 1) grafo-planificacion ())))

(defun elimina-persistencias (acciones)
  (loop for a in acciones when (not (string= (nth 2 a) "persiste-" :end1 9)) collect a))

(defun extrae-solucion (objetivo i grafo-planificacion solucion)
  (let* ()
    (if (equal i 0)
	solucion
	(let* ((subconjunto-acciones (selecciona-acciones (nth i grafo-planificacion) objetivo 0 ()))
	       (objetivo-siguiente (extrae-precondiciones subconjunto-acciones))
	       (solucion-siguiente (append subconjunto-acciones solucion)))
	  (write-line (concatenate 'string "Extrayendo solucion nivel " (write-to-string i) ": " (write-to-string (loop for a in solucion-siguiente collect (nth 2 a)))))
	  (extrae-solucion objetivo-siguiente (- i 1) grafo-planificacion solucion-siguiente)))))

(defun selecciona-acciones (capa objetivo i acciones)
  (let* ()
    (if (equal i (length objetivo))
	acciones
	(let* ((acciones-posibles (selecciona-acciones-posibles (nth i objetivo) (nth 0 capa)))
	       (res ()))
	  (loop for ap in acciones-posibles when (equal res ()) do
	       (let* ((acciones-siguiente (unir-si-posible acciones ap (nth 4 capa))))
		 (if (not (equal (length acciones-siguiente) (length acciones)))
		     (setf res (selecciona-acciones capa objetivo (+ i 1) acciones-siguiente)))))
	  res))))

(defun selecciona-acciones-posibles (literal acciones)
  (loop for a in acciones when (member literal (second a) :test #'equal) collect a))

(defun unir-si-posible (acciones accion mutex)
  (let* ((hay-mutex NIL))
    (loop for a in acciones when (or (contiene-mutex-en a accion (first mutex)) (contiene-mutex-en a accion (second mutex)) (contiene-mutex-en a accion (nth 2 mutex))) do
	 (setf hay-mutex T))
    (if hay-mutex
	acciones
	(append acciones (list accion)))))

(defun extrae-precondiciones (acciones)
  (let* ((literales ()))
    (loop for a in acciones do
	 (loop for pre in (first a) when (equal (member pre literales :test #'equal) NIL) do
	      (setf literales (append literales (list pre)))))
    literales))


;; Crea-A crea las acciones de la capa siguiente. 
(defun crea-a (operadores literales mutex)
	(loop for op in operadores when (cumple-condicion-accion op literales mutex)
		collect op))

;; Funciones basicas para crea-a.
(defun cumple-condicion-accion (op literales mutex)
	(and (precondiciones-en-literales (first op) literales) (precondiciones-sin-mutex (first op) mutex)))

(defun precondiciones-en-literales (precondiciones literales)
	(contiene-todo-en precondiciones literales))

(defun precondiciones-sin-mutex (precondiciones mutex)
	(let* ((resultado T))
		(loop for mu in mutex when (contiene-todo-en mu precondiciones) do (setf resultado NIL))
		resultado))

;; Crea-L crea los literales de la capa siguiente.
(defun crea-l (acciones)
	(let* ((resultado ()))
		(loop for acc in acciones do (setf resultado (anadir-literales (second acc) resultado)))
		resultado))

;; Funciones basicas para crea-l.
(defun anadir-literales (literales lista)
	(let* ((resultado lista))
		(loop for lit in literales when (equal (member lit resultado :test #'equal) NIL) do (setf resultado (append resultado (list lit))))
		resultado))
	
;; Crea-LA crea los enlaces de literales-acciones para poder hacer la busqueda despues.
(defun crea-la (acciones)
	(let* ((resultado ()))
		(loop for acc in acciones do 
			(loop for pre in (first acc) do (setf resultado (append resultado (list (list pre acc))))))
		resultado))
		
;; Crea-AL crea los enlaces de acciones-literales para poder hacer la busqueda despues.
(defun crea-al (acciones)
	(let* ((resultado ()))
		(loop for acc in acciones do 
			(loop for pos in (second acc) do (setf resultado (append resultado (list (list acc pos))))))
		resultado))

;; Crea-MA crea los enlaces de exclusion mutua entre acciones.
(defun crea-ma (acciones mutexliterales)
  (append (list (mutex-interferencia acciones)) (list (mutex-inconsistencia acciones)) (list (mutex-precondiciones acciones mutexliterales))))

;; Funciones basicas para crea-ma.
(defun mutex-interferencia (acciones)
  (let* ((resultado ()))
    (loop for acc in acciones do
	 (loop for acc2 in acciones when (and (not (equal acc acc2)) (not (contiene-mutex-en acc acc2 resultado)) (existe-mutex-interferencia acc acc2)) do
	      (setf resultado (append resultado (list (list acc acc2))))))
    resultado))

(defun mutex-inconsistencia (acciones)
  (let* ((resultado ()))
    (loop for acc in acciones do
	 (loop for acc2 in acciones when (and (not (equal acc acc2)) (not (contiene-mutex-en acc acc2 resultado)) (existe-mutex-inconsistencia acc acc2)) do
	      (setf resultado (append resultado (list (list acc acc2))))))
    resultado))

(defun mutex-precondiciones (acciones mutexliterales)
  (let* ((resultado ()))
    (loop for acc in acciones do
	 (loop for acc2 in acciones when (and (not (equal acc acc2)) (not (contiene-mutex-en acc acc2 resultado)) (existe-mutex-precondiciones acc acc2 mutexliterales)) do
	      (setf resultado (append resultado (list (list acc acc2))))))
    resultado))

(defun existe-mutex-interferencia (acc acc2)
  (let* ((resultado NIL))
    (loop for efecto in (second acc) when (member (literal-contrario efecto) (first acc2) :test #'equal) do
	 (setf resultado T))
    resultado))

(defun existe-mutex-inconsistencia (acc acc2)
  (let* ((resultado NIL))
    (loop for efecto in (second acc) when (member (literal-contrario efecto) (second acc2) :test #'equal) do
	 (setf resultado T))
    resultado))

(defun existe-mutex-precondiciones (acc acc2 mutexliterales)
  (let* ((resultado NIL))
    (loop for precondicion in (first acc) do
	 (loop for precondicion2 in (first acc2) when (contiene-mutex-en precondicion precondicion2 mutexliterales) do
	      (setf resultado T)))
    resultado))

;; Crea-ML crea los enlaces de exclusion mutua entre literales.
(defun crea-ml (mutex literales)
  (append (list (mutex-lit-inconsistentes literales)) (list (mutex-lit-efectos-excluyentes mutex literales))))

;; Funciones basicas para crea-ml.
(defun mutex-lit-inconsistentes (literales)
  (let* ((resultado ()))
    (loop for lit in literales do
	 (loop for lit2 in literales when (and (not (equal lit lit2)) (not (contiene-mutex-en lit lit2 resultado)) (equal (literal-contrario lit) lit2)) do
	      (setf resultado (append resultado (list (list lit lit2))))))
    resultado))

(defun mutex-lit-efectos-excluyentes (mutex literales)
  (let* ((resultado ()))
    (loop for lit in literales do
	 (loop for lit2 in literales when (and (not (equal lit lit2)) (not (contiene-mutex-en lit lit2 resultado)) (existe-mutex-lit-efectos-excluyentes lit lit2 mutex)) do
	      (setf resultado (append resultado (list (list lit lit2))))))
    resultado))

(defun existe-mutex-lit-efectos-excluyentes (lit lit2 mutex) 
  (let* ((resultado NIL))
    (loop for mut in (first mutex) when (or (and (member lit (second (first mut)) :test #'equal) (member lit2 (second (second mut)) :test #'equal)) (and (member lit (second (second mut)) :test #'equal) (member lit2 (second (first mut)) :test #'equal))) do
	 (setf resultado T))
    (loop for mut in (second mutex) when (or (and (member lit (second (first mut)) :test #'equal) (member lit2 (second (second mut)) :test #'equal)) (and (member lit (second (second mut)) :test #'equal) (member lit2 (second (first mut)) :test #'equal))) do
	 (setf resultado T))
    (loop for mut in (nth 2 mutex) when (or (and (member lit (second (first mut)) :test #'equal) (member lit2 (second (second mut)) :test #'equal)) (and (member lit (second (second mut)) :test #'equal) (member lit2 (second (first mut)) :test #'equal))) do
	 (setf resultado T))
    resultado))

;;;;;;;;;;;;;;;;;;;;   ALGORITMO GRAPHPLAN   ;;;;;;;;;;;;;;;;;;;;

(defun graphplan (estado-inicial objetivo operadores)
  (let* ((acciones ())
	 (literales estado-inicial)
	 (enlaces-la ())
	 (enlaces-al ())
	 (mutex-a ())
	 (mutex-l ())
	 (grafo-planificacion (list (list acciones literales enlaces-la enlaces-al mutex-a mutex-l)))
	 (capa-actual 0)
	 (a-previa ())
	 (l-previa ())
	 (la-previa ())
	 (al-previa ())
	 (ma-previa (list () () ()))
	 (ml-previa (list () ()))
	 (res 0))
    (write-line "Comenzando algoritmo...")
    (loop while (and (equal res 0) (or (not (equal (nth 0 (nth capa-actual grafo-planificacion)) a-previa)) (not (equal (nth 1 (nth capa-actual grafo-planificacion)) l-previa))))
       do (let* ()
	    (write-line (concatenate 'string "Ejecutando capa " (write-to-string capa-actual)))
	    (if (comprobar-objetivo (nth 1 (nth capa-actual grafo-planificacion)) objetivo ml-previa) 
		(let* ()
		  (write-line "Objetivo encontrado. Ejecutando búsqueda de plan...")
		  (setf res (extrae-plan grafo-planificacion objetivo))))
	    (if (equal res 0)
		(let* ()
		  (setf a-previa (nth 0 (nth capa-actual grafo-planificacion)))
		  (setf l-previa (nth 1 (nth capa-actual grafo-planificacion))) 
		  (setf la-previa (nth 2 (nth capa-actual grafo-planificacion)))
		  (setf al-previa (nth 3 (nth capa-actual grafo-planificacion)))
		  (setf ma-previa (nth 4 (nth capa-actual grafo-planificacion)))
		  (setf ml-previa (nth 5 (nth capa-actual grafo-planificacion)))
		  (setf capa-actual (+ capa-actual 1))
		  (let* ((a-actual (crea-a operadores l-previa ml-previa))
			 (l-actual (crea-l a-actual))
			 (la-actual (crea-la a-actual))
			 (al-actual (crea-al a-actual))
			 (ma-actual (crea-ma a-actual ml-previa))
			 (ml-actual (crea-ml ma-actual l-actual)))
		    (setf grafo-planificacion (append grafo-planificacion (list (list a-actual l-actual la-actual al-actual ma-actual ml-actual)))))))))
    (write-line "Fin de algoritmo.")
    (loop for a in res collect (nth 2 a))))


