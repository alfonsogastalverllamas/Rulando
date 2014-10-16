Rulando
=======

Is a project in Lisp for planification problems
Abrir archivo con emacs. Poner Alt-X Slime y dentro Compilar el archivo.

Problema 1: (graphplan *estado-inicial1* *objetivo1* *operadores1*)
Problema 2: (graphplan *estado-inicial2* *objetivo2* *operadores2*)
Problema 3: (graphplan *estado-inicial3* *objetivo3* *operadores3*)

ESTRUCTURAS USADAS: Listas

Una capa es una Lista de Acciones, Literales, Enlaces-Acciones-Literales, Enlaces-Literales-Accion, Mutex-Literales, Mutex-Acciones
Un grafo es una lista de capas.

Una accion es una lista con una lista de precondiciones y una lista de efectos.
Un literal es una lista de Simbolo +/- y nombre del literal.

Enlaces es una lista de listas de dos elementos (accion/literal o literal/accion)
Mutex es una lista de listas de listas de dos elementos (accion/accion o literal/literal)
	En el caso de Mutex literales es una lista de dos listas (soporte inconsistente) y (efectos excluyentes)
	En el caso de Mutex acciones es una lista de tres listas (interferencias), (efectos inconsistentes) y (necesidades que compiten)
