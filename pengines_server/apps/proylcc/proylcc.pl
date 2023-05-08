:- module(proylcc, 
	[  
		join/4,
		booster/3
	]).

/**
 * join(+Grid, +NumOfColumns, +Paxth, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Paxth
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 
join(Grid, _NumOfColumns, _Paxth, RGrids):-
	sumarPath(Grid, _Paxth, _NumOfColumns, Suma),
    menorPotenciaDe2(Suma, Potencia),
	eliminarLista(Grid,_Paxth,_NumOfColumns,Potencia,Resultante),
	eliminarUltimo(_Paxth,PathAux),
	ordenarPorX(_Paxth,PathAux2),
	gravedad(Resultante,PathAux2,_NumOfColumns,Res),
	reemplazarCeros(Res,Res2),
	RGrids = [Resultante,Res,Res2].
		% formula para transformar una coordenada en indice--> (X * NumOfColumns) + (Y mod NumOfColumns)

	% calcula la suma de los números  que forman parte de un  path
	sumarPath(Grid, [],_, 0).
	sumarPath(Grid, [[X,Y]|Resto],NumOfColumns, Suma) :-
		Posicion is X * NumOfColumns + Y,
		nth0(Posicion, Grid, Numero),% este predicado es predefinido por prolog y encuentra en una lista una posicion
		sumarPath(Grid, Resto, NumOfColumns, SumaResto),
		Suma is SumaResto + Numero.

	% encuentra la menor potencia de 2 mayor o igual que N
	menorPotenciaDe2(N, Potencia) :-
		Potencia is 2 ** (ceil(log(N)/log(2))).


	eliminarLista(Grid, [], _NumOfColumns,_Potencia, Grid):-  % Caso base, lista vacía
		Grid = Res. 

	/** caso recursivo 1.
	*   si es el ultimo elemento de la cola, llamamos al predicado que reemplaza
	*	ese ultimo elemento por la potencia correspondiente.
	*/
	eliminarLista(Grid, [Coordenada|Cola], _NumOfColumns,_Potencia, Res):- 
		length(Cola, Resultado),
		Resultado =:= 0,
		coordXnYAux(Grid, Coordenada, _NumOfColumns,_Potencia, ResAux),
		Res = ResAux.
	
	/** caso recursivo 2.
	*   si NO es el ultimo elemento de la cola, llamamos al predicado que reemplaza
	*	el elemento en cuestion por un 0.
	*/
	eliminarLista(Grid, [Coordenada|Cola], NumOfColumns,Potencia, Resultante) :-
		coordXnY(Grid, Coordenada, NumOfColumns, Res),
		eliminarLista(Res, Cola, NumOfColumns, Potencia, Resultante).


	 % Caso base, lista vacía
	coordXnY(Grid,[],_,_).

	/** caso recursivo.
	*   se separa el par de coordenadas y se llama al predicado eliminar para reemplazar
	*	la posicion correspondiente en la grid por 0.
	*/
	coordXnY(Grid,[X|Y],NumOfColumns,Res):- % Substrae los componentes X e Y de la coordenada --> [1,2] en X=1 y Y=2
		X1 is X,
		predicadoY(Y,Resauxiliar),
		Y1 is Resauxiliar,
		eliminar(Grid,(X1 * NumOfColumns) + (Y1 mod NumOfColumns),0,ResAux),
		Res = ResAux.

	/*
	* 	se separa el par de coordenadas y se llama al predicado eliminarAux/5 para reemplazar
	*	el contenido de la posicion correspondiente de la grid por la potencia resultante de cada caso.
	*/
	coordXnYAux(Grid,[X|Y],NumOfColumns,Potencia, Res):-
		X1 is X,
		predicadoY(Y,Resauxiliar),
		Y1 is Resauxiliar,
		eliminarAux(Grid,(X1 * NumOfColumns) + (Y1 mod NumOfColumns),0,Potencia,ResAux),
		Res = ResAux.	

	% se ingresa en caso de que el contador llegue a la posicion que buscamos.se reemplaza el contenido en esa posicion por 0	
	eliminar([Cabeza|Cola],Posicion,Contador,Res):- 
		Contador =:= Posicion,
		NuevaCabeza is 0, 
		Res = [NuevaCabeza|Cola].

 	%itera sobre la grid mientras no haya llegado a la posicion a eliminar
	eliminar([Cabeza|Cola],Posicion,Contador,Res):- 
    	NuevoContador is Contador + 1,
		eliminar(Cola,Posicion,NuevoContador,ResAux),
		Res = [Cabeza|ResAux].

	% % se ingresa en caso de que el contador llegue a la posicion que buscamos.se reemplaza el contenido en esa posicion por la potencia resultantedel camino
	eliminarAux([Cabeza|Cola],Posicion,Contador,Potencia,Res):- 
		Contador =:= Posicion,
		NuevaCabeza is Potencia,
		Res = [NuevaCabeza|Cola].

	%itera sobre la grid mientras no haya llegado a la posicion a eliminar	
	eliminarAux([Cabeza|Cola],Posicion,Contador,_Potencia, Res):-
    	NuevoContador is Contador + 1,
		eliminarAux(Cola,Posicion,NuevoContador,_Potencia, _ResAux),
		Res = [Cabeza|_ResAux].

	predicadoY(Y,Resauxiliar):-
		Resauxiliar is Y.

	% caso base. cuando el path este vacío se devuelve la Grid pasada como parametro.
	gravedad(Grid,[],_NumOfColumns, Grid).

	/*	caso recursivo.
	*	si el path NO esta vacío descompone la cabeza de la lista de entrada en las variables X e Y
	*	descompone la variable Y para acceder al elemento que tiene dentro.
	* 	X seria la fila y P la columna en este punto,
	*	se calcula el indice a partir de X y P y se procede a llamar al metodo bajarTodo/4 y realizar el llamado recursivo
	*/
	gravedad(Grid,[Cabeza|Cola],NumOfColumns, NuevaGrid):-
		Cabeza = [X|Y],
		Y = [P|Q],
		Indice is (X * NumOfColumns) + (P mod NumOfColumns),
		bajarTodo(Indice,Grid,NumOfColumns,NuevaGridAux),
		gravedad(NuevaGridAux,Cola,NumOfColumns,NuevaGridAux2),
		NuevaGrid = NuevaGridAux2.
	
	
	
	%caso en el que se esta en la primer fila de la grid.
	bajarTodo(Indice,Grid,NumOfColumns,NuevaGrid):-
		Indice >= 0,
		Indice =< (NumOfColumns - 1),
		NuevaGrid = Grid.
	
	/* caso recursivo 1.
	*	caso en el que NO se esta en la primer fila de la Grid
	*	si el elemento es 0, se busca hacia arriba y se baja el numero que esta en la posicion 
	*	inmediatamente superior si es distino de 0.
	*/
	bajarTodo(Indice,Grid,NumOfColumns,NuevaGrid):-
		nth0(Indice,Grid,Temp),
		Temp =:= 0,
		Buscar is Indice - NumOfColumns,
		nth0(Buscar,Grid,ResAux),
		ResAux \= 0,
		reemplazarElemento(Grid, Buscar, 0, Result2),
		reemplazarElemento(Result2, Indice, ResAux, Result),
		bajarTodo(Indice,Result,NumOfColumns,ResAux2),
		NuevaGrid = ResAux2.
	
	/*	caso recursivo 2.
	*	caso en el que NO se esta en la primer fila de la Grid
	*	si el elemento NO es 0, o el inmediatamente superior ya es 0, se llama recursivamente sin reemplazar
	*/
	bajarTodo(Indice,Grid,NumOfColumns,NuevaGrid):-
		NuevoIndice is Indice - NumOfColumns,
		bajarTodo(NuevoIndice, Grid,NumOfColumns, NuevaGrid).

	% ordena una lista de pares, de forma descendente con respecto a sus componentes x.
	ordenarPorX(Lista, Resultado) :- 
		sort(0, @=<, Lista, Resultado).

	% caso base.
	reemplazarCeros([], []).
	% caso recursivo. si la cabeza es un 0 se lo reemplaza por una potencia de 2 aleatoria.
	reemplazarCeros([0|Cola], [Potencia|Cola2]) :-
		potencia_aleatoria(2, Potencia),
		reemplazarCeros(Cola, Cola2).
	% caso recursivo 2. si el contenido de la cabeza no es 0, se sigue descomponiendo sin reemplazar el contenido.
	reemplazarCeros([Cabeza|Cola], [Cabeza|Cola2]) :-
		Cabeza \= 0,
		reemplazarCeros(Cola, Cola2).


	eliminarUltimo([_], []).
		eliminarUltimo([X|Xs], [X|Ys]) :- eliminarUltimo(Xs, Ys).


	% genera una potencia de 2 elevado a un numero aleatorio entre 1 y 12.
   	potencia_aleatoria(Num, Potencia) :-
    	random_between(1, 12, Aleatorio),
    	Potencia is Num ** Aleatorio.

	%reemplaza un elemento en una lista.	
	reemplazarElemento(Lista, Indice, Elemento, Result) :-
		nth0(Indice, Lista, _, Temp),
		nth0(Indice, Result, Elemento, Temp).
	
	/*  se encarga de realizar el comportamiento esperado al presionar el boton colapsar iguales
	*	colapsa todos los grupos de bloques adyacentes y de igual valor
	*	reemplaza a la posición más abajo y a la derecha del grupo por la potencia
	*	correspondientes de acuerdo a la sumatoria de los bloques de ese grupo.
	* 	luego aplica gravedad. y reemplaza los 0´s por potencias de 2 aleatorias.
	*/
	booster(Grid,NumOfColumns,RGrids):-
		length(Grid, Size),
		CantidadFilas is Size/NumOfColumns,
		ListaGrupos = [],
		Indice = 0,
		agregarAListaGrupos(Grid,NumOfColumns,ListaGrupos,CantidadFilas,Indice,ResAux),
		eliminarListasVacias(ResAux,ResAux2),
		flatten(ResAux2,ListaGruposAplanada),
		eliminarBooster(Grid,ResAux2,NumOfColumns,ResFinal),
		traducirIndicesACoordenadas(ListaGruposAplanada,[],NumOfColumns,ListaCoordenadasGrupos),
		gravedad(ResFinal,ListaCoordenadasGrupos,NumOfColumns,Res),
		reemplazarCeros(Res,Res2),
		RGrids = [ResFinal,Res,Res2].
	
	/* elimina listas vacias de una lista que las contiene
	* caso base: lista vacía
	*/
	eliminarListasVacias([], []).

	% caso recursivo. se elimina la cabeza de la lista si la misma es una lista vacia.
	eliminarListasVacias([[]|Cola], Res) :- 
		eliminarListasVacias(Cola, Res).
		eliminarListasVacias([Cabeza|Cola], [Cabeza|Res]) :- 
		Cabeza \= [], 
		eliminarListasVacias(Cola, Res). % conserva la lista no vacía

	% Caso base.
	eliminarBooster(Grid,[],_NumOfColumns,Grid).  

	%va enviando cada grupo formado por el booster al metodo que se encarga de convertirlos en 0
	eliminarBooster(Grid,[Cabeza|Cola],NumOfColumns,NuevaGrid):- 
		eliminarBoosterAux(Grid,Cabeza,NumOfColumns, Res),
		eliminarBooster(Res,Cola,NumOfColumns,ResAux),
		NuevaGrid = ResAux.

	/*  se encarga de convertir el contenido de todos los indices contenidos en la lista Grupo	
	* 	en un 0, reemplazando el de mas abajo a la derecha por la potencia resultante correspondiente a la suma
	* 	de los contenidos de los indices a eliminar
	*/
	eliminarBoosterAux(Grid,Grupo,_NumOfColumns, NuevaGrid):-  
		traducirIndicesACoordenadas(Grupo,[],_NumOfColumns,ListaCoordenadasGrupo), % la listaCoordenadasGrupo la vamos a tratar como un path a partir de aqui
		sumarPath(Grid, ListaCoordenadasGrupo, _NumOfColumns, Suma),
    	menorPotenciaDe2(Suma, Potencia),
		ordenarListaRespectoAXeY(ListaCoordenadasGrupo, Ordenada),
		eliminarLista(Grid,Ordenada,_NumOfColumns,Potencia,Resultante),
		NuevaGrid = Resultante.
	
	% caso base, lista vacia.
	traducirIndicesACoordenadas([],ListaCoordenadasGrupo,_NumOfColumns,ListaCoordenadasGrupo). 
	
	% Caso recursivo, este metodo traduce una lista de indices a lista de coordenadas X e Y que representan las filas y columnas de la Grid respectivamente.
	traducirIndicesACoordenadas([Cabeza|Cola],ListaCoordenadasGrupo,_NumOfColumns,Res):-
		armarCoordenada(Cabeza,ListaCoordenadasGrupo,_NumOfColumns,ResAux),
		traducirIndicesACoordenadas(Cola,ResAux,_NumOfColumns,Res).
	
	% calcula la coordenada a parti del indice.
	armarCoordenada(Indice,ListaCoordenadasGrupo,NumOfColumns,ListaCoordenadasGrupoAux):-
		X is Indice div NumOfColumns,
		Y is Indice mod NumOfColumns,
		Par =[X,Y],
		ListaCoordenadasGrupoAux = [Par|ListaCoordenadasGrupo].


	/* ordena una lista de coordenadas primero respecto de su componente Y(columnas)
	*  y luego respecto de su componente X(filas). luego la invierte
	*/ 	
	ordenarListaRespectoAXeY(Lista, Ordenada) :-
	sort(2, @>=, Lista, OrdenadaPorY), % ordena primero por columnas
	sort(1, @>=, OrdenadaPorY, OrdenadaAux), % luego por filas
	reverse(OrdenadaAux, Ordenada). % invierte la lista resultante para que el elemento más abajo y a la derecha quede al final
		
	% chequea si un elemento pertenece a una lista de sublistas.	
	memberAux(Elemento, ListaDeListas) :-
			member(Sublista, ListaDeListas),
			member(Elemento, Sublista).

    /*  crea  y devuelve la lista de listas de indices, donde cada lista de indices corresponde a un
	*	grupo de bloques adyacentes y de igual valor a eliminar.
	* 	una vez que esta dentro de la recursión, si el indice en cuestion no es miembro de la lista de listas, llama para chequear 
	*	si hay que agregarlo a la lista del grupo en cuestion, si ya es miembro lo saltea.
	*/
	agregarAListaGrupos(Grid,NumOfColumns,ListaGrupos,CantidadFilas,Indice,ListaGruposResultante):-
		CantidadIndices is (NumOfColumns * CantidadFilas) - 1,
		Indice =< CantidadIndices,
		not(memberAux(Indice,ListaGrupos)),
		nth0(Indice,Grid,Valor),
		ListaGrupo = [],
		agregarAGrupoAux(Grid,NumOfColumns,CantidadFilas,ListaGrupo,Valor,Indice,ResAux),
		ListaGruposAux = [ResAux|ListaGrupos],
		NuevoIndice is Indice + 1,
		agregarAListaGrupos(Grid,NumOfColumns,ListaGruposAux,CantidadFilas,NuevoIndice,ListaGruposRes),
		ListaGruposResultante = ListaGruposRes.
	/*
	* 	mismo comportamiento que el anterior, se entra cuando el indice ya es miembro de la lista de listas, para saltearlo y seguir la recursion con el siguiente
	*/	
	agregarAListaGrupos(Grid,NumOfColumns,ListaGrupos,CantidadFilas,Indice,ListaGruposResultante):-
		CantidadIndices is (NumOfColumns * CantidadFilas) - 1,
		Indice =< CantidadIndices,
		NuevoIndice is Indice + 1,
		agregarAListaGrupos(Grid,NumOfColumns,ListaGrupos,CantidadFilas,NuevoIndice,ListaGruposRes),
		ListaGruposResultante = ListaGruposRes.

	% 	se llega a este punto cuando el indice esta fuera del alcance de la Grid, devuelve la lista de grupos que le llega	
	agregarAListaGrupos(Grid,NumOfColumns,ListaGrupos,CantidadFilas,NuevoIndice,ListaGruposRes):-
		ListaGruposRes = ListaGrupos.

	/*
	*	transforma el indice recibido en coordenadas X e Y, y llama al metodo que segun el caso en el que se esté, va a chequear los bloques adyacentes correspondientes
	*/	
	agregarAGrupoAux(Grid,NumOfColumns,CantidadFilas,ListaGrupo,Valor,Indice,ResAux):-
		X is Indice div NumOfColumns,
		Y is Indice mod NumOfColumns,
		chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,NuevaListaGrupo),
		ResAux = NuevaListaGrupo.
	
	agregarAGrupoAux(Grid,NumOfColumns,CantidadFilas,ListaGrupo,Valor,Indice,[]).

	%	caso aux 1. grid de una sola fila e Y = 0
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):-
		CantidadFilas =:= 1,
		Y =:= 0,
		NuevoIndice is Indice + 1,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		Resultado = NuevaLista.
	%	caso aux 2. grid de una sola fila e Y \= 0
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):-
		CantidadFilas =:= 1,
		Y \= 0,
		NuevoIndice is Indice + 1,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - 1,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		Resultado = NuevaLista2.

	%	caso aux 3. grid de una sola fila e Y = NumOfColumns - 1
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- 
		CantidadFilas =:= 1,
		Y =:= (NumOfColumns - 1),
		NuevoIndice is Indice - 1,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		Resultado = NuevaLista.

	%	caso 1 : X = 0 e Y = 0
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):-
		X =:= 0,
		Y =:= 0,
		NuevoIndice is Indice + 1,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice + (NumOfColumns + 1),
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice + NumOfColumns,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		Resultado = NuevaLista3.
	
	% 	caso 2: X \=0 e Y = 0
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):-  
		X \= 0,
		Y =:= 0,
		NuevoIndice is Indice - NumOfColumns,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - (NumOfColumns - 1),
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice + 1,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		NuevoIndice4 is Indice + (NumOfColumns + 1),
		compararValores(Grid,NumOfColumns,NuevaLista3,Valor,CantidadFilas,NuevoIndice4,Indice,NuevaLista4),
		NuevoIndice5 is Indice + NumOfColumns,
		compararValores(Grid,NumOfColumns,NuevaLista4,Valor,CantidadFilas,NuevoIndice5,Indice,NuevaLista5),
		Resultado = NuevaLista5.
		
	% 	caso 3: X = cantidad de filas -1 e Y = 0
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):-  
		X =:= CantidadFilas - 1,
		Y =:= 0,
		NuevoIndice is Indice - NumOfColumns,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - (NumOfColumns - 1),
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice + 1,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		Resultado = NuevaLista3.

	% 	caso 4: X = 0 e Y \= 0
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- 
		X =:= 0,
		Y \= 0,
		NuevoIndice is Indice - 1,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice + 1,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice + (NumOfColumns + 1),
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		NuevoIndice4 is Indice + NumOfColumns,
		compararValores(Grid,NumOfColumns,NuevaLista3,Valor,CantidadFilas,NuevoIndice4,Indice,NuevaLista4),
		NuevoIndice5 is Indice + (NumOfColumns - 1),
		compararValores(Grid,NumOfColumns,NuevaLista4,Valor,CantidadFilas,NuevoIndice5,Indice,NuevaLista5),
		Resultado = NuevaLista5.
	
	% 	caso 5 X = 0 e Y = Num de Columnas - 1
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- 
		X =:= 0,
		Y \= NumOfColumns -1 ,
		NuevoIndice is Indice - 1 ,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice + NumOfColumns,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice + (NumOfColumns - 1),
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		Resultado = NuevaLista3.

	% 	caso 6: X \=0 e Y = Num de Columnas - 1
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- 
		X \= 0,
		Y =:= NumOfColumns-1,
		NuevoIndice is Indice - (NumOfColumns + 1),
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - NumOfColumns,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice - 1,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		NuevoIndice4 is Indice + NumOfColumns,
		compararValores(Grid,NumOfColumns,NuevaLista3,Valor,CantidadFilas,NuevoIndice4,Indice,NuevaLista4),
		NuevoIndice5 is Indice + (NumOfColumns - 1),
		compararValores(Grid,NumOfColumns,NuevaLista4,Valor,CantidadFilas,NuevoIndice5,Indice,NuevaLista5),
		Resultado = NuevaLista5.

	% 	caso 7: X = Cantidad Filas - 1 e Y = Num de Columnas - 1
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- 
		X =:= CantidadFilas-1,
		Y =:= NumOfColumns-1,
		NuevoIndice is Indice - (NumOfColumns + 1),
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - NumOfColumns,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice - 1,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		Resultado = NuevaLista3.

	% 	caso 8 : X = 0 e Y \= 0
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- 
		X =:= CantidadFilas - 1,
		Y \= 0,
		NuevoIndice is Indice - (NumOfColumns + 1),
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - NumOfColumns,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice - (NumOfColumns - 1),
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		NuevoIndice4 is Indice - 1,
		compararValores(Grid,NumOfColumns,NuevaLista3,Valor,CantidadFilas,NuevoIndice4,Indice,NuevaLista4),
		NuevoIndice5 is Indice + 1,
		compararValores(Grid,NumOfColumns,NuevaLista4,Valor,CantidadFilas,NuevoIndice5,Indice,NuevaLista5),
		Resultado = NuevaLista5.

	% 	caso 9 : X < CantidadFilas - 1, X > 0, Y > 0, Y < NumofColumns - 1.
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- % caso 9 : X < CantidadFilas - 1, X > 0, Y > 0, Y < NumofColumns - 1.
		X < CantidadFilas - 1,
		X > 0,
		Y > 0,
		Y < NumOfColumns - 1,
		NuevoIndice is Indice - (NumOfColumns + 1),
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - NumOfColumns,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice - (NumOfColumns - 1),
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		NuevoIndice4 is Indice - 1,
		compararValores(Grid,NumOfColumns,NuevaLista3,Valor,CantidadFilas,NuevoIndice4,Indice,NuevaLista4),
		NuevoIndice5 is Indice + 1,
		compararValores(Grid,NumOfColumns,NuevaLista4,Valor,CantidadFilas,NuevoIndice5,Indice,NuevaLista5),
		NuevoIndice6 is Indice + (NumOfColumns + 1),
		compararValores(Grid,NumOfColumns,NuevaLista5,Valor,CantidadFilas,NuevoIndice6,Indice,NuevaLista6),
		NuevoIndice7 is Indice + NumOfColumns,
		compararValores(Grid,NumOfColumns,NuevaLista6,Valor,CantidadFilas,NuevoIndice7,Indice,NuevaLista7),
		NuevoIndice8 is Indice + (NumOfColumns - 1),
		compararValores(Grid,NumOfColumns,NuevaLista7,Valor,CantidadFilas,NuevoIndice8,Indice,NuevaLista8),
		Resultado = NuevaLista8.

	/*
	*	compara valores entre dos posiciones de la grid para saber si pertenecen al mismo grupo
	*	se chequea que los dos no sean miembros ya de la lista de listas de indices (caso en el que el indice inicial del grupo todavia no se agrego a la lista)
	*/
	compararValores(Grid,NumOfColumns,Lista,Valor,CantidadFilas,IndiceNuevo,IndiceInicialGrupo,Nueva):-
		nth0(IndiceNuevo, Grid, NuevoValor),
		not(member(IndiceNuevo,Lista)),
		Valor =:= NuevoValor,
		not(member(IndiceInicialGrupo,Lista)),
		NuevaLista =[IndiceNuevo|Lista],
		NuevaLista2 = [IndiceInicialGrupo|NuevaLista],
		agregarAGrupoAux(Grid,NumOfColumns,CantidadFilas,NuevaLista2,Valor,IndiceNuevo,ResAux),
		Nueva = ResAux.

	/*
	*	compara valores entre dos posiciones de la grid para saber si pertenecen al mismo grupo
	*	se entra cuando el indice inicial del grupo ya es es parte de la lista
	*/
	compararValores(Grid,NumOfColumns,Lista,Valor,CantidadFilas,IndiceNuevo,_IndiceInicialGrupo,Nueva):-
		nth0(IndiceNuevo, Grid, NuevoValor),
		not(member(IndiceNuevo,Lista)),
		Valor =:= NuevoValor,
		NuevaLista =[IndiceNuevo|Lista],
		agregarAGrupoAux(Grid,NumOfColumns,CantidadFilas,NuevaLista,Valor,IndiceNuevo,ResAux),
		Nueva = ResAux.

	% 	se llega cuando los dos indices ya forman parte de la lista
	compararValores(_Grid,_NumOfColumns,Lista,_Valor,_CantidadFilas,_IndiceNuevo,_IndiceInicialGrupo,Lista).