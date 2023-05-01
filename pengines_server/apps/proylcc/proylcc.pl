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
	eliminar_ultimo(_Paxth,PathAux),
	ordenar_por_x(_Paxth,PathAux2),
	gravedadCamino(PathAux2,Resultante,_NumOfColumns,Res),
	reemplazarCeros(Res,Res2),
	RGrids = [Resultante,Res,Res2].
		% Esto sirve para encontrar el numero a eliminar en la grilla --> (X * NumOfColumns) + (Y mod NumOfColumns)

	% calcula la suma de los números en el path
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
	eliminarLista(Grid, [Coordenada|Cola], _NumOfColumns,_Potencia, Res):-
		length(Cola, Resultado),
		Resultado =:= 0,
		coordXnYAux(Grid, Coordenada, _NumOfColumns,_Potencia, ResAux),
		Res = ResAux.
	
	eliminarLista(Grid, [Coordenada|Cola], NumOfColumns,Potencia, Resultante) :-
		coordXnY(Grid, Coordenada, NumOfColumns, Res),
		eliminarLista(Res, Cola, NumOfColumns, Potencia, Resultante).

	coordXnY(Grid,[],_,_).
	coordXnY(Grid,[X|Y],NumOfColumns,Res):- % Substrae los componentes X e Y de la coordenada --> [1,2] en X=1 y Y=2
		X1 is X,
		predicadoY(Y,Resauxiliar),
		Y1 is Resauxiliar,
		eliminar(Grid,(X1 * NumOfColumns) + (Y1 mod NumOfColumns),0,ResAux),
		Res = ResAux.

		
	eliminar([Cabeza|Cola],Posicion,Contador,Res):- % este eliminar es en caso de que el contador llegue a la posicion. eliminar que reemplaza por 0
		Contador =:= Posicion,
		NuevaCabeza is 0, 
		Res = [NuevaCabeza|Cola].
	eliminar([Cabeza|Cola],Posicion,Contador,Res):- % este caso es para iterar sobre la grid hasta llegar al elemento a eliminar
    	NuevoContador is Contador + 1,
		eliminar(Cola,Posicion,NuevoContador,ResAux),
		Res = [Cabeza|ResAux].
			
	coordXnYAux(Grid,[X|Y],NumOfColumns,Potencia, Res):-
		X1 is X,
		predicadoY(Y,Resauxiliar),
		Y1 is Resauxiliar,
		eliminarAux(Grid,(X1 * NumOfColumns) + (Y1 mod NumOfColumns),0,Potencia,ResAux),
		Res = ResAux.	

	eliminarAux([Cabeza|Cola],Posicion,Contador,Potencia,Res):- % eliminar que reemplaza la ultima posicion del camino con la potencia correspondiente
		Contador =:= Posicion,
		NuevaCabeza is Potencia,
		Res = [NuevaCabeza|Cola].
	eliminarAux([Cabeza|Cola],Posicion,Contador,_Potencia, Res):-
    	NuevoContador is Contador + 1,
		eliminarAux(Cola,Posicion,NuevoContador,_Potencia, _ResAux),
		Res = [Cabeza|_ResAux].

	predicadoY(Y,Resauxiliar):-
		Resauxiliar is Y.
	
	gravedadCamino(Path,Grid,NumOfColumns,NuevaGrid):-
		Cabeza = [X|Y],
		gravedad(Grid,Path,NumOfColumns,Res),
		NuevaGrid = Res.

	gravedad(Grid,[],_NumOfColumns, Grid).
	gravedad(Grid,[Cabeza|Cola],NumOfColumns, NuevaGrid):-
		Cabeza = [X|Y],
		Y = [P|Q],
		Indice is (X * NumOfColumns) + (P mod NumOfColumns),
		bajarTodo(Indice,Grid,NuevaGridAux),
		gravedad(NuevaGridAux,Cola,NumOfColumns,NuevaGridAux2),
		NuevaGrid = NuevaGridAux2.
	
	bajarTodo(Indice,Grid,NuevaGrid):-
		Indice >= 0,
		Indice =< 4,
		NuevaGrid = Grid.
	bajarTodo(Indice,Grid,NuevaGrid):-
		nth0(Indice,Grid,Temp),
		Temp =:= 0,
		Buscar is Indice -5,
		nth0(Buscar,Grid,ResAux),
		ResAux \= 0,
		reemplazarElemento(Grid, Buscar, 0, Result2),
		reemplazarElemento(Result2, Indice, ResAux, Result),
		bajarTodo(Indice,Result,ResAux2),
		NuevaGrid = ResAux2.
	
	bajarTodo(Indice,Grid,NuevaGrid):-
		NuevoIndice is Indice - 5,
		bajarTodo(NuevoIndice, Grid, NuevaGrid).

	ordenar_por_x(Lista, Resultado) :- % ordena una lista de pares, de forma descendente con respecto a sus componentes x.
		sort(0, @=<, Lista, Resultado).

	reemplazarCeros([], []).
	reemplazarCeros([0|Cola], [Potencia|T2]) :-
		potencia_aleatoria(2, Potencia),
		reemplazarCeros(Cola, T2).
	reemplazarCeros([H|T], [H|T2]) :-
		H \= 0,
		reemplazarCeros(T, T2).


	eliminar_ultimo([_], []).
		eliminar_ultimo([X|Xs], [X|Ys]) :- eliminar_ultimo(Xs, Ys).


% Genera un número aleatorio entre 1 y 2 elevado a la potencia de la longitud de la lista potencias
   	potencia_aleatoria(Num, Potencia) :-
    	random_between(1, 6, Aleatorio),
    	Potencia is Num ** Aleatorio.

	reemplazarElemento(Lista, Indice, Elemento, Result) :-
		nth0(Indice, Lista, _, Temp),
		nth0(Indice, Result, Elemento, Temp).

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
		gravedadCamino(ListaCoordenadasGrupos,ResFinal,NumOfColumns,Res),
		reemplazarCeros(Res,Res2),
		RGrids = [ResFinal,Res,Res2].

	eliminarListasVacias([], []). % caso base: lista vacía
	eliminarListasVacias([[]|Cola], Res) :- 
		eliminarListasVacias(Cola, Res). % elimina la lista vacía
		eliminarListasVacias([Cabeza|Cola], [Cabeza|Res]) :- 
		Cabeza \= [], 
		eliminarListasVacias(Cola, Res). % conserva la lista no vacía

	eliminarBooster(Grid,[],_NumOfColumns,Grid). % Caso base. 
	eliminarBooster(Grid,[Cabeza|Cola],NumOfColumns,NuevaGrid):- % este metodo va enviando cada grupo formado por el booster al metodo que se encarga de convertirlos en 0
		eliminarBoosterAux(Grid,Cabeza,NumOfColumns, Res),
		eliminarBooster(Res,Cola,NumOfColumns,ResAux),
		NuevaGrid = ResAux.

	eliminarBoosterAux(Grid,Grupo,_NumOfColumns, NuevaGrid):-  
		traducirIndicesACoordenadas(Grupo,[],_NumOfColumns,ListaCoordenadasGrupo), % la listaCoordenadasGrupo la vamos a tratar como un path a partir de aqui
		sumarPath(Grid, ListaCoordenadasGrupo, _NumOfColumns, Suma),
    	menorPotenciaDe2(Suma, Potencia),
		ordenarListaRespectoAXeY(ListaCoordenadasGrupo, Ordenada),
		eliminarLista(Grid,Ordenada,_NumOfColumns,Potencia,Resultante),
		NuevaGrid = Resultante.

	traducirIndicesACoordenadas([],ListaCoordenadasGrupo,_NumOfColumns,ListaCoordenadasGrupo). % Caso recursivo, este metodo traduce una lista de indices a lista de coordenadas.
	traducirIndicesACoordenadas([Cabeza|Cola],ListaCoordenadasGrupo,_NumOfColumns,Res):-
		armarCoordenada(Cabeza,ListaCoordenadasGrupo,_NumOfColumns,ResAux),
		traducirIndicesACoordenadas(Cola,ResAux,_NumOfColumns,Res).

	armarCoordenada(Indice,ListaCoordenadasGrupo,NumOfColumns,ListaCoordenadasGrupoAux):-
		X is Indice div NumOfColumns,
		Y is Indice mod NumOfColumns,
		Par =[X,Y],
		ListaCoordenadasGrupoAux = [Par|ListaCoordenadasGrupo].


	

	ordenarListaRespectoAXeY(Lista, Ordenada) :-% metodo que ordena una lista de coordenadas con respecto a X e Y respectivamente, ambos en forma ascendente
    sort(1, @=<, Lista, OrdenadaPorX),
    sort(2, @=<, OrdenadaPorX, Ordenada).

	memberAux(Elemento, ListaDeListas) :-
			member(Sublista, ListaDeListas),
			member(Elemento, Sublista).


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

	agregarAListaGrupos(Grid,NumOfColumns,ListaGrupos,CantidadFilas,Indice,ListaGruposResultante):-
		CantidadIndices is (NumOfColumns * CantidadFilas) - 1,
		Indice =< CantidadIndices,
		NuevoIndice is Indice + 1,
		agregarAListaGrupos(Grid,NumOfColumns,ListaGrupos,CantidadFilas,NuevoIndice,ListaGruposRes),
		ListaGruposResultante = ListaGruposRes.

	agregarAListaGrupos(Grid,NumOfColumns,ListaGrupos,CantidadFilas,NuevoIndice,ListaGruposRes):-
		ListaGruposRes = ListaGrupos.

	agregarAGrupoAux(Grid,NumOfColumns,CantidadFilas,ListaGrupo,Valor,Indice,ResAux):-
		X is Indice div NumOfColumns,
		Y is Indice mod NumOfColumns,
		chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,NuevaListaGrupo),
		ResAux = NuevaListaGrupo.
	
	agregarAGrupoAux(Grid,NumOfColumns,CantidadFilas,ListaGrupo,Valor,Indice,[]).


	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):-  % caso 1 : X = 0 e Y = 0
		X =:= 0,
		Y =:= 0,
		NuevoIndice is Indice + 1,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice + 6,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice +5,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		Resultado = NuevaLista3.
	
	
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):-  % caso 2: X \=0 e Y = 0
		X \= 0,
		Y =:= 0,
		NuevoIndice is Indice - 5,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - 4,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice + 1,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		NuevoIndice4 is Indice + 6,
		compararValores(Grid,NumOfColumns,NuevaLista3,Valor,CantidadFilas,NuevoIndice4,Indice,NuevaLista4),
		NuevoIndice5 is Indice + 5,
		compararValores(Grid,NumOfColumns,NuevaLista4,Valor,CantidadFilas,NuevoIndice5,Indice,NuevaLista5),
		Resultado = NuevaLista5.
		
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):-  % caso 3: X = cantidad de filas -1 e Y = 0
		X =:= CantidadFilas - 1,
		Y =:= 0,
		NuevoIndice is Indice - 5,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - 4,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice + 1,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		Resultado = NuevaLista3.

	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- % caso 4: X = 0 e Y \= 0
		X =:= 0,
		Y \= 0,
		NuevoIndice is Indice - 1,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice + 1,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice + 6,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		NuevoIndice4 is Indice + 5,
		compararValores(Grid,NumOfColumns,NuevaLista3,Valor,CantidadFilas,NuevoIndice4,Indice,NuevaLista4),
		NuevoIndice5 is Indice + 4,
		compararValores(Grid,NumOfColumns,NuevaLista4,Valor,CantidadFilas,NuevoIndice5,Indice,NuevaLista5),
		Resultado = NuevaLista5.
	
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- % caso 5 X = 0 e Y = Num de Columnas - 1
		X =:= 0,
		Y \= NumOfColumns -1 ,
		NuevoIndice is Indice - 1 ,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice + 5,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice + 4,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		Resultado = NuevaLista3.

	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- % caso 6: X \=0 e Y = Num de Columnas - 1
		X \= 0,
		Y =:= NumOfColumns-1,
		NuevoIndice is Indice - 6,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - 5,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice - 1,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		NuevoIndice4 is Indice + 5,
		compararValores(Grid,NumOfColumns,NuevaLista3,Valor,CantidadFilas,NuevoIndice4,Indice,NuevaLista4),
		NuevoIndice5 is Indice + 4,
		compararValores(Grid,NumOfColumns,NuevaLista4,Valor,CantidadFilas,NuevoIndice5,Indice,NuevaLista5),
		Resultado = NuevaLista5.


	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- % caso 7: X = Cantidad Filas - 1 e Y = Num de Columnas - 1
		X =:= CantidadFilas-1,
		Y =:= NumOfColumns-1,
		NuevoIndice is Indice - 6,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - 5,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice - 1,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		Resultado = NuevaLista3.

	
	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- % caso 8 : X = 0 e Y \= 0
		X =:= CantidadFilas - 1,
		Y \= 0,
		NuevoIndice is Indice - 6,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - 5,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice - 4,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		NuevoIndice4 is Indice - 1,
		compararValores(Grid,NumOfColumns,NuevaLista3,Valor,CantidadFilas,NuevoIndice4,Indice,NuevaLista4),
		NuevoIndice5 is Indice + 1,
		compararValores(Grid,NumOfColumns,NuevaLista4,Valor,CantidadFilas,NuevoIndice5,Indice,NuevaLista5),
		Resultado = NuevaLista5.


	chequearCaso(Grid,Indice,X,Y,NumOfColumns,Valor,ListaGrupo,CantidadFilas,Resultado):- % caso 9 : X < CantidadFilas - 1, X > 0, Y > 0, Y < NumofColumns - 1.
		X < CantidadFilas - 1,
		X > 0,
		Y > 0,
		Y < NumOfColumns - 1,
		NuevoIndice is Indice - 6,
		compararValores(Grid,NumOfColumns,ListaGrupo,Valor,CantidadFilas,NuevoIndice,Indice,NuevaLista),
		NuevoIndice2 is Indice - 5,
		compararValores(Grid,NumOfColumns,NuevaLista,Valor,CantidadFilas,NuevoIndice2,Indice,NuevaLista2),
		NuevoIndice3 is Indice - 4,
		compararValores(Grid,NumOfColumns,NuevaLista2,Valor,CantidadFilas,NuevoIndice3,Indice,NuevaLista3),
		NuevoIndice4 is Indice - 1,
		compararValores(Grid,NumOfColumns,NuevaLista3,Valor,CantidadFilas,NuevoIndice4,Indice,NuevaLista4),
		NuevoIndice5 is Indice + 1,
		compararValores(Grid,NumOfColumns,NuevaLista4,Valor,CantidadFilas,NuevoIndice5,Indice,NuevaLista5),
		NuevoIndice6 is Indice + 6,
		compararValores(Grid,NumOfColumns,NuevaLista5,Valor,CantidadFilas,NuevoIndice6,Indice,NuevaLista6),
		NuevoIndice7 is Indice + 5,
		compararValores(Grid,NumOfColumns,NuevaLista6,Valor,CantidadFilas,NuevoIndice7,Indice,NuevaLista7),
		NuevoIndice8 is Indice + 4,
		compararValores(Grid,NumOfColumns,NuevaLista7,Valor,CantidadFilas,NuevoIndice8,Indice,NuevaLista8),
		Resultado = NuevaLista8.


	compararValores(Grid,NumOfColumns,Lista,Valor,CantidadFilas,IndiceNuevo,IndiceInicialGrupo,Nueva):-
		nth0(IndiceNuevo, Grid, NuevoValor),
		not(member(IndiceNuevo,Lista)),
		Valor =:= NuevoValor,
		not(member(IndiceInicialGrupo,Lista)),
		NuevaLista =[IndiceNuevo|Lista],
		NuevaLista2 = [IndiceInicialGrupo|NuevaLista],
		agregarAGrupoAux(Grid,NumOfColumns,CantidadFilas,NuevaLista2,Valor,IndiceNuevo,ResAux),
		Nueva = ResAux.
	compararValores(Grid,NumOfColumns,Lista,Valor,CantidadFilas,IndiceNuevo,_IndiceInicialGrupo,Nueva):-
		nth0(IndiceNuevo, Grid, NuevoValor),
		not(member(IndiceNuevo,Lista)),
		Valor =:= NuevoValor,
		NuevaLista =[IndiceNuevo|Lista],
		agregarAGrupoAux(Grid,NumOfColumns,CantidadFilas,NuevaLista,Valor,IndiceNuevo,ResAux),
		Nueva = ResAux.
	compararValores(_Grid,_NumOfColumns,Lista,_Valor,_CantidadFilas,_IndiceNuevo,_IndiceInicialGrupo,Lista).