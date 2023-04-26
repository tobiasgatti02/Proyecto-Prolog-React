:- module(proylcc, 
	[  
		join/4
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
	RGrids = [Resultante].
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

		
	eliminar([Cabeza|Cola],Posicion,Contador,Res):- % este eliminar es en caso de que el contador llegue a la posicion
		Contador =:= Posicion,
		NuevaCabeza is 0, % esto esta bien?
		Res = [NuevaCabeza|Cola].
	eliminar([Cabeza|Cola],Posicion,Contador,Res):- % este caso es para iterar sobre el grid hasta llegar al elemento a eliminar
    	NuevoContador is Contador + 1,
		eliminar(Cola,Posicion,NuevoContador,ResAux),
		Res = [Cabeza|ResAux].
			
	coordXnYAux(Grid,[X|Y],NumOfColumns,Potencia, Res):-
		X1 is X,
		predicadoY(Y,Resauxiliar),
		Y1 is Resauxiliar,
		eliminarAux(Grid,(X1 * NumOfColumns) + (Y1 mod NumOfColumns),0,Potencia,ResAux),
		Res = ResAux.	
	eliminarAux([Cabeza|Cola],Posicion,Contador,Potencia,Res):-
		Contador =:= Posicion,
		NuevaCabeza is Potencia, % esto esta bien?
		Res = [NuevaCabeza|Cola]. % cambiar 0 por el numero resultante del camino...
	eliminarAux([Cabeza|Cola],Posicion,Contador,_Potencia, Res):-
    	NuevoContador is Contador + 1,
		eliminarAux(Cola,Posicion,NuevoContador,_Potencia, _ResAux),
		Res = [Cabeza|_ResAux].

	predicadoY(Y,Resauxiliar):-
		Resauxiliar is Y.

 