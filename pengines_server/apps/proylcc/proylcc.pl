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
	eliminarLista(Grid,_Paxth,_NumOfColumns,Resultante),
	RGrids = [Resultante].
% Esto sirve para encontrar el numero a eliminar en la grilla --> (X * NumOfColumns) + (Y mod NumOfColumns)


	
join(Grid, _NumOfColumns, _Paxth, RGrids) :-
    eliminarLista(Grid, _Paxth, _NumOfColumns, Resultante),
    RGrids = [Resultante].

eliminarLista(Grid, [], _NumOfColumns, Grid):-  % Caso base, lista vacía
	Grid = Res. 
eliminarLista(Grid, [Coordenada|Cola], NumOfColumns, Resultante) :-
    coordXnYAux(Grid, Coordenada, NumOfColumns, Res),
    eliminarLista(Res, Cola, NumOfColumns, Resultante).

	coordXnY(Grid,[X|Y],NumOfColumns,Res):- % Substrae los componentes X e Y de la coordenada --> [1,2] en X=1 y Y=2
		X1 is X,
		predicadoY(Y,Resauxiliar),
		Y1 is Resauxiliar,
		eliminar(Grid,(X1 * NumOfColumns) + (Y1 mod NumOfColumns),0,Res).

		
	eliminar([Cabeza|Cola],Posicion,Contador,Res):- % este eliminar es en caso de que el contador llegue a la posicion
		Contador =:= Posicion,
		NuevaCabeza is 0, % esto esta bien?
		Res = [NuevaCabeza|Cola].
	eliminar([Cabeza|Cola],Posicion,Contador,Res):- % este caso es para iterar sobre el grid hasta llegar al elemento a eliminar
    	NuevoContador is Contador + 1,
		eliminar(Cola,Posicion,NuevoContador,ResAux),
		Res = [Cabeza|ResAux].
			
	coordXnYAux(Grid,[X|Y],NumOfColumns,Res):-
		X1 is X,
		predicadoY(Y,Resauxiliar),
		Y1 is Resauxiliar,
		eliminarAux(Grid,(X1 * NumOfColumns) + (Y1 mod NumOfColumns),0,ResAux),
		Res = ResAux. 
	
	eliminarAux([Cabeza|Cola],Posicion,Contador,Res):-
		Contador =:= Posicion,
		Cabeza is 1. % cambiar 0 por el numero resultante del camino...
	eliminarAux([Cabeza|Cola],Posicion,Contador,Res):-
    	NuevoContador is Contador + 1,
		eliminar(Cola,Posicion,NuevoContador,ResAux),
		Res = [Cabeza|ResAux].

	predicadoY(Y,Resauxiliar):-
		Resauxiliar is Y.