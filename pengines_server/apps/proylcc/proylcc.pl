:- module(proylcc, 
	[  
		join/4
	]).



/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, _NumOfColumns, _Paxth, RGrids):-
	RGrids = eliminarLista(Grid,Paxth,NumOfColumns,Resultante).
% Esto sirve para encontrar el numero a eliminar en la grilla --> (X * NumOfColumns) + (Y mod NumOfColumns)


	
	eliminarLista(Grid,[Coordenada|Cola],NumOfColumns,Resultante):-
		length(Lista, Longitud),
        Longitud =:= 0,
		coordXnYAux(Grid,Coordenada,NumOfColumns,Res),
		Grid = Res.
	eliminarLista(Grid,[Coordenada|Cola],NumOfColumns,Resultante):- % Elimina las coordenadas de la grid
		Grid = coordXnY(Grid,Coordenada,NumOfColumns,Res),
		eliminarLista(Grid,Cola,NumOfColumns,Resultante),
		Resultante = Grid.

	coordXnY(Grid,[X|Y],NumOfColumns,Res):- % Substrae los componentes X e Y de la coordenada --> [1,2] en X=1 y Y=2
		X1 is X,
		Y1 is Y,
		eliminar(Grid,(X * NumOfColumns) + (Y mod NumOfColumns),0,Res).

		
	eliminar([Cabeza|Cola],Posicion,Contador,Res):-
		Contador =:= Posicion,
		Cabeza is 0.
	eliminar([Cabeza|Cola],Posicion,Contador,Res):-
    	NuevoContador is Contador + 1,
		eliminar(Cola,Posicion,NuevoContador,ResAux),
		Res = [Cabeza|ResAux].
			
	coordXnYAux(Grid,[X|Y],NumOfColumns,Res):-
		X1 is X,
		Y1 is Y,
		eliminarAux(Grid,(X * NumOfColumns) + (Y mod NumOfColumns),0,Res).
	
	eliminarAux([Cabeza|Cola],Posicion,Contador,Res):-
		Contador = Posicion,
		Cabeza is joinResult. % cambiar 0 por el numero resultante del camino...
	eliminarAux([Cabeza|Cola],Posicion,Contador,Res):-
    	NuevoContador is Contador + 1,
		eliminar(Cola,Posicion,NuevoContador,ResAux),
		Res = [Cabeza|ResAux].