:- module(proylcc, 
	[  
		join/4,
		booster/3,
		movidaMaxima/3,
		movidaMaximaAdyacenteIgual/3
	]).

/*
 * join(+Grid, +NumOfColumns, +Paxth, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Paxth
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 
join(Grid, NumOfColumns, Paxth, RGrids):-
	length(Grid, Size),
	CantidadFilas is Size/NumOfColumns,
	CantidadIndices is (NumOfColumns * CantidadFilas) - 1,
	sumarPath(Grid, Paxth, NumOfColumns, Suma),
    menorPotenciaDe2(Suma, Potencia),
	eliminarLista(Grid,Paxth,NumOfColumns,Potencia,Resultante),
	ordenarPorX(Paxth,PathAux2),
	gravedad(Resultante,PathAux2,NumOfColumns,CantidadIndices,Res),
	reemplazarCeros(Res,Res2),
	RGrids = [Resultante,Res,Res2].
		

% calcula la suma de los números  que forman parte de un  path
sumarPath(_, [],_, 0).
sumarPath(Grid, [[X,Y]|Resto],NumOfColumns, Suma) :-
	Posicion is X * NumOfColumns + Y,
	nth0(Posicion, Grid, Numero),% este predicado es predefinido por prolog y encuentra en una lista una posicion
	sumarPath(Grid, Resto, NumOfColumns, SumaResto),
	Suma is SumaResto + Numero.

menorPotenciaDe2(0, 0):-!.

% encuentra la menor potencia de 2 mayor o igual que N
menorPotenciaDe2(N, Potencia) :-
	Potencia is 2 ** (ceil(log(N)/log(2))).


eliminarLista(Grid, [], _NumOfColumns,_Potencia, Grid).  % Caso base, lista vacía
	 

/* caso recursivo 1.
*   si es el ultimo elemento de la cola, llamamos al predicado que reemplaza
*	ese ultimo elemento por la potencia correspondiente.
*/
eliminarLista(Grid, [Coordenada|Cola], NumOfColumns,Potencia, Res):- 
	length(Cola, 0),
	separarParAux(Grid, Coordenada, NumOfColumns,Potencia, ResAux),
	Res = ResAux.
	
/* caso recursivo 2.
*   si NO es el ultimo elemento de la cola, llamamos al predicado que reemplaza
*	el elemento en cuestion por un 0.
*/
eliminarLista(Grid, [Coordenada|Cola], NumOfColumns,Potencia, Resultante) :-
	separarPar(Grid, Coordenada, NumOfColumns, Res),
	eliminarLista(Res, Cola, NumOfColumns, Potencia, Resultante).

 % Caso base, lista vacía
separarPar(_,[],_,_).

/* caso recursivo.
*   se separa el par de coordenadas y se llama al predicado eliminar para reemplazar
*	la posicion correspondiente en la grid por 0.
*/
separarPar(Grid,[X|Y],NumOfColumns,Res):- % Substrae los componentes X e Y de la coordenada --> [1,2] en X=1 y Y=2
	X1 is X,
	Y1 is Y,
	eliminar(Grid,(X1 * NumOfColumns) + (Y1 mod NumOfColumns),0,ResAux),
	Res = ResAux.

/*
* 	se separa el par de coordenadas y se llama al predicado reemplazarPorPotencia/5 para reemplazar
*	el contenido de la posicion correspondiente de la grid por la potencia resultante de cada caso.
*/
separarParAux(Grid,[X|Y],NumOfColumns,Potencia, Res):-
	X1 is X,
	Y1 is Y,
	reemplazarPorPotencia(Grid,(X1 * NumOfColumns) + (Y1 mod NumOfColumns),0,Potencia,ResAux),
	Res = ResAux.	

% se ingresa en caso de que el contador llegue a la posicion que buscamos.se reemplaza el contenido en esa posicion por 0	
eliminar([_Cabeza|Cola],Posicion,Contador,Res):- 
	Contador =:= Posicion,
	NuevaCabeza is 0, 
	Res = [NuevaCabeza|Cola].

%itera sobre la grid mientras no haya llegado a la posicion a eliminar
eliminar([Cabeza|Cola],Posicion,Contador,Res):- 
   	NuevoContador is Contador + 1,
	eliminar(Cola,Posicion,NuevoContador,ResAux),
	Res = [Cabeza|ResAux].

% % se ingresa en caso de que el contador llegue a la posicion que buscamos.se reemplaza el contenido en esa posicion por la potencia resultantedel camino
reemplazarPorPotencia([_Cabeza|Cola],Posicion,Contador,Potencia,Res):- 
	Contador =:= Posicion,
	NuevaCabeza is Potencia,
	Res = [NuevaCabeza|Cola].

%itera sobre la grid mientras no haya llegado a la posicion a eliminar	
reemplazarPorPotencia([Cabeza|Cola],Posicion,Contador,Potencia, Res):-
   	NuevoContador is Contador + 1,
	reemplazarPorPotencia(Cola,Posicion,NuevoContador,Potencia, ResAux),
	Res = [Cabeza|ResAux].


% caso base. cuando el path este vacío se devuelve la Grid pasada como parametro.
gravedad(Grid,[],_NumOfColumns,_CantidadIndices, Grid).

/*	caso recursivo.
*	si el path NO esta vacío descompone la cabeza de la lista de entrada en las variables X e Y
*	descompone la variable Y para acceder al elemento que tiene dentro.
* 	X seria la fila y P la columna en este punto,
*	se calcula el indice a partir de X y P y se procede a llamar al metodo bajarTodo/4 y realizar el llamado recursivo
*/
gravedad(Grid,[Cabeza|Cola],NumOfColumns,CantidadIndices, NuevaGrid):-
	Cabeza = [X|Y],
	Y = [P|_Q],
	Indice is (X * NumOfColumns) + (P mod NumOfColumns),
	bajarTodo(Indice,Grid,NumOfColumns,CantidadIndices,NuevaGridAux),
	gravedad(NuevaGridAux,Cola,NumOfColumns,CantidadIndices,NuevaGridAux2),
	NuevaGrid = NuevaGridAux2.

	
	
%caso en el que se esta en la primer fila de la grid.
bajarTodo(Indice,Grid,NumOfColumns,CantidadIndices,NuevaGrid):-
	Indice < CantidadIndices,
	Indice >= 0,
	Indice =< (NumOfColumns - 1),
	NuevaGrid = Grid.
	
/* caso recursivo 1.
*	caso en el que NO se esta en la primer fila de la Grid
*	si el elemento es 0, se busca hacia arriba y se baja el numero que esta en la posicion 
*	inmediatamente superior si es distino de 0.
*/
bajarTodo(Indice,Grid,NumOfColumns,CantidadIndices,NuevaGrid):-
	Indice =< CantidadIndices,
	nth0(Indice,Grid,Temp),
	Temp =:= 0,
	Buscar is Indice - NumOfColumns,
	nth0(Buscar,Grid,ResAux),
	ResAux \= 0,
	reemplazarElemento(Grid, Buscar, 0, Result2),
	reemplazarElemento(Result2, Indice, ResAux, Result),
	IndiceAnterior is Indice + NumOfColumns,
	bajarTodo(Buscar,Result,NumOfColumns,CantidadIndices,ResAux2),
	bajarTodo(IndiceAnterior,ResAux2,NumOfColumns,CantidadIndices,ResAux3),
	NuevaGrid = ResAux3.

/*	caso recursivo 2.
*	caso en el que NO se esta en la primer fila de la Grid
*	si el elemento NO es 0, o el inmediatamente superior ya es 0, se llama recursivamente sin reemplazar
*/
bajarTodo(Indice,Grid,NumOfColumns,CantidadIndices,NuevaGrid):-
	Indice =< CantidadIndices,
	NuevoIndice is Indice - NumOfColumns,
	bajarTodo(NuevoIndice, Grid,NumOfColumns,CantidadIndices, NuevaGrid).

bajarTodo(_,Grid,_,_,Grid).

% ordena una lista de pares, de forma descendente con respecto a sus componentes x.
ordenarPorX(Lista, Resultado) :- 
	sort(0, @=<, Lista, Resultado).

% caso base.
reemplazarCeros([], []).
% caso recursivo. si la cabeza es un 0 se lo reemplaza por una potencia de 2 aleatoria.
reemplazarCeros([0|Cola], [Potencia|Cola2]) :-
	potenciaAleatoria(2, Potencia),
	reemplazarCeros(Cola, Cola2).
% caso recursivo 2. si el contenido de la cabeza no es 0, se sigue descomponiendo sin reemplazar el contenido.
reemplazarCeros([Cabeza|Cola], [Cabeza|Cola2]) :-
	Cabeza \= 0,
	reemplazarCeros(Cola, Cola2).


eliminarUltimo([_], []).
	eliminarUltimo([X|Xs], [X|Ys]) :- eliminarUltimo(Xs, Ys).

% genera una potencia de 2 elevado a un numero aleatorio entre 1 y 8.
potenciaAleatoria(Num, Potencia) :-
	random_between(1, 8, Aleatorio),
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
	CantidadIndices is (NumOfColumns * CantidadFilas) - 1,
	ListaGrupos = [],
	Indice = 0,
	agregarAListaGrupos(Grid,NumOfColumns,ListaGrupos,CantidadFilas,Indice,ResAux),
	eliminarListasDeUnElemento(ResAux,ResAux2),
	flatten(ResAux2,ListaGruposAplanada),
	eliminarBooster(Grid,ResAux2,NumOfColumns,ResFinal),
	traducirIndicesACoordenadas(ListaGruposAplanada,[],NumOfColumns,ListaCoordenadasGrupos),
	gravedad(ResFinal,ListaCoordenadasGrupos,NumOfColumns,CantidadIndices,Res2),
	reemplazarCeros(Res2,Res3),
	RGrids = [ResFinal,Res2,Res3].
	
/* elimina listas vacias de una lista que las contiene
* caso base: lista vacía
*/
eliminarListasDeUnElemento([], []).

% caso recursivo. se elimina la cabeza de la lista si la misma es una lista vacia.
eliminarListasDeUnElemento([Cabeza|Cola], Res) :-
	length(Cabeza, Largo),
	Largo \= 1,
	eliminarListasDeUnElemento(Cola, ResAux),
	Res = [Cabeza| ResAux].
	 % conserva la lista de largo distinto a un elemento

eliminarListasDeUnElemento([_Cabeza|Cola], Res) :-
	eliminarListasDeUnElemento(Cola, ResAux),
	Res = ResAux.

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
eliminarBoosterAux(Grid,Grupo,NumOfColumns, NuevaGrid):-  
	traducirIndicesACoordenadas(Grupo,[],NumOfColumns,ListaCoordenadasGrupo), % la listaCoordenadasGrupo la vamos a tratar como un path a partir de aqui
	sumarPath(Grid, ListaCoordenadasGrupo, NumOfColumns, Suma),
   	menorPotenciaDe2(Suma, Potencia),
	ordenarListaRespectoAXeY(ListaCoordenadasGrupo, Ordenada),
	eliminarLista(Grid,Ordenada,NumOfColumns,Potencia,Resultante),
	NuevaGrid = Resultante.
	
% caso base, lista vacia.
traducirIndicesACoordenadas([],ListaCoordenadasGrupo,_NumOfColumns,ListaCoordenadasGrupo). 
	
% Caso recursivo, este metodo traduce una lista de indices a lista de coordenadas X e Y que representan las filas y columnas de la Grid respectivamente.
traducirIndicesACoordenadas([Cabeza|Cola],ListaCoordenadasGrupo,NumOfColumns,Res):-
	armarCoordenada(Cabeza,ListaCoordenadasGrupo,NumOfColumns,ResAux),
	traducirIndicesACoordenadas(Cola,ResAux,NumOfColumns,Res).
	
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
agregarAListaGrupos(_Grid,_NumOfColumns,ListaGrupos,_CantidadFilas,_NuevoIndice,ListaGruposRes):-
	ListaGruposRes = ListaGrupos.

/*
*  si el indice no pertenece ya a la lista del grupo, se lo agrega, se llama al predicado getIndicesAdyacentes/6 y luego al predicado compararValores/7
*  para seguir armando el grupo.
*/
agregarAGrupoAux(Grid,NumOfColumns,CantidadFilas,ListaGrupo,Valor,Indice,ResAux):-
	not(member(Indice,ListaGrupo)),
	NuevaListaGrupo = [Indice| ListaGrupo],
	X is Indice div NumOfColumns,
	Y is Indice mod NumOfColumns,
	getIndicesAdyacentes(NumOfColumns,CantidadFilas,Indice, X, Y, ListaIndices),
	compararValores(Grid,NumOfColumns, CantidadFilas, ListaIndices,Valor, NuevaListaGrupo, Resultado),
	ResAux = Resultado.
	
agregarAGrupoAux(_,_,_,_,_,_,[]).

% 	a partir de un Indice devuelve la lista de sus Indices adyacentes validos.
getIndicesAdyacentes(NumOfColumns, CantidadFilas, Indice,X, Y, Res) :-
	Indice1 is Indice + (NumOfColumns + 1),
    Indice2 is Indice + NumOfColumns,
    Indice3 is Indice + (NumOfColumns - 1),
    Indice4 is Indice + 1,
    Indice5 is Indice - (NumOfColumns - 1),
    Indice6 is Indice - NumOfColumns,
    Indice7 is Indice - (NumOfColumns + 1),
    Indice8 is Indice - 1,
    Lista = [Indice1 , Indice2 , Indice3 , Indice4 , Indice5 , Indice6 , Indice7 , Indice8],
	verificarListaIndices(Lista, NumOfColumns, CantidadFilas,X, Y, ResAux),
	Res = ResAux.

% 	verifica que los indices pertenecientes a una lista sean realmente adyacentes a un indice, si no es asi los elimina de la lista.
verificarListaIndices([],_,_,_,_,[]). % caso base

%	primer caso recursivo: indice valido
verificarListaIndices([Cabeza|Cola], NumOfColumns, CantidadFilas, X, Y, Res):-
	Cabeza >= 0,
	Cabeza =< (NumOfColumns * CantidadFilas),
	CabezaX is Cabeza div NumOfColumns,
	CabezaY is Cabeza mod NumOfColumns,
	CabezaX >= X - 1,
	CabezaX =< X + 1,
	CabezaY >= Y - 1,
	CabezaY =< Y + 1,
	verificarListaIndices(Cola, NumOfColumns, CantidadFilas, X, Y,  ResAux),
	Res = [Cabeza| ResAux].
% 	segundo caso recursivo: indice invalido
verificarListaIndices([_Cabeza|Cola], NumOfColumns, CantidadFilas,X, Y, Res):-
	verificarListaIndices(Cola, NumOfColumns, CantidadFilas, X, Y, ResAux),
	Res = ResAux.

/*
*	compara los valores (en la grid) de los indices pertenecientes a una lista pasada por parametro, para usarlos,
*	en  el caso que sea posible para seguir construyendo el grupo de indices. 
*/
%	caso base
compararValores(_Grid,_NumOfColumns,_CantidadFilas,[],_Valor,Lista, Lista).

%	caso recursivo 1: el indice no es miembro de la lista del grupo, y el valor coincide con el valor del grupo. 
compararValores(Grid,NumOfColumns,CantidadFilas,[Cabeza|Cola], Valor, Lista, ListaResultante):-
	not(member(Cabeza,Lista)),
	nth0(Cabeza, Grid, NuevoValor),
	Valor =:= NuevoValor,
	agregarAGrupoAux(Grid,NumOfColumns,CantidadFilas,Lista,Valor,Cabeza,ResAux),
	compararValores(Grid,NumOfColumns,CantidadFilas,Cola, Valor, ResAux, ListaResultanteAux),
	ListaResultante = ListaResultanteAux.

%	caso recursivo 2: el sindice o ya es miembro de la lista, o no coincide con el valor del grupo, por lo cual se descarta.
compararValores(Grid,NumOfColumns,CantidadFilas,[_Cabeza|Cola],Valor,Lista, ListaResultante):-
	compararValores(Grid,NumOfColumns,CantidadFilas,Cola, Valor, Lista, ListaResultanteAux),
	ListaResultante = ListaResultanteAux.

 %Etapa 2: 

/*	se encarga de realizar el comportamiento esperado al presionar el boton movida Maxima.
*	Recorre la grid en busqueda del camino que genere el mayor numero posible 
*	con la configuracion actual de la Grid.
*	crea una lista vacia que va a representar el mayor camino hasta ese momento(la lista vacia),
* 	llama a movidaMaximaAuxiliar/6 con el indice en 0 y la lista vacia como mayor camino.
*	Éste metodo devuelve el mayor camino de la grilla y movidaMaxima/3 lo devuelve como resultado.
*/
movidaMaxima(Grid,NumOfColumns,CaminoMaximo):-
	length(Grid, Size),
	CantidadFilas is Size/NumOfColumns,
	Indice = 0,
	MayorCaminoHastaElMomento = [],
	movidaMaximaAuxiliar(Grid,NumOfColumns,CantidadFilas,Indice, MayorCaminoHastaElMomento,ResAux),
	traducirIndicesACoordenadas(ResAux,[],NumOfColumns,ListaCoordenadasMayorCamino),
	CaminoMaximo = ListaCoordenadasMayorCamino.


/*	si el indice está dentro de la Grid, se calcula el valor de ese indice, se crea una lista 
*	vacía en representación del camino actual(como está empezando es vacío) y se llama al método
*	movidaMaximaRecorrido/8 con ese valor y la lista vacia para que genere el camino esperado.
*	cuando se obtiene el mayor camino vinculado a ese índice(si es que es mayor al mayor almacenado hasta al momento),
*	se incrementa el índice en uno y se llama recursivamente a si mismo con ese indice incrementado.
*/	
movidaMaximaAuxiliar(Grid,NumOfColumns,CantidadFilas,Indice,MayorCaminoHastaElMomento,Resultado):-
	CantidadIndices is (NumOfColumns * CantidadFilas) - 1,
	Indice =< CantidadIndices,
	nth0(Indice,Grid,Valor),
	CaminoActual = [],
	movidaMaximaRecorrido(Grid,NumOfColumns,CantidadFilas,CaminoActual,Valor,Indice,MayorCaminoHastaElMomento,ResAux),
	NuevoIndice is Indice + 1,
	movidaMaximaAuxiliar(Grid,NumOfColumns,CantidadFilas,NuevoIndice,ResAux,ListaCaminoRes),
	Resultado = ListaCaminoRes.

/*
*	se llega a este metodo cuando el Indice sale del alcance de la Grid, por lo tanto el mayor camino es el almacenado hasta el momento.
*	no hay mas por recorrer.
*/
movidaMaximaAuxiliar(_,_,_,_,MayorCaminoHastaElMomento,MayorCaminoHastaElMomento).


/*	Si el indice recibido como parámetro no es miembro de la lista que representa al camino actual que se esta generando, 
*	se agrega el mismo a la lista de indices de dicho camino, se calcula su coordenada X e Y y se llama al predicado
*	getIndicesAdyacentes/6 y luego a compararValoresMovidaMaxima/8 con esa lista de indices. se devuelve el resultado de este ultimo.
*/
movidaMaximaRecorrido(Grid,NumOfColumns,CantidadFilas,ListaCamino,Valor,Indice,MayorCaminoHastaElMomento,Resultado):-
	not(member(Indice,ListaCamino)),
	NuevaListaCamino = [Indice| ListaCamino],
	X is Indice div NumOfColumns,
	Y is Indice mod NumOfColumns,
	getIndicesAdyacentes(NumOfColumns,CantidadFilas,Indice, X, Y, ListaIndices),
	compararValoresMovidaMaxima(Grid, NumOfColumns, CantidadFilas, ListaIndices, Valor, NuevaListaCamino, MayorCaminoHastaElMomento, ResAux),
	Resultado = ResAux.

/*
*	cuando la lista de indices adyacentes está vacia, significa que la rama ya no puede continuar, por lo tanto se compara el camino 
*	que se esta recorriendo con el almacenado como mayor hasta el momento. si la potencia que genera el que se esta recorriendo
*	es mayor o igual a la que genera el MayorHastaElMomento, entonces el que se esta recorriendo pasa a ser el mayor, y se lo devuelve.
*/
compararValoresMovidaMaxima(Grid,NumOfColumns,_,[],_,ListaCamino,MayorCaminoHastaElMomento,Res):-
	length(ListaCamino,Largo),
	Largo>1,
	traducirIndicesACoordenadas(ListaCamino,[],NumOfColumns,ListaCoordenadasCamino), % la listaCoordenadasGrupo la vamos a tratar como un path a partir de aqui
	sumarPath(Grid, ListaCoordenadasCamino, NumOfColumns, Suma),
   	menorPotenciaDe2(Suma, Potencia),
	traducirIndicesACoordenadas(MayorCaminoHastaElMomento,[],NumOfColumns,ListaCoordenadasMayorCamino), % la listaCoordenadasGrupo la vamos a tratar como un path a partir de aqui
	sumarPath(Grid, ListaCoordenadasMayorCamino, NumOfColumns, SumaMayor),
	menorPotenciaDe2(SumaMayor, PotenciaMayor),
	Potencia >= PotenciaMayor,
	Res = ListaCamino.

/*	si el anterior falló, significa que el camino que se esta recorriendo no debe reemplazar al mayor hasta el momento.*/
compararValoresMovidaMaxima(_,_,_,[],_,_,MayorCaminoHastaElMomento,Res):-
	Res = MayorCaminoHastaElMomento.

/*	si la cabeza de la lista de indices adyacentes no es miembro aún del camino que se está armando, se calcula el valor 
*	del mismo y se compara con el valor del indice anterior del camino. si coinciden se llama recursivamente a 
*	movidaMaximaRecorrido/8 con la cabeza de la lista de adyacentes ahora como índice actual. cuando se sale de dicho metodo,  
*	se llama recursivamente a compararValoresMovidaMaxima/8 con la cola de la lista de indices adyacentes, para no perder
*	posibles caminos.
*/
compararValoresMovidaMaxima(Grid, NumOfColumns, CantidadFilas, [Cabeza|Cola], Valor, ListaCamino, MayorCaminoHastaElMomento, Resultado):-
	not(member(Cabeza,ListaCamino)),
	nth0(Cabeza, Grid, NuevoValor),
	Valor =:= NuevoValor,
	movidaMaximaRecorrido(Grid,NumOfColumns,CantidadFilas,ListaCamino,Valor,Cabeza,MayorCaminoHastaElMomento,ResAux),
	compararValoresMovidaMaxima(Grid, NumOfColumns, CantidadFilas,Cola,Valor,ListaCamino,ResAux,ResAux2),
	Resultado = ResAux2. 
	

/*	si el largo de la lista del camino actual es mayor a 1, se chequea que el indice Cabeza de la lista de indices adyacentes no sea 
*	aun miembro de la lista del camino, si no lo es, se calcula el valor y se chequea si coincide con la potencia siguiente a la 
*	almacenada como actual. Luego se llama recursivamente a movidaMaximaRecorrido/8 con la cabeza de la lista de adyacentes ahora como 
*	índice actual. cuando se sale de dicho metodo, se llama recursivamente a compararValoresMovidaMaxima/8 con la cola de la lista 
*	de indices adyacentes, para no perder posibles caminos.
*/	
compararValoresMovidaMaxima(Grid, NumOfColumns, CantidadFilas, [Cabeza|Cola], Valor, ListaCamino, MayorCaminoHastaElMomento, Resultante):-
	length(ListaCamino, Largo),
	Largo > 1,
	not(member(Cabeza,ListaCamino)),
	nth0(Cabeza, Grid, NuevoValor),
	SiguientePotencia is Valor*2,
	NuevoValor =:= SiguientePotencia,
	movidaMaximaRecorrido(Grid,NumOfColumns,CantidadFilas,ListaCamino,SiguientePotencia,Cabeza,MayorCaminoHastaElMomento,ResAux),
	compararValoresMovidaMaxima(Grid, NumOfColumns, CantidadFilas,Cola,Valor,ListaCamino,ResAux,ResAux2),
	Resultante = ResAux2.


/*	Si se llega a entrar en este metodo, significa que el indice Cabeza de la lista de indices adyacentes no contiene
*	ni el mismo valor, ni la potencia siguiente al valor del ultimo del camino hasta ael momento, por ende se descarta ese 
*	indice, y se llama recursivamente con la cola de la lista de indices.
*/
compararValoresMovidaMaxima(Grid, NumOfColumns, CantidadFilas, [_Cabeza|Cola], Valor, ListaCamino, Mayor, Resultado):-
	compararValoresMovidaMaxima(Grid, NumOfColumns, CantidadFilas, Cola, Valor, ListaCamino, Mayor, ResAux),
	Resultado = ResAux.


/*	se encarga de realizar el comportamiento esperado al presionar el boton movida Maxima Adyacente Igual.
*	Recorre la grid en busqueda del camino que genere el mayor numero posible con la configuracion actual de la Grid.
*	crea una lista vacia que va a representar el mayor camino hasta ese momento(la lista vacia),
* 	llama a movidaMaximaAdtayenteAuxiliar/6 con el indice en 0 y la lista vacia como mayor camino.
*	Éste metodo devuelve el mayor camino de la grilla con una potencia igual adyacente(preexistente) 
*	y movidaMaximaAdyacenteIgual/3 lo devuelve como resultado.
*/
movidaMaximaAdyacenteIgual(Grid,NumOfColumns,CaminoMaximo):-
	length(Grid, Size),
	CantidadFilas is Size/NumOfColumns,
	Indice = 0,
	MayorCaminoHastaElMomento = [],
	movidaMaximaAdyacenteAuxiliar(Grid,NumOfColumns,CantidadFilas,Indice, MayorCaminoHastaElMomento,ResAux),
	traducirIndicesACoordenadas(ResAux,[],NumOfColumns,ListaCoordenadasMayorCamino),
	CaminoMaximo = ListaCoordenadasMayorCamino.


/*	si el indice está dentro de la Grid, se calcula el valor de ese indice, se crea una lista 
*	vacía en representación del camino actual(como está empezando es vacío) y se llama al método
*	movidaMaximaAdyacenteRecorrido/8 con ese valor y la lista vacia para que genere el camino esperado.
*	cuando se obtiene el mayor camino vinculado a ese índice(si es que existe alguno que cumpla la propiedad,
*	y a su vez es mayor al mayor almacenado hasta al momento), se incrementa el índice en uno y se llama 
*	recursivamente a si mismo con ese indice.
*/
movidaMaximaAdyacenteAuxiliar(Grid,NumOfColumns,CantidadFilas,Indice,MayorCaminoHastaElMomento,Resultado):-
	CantidadIndices is (NumOfColumns * CantidadFilas) - 1,
	Indice =< CantidadIndices,
	nth0(Indice,Grid,Valor),
	CaminoActual = [],
	movidaMaximaAdyacenteRecorrido(Grid,NumOfColumns,CantidadFilas,CaminoActual,Valor,Indice,MayorCaminoHastaElMomento,ResAux),
	NuevoIndice is Indice + 1,
	movidaMaximaAdyacenteAuxiliar(Grid,NumOfColumns,CantidadFilas,NuevoIndice,ResAux,ListaCaminoRes),
	Resultado = ListaCaminoRes.

/*
*	se llega a este metodo cuando el Indice sale del alcance de la Grid, por lo tanto el mayor camino es el almacenado hasta el momento.
*	no hay mas por recorrer.
*/
movidaMaximaAdyacenteAuxiliar(_,_,_,_,MayorCaminoHastaElMomento,MayorCaminoHastaElMomento).


/*	Si el indice recibido como parámetro no es miembro de la lista que representa al camino actual que se esta generando, 
*	se agrega el mismo a la lista de indices de dicho camino, se calcula su coordenada X e Y y se llama al predicado
*	getIndicesAdyacentes/6 y luego a compararValoresMovidaMaxima/8 con esa lista de indices. se devuelve el resultado de este ultimo.
*/
movidaMaximaAdyacenteRecorrido(Grid,NumOfColumns,CantidadFilas,ListaCamino,Valor,Indice,MayorCaminoHastaElMomento,Resultado):-
	not(member(Indice,ListaCamino)),
	NuevaListaCamino = [Indice| ListaCamino],
	X is Indice div NumOfColumns,
	Y is Indice mod NumOfColumns,
	getIndicesAdyacentes(NumOfColumns,CantidadFilas,Indice, X, Y, ListaIndices),
	compararValoresMovidaMaximaAdyacente(Grid, NumOfColumns, CantidadFilas, ListaIndices, Valor, NuevaListaCamino, MayorCaminoHastaElMomento, ResAux),
	Resultado = ResAux.

/*
*	cuando la lista de indices adyacentes está vacia, significa que la rama ya no puede continuar, por lo tanto se compara el camino 
*	que se esta recorriendo con el almacenado como mayor hasta el momento. si la potencia que genera el que se esta recorriendo
*	es mayor o igual a la que genera el MayorHastaElMomento, entonces pasamos al siguiente paso que consiste en llamar al metodo 
* 	chequearAdyacenciaCamino/6, si no falla significa que hay adyacencia, por lo tanto, el que se esta recorriendo pasa a ser el mayor, y se lo devuelve.
*/
compararValoresMovidaMaximaAdyacente(Grid,NumOfColumns,CantidadFilas,[],_,ListaCamino,MayorCaminoHastaElMomento,Res):-
	length(ListaCamino,Largo),
	Largo > 1,
	traducirIndicesACoordenadas(ListaCamino,[],NumOfColumns,ListaCoordenadasCamino), % la listaCoordenadasGrupo la vamos a tratar como un path a partir de aqui
	sumarPath(Grid, ListaCoordenadasCamino, NumOfColumns, Suma),
	menorPotenciaDe2(Suma, Potencia),
	traducirIndicesACoordenadas(MayorCaminoHastaElMomento,[],NumOfColumns,ListaCoordenadasMayorCamino), % la listaCoordenadasGrupo la vamos a tratar como un path a partir de aqui
	sumarPath(Grid, ListaCoordenadasMayorCamino, NumOfColumns, SumaMayor),
	menorPotenciaDe2(SumaMayor, PotenciaMayor),
	Potencia >= PotenciaMayor,
	chequearAdyacenciaCamino(Grid,NumOfColumns,CantidadFilas,ListaCamino,MayorCaminoHastaElMomento,NuevoMayorCamino),
	Res = NuevoMayorCamino.

/*	si el anterior falló, o porque la potencia no es mayor a la del mayor camino hasta el momento, o porque no hay adyacencia, 
*	significa que el camino que se esta recorriendo no debe reemplazar al mayor hasta el momento.
*/
compararValoresMovidaMaximaAdyacente(_,_,_,[],_,_,MayorCaminoHastaElMomento,Res):-
	Res = MayorCaminoHastaElMomento.

/*	si la cabeza de la lista de indices adyacentes no es miembro aún del camino que se está armando, se calcula el valor 
*	del mismo y se compara con el valor del indice anterior del camino. si coinciden se llama recursivamente a 
*	movidaMaximaAdyacenteRecorrido/8 con la cabeza de la lista de adyacentes ahora como índice actual. cuando se sale de dicho metodo,  
*	se llama recursivamente a compararValoresMovidaMaximaAdyacente/8 con la cola de la lista de indices adyacentes, para no perder
*	posibles caminos.
*/
compararValoresMovidaMaximaAdyacente(Grid, NumOfColumns, CantidadFilas, [Cabeza|Cola], Valor, ListaCamino, MayorCaminoHastaElMomento, Resultado):-
	not(member(Cabeza,ListaCamino)),
	nth0(Cabeza, Grid, NuevoValor),
	Valor =:= NuevoValor,
	movidaMaximaAdyacenteRecorrido(Grid,NumOfColumns,CantidadFilas,ListaCamino,Valor,Cabeza,MayorCaminoHastaElMomento,ResAux),
	compararValoresMovidaMaximaAdyacente(Grid, NumOfColumns, CantidadFilas,Cola,Valor,ListaCamino,ResAux,ResAux2),
	Resultado = ResAux2. 
	
/*	si el largo de la lista del camino actual es mayor a 1, se chequea que el indice Cabeza de la lista de indices adyacentes no sea 
*	aun miembro de la lista del camino, si no lo es, se calcula el valor y se chequea si coincide con la potencia siguiente a la 
*	almacenada como actual. Luego se llama recursivamente a movidaMaximaAdyacenteRecorrido/8 con la cabeza de la lista de adyacentes ahora como 
*	índice actual. cuando se sale de dicho metodo, se llama recursivamente a compararValoresMovidaMaximaAdyacente/8 con la cola de la lista 
*	de indices adyacentes, para no perder posibles caminos.
*/	
compararValoresMovidaMaximaAdyacente(Grid, NumOfColumns, CantidadFilas, [Cabeza|Cola], Valor, ListaCamino, MayorCaminoHastaElMomento, Resultante):-
	length(ListaCamino, Largo),
	Largo > 1,
	not(member(Cabeza,ListaCamino)),
	nth0(Cabeza, Grid, NuevoValor),
	SiguientePotencia is Valor*2,
	NuevoValor =:= SiguientePotencia,
	movidaMaximaAdyacenteRecorrido(Grid,NumOfColumns,CantidadFilas,ListaCamino,SiguientePotencia,Cabeza,MayorCaminoHastaElMomento,ResAux),
	compararValoresMovidaMaximaAdyacente(Grid, NumOfColumns, CantidadFilas,Cola,Valor,ListaCamino,ResAux,ResAux2),
	Resultante = ResAux2.

/*	Si se llega a entrar en este metodo, significa que el indice Cabeza de la lista de indices adyacentes no contiene
*	ni el mismo valor, ni la potencia siguiente al valor del ultimo del camino hasta el momento, por ende se descarta ese 
*	indice, y se llama recursivamente con la cola de la lista de indices.
*/
compararValoresMovidaMaximaAdyacente(Grid, NumOfColumns, CantidadFilas, [_Cabeza|Cola], Valor, ListaCamino, Mayor, Resultado):-
	compararValoresMovidaMaximaAdyacente(Grid, NumOfColumns, CantidadFilas, Cola, Valor, ListaCamino, Mayor, ResAux),
	Resultado = ResAux.

/*	se calcula la potencia del camino, y se llama al predicado verificarIgualPotenciaAdyacente/6, si no falla, nuevoMayorCamino
*	se iguala al camino que se esta recorriendo. 
*/	
chequearAdyacenciaCamino(Grid,NumOfColumns,CantidadFilas,ListaCamino,MayorCaminoHastaElMomento,NuevoMayorCamino):-
	traducirIndicesACoordenadas(ListaCamino,[],NumOfColumns,ListaCoordenadasCamino),
	sumarPath(Grid, ListaCoordenadasCamino, NumOfColumns, Suma),
	menorPotenciaDe2(Suma, Potencia),
	traducirIndicesACoordenadas(MayorCaminoHastaElMomento,[],NumOfColumns,ListaCoordenadasMayorCamino), % la listaCoordenadasGrupo la vamos a tratar como un path a partir de aqui
	sumarPath(Grid, ListaCoordenadasMayorCamino, NumOfColumns, SumaMayor),
	menorPotenciaDe2(SumaMayor, PotenciaMayor),
	Potencia >= PotenciaMayor,
	verificarIgualPotenciaAdyacente(Grid,NumOfColumns,CantidadFilas,Potencia,ListaCamino,_),
	NuevoMayorCamino = ListaCamino,!.

/*	si el anterior falló, descompone el camino que se recorrió, y prueba adyacencia cada vez que elimina un indice del camino
*	para no perder ningun camino.
*/	
chequearAdyacenciaCamino(Grid,NumOfColumns,CantidadFilas,[_Cabeza|Cola],MayorCaminoHastaElMomento,NuevoMayorCamino):-
	length(Cola,LargoCola),
	LargoCola > 1,
	chequearAdyacenciaCamino(Grid,NumOfColumns,CantidadFilas,Cola,MayorCaminoHastaElMomento,NuevoMayorCaminoAuxiliar),
	NuevoMayorCamino = NuevoMayorCaminoAuxiliar.

chequearAdyacenciaCamino(_Grid,_NumOfColumns,_CantidadFilas,_,MayorCaminoHastaElMomento,MayorCaminoHastaElMomento).


/* 	Verifica que la potencia resultante sea igual a una potencia adyacente preexistente en la grid
*	si la potencia generada no va a quedar como adyacente a una potencia igual(preexistente) luego de eliminar el camino
*	que se va a marcar como maximo, el predicado falla.
*/
verificarIgualPotenciaAdyacente(Grid,NumOfColumns,CantidadFilas,Potencia,ListaCamino,Res):-
	CantidadIndices is (NumOfColumns * CantidadFilas) - 1,
	ListaCamino =[Cabeza|_Cola],
	X is Cabeza div NumOfColumns,
	Y is Cabeza mod NumOfColumns,
	traducirIndicesACoordenadas(ListaCamino,[],NumOfColumns,ListaCoordenadasCamino),
	contarPares(ListaCoordenadasCamino,X,Y,Cantidad),
	NuevaPosicionPotencia is Cabeza + (NumOfColumns * Cantidad),
	eliminarLista(Grid,ListaCoordenadasCamino,NumOfColumns,Potencia,Resultante),
	ordenarPorX(ListaCoordenadasCamino,ListaCoordenadasCamino2),
	gravedad(Resultante,ListaCoordenadasCamino2,NumOfColumns,CantidadIndices,Result),!,
	reemplazarCerosPorDos(Result,Res2),
	chequearAdyacencia(Res2,NumOfColumns,CantidadFilas,NuevaPosicionPotencia,Potencia,_),
	Res = [].

/*	obtiene los indices adyacentes a la posicion en la que quedaria la potencia generada, y chequea si 
*	hay alguna coincidencia con sus indices adyacentes.
*/
chequearAdyacencia(Grid,NumOfColumns,CantidadFilas,NuevaPosicionPotencia,Potencia,_):-
	X is NuevaPosicionPotencia div NumOfColumns,
	Y is NuevaPosicionPotencia mod NumOfColumns,
	getIndicesAdyacentes(NumOfColumns,CantidadFilas,NuevaPosicionPotencia, X, Y, ListaIndices),!,
	hayCoincidencias(Grid,Potencia,ListaIndices,_).


/* 	lleva una cuenta de los pares que en la grid estan por debajo del ultimo elemento en el camino
*	es decir que su componente X (H), es mayor a la compente X del par que representa al ultimo elemento del camino
*	y su componente Y (Q) es igual a la componente Y del par que representa al ultimo elemento del camino.
*/
contarPares(Lista, X, Y, Suma) :-
	findall(_, (member([H, Q], Lista), H > X, Q =:= Y), Pares),
	length(Pares, Suma).


% busca coincidencias entre un valor, y los valores almacenados en los indices adyacentes de la grilla.
hayCoincidencias(_Grid,_Potencia,[],_):- fail.
hayCoincidencias(Grid,Potencia,[Cabeza|_Cola],_):-
	nth0(Cabeza,Grid,Valor),
	Potencia =:= Valor.
hayCoincidencias(Grid,Potencia,[_Cabeza|Cola],_):-
	hayCoincidencias(Grid,Potencia,Cola,_).


/*  
    Reemplaza los ceros que genero Gravedad por 2, con el fin de que la grilla alternativa generada
    por la Gravedad no afecte al funcionamiento generando numeros aleatorios por los que la grilla 
	actual pueda conectar
*/ 
reemplazarCerosPorDos([], []).
% caso recursivo. si la cabeza es un 0 se lo reemplaza por un 2.
reemplazarCerosPorDos([0|Cola], [Potencia|Cola2]) :-
	Potencia = 2,
	reemplazarCerosPorDos(Cola, Cola2).
% caso recursivo 2. si el contenido de la cabeza no es 0, se sigue descomponiendo sin reemplazar el contenido.
reemplazarCerosPorDos([Cabeza|Cola], [Cabeza|Cola2]) :-
	Cabeza \= 0,
	reemplazarCerosPorDos(Cola, Cola2).

	