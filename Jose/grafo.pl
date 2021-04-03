arco("curridabat","tresrios",3,15,30).
arco("tresrios","curridabat",3,15,30).

arco("curridabat","sanpedro",2,10,20).
arco("sanpedro","curridabat",2,10,20).

arco("sanpedro","tresrios",3,15,30).
arco("tresrios","sanpedro",3,15,30).

arco("tresrios","taras",10,50,100).
arco("taras","tresrios",10,50,100).

arco("taras","patarra",5,25,50).
arco("patarra","taras",5,25,50).

arco("taras","dota",100,25,50).
arco("dota","taras",100,25,50).

arco("taras","cartago",2,10,20).
arco("cartago","taras",2,10,20).

arco("cartago","dota",7,35,70).
arco("dota","cartago",7,35,70).

arco("cartago","narnia",20,35,70).
arco("narnia","cartago",20,35,70).

arco("dota","narnia",20,35,70).
arco("narnia","dota",20,35,70).

%La concatenacion de una lista vacia y un elemento es el elemento
concatenar([],Elem,Elem).

% La concatenacion de una lista con un elemento es la concatenacion de
% la cola con la concatenacion de ese elemento
concatenar([H|T],Elem,[H|Aux]) :-concatenar(T,Elem,Aux).


camino(Inicio,Final,Camino,Largo,Tiempo,TiempoPresa) :-
    viajar(Inicio,Final,[Inicio],Cola,Largo,Tiempo,TiempoPresa), %empieza a probar rutas para llegar a final
    reverse(Cola,Camino). %la lista viene invertida


viajar(Inicio,Final,Visitados,[Final|Visitados],Largo,Tiempo,TiempoPresa) :-  
    arco(Inicio,Final,Largo,Tiempo,TiempoPresa). %comprueba ruta diracta / agrega Final al camino

viajar(Inicio,Final,Visitados,Camino,Largo,Tiempo,TiempoPresa) :-
    arco(Inicio,Conexion,Distancia,Duracion,DuracionPresa), %comprueba un nodo de conexion          
    Conexion \== Final,
    not(member(Conexion,Visitados)), %si la conexion no se ha visitado ya 
    viajar(Conexion,Final,[Conexion|Visitados],Camino,LargoAux,TiempoAux,TiempoPresaAux), %comprobar si la conexion lleva a final, recursivo
    Largo is Distancia+LargoAux,  %suma de distancias
    Tiempo is Duracion+TiempoAux,  %suma de tiempos
    TiempoPresa is DuracionPresa+TiempoPresaAux.  %suma de tiempos en presa

shortest(Inicio,Final,Camino,Largo,Tiempo,TiempoPresa) :-
    setof([Camino,Largo,Tiempo,TiempoPresa],camino(Inicio,Final,Camino,Largo,Tiempo,TiempoPresa),Set), % setof (template para set(lo que extraigo),de donde lo extraigo ,todas las soluciones encotradas)
    Set = [_|_], % fallo si no hay rutas
    minimal(Set,[Camino,Largo,Tiempo,TiempoPresa]). %obtener el minimo del set, el minimo es una lista con el camino, su logitud, su tiempo, y su tiempo en presa

minimal([Frente|Resto],Minimo) :- min(Resto,Frente,Minimo). 
min([],Minimo,Minimo). %si ya no hay mas rutas que revisar
min([[Camino,Largo,Tiempo,TiempoPresa]|Resto],[_,Minimo,_,_],MinLista) :- Largo < Minimo, !, min(Resto,[Camino,Largo,Tiempo,TiempoPresa],MinLista). % ! significa encontro a uno menor, asi q no vale la pena seguir con ese largo
min([_|Resto],Minimo,MinLista) :- min(Resto,Minimo,MinLista). %Llamada recursiva para el resto de las rutas en caso de que no se cumpla resto = [] o Largo < Minimo


miRuta([A,B],Ruta,Largo,Tiempo,TiempoPresa):-
    shortest(A,B,Ruta,Largo,Tiempo,TiempoPresa),!. % ruta mas corta y corte.

miRuta([A,B|Resto],Ruta,Largo,Tiempo,TiempoPresa):-
    shortest(A,B,Camino,Distancia,Duracion,DuracionPresa), %Ruta mas corta de los primeros elementos de la lista
    miRuta([B|Resto],[_|RutaAux],LargoAux,TiempoAux,TiempoPresaAux), % llamada recursiva para el resto de la lista de destinos
    concatenar(Camino,RutaAux,Ruta), % Definicion de ruta
    Largo is Distancia + LargoAux, % Definicion de largo
    Tiempo is Duracion + TiempoAux, % Definicion de tiempo
    TiempoPresa is DuracionPresa + TiempoPresaAux. % Definicion de tiempoPresa

%Destinos es una lista de lugares
wazeLog([A,A]):-
    write("Ya se encuentra en su destino. \n"),
    !,fail.

wazeLog(Destinos):-
    write("Es hora pico?"),
    read(X),
    wazeLogOut(Destinos,X).

%Imprime las rutas para cuando hay presa
wazeLogOut(Destinos,Bool):-
    afirmativo(Bool),
    miRuta(Destinos,Ruta,Largo,_,TiempoPresa),
    write("Su ruta es "),
    printList(Ruta),
    write(" la distancia por recorrer es "),write(Largo),write(" kilometros"),
    write(" y el tiempo estimado es "), write(TiempoPresa),write(" minutos."),
    write(" Que tenga un buen viaje!"),!.

%Imprime las rutas para cuando no hay presa
wazeLogOut(Destinos,Bool):-
    negativo(Bool),
    miRuta(Destinos,Ruta,Largo,Tiempo,_),
    write("Su ruta es "),
    printList(Ruta),
    write(" la distancia por recorrer es "),write(Largo),write(" kilometros"),
    write(" y el tiempo estimado es "), write(Tiempo),write(" minutos."),
    write(" Que tenga un buen viaje!"),!.

%Si no se comprende la respuesta a si es hora pico o no.
wazeLogOut(Destinos,_):-
    miRuta(Destinos,_,_,_,_),
    write("No lo he entendido, por favor responda con si o no. \n"),
    wazeLog(Destinos).

wazeLogOut(_,_):-
    write("Lo sentimos, no posible llegar al o los destinos solicitados").

%Imprime una lista de strigs separada por comas y con un ; al final
printList([Head]):-
    write(Head),write(";").

printList([Head|Tail]):-
    write(Head),write(", "),
    printList(Tail).


afirmativo("si").
afirmativo("Si").
afirmativo(si).
negativo("no").
negativo("No").
negativo(no).
