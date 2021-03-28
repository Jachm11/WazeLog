arco("Curridabat","Tres rios",3,15,30).
arco("Tres rios","Curridabat",3,15,30).

arco("Curridabat","San Pedro",2,10,20).
arco("San Pedro","Curridabat",2,10,20).

arco("San Pedro","Tres rios",3,15,30).
arco("Tres rios","San Pedro",3,15,30).


arco("Tres rios","Taras",10,50,100).
arco("Taras","Tres rios",10,50,100).

arco("Taras","Patarra",5,25,50).
arco("Patarra","Taras",5,25,50).

arco("Taras","Dota",100,25,50).
arco("Dota","Taras",100,25,50).

arco("Taras","Cartago",2,10,20).
arco("Cartago","Taras",2,10,20).

arco("Cartago","Dota",7,35,70).
arco("Dota","Cartago",7,35,70).

arco("Cartago","Narnia",20,35,70).
arco("Narnia","Cartago",20,35,70).

arco("Dota","Narnia",20,35,70).
arco("Narnia","Dota",20,35,70).

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
    shortest(A,B,Ruta,Largo,Tiempo,TiempoPresa),!.

miRuta([A,B|Resto],Ruta,Largo,Tiempo,TiempoPresa):-
    shortest(A,B,Camino,Distancia,Duracion,DuracionPresa),
    miRuta([B|Resto],[_|RutaAux],LargoAux,TiempoAux,TiempoPresaAux),
    concatenar(Camino,RutaAux,Ruta),
    Largo is Distancia + LargoAux,
    Tiempo is Duracion + TiempoAux, 
    TiempoPresa is DuracionPresa + TiempoPresaAux.