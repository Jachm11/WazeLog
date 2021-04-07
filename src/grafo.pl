%                 __________________________________
%________________/ Base de datos del grafo

% arco(origen, destino, distancia, tiempo estimado, tiempo estimado en presa)
% Descripción: la relacion arco describe un arco o arista de un grafo. Es un grafo mixto. Cada arco es una relacion dirigida.

arco("corralillo","san jose",22,25,50).
arco("san jose","corralillo",22,25,50).

arco("corralillo","musgo verde",6,10,20).
arco("musgo verde","corralillo",6,10,20).

arco("musgo verde","cartago",10,15,30).
arco("cartago","musgo verde",10,15,30).

arco("cartago","san jose",20,25,50).
arco("san jose","cartago",20,25,50).

arco("cartago","tres rios",8,10,20).

arco("tres rios","san jose",8,10,20).

arco("cartago","paraiso",10,10,20).

arco("cartago","pacayas",13,15,30).
arco("pacayas","cartago",13,15,30).

arco("pacayas","tres rios",15,20,40).
arco("tres rios","pacayas",15,20,40).

arco("pacayas","cervantes",8,10,20).
arco("cervantes","pacayas",8,10,20).

arco("paraiso","cervantes",4,5,10).

arco("paraiso","orosi",8,10,20).
arco("orosi","paraiso",8,10,20).

arco("paraiso","cachi",10,15,30).
arco("cachi","paraiso",10,15,30).

arco("cachi","orosi",12,15,30).
arco("orosi","cachi",12,15,30).

arco("cachi","cervantes",7,10,20).
arco("cervantes","cachi",7,10,20).

arco("cachi","turrialba",40,50,100).
arco("turrialba","cachi",40,50,100).

arco("cervantes","juan vinas",5,5,10).

arco("juan vinas","turrialba",4,5,10).

arco("turrialba","pacayas",18,20,40).


%________________________________________________________ 


%                 _______________________________________
%________________/ Reglas secundarias 

%Condicion de parada
%Descripción: La concatenacion de una lista vacia y un elemento es el elemento
concatenar([],Elem,Elem).

% Concatenar (Lista, Elemento a concatenar, Lista con elemento)
% Descripción: La concatenacion de una lista con un elemento es la concatenacion de la cola con la concatenacion de ese elemento
concatenar([H|T],Elem,[H|Aux]) :-concatenar(T,Elem,Aux).

% printList(Lista):-
% Descripción:Imprime una lista de strigs separada por comas y con un ; al final
printList([Head]):-
    write(Head),write(";\n").

printList([Head|Tail]):-
    write(Head),write(", "),
    printList(Tail).


%__________________________________________________________

%                 _______________________________________
%________________/ Reglas para determinacion de rutas

% - - - - - - Codigo original tomado y adaptado de https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_15A.pl - - - - - - 

% camino(Inicio,Final,Camino,Largo,Tiempo,TiempoPresa)
% Descripción: regla principal de viajar. Encuentra todas las rutas posibles que hay desde un nodo A a un nodo B.
camino(Inicio,Final,Camino,Largo,Tiempo,TiempoPresa) :-
    viajar(Inicio,Final,[Inicio],Cola,Largo,Tiempo,TiempoPresa),
    reverse(Cola,Camino). % Efectua reverse para mitigar el backtraking


% Condicion de parada
% Descripción: comprueba ruta diracta o bien agrega Final al camino
viajar(Inicio,Final,Visitados,[Final|Visitados],Largo,Tiempo,TiempoPresa) :-  
    arco(Inicio,Final,Largo,Tiempo,TiempoPresa).
% viajar(Inicio,Final,Visitados,Camino,Largo,Tiempo,TiempoPresa)
% Descripción: comprueba un nodo de conexion he intenta llegar recursivamente al destino desde ese nodo
viajar(Inicio,Final,Visitados,Camino,Largo,Tiempo,TiempoPresa) :-
    arco(Inicio,Conexion,Distancia,Duracion,DuracionPresa), %comprueba un nodo de conexion          
    Conexion \== Final,
    not(member(Conexion,Visitados)), %si la conexion no se ha visitado ya 
    viajar(Conexion,Final,[Conexion|Visitados],Camino,LargoAux,TiempoAux,TiempoPresaAux), %comprobar si la conexion lleva a final, recursivo
    Largo is Distancia+LargoAux,  %suma de distancias
    Tiempo is Duracion+TiempoAux,  %suma de tiempos
    TiempoPresa is DuracionPresa+TiempoPresaAux.  %suma de tiempos en presa


% shortest(Inicio,Final,Camino,Largo,Tiempo,TiempoPresa)
% Descripción: utilzando camino y minimal encuentra la ruta mas corta de un nodo A a B
shortest(Inicio,Final,Camino,Largo,Tiempo,TiempoPresa) :-
    setof([Camino,Largo,Tiempo,TiempoPresa],camino(Inicio,Final,Camino,Largo,Tiempo,TiempoPresa),Set), % setof (template para set(lo que extraigo),de donde lo extraigo ,todas las soluciones encotradas)
    Set = [_|_], % fallo si no hay rutas
    minimal(Set,[Camino,Largo,Tiempo,TiempoPresa]).


% minimal(Set de rutas completas , ruta completa minima) Donde completo significa [Camino,Largo,Tiempo,TiempoPresa]
% Descripción: Determina de un set de caminos cual es el que tiene menor distancia
minimal([Frente|Resto],Minimo) :- min(Resto,Frente,Minimo). 
% Condicion de parada.
% Descripción: Cuando ya no hay mas rutas completas por revisar.
min([],Minimo,Minimo).
% min(Resto,Frente,Minimo) Donde minimo es una ruta completa
% Descripción: Comprueba si una ruta tiene una distacia menor que otra
min([[Camino,Largo,Tiempo,TiempoPresa]|Resto],[_,Minimo,_,_],MinLista) :- Largo < Minimo, !, min(Resto,[Camino,Largo,Tiempo,TiempoPresa],MinLista). % ! significa encontro a uno menor, asi que no vale la pena seguir con ese largo
% Descripción: Llamada recursiva para el resto de las rutas en caso de que no se cumpla resto = [] o Largo < Minimo
min([_|Resto],Minimo,MinLista) :- min(Resto,Minimo,MinLista).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% Condicion de parada
% Descripción: busca la ruta mas corta entre A y B y corte
miRuta([A,B],Ruta,Largo,Tiempo,TiempoPresa):-
    shortest(A,B,Ruta,Largo,Tiempo,TiempoPresa),!. % ruta mas corta y corte.

% miRuta(Lista de destinos,Ruta,Largo,Tiempo,TiempoPresa)
% Descripción: Recurvisamente busca y concatena las rutas mas cortas de cada lugar en la lista de destinos en el orden dado
% e.g: si Lista de destinos es [A,B,C] recursivamente busca la ruta mas corta de A a B, luego de B a C y las concatena sumando sus valores de distancia y tiempos
miRuta([A,B|Resto],Ruta,Largo,Tiempo,TiempoPresa):-
    shortest(A,B,Camino,Distancia,Duracion,DuracionPresa), %Ruta mas corta de los primeros elementos de la lista
    miRuta([B|Resto],[_|RutaAux],LargoAux,TiempoAux,TiempoPresaAux), % llamada recursiva para el resto de la lista de destinos
    concatenar(Camino,RutaAux,Ruta), % Definicion de ruta
    Largo is Distancia + LargoAux, % Definicion de largo
    Tiempo is Duracion + TiempoAux, % Definicion de tiempo
    TiempoPresa is DuracionPresa + TiempoPresaAux. % Definicion de tiempoPresa


%______________________________________________________________

%                 _____________________________________________
%________________/ Reglas y hechos de interfaz ligadas al grafo

% Condicion especial
% Descripción: Da error si se intenta ir a lugar de origen si realizar ninguna parada
wazeLogIn([A,A]):-
    write("WazeLog: Ya se encuentra en su destino! \n"),
    !.

% wazeLogIn(Destinos) Destinos es una lista de lugares
% Descripción: Imprime las preguntas necesarias para saber si que tiempo se requiere e imprime la ruta final al usuario. regla principal de wazeLogIn
wazeLogIn(Destinos):-
    write("WazeLog: Es hora pico? \nUsuario: "),
    read_line_to_string(user_input,Input),
    string_lower(Input,NewInput),
    wazeLogOut(Destinos,NewInput).

% wazeLogOut(Destinos,Bool)
% Descripción: Imprime las rutas para cuando hay presa.
wazeLogOut(Destinos,Bool):-
    afirmativo(Bool),
    miRuta(Destinos,Ruta,Largo,_,TiempoPresa),
    write("WazeLog: Su ruta es "),
    printList(Ruta),
    write("WazeLog: La distancia por recorrer es "),write(Largo),write(" kilometros;\n"),
    write("WazeLog: Y el tiempo estimado es "), write(TiempoPresa),write(" minutos. \n"),
    write("WazeLog: Que tenga un buen viaje! Y gracias por usar WazeLog! "),!.

% Descripción: Imprime las rutas para cuando no hay presa
wazeLogOut(Destinos,Bool):-
    negativo(Bool),
    miRuta(Destinos,Ruta,Largo,Tiempo,_),
    write("WazeLog: Su ruta es "),
    printList(Ruta),
    write("WazeLog: La distancia por recorrer es "),write(Largo),write(" kilometros;\n"),
    write("WazeLog: Y el tiempo estimado es "), write(Tiempo),write(" minutos. \n"),
    write("WazeLog: Que tenga un buen viaje! Y gracias por usar WazeLog! "),!.

% Descripción: Si no se comprende la respuesta a si es hora pico o no.
wazeLogOut(Destinos,_):-
    miRuta(Destinos,_,_,_,_),
    write("WazeLog: No lo he entendido, por favor responda con si o no. \n"),
    wazeLogIn(Destinos).
% Descripción: Si no se logra encotrar una ruta. Sucede cuando no exite la ruta
wazeLogOut(_,_):-
    write("WazeLog: Lo sentimos, no posible llegar al o los destinos solicitados").

% Hechos linguisticos booleanos
afirmativo("si").
negativo("no").


%_____________________________________________________________
