%Incluir el archivo del grafo
:-include('grafo.pl').
%                 ____________________________________________________________
%________________/ Reglas para formar oraciones (Gramática libre de contexto)

%_____________________________________________________________________________________________________________________  
%oracion(X,Y), donde X es la representación de una oración (lista de strings) y L es es lugar que se detecta en la oración
%Descripción: Con esta regla se puede formar una oración con un nombre y un sintagma_verbal_lugar. Ej: Yo voy a Aserri
oracion(O,L) :- nombre(N),
                 sintagma_verbal_lugar(SV,L),
                 append(N,SV,O).
%Descripción: Con esta regla se puede formar una oración con un nombre y un sintagma_nominal_lugar. Ej: Yo estoy en Aserri
oracion(O,L) :- nombre(N),
                sintagma_nominal_lugar(SV,L),
                append(N,SV,O).
%Descripción: Con esta regla se puede formar una oración con un sintagma_verbal_lugar. Ej: Estoy en Aserri
oracion(O,L) :- sintagma_verbal_lugar(O,L).
%Descripción: Con esta regla se puede formar una oración un sintagma_nominal. Ej: Al supermercado
oracion(O,L) :- sintagma_nominal(O,L).
%Descripción: Con esta regla se puede formar una oración con un sintagma_nominal_lugar. Ej: A Aserri
oracion(O,L) :- sintagma_nominal_lugar(O,L).
%Descripción: Con esta regla se puede formar una oración con un sintagma_verbal y un sintagma_nominal_lugar. Ej: Yo voy a Aserri
oracion(O,L) :- sintagma_verbal(SV),
                sintagma_nominal_lugar(SNL,L),
                append(SV,SNL,O).
%Descripción: Con esta regla se puede formar una oración con un sintagma_verbal y un sintagma_verbal_lugar. Ej: Yo quiero ir a Aserri
oracion(O,L) :- sintagma_verbal(SV),
                sintagma_verbal_lugar(SVL,L),
                append(SV,SVL,O).
%Descripción: Con esta regla se puede formar una oración con un sintagma_nominal y un sintagma_verbal_lugar. Ej: El supermercado esta en Aserri
oracion(O,L) :- sintagma_nominal(SN,_),
                sintagma_verbal_lugar(SVL,L),
                append(SN,SVL,O).
%_____________________________________________________________________________________________________________________  
%sintagma_nominal_lugar(X,Y), donde X es la representación del sintagma (lista de strings) y Y es el lugar que se detecta
%Descripción: Con esta regla se puede formar un sintagma de lugar formado solamente por el lugar. Ej: Aserri
sintagma_nominal_lugar(SN,SN) :- lugar(SN). %Narnia
%Descripción: Con esta regla se puede formar un sintagma de lugar formado por una preposicion y un lugar. Ej: A Aserri
sintagma_nominal_lugar(SN,L) :- preposicion(A),
                                lugar(L),
                                append(A,L,SN).
%Descripción: Con esta regla se puede formar un sintagma de lugar formado por un sintagma nominal y un lugar. Ej: A la farmacia
sintagma_nominal_lugar(SNL,L) :- sintagma_nominal(SN),
                                lugar(L),   
                                append(SN,L,SNL).
%_____________________________________________________________________________________________________________________                   
%sintagma_nominal(X,Y), donde X es la representación del sintagma (lista de strings) y Y es el lugar que se detecta
%Descripción: Con esta regla se puede formar un sintagma nominal formado por un articulo y un lugar. Ej: La farmacia           
sintagma_nominal(SN,L) :- articulo(A),
                            lugar(L),
                            append(A,L,SN).
%Descripción: Con esta regla se puede formar un sintagma de lugar formado por un artículo y un lugar, que a su vez es un objeto. Ej: Una tienda
sintagma_nominal(SN,O) :- articulo(A),
                        objeto(O),
                        append(A,O,SN).
%Descripción: Con esta regla se puede formar un sintagma de lugar formado por una preposición y un artículo. Ej: En la
sintagma_nominal(SN) :- preposicion(P),
                        articulo(A),%a la
                        append(P,A,SN).
%_____________________________________________________________________________________________________________________  
%sintagma_verbal_lugar(X,Y), donde X es la representación del sintagma (lista de strings) y Y es el lugar que se detecta
%Descripción: Con esta regla se puede formar un sintagma verbal de lugar formado solamente por el verbo de lugar. Ej: Ir
sintagma_verbal_lugar(SV,SV) :- verbo_lugar(SV).
%Descripción: Con esta regla se puede formar un sintagma de lugar formado por el verbo de lugar y un sintagma_nominal de lugar. Ej: Estoy en Aserri
sintagma_verbal_lugar(SV,L) :- verbo_lugar(V),%ir a Narnia /ir Narnia
                               sintagma_nominal_lugar(SN,L),
                               append(V,SN,SV).
%_____________________________________________________________________________________________________________________  
%sintagma_verbal(X,Y), donde X es la representación del sintagma (lista de strings)
%Descripción: Con esta regla se puede formar un sintagma verbal formado por un verbo. Ej: Quiero
sintagma_verbal(SV):-verbo(SV).%quiero
%Descripción: Con esta regla se puede formar un sintagma verbal formado por un nombre y un verbo. Ej: Yo quiero
sintagma_verbal(SV):-nombre(N),
                     verbo(V),
                     append(N,V,SV).
%Descripción: Con esta regla se puede formar un sintagma verbal formado por un verbo y un conector. Ej: Tengo que
sintagma_verbal(SV):-verbo(L),
                     conector(C),
                     append(L,C,SV).
%_____________________________________________________________________________________________________________________

%                 ______________________________________________________
%________________/ Hechos para las oraciones

%preposición(X), donde X es una preposición representada con una lista con un solo string
preposicion(["a"]).
preposicion(["ante"]).
preposicion(["bajo"]).
preposicion(["de"]).
preposicion(["desde"]).
preposicion(["en"]).
preposicion(["entre"]).
preposicion(["para"]).
preposicion(["por"]).
preposicion(["en"]).

%nombre(X), donde X es un nombre representado con una lista con un solo string
nombre(["yo"]).
nombre(["me"]).
nombre(["se"]).

%conector(X), donde X es un conector representado con una lista con un solo string
conector(["que"]).
%artículo(X), donde X es un artículo representado con una lista con un solo string
articulo(["el"]).
articulo(["la"]).
articulo(["un"]).
articulo(["una"]).
articulo(["al"]).

%objeto(X), donde X es un objeto representado con una lista con un solo string. Los objetos son lugares no específicos
objeto(["supermercado"]).
objeto(["farmacia"]).
objeto(["hospital"]).
objeto(["tienda"]).

%lugar(X), donde X es un lugar representado con una lista con un solo string
lugar(["farmacia"]).
lugar(["tienda"]).
lugar(["hospital"]).
lugar(["supermercado"]).

lugar(["corralillo"]).
lugar(["cartago"]).
lugar(["pacayas"]).
lugar(["paraiso"]).
lugar(["cervantes"]).
lugar(["orosi"]).
lugar(["cachi"]).
lugar(["turrialba"]).

lugar(L):-list(L,H,T), lugar_compuesto(H),lugar_compuesto(T).

lugar_compuesto(["musgo"]).
lugar_compuesto(["verde"]).

lugar_compuesto(["san"]).
lugar_compuesto(["jose"]).

lugar_compuesto(["tres"]).
lugar_compuesto(["rios"]).

lugar_compuesto(["juan"]).
lugar_compuesto(["viñas"]).


%verbo(X), donde X es un verbo representado con una lista con un solo string
verbo(["quiero"]).
verbo(["gustaria"]).
verbo(["tengo"]).
verbo(["esta"]).

%verbo(X), donde X es un verbo de lugar representado con una lista con un solo string.
verbo_lugar(["esta"]).
verbo_lugar(["voy"]).
verbo_lugar(["encuentro"]).
verbo_lugar(["ubico"]).
verbo_lugar(["ubica"]).
verbo_lugar(["ir"]).
verbo_lugar(["estoy"]).
verbo_lugar(["pasar"]).
verbo_lugar(["parar"]).
verbo_lugar(["queda"]).
verbo_lugar(["ubicado"]).
verbo_lugar(["ubicada"]).



%                 ____________________________________________________________
%________________/ Reglas para la interfaz del usuario

%Descripción: usrLog() se encarga de tomar el camino construido y enviarselo al grafo para que analie la mejor ruta
usrLog():-usrOracion(Camino),!,wazeLogIn(Camino).
%Descripción: Escribe la oración inicial de WazeLog
wazeSaludo:-write("WazeLog: Bienvenido! WazeLog para servirle! \n").

%_____________________________________________________________________________________________________________________ 
%usrOración(X) donde X es el camino que se forma después de tener una conversación con el usuario 
%Descripción: Se encarga de llamar a todas las funciones que toman las respuestas del usuario y forma una lista con los lugares de origen, paradas y destino
usrOracion(Camino):- write("WazeLog: Primero digame; \n"),
                    
                     usrOrigen(Origen),
                     write("WazeLog: Muy bien, ahora; \n"),
                     usrDestino(Destino),
                     write("WazeLog: Excelente, finalmente; \n"),
                     write("WazeLog: Tiene alguna parada intermedia? \nUsuario: "),
                     usrParadas(Paradas),
                     !,           
                     append([[Origen],Paradas,[Destino]],Camino).   
%_____________________________________________________________________________________________________________________ 
%usrOrigen(X) donde X es el lugar de origen, en donde se encuentre el usuario 
%Descripción: Si la respuesta es una oración válida, encuentra el lugar de origen indicado por el usuario  
usrOrigen(Origen):- write("WazeLog: En donde se encuentra? \nUsuario: "),
                    read_line_to_string(user_input,Input),
                    string_lower(Input,NewInput),
                    split_string(NewInput, "\s", "\s", Lista), 
                    oracion(Lista,OrigenIn),
                    usrObjeto(OrigenIn,OrigenOut),
                    compound(OrigenOut,Origen), 
                    write("WazeLog: OK, esta en "),write(Origen),write(".\n").
%Descripción: Si la respuesta no es una oración válida le indica al usuario que no entendió y se vuelve a llamar recusivamente                    
usrOrigen(Origen):- write("WazeLog: Lo lamento pero no entendi. Por favor especifique una ubiacion valida. \n"),usrOrigen(Origen).
%_____________________________________________________________________________________________________________________  
%usrDestino(X) donde X es el lugar de destino, a donde quiere llegar el usuario 
%Descripción: Si la respuesta es una oración válida, encuentra el lugar de destino indicado por el usuario 
usrDestino(Destino):- write("WazeLog: A donde se dirige? \nUsuario: "),
                      read_line_to_string(user_input,Input),
                      string_lower(Input,NewInput),
                      split_string(NewInput, "\s", "\s", Lista), 
                      oracion(Lista,DestinoIn),
                      usrObjeto(DestinoIn,DestinoOut),
                      compound(DestinoOut,Destino), 
                      write("WazeLog: Entendido! Va para "),write(Destino),write(".\n").
%Descripción: Si la respuesta no es una oración válida le indica al usuario que no entendió y se vuelve a llamar recusivamente                     
usrDestino(Destino):- write("WazeLog: No entendi. Podria decirme, utilizando un lugar conocido; \n"),usrDestino(Destino).
%_____________________________________________________________________________________________________________________  
%usrParadas(X) donde X es una lista con todas las paradas que necesita hacer el usuario antes de llegar a su destino 
%Descripción: Recibe la respuesta del usuario y llama a todas las reglas auxiliares recursivas (paradas) para definir que hacer con la respuesta
usrParadas([Parada|Paradas]):- read_line_to_string(user_input,Input),
                               string_lower(Input,NewInput),
                               split_string(NewInput, "\s", "\s", Lista), 
                               paradas(NewInput,Lista,Parada,Paradas).
usrParadas([]).
%Descripción: Si la respuesta no es una oración válida le indica al usuario que no entendió y se vuelve a llamar recusivamente 
usrParadas([Parada|Paradas]):- write("WazeLog: No comprendi, lo siento. Por favor mencione una locacion valida. \n"),
                               write("WazeLog: Tiene una parada adicional? \nUsuario: "),
                               usrParadas([Parada|Paradas]).
%paradas(Input,Lista,Parada,Paradas) donde Input es la respuesta del usuario en forma de string,
%                                    Lista es la oración divida en una lista de palabras, 
%                                    Parada es la nuva parada que indica el usuario y
%                                    Paradas es una lista con todas las paradas

%Descripción: Si la respuesta del usuario es no, es decir no tiene más paradas, termina la recursión de paradas
paradas(Input,_,_,_):- not(dif(Input,"no")),!,fail.
%Descripción: Si la respuesta del usuario es si, es decir tiene más paradas, vuelve a llamar a usrParadas
paradas(Input,_,Parada,Paradas):- not(dif(Input,"si")),
                   write("WazeLog: Muy bien, cual seria? \nUsuario: "),
                   usrParadas([Parada|Paradas]).
%Descripción: Si la oración es válida, extrae el lugar y lo agrega a la lista de Paradas y vuelve a llamar a usrParadas  
paradas(_,Lista,Parada,Paradas):- oracion(Lista,ParadaLIn),
usrObjeto(ParadaLIn,ParadaL),
compound(ParadaL,Parada),
write("WazeLog: Anotado! Parada en "),write(Parada),write(".\n"),
write("WazeLog: Alguna otra parada adicional? \nUsuario: "),
usrParadas(Paradas).
%_____________________________________________________________________________________________________________________  
%usrObjeto(Lugar,X), donde X es es lugar indicado por el usuario, ya sea un objeto, y X es el lugar una vez que especifique
%Descripción: Si el lugar indicado es especifico y no un objeto, le asigna el valor de Lugar a X
usrObjeto(Lugar,X):-lugar(Lugar),not(objeto(Lugar)),X = Lugar.
%Descripción: Si el lugar indicado por el usuario no es lo suficientemente especifico, se llama recursivamente hasta que indique un lugar que se encuentre en el mapa
usrObjeto(Lugar,X):-lugar(Lugar),objeto(Lugar),
                    write("WazeLog: Por favor especifique donde queda (el/la) "),compound(Lugar,String), write(String), write("\nUsuario: "),
                    read_line_to_string(user_input,Input),
                    string_lower(Input,NewInput),
                    split_string(NewInput, "\s", "\s", Lista), 
                    oracion(Lista,L),
                    usrObjeto(L,X).

%Descripción: Si la respuesta no es válida, se le avisa al usuario y se vuelve a llamar usrObjeto
usrObjeto(Lugar,X):-write("WazeLog: No comprendi, lo siento. Por favor mencione una locacion valida. \n"),
                    usrObjeto(Lugar,X).
%_____________________________________________________________________________________________________________________  


%head(L,X) donde L es una lista y X es la cabeza de esa lista
head([H|_], H).
%list(Lista) separa una lista de dos elementos en dos listas con H y T
list([H,T],[H],[T]).
%compound(List,String) toma un nombre compuesto por dos partes en una lista y lo transforma en un string separado por un espacio
compound(List,String):-list(List,H,T),head(H,Primero),head(T,Segundo), string_concat(Primero," ",Space),string_concat(Space,Segundo,String),!.
compound(List,String):-head(List,String).


%                 ____________________________________________________________
%________________/ Regla para iniciar WazeLog
%Descripción: Junta la oración inicial con el usrLog
wazeLog:-wazeSaludo(),usrLog().
