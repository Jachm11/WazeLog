:-include('grafo.pl').

oracion(O,L) :- nombre(N),
                 sintagma_verbal_lugar(SV,L),
                 append(N,SV,O).
oracion(O,L) :- nombre(N),
                sintagma_nominal_lugar(SV,L),
                append(N,SV,O).
oracion(O,L) :- sintagma_verbal_lugar(O,L).
oracion(O,L) :- sintagma_nominal_lugar(O,L).
oracion(O,L) :- sintagma_verbal(SV),
                sintagma_nominal_lugar(SNL,L),
                append(SV,SNL,O).
oracion(O,L) :- sintagma_verbal(SV),
                sintagma_verbal_lugar(SVL,L),
                append(SV,SVL,O).

sintagma_nominal_lugar(SN,SN) :- lugar(SN). %Narnia
sintagma_nominal_lugar(SN,L) :- preposicion(A),%a Narnia
                                lugar(L),
                                append(A,L,SN).
sintagma_nominal_lugar(SNL,L) :- sintagma_nominal(SN),%a la farmacia
                                lugar(L),   
                                append(SN,L,SNL).
sintagma_nominal(SN) :- preposicion(P),
                        articulo(A),%a la
                        append(P,A,SN).

sintagma_verbal_lugar(SV,SV) :- verbo_lugar(SV).%ir
sintagma_verbal_lugar(SV,L) :- verbo_lugar(V),%ir a Narnia /ir Narnia
                        sintagma_nominal_lugar(SN,L),
                        append(V,SN,SV).

sintagma_verbal(SV):-verbo(SV).%quiero
sintagma_verbal(SV):-nombre(N),%me gustaria
                     verbo(V),
                     append(N,V,SV).
sintagma_verbal(SV):-verbo(L),
                     conector(C),
                     append(L,C,SV).

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

nombre(["yo"]).
nombre(["me"]).
conector(["que"]).
articulo(["el"]).
articulo(["la"]).
objeto(["supermercado"]).
lugar(["supermercado"]).
objeto(["farmacia"]).
lugar(["farmacia"]).
lugar(["tresrios"]).
lugar(["aserri"]).
lugar(["pasoancho"]).
lugar(["cartago"]).
lugar(["taras"]).
lugar(["dota"]).
lugar(["sanpedro"]).
lugar(["curridabat"]).
lugar(["patarra"]).
lugar(["narnia"]).

verbo(["quiero"]).
verbo(["gustaria"]).
verbo(["tengo"]).
verbo_lugar(["voy"]).
verbo_lugar(["encuentro"]).
verbo_lugar(["ubico"]).
verbo_lugar(["ir"]).
verbo_lugar(["estoy"]).
verbo_lugar(["pasar"]).
verbo_lugar(["parar"]).

lugares(Ele, [Ele|_]):- lugar([Ele]).
lugares(Ele, [_|Lista]):-lugares(Ele, Lista).

usrLog():-usrOracion(Camino),!,wazeLogIn(Camino).
wazeSaludo:-write("WazeLog: Bienvenido! WazeLog para servirle! \n").

%pregunta:-  write("Donde está?: "),read(Input), lugares(Output,Input), write(Output).
usrOracion(Camino):- write("WazeLog: Primero digame; \n"),
                    
                     usrOrigen(Origen),
                     write("WazeLog: Muy bien, ahora; \n"),
                     usrDestino(Destino),
                     write("WazeLog: Excelente, finalmente; \n"),
                     write("WazeLog: Tiene alguna parada intermedia? \nUsuario: "),
                     usrParadas(Paradas),
                     !,           
                     append([Origen,Paradas,Destino],Camino).   

usrOrigen(Origen):- write("WazeLog: En donde se encuentra? \nUsuario: "),
                    read_line_to_string(user_input,Input),
                    string_lower(Input,NewInput),
                    split_string(NewInput, "\s", "\s", Lista), 
                    oracion(Lista,OrigenIn),
                    usrObjeto(OrigenIn,Origen),
                    head(Origen,Org), 
                    write("WazeLog: OK, esta en "),write(Org),write(".\n").
                    
usrOrigen(Origen):- write("WazeLog: Lo lamento pero no entendi. Por favor especifique una ubiacion valida. \n"),usrOrigen(Origen).

usrDestino(Destino):- write("WazeLog: A donde se dirige? \nUsuario: "),
                      read_line_to_string(user_input,Input),
                      string_lower(Input,NewInput),
                      split_string(NewInput, "\s", "\s", Lista), 
                      oracion(Lista,DestinoIn),
                      usrObjeto(DestinoIn,Destino),
                      head(Destino,Dest), 
                      write("WazeLog: Entendido! Va para "),write(Dest),write(".\n").
                    
usrDestino(Destino):- write("WazeLog: No entendi. Podria decirme, utilizando un lugar conocido; \n"),usrDestino(Destino).

usrParadas([Parada|Paradas]):- read_line_to_string(user_input,Input),
                               string_lower(Input,NewInput),
                               split_string(NewInput, "\s", "\s", Lista), 
                               paradas(NewInput,Lista,Parada,Paradas).
usrParadas([]).

usrParadas([Parada|Paradas]):- write("WazeLog: No comprendi, lo siento. Por favor mencione una locacion valida. \n"),
                               write("WazeLog: Tiene una parada adicional? \nUsuario: "),
                               usrParadas([Parada|Paradas]).

paradas(_,Lista,Parada,Paradas):- oracion(Lista,ParadaLIn),
                                  usrObjeto(ParadaLIn,ParadaL),
                                  head(ParadaL,Parada),
                                  write("WazeLog: Anotado! Parada en "),write(Parada),write(".\n"),
                                  write("WazeLog: Alguna otra parada adicional? \nUsuario: "),
                                  usrParadas(Paradas).

paradas(Input,_,_,_):- not(dif(Input,"no")),!,fail.

paradas(Input,_,Parada,Paradas):- not(dif(Input,"si")),
                   write("WazeLog: Muy bien, cual seria? \nUsuario: "),
                   usrParadas([Parada|Paradas]).

usrObjeto(Lugar,X):-lugar(Lugar),not(objeto(Lugar)),X = Lugar.
usrObjeto(Lugar,X):-lugar(Lugar),objeto(Lugar),
                    write("WazeLog: Por favor especifique donde queda (el/la) "),head(Lugar,H), write(H), write("\nUsuario: "),
                    read_line_to_string(user_input,Input),
                    string_lower(Input,NewInput),
                    split_string(NewInput, "\s", "\s", Lista), 
                    oracion(Lista,L),
                    usrObjeto(L,X).


usrObjeto(Lugar,X):-write("WazeLog: No comprendi, lo siento. Por favor mencione una locacion valida. \n"),
                    usrObjeto(Lugar,X).





head([H|_], H).

wazeLog:-wazeSaludo(),usrLog().
