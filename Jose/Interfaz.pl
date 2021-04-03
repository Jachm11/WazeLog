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
verbo_lugar(["ir"]).
verbo_lugar(["estoy"]).
verbo_lugar(["pasar"]).
verbo_lugar(["parar"]).

lugares(Ele, [Ele|_]):- lugar([Ele]).
lugares(Ele, [_|Lista]):-lugares(Ele, Lista).

usrLog():-usrOracion(Camino),wazeLog(Camino).
wazeSaludo:-write("Bienvenido").
%pregunta:-  write("Donde está?: "),read(Input), lugares(Output,Input), write(Output).
usrOracion(Camino):- usrOrigen(Origen),
                     usrDestino(Destino),
                     usrParadas(Paradas),           
                     append([Origen,Paradas,Destino],Camino).
                     %wazeLog(["Cartago", "Dota", "Taras"]).    

usrOrigen(Origen):- write("Donde está?: "),
                    read_line_to_string(user_input,Input),
                    string_lower(Input,NewInput),
                    split_string(NewInput, "\s", "\s", Lista), 
                    oracion(Lista,Origen).
usrOrigen(Origen):- write("No entendi"),usrOrigen(Origen).
usrDestino(Destino):- write("Donde va?: "),
                      read_line_to_string(user_input,Input),
                      string_lower(Input,NewInput),
                      split_string(NewInput, "\s", "\s", Lista), 
                      oracion(Lista,Destino).

usrDestino(Destino):- write("No entendi"),usrDestino(Destino).
usrParadas([Parada|Paradas]):- write("Alguna parada?: "),
                               read_line_to_string(user_input,Input),
                               string_lower(Input,NewInput),
                               split_string(NewInput, "\s", "\s", Lista), 
                               
                               oracion(Lista,ParadaL),
                               head(ParadaL,Parada),

                               dif(NewInput,"no"),
                                
                               usrParadas(Paradas).
usrParadas([]).
usrParadas([Parada|Paradas]):- write("No entendi"),usrParadas([Parada|Paradas]).
head([H|_], H).