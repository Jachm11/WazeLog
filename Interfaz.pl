
oracion(O) :- sintagma_nominal(SN),
              sintagma_verbal(SV),
              append(SN,SV,O).
oracion(O) :- sintagma_nominal(SN),
              sintagma_verbal(SV),
              append(SV,SN,O).
oracion(O,L) :- sintagma_destino(SD),
                lugar(L), 
                append(SD,L,O).
oracion(O,L) :- sintagma_origen(SO),
                lugar(L), 
                append(SO,L,O).
oracion(O) :- negacion(O).
sintagma_nominal(SN) :- nombre(SN).
sintagma_nominal(SN) :- articulo(A),
                        nombre(N),
                        append(A,N,SN).
sintagma_nominal(SN) :- articulo(A),
                        lugar(L),
                        append(A,L,SN).
sintagma_destino(SD) :- verbo_destino(V),
                        preposicion(P),
                        append(V,P,SD).
sintagma_origen(SD) :- verbo_origen(V),
                        preposicion(P),
                        append(V,P,SD).
sintagma_verbal(SV) :- verbo(V).
sintagma_verbal(SV) :- verbo(V),
                        sintagma_nominal(SN),
                        append(V,SN,SV).

negacion(["no"]).
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
articulo(["el"]).

nombre(["supermercado"]).
nombre(["perro"]).
nombre(["pescado"]).
nombre(["carne"]).

lugar(["TresRios"]).
lugar(["Aserri"]).
lugar(["PasoAncho"]).
lugar(["Cartago"]).

verbo(["quiero"]).
verbo_destino(["voy"]).
verbo_destino(["ir"]).
verbo_origen(["estoy"]).
verbo_origen(["pasar"]).

lugares(Ele, [Ele|_]):- lugar([Ele]).
lugares(Ele, [_|Lista]):-lugares(Ele, Lista).

%pregunta:-  write("Donde está?: "),read(Input), lugares(Output,Input), write(Output).
usrOracion(Camino):- usrOrigen(Origen),
                     usrDestino(Destino),
                     usrParadas(Paradas),           
                     append([Origen,Paradas,Destino],Camino).    

usrOrigen(Origen):- write("Donde está?: "),
                    read_line_to_string(user_input,Input),
                    split_string(Input, "\s", "\s", Lista), 
                    oracion(Lista,Origen).
usrDestino(Destino):- write("Donde va?: "),
                      read_line_to_string(user_input,Input),
                      split_string(Input, "\s", "\s", Lista), 
                      oracion(Lista,Destino).


usrParadas([Parada|Paradas]):- write("Alguna parada?: "),
                      read_line_to_string(user_input,Input),
                      split_string(Input, "\s", "\s", Lista), 
                      oracion(Lista,ParadaL),
                      head(ParadaL,Parada),

                      dif(Input,"no"),
                      
                      usrParadas(Paradas).
usrParadas([]).

head([H|_], H).