% Sesion de laboratorio 6
% Martin Fernandez de Diego


% Problema 1
% Usando igualdad sintactica
elimina1([ ],X,[ ]).
elimina1([X|R],Y,NR) :- Y == X, elimina1(R,Y,NR).
elimina1([X|R],Y,[X|NR]) :- Y \== X, elimina1(R,Y,NR).
% Usando unificacion
elimina2([ ],X,[ ]).
elimina2([X|R],Y,NR) :- Y = X, elimina2(R,Y,NR).
elimina2([X|R],Y,[X|NR]) :- Y \= X, elimina2(R,Y,NR).
% Combinando las dos anteriores
elimina3([ ],X,[ ]).
elimina3([X|R],X,NR) :- elimina3(R,X,NR).
elimina3([X|R],Y,[X|NR]) :- Y \== X, elimina3(R,Y,NR).

% ?- elimina1([a,b,a,c],a,L).
% L = [b, c].
%
% ?- elimina1([a,b,a,c],X,L).
% L = [a, b, a, c].
%
% 1. El comportamiento para un valor dado de argumento:
%    Como X esta instanciada, ==,\== devuelven true o false normalmente.
% 2. El comportamiento al recibir una variable X:
%    Como X no esta instanciada, la igualdad == devolvera false.
%    Como X no esta instanciada, la desigualdad \== devolvera true.
%
% ?- elimina2([a,b,a,c],a,L).
% L = [b, c].
%
% ?- elimina2([a,b,a,c],X,L).
% X = a,
% L = [b, c].
%
% 1. Como X esta instanciada, =,\= devuelven true o false normalmente.
% 2. Como X no esta instanciada,
%    toma el valor del primer elemento del vector y continua el resto
%    de la evaluacion con ese valor asignado.
%
% ?- elimina3([a,b,a,c],a,L).
% L = [b, c].
%
% ?- elimina3([a,b,a,c],X,L).
% X = a,
% L = [b, c] ;
% X = b,
% L = [a, a, c] ;
% X = a,
% L = [a, b, c] ;
% X = c,
% L = [a, b, a] ;
% L = [a, b, a, c].
%
% 1. Como X esta instanciada, al unificar X siempre tomara valor a.
% 2. Como X no esta instanciada,
%    X unificara en cada posible valor hasta agotar las posibilidades.


% Problema 2
% a)
sumatree(_,0).
sumatree(arbol(R,I,D),N):-
    sumatree(I,NI),
    sumatree(D,ND),
    N1 is NI + ND,
    N is N1 + R.

% b)
maximo(_,0).
maximo(arbol(R,I,D),X):-
    maximo(I,XI),
    maximo(D,XD),
    X1 is max(XI,XD),
    X is max(X1,R).


% Problema 3
sublista([ ],[ ]).
sublista([_|Xs],Ys):-sublista(Xs,Ys).
sublista([X|Xs],[X|Ys]):-sublista(Xs,Ys).