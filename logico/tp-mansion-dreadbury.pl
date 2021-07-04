viveEnMansion(agatha).
viveEnMansion(mayordomo). 
viveEnMansion(charles).
viveEnMansion(milhouse).  

odia(agatha, Persona) :-
    viveEnMansion(Persona),
    Persona \= mayordomo.

odia(charles, Persona) :-
    viveEnMansion(Persona),
    not(odia(agatha, Persona)).

odia(mayordomo, Persona) :-
    viveEnMansion(Persona),
    odia(agatha, Persona).

masRicoQueAgatha(Persona) :-
    viveEnMansion(Persona),
    not(odia(mayordomo, Persona)).

mataAAgatha(Asesino) :-
    viveEnMansion(Asesino),
    odia(Asesino, agatha), 
    not(masRicoQueAgatha(Asesino)).


% Punto 1: ¿Quién mató a la Tía Agatha? Agatha se suicida.
/* 
    ?- mataAAgatha(Asesino).
    Asesino = agatha ; 
*/

% Punto 2A: Existe alguien que odie a milhouse
/* 
    ?- odia(_, milhouse).
    true ;
    -----------------------------------
    ?- odia(Persona, milhouse).
    Persona = agatha ;
    Persona = mayordomo.
*/

% Punto 2B: A quién odia charles
/* 
    ?- odia(charles, Persona).
    Persona = mayordomo ;
*/
    
% Punto 2C: Quién odia a Tía Agatha
/* 
    ?- odia(Persona, agatha).
    Persona = agatha ;
    Persona = mayordomo.
*/

% Punto 2D: Todos los odiadores y sus odiados
/* 
    ?- odia(Odiador, Odiado).
    Odiador = Odiado, Odiado = agatha ;
    Odiador = agatha, Odiado = charles ;
    Odiador = agatha, Odiado = milhouse ;
    -----------------------------------
    Odiador = charles, Odiado = mayordomo ;
    -----------------------------------
    Odiador = mayordomo, Odiado = agatha ;
    Odiador = mayordomo, Odiado = charles ;
    Odiador = mayordomo, Odiado = milhouse.
*/

% Punto 2E: Es cierto que el mayordomo odia a alguien
/* 
    ?- odia(mayordomo, _).
    true ;
    -----------------------------------
    ?- odia(mayordomo, Persona).
    Persona = agatha ;
    Persona = charles ;
    Persona = milhouse.
*/