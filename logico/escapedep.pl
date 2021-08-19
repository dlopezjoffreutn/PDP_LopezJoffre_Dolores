persona(ale, 15, [claustrofobia, cuentasRapidas, amorPorLosPerros]).
persona(agus, 25, [lecturaVeloz, ojoObservador, minuciosidad]).
persona(fran, 30, [fanDeLosComics]).
persona(rolo, 12, []).


esSalaDe(elPayasoExorcista, salSiPuedes).
esSalaDe(socorro, salSiPuedes).
esSalaDe(linternas, elLaberintoso).
esSalaDe(guerrasEstelares, escapepepe).
esSalaDe(fundacionDelMulo, escapepepe).

sala(elPayasoExorcista, terrorifica(100, 18)).
sala(socorro, terrorifica(20, 12)).
sala(linternas, familiar(comics, 5)).
sala(guerrasEstelares, familiar(futurista, 7)).
sala(fundacionDelMulo, enigmatica([combinacionAlfanumerica, deLlave, deBoton])).


% Helpers
esPersonaNoClaustrofobica(Persona) :-
    persona(Persona, _, Peculiaridades),
    not(member(claustrofobia, Peculiaridades)).

esSalaTerrorifica(Sala) :-
    sala(Sala, terrorifica(_, _)).

promedio(Lista, Promedio) :-
    length(Lista, Cantidad),
    sum_list(Lista, Suma),
    Promedio is Suma / Cantidad.


% Punto 1: nivelDeDificultadDeLaSala/2 - Para cada sala nos dice su dificultad

nivelDeDificultadDeLaSala(Sala, NivelDeDificultad) :-
    sala(Sala, Experiencia),
    nivelDeDificultadDeLaExperiencia(Experiencia, NivelDeDificultad).


nivelDeDificultadDeLaExperiencia(terrorifica(CantidadDeSustos, EdadMinima), NivelDeDificultad) :-
    NivelDeDificultad is (CantidadDeSustos - EdadMinima).

nivelDeDificultadDeLaExperiencia(familiar(futurista, _), 15).
nivelDeDificultadDeLaExperiencia(familiar(Tematica, CantidadDeHabitaciones), CantidadDeHabitaciones) :-
    Tematica \= futurista.

nivelDeDificultadDeLaExperiencia(enigmatica(Candados), NivelDeDificultad) :-
    length(Candados, NivelDeDificultad).


% Punto 2: puedeSalir/2 - Nos dice si una persona puede salir de una sala

puedeSalir(Persona, Sala) :-
    esPersonaNoClaustrofobica(Persona),
    personaPuedeSalir(Persona, Sala).

personaPuedeSalir(_, Sala) :-
    nivelDeDificultadDeLaSala(Sala, 1).

personaPuedeSalir(Persona, Sala) :-
    persona(Persona, Edad, _), 
    Edad > 13,
    nivelDeDificultadDeLaSala(Sala, NivelDeDificultad),
    NivelDeDificultad < 5.


% Punto 4: esMacabra/1 - Nos dice si todas las salas de una empresa son terrorificas
esMacabra(Empresa) :-
    esSalaDe(_, Empresa),
    forall(esSalaDe(Sala, Empresa), esSalaTerrorifica(Sala)).


% Punto 5: esCopada/1 - Nos dice si una empresa no es macabra y tiene un promedio de dificultad de 4
esCopada(Empresa) :-
    not(esMacabra(Empresa)),
    promedioDeDificultadDeEmpresa(Empresa, Promedio).

promedioDeDificultadDeEmpresa(Empresa, Promedio) :-
    esSalaDe(_, Empresa),
    promedioDeDificultadDeSalas(Empresa, Promedio),
    Promedio < 4.

promedioDeDificultadDeSalas(Empresa, Promedio) :-
    findall(NivelDeDificultad, nivelDeDificultadDeLaEmpresa(Empresa, Sala, NivelDeDificultad), ListaDeDificultades),
    promedio(ListaDeDificultades, Promedio).

nivelDeDificultadDeLaEmpresa(Empresa, Sala, NivelDeDificultad) :-
    esSalaDe(Sala, Empresa), 
    nivelDeDificultadDeLaSala(Sala, NivelDeDificultad).


% Punto 6: Nuevas empresas y salas
sala(estrellaDePeleas, familiar(videojuegos, 7)).
sala(miseriaDeLaNoche, terrorifica(21, 150)).

esSalaDe(estrellaDePeleas, supercelula).
esSalaDe(choqueDeLaRealeza, supercelula).
esSalaDe(miseriaDeLaNoche, skPista).
