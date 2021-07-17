
viveEnLaMansionDreadbury(tiaAgatha).
viveEnLaMansionDreadbury(mayordomo).
viveEnLaMansionDreadbury(charles).

esOdiadoPor(charles,Persona) :-
    viveEnLaMansionDreadbury(Persona),
    not(esOdiadoPor(tiaAgatha,Persona)).

esOdiadoPor(tiaAgatha,Persona) :-
    viveEnLaMansionDreadbury(Persona),
    Persona \= mayordomo.

esOdiadoPor(mayordomo,Persona) :-
    esOdiadoPor(tiaAgatha,Persona).

esMasRicoQue(tiaAgatha,Persona) :-
    viveEnLaMansionDreadbury(Persona),
    not(esOdiadoPor(mayordomo,Persona)).

mataA(Victima,Asesino) :-
    viveEnLaMansionDreadbury(Asesino),
    esOdiadoPor(Asesino,Victima),
    not(esMasRicoQue(Victima,Asesino)).

/*

EJERCICIO 1
a) El programa debe resolver el problema de quién mató a la tía Agatha. 
b) Mostrar la consulta utilizada y la respuesta obtenida.

Se utilizo la consulta -> mataA(tiaAgatha,Alguien).
La consola responde que la tiaAgatha se suicidó.

EJERCICIO 2
a) Agregar los mínimos hechos y reglas necesarios para poder consultar.
b) Mostrar las consultas utilizadas para conseguir lo anterior, junto con las respuestas obtenidas.

- Existe alguien que odie a Milhouse.
Se utiliza la consulta -> esOdiadoPor(_,milhouse).
La consola responde False.

- A quien odia Charles.
Se utiliza la consulta -> esOdiadoPor(charles,Alguien).
La consola responde Alguien = mayordomo.

- El nombre de quien odia a Agatha.
Se utiliza la consulta -> esOdiadoPor(Alguien,tiaAgatha).
La consola responde Alguien = tiaAgatha; Alguien = mayordomo.

- Todos los odiadores y sus odiados.
Se utiliza la consulta para el odiador Charles -> esOdiadoPor(charles,Alguien).
La consola responde Alguien = mayordomo.
Se utiliza la consulta para el odiador tiaAgatha -> esOdiadoPor(tiaAgatha,Alguien).
La consola responde Alguien = tiaAgatha; Alguien = charles.
Se utiliza la consulta para el odiador mayordomo -> esOdiadoPor(mayordomo,Alguien).
La consola responde Alguien = tiaAgatha; Alguien = charles.

- Es cierto que el mayordomo odia a alguien.
Se utiliza la consulta -> esOdiadoPor(mayordomo,_).
La consola responde True.

*/


   
