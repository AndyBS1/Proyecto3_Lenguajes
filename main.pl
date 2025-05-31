% Proyecto 3 de Lenguajes de Programación
% Andrés Bonilla y Emily Sanchez

palabra('clase').
palabra('esqueleto').
palabra('guitarra').
palabra('presente').
palabra('tercero').
palabra('biblioteca').
palabra('tatuaje').
palabra('lenguaje').
palabra('tortuga').
palabra('regalo').

elemento_en_posicion(0, [H|_], H).
elemento_en_posicion(Pos, [_|RestoLista], Elemento) :- Pos > 0, PosAnterior is Pos - 1, elemento_en_posicion(PosAnterior, RestoLista, Elemento).

:- use_module(library(random)).

seleccionar_palabra_random(PalabraSeleccionada) :- findall(Palabra, palabra(Palabra), ListaPalabras),
    length(ListaPalabras, TotalPalabras),
    random(0, TotalPalabras, NumRandom),
    elemento_en_posicion(NumRandom, ListaPalabras, PalabraSeleccionada).