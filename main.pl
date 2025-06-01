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

leer_palabra_oculta(Palabra) :- writeln('Jugador 2, no mire la pantalla hasta que el jugador 1 le diga'),
    writeln('Escriba la palabra oculta y presione "enter":'),
    read_line_to_string(user_input, Palabra).

leer_opcion(OpcionNum) :- read_line_to_string(user_input, OpcionStr),
    number_string(OpcionNum, OpcionStr).

validar_palabra(Palabra) :- string_chars(Palabra, Letras), 
    % la palabra debe contener solo caracteres alfabéticos y minúsculas:
    forall(member(Letra, Letras), (char_type(Letra, alpha), char_type(Letra, lower))).

seleccionar_modo(1, Palabra) :- seleccionar_palabra_random(Palabra),
    atom_length(Palabra, Longitud),
    format("Longitud de la palabra: ~d~n", [Longitud]).
seleccionar_modo(2, Palabra) :- writeln('Jugador 1, ingrese una palabra (solamente letras minúsculas): '),
    leer_palabra_oculta(Palabra),
    (validar_palabra(Palabra) -> true; 
        writeln('Palabra inválida. Intente de nuevo.'), seleccionar_modo(2, Palabra)
    ).

iniciar :- % PENDIENTE: agregar la opción de editar la cantidad de intentos.
    writeln('===== AHORCADO ====='),
    writeln('Seleccione el modo de juego:'),
    writeln('1. Un jugador (palabra aleatoria)'),
    writeln('2. Dos jugadores (una persona ingresa la palabra)'),
    write('Opcion:'),
    leer_opcion(Opcion),
    writeln('===================='),
    (member(Opcion, [1,2]) ->
        seleccionar_modo(Opcion, PalabraSecreta),
        % jugar(PalabraSecreta).  FALTA: CONECTAR CON LA LÓGICA DEL JUEGO
        write('La palabra secreta es: '),
        writeln(PalabraSecreta)  % SOLO PARA PRUEBAS
    ;
        writeln('Opción inválida. Intente de nuevo.'), iniciar
    ).