% Proyecto 3 de Lenguajes de Programación
% Andrés Bonilla y Emily Sanchez
:- dynamic letra_adivinada/1.
:- dynamic intentos_restantes/1.
:- use_module(library(random)).

% ====== BASE DE PALABRAS ======
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

% ====== UTILIDADES ======

% Obtener el elemento en la posición Pos de una lista (0-based)
elemento_en_posicion(0, [H|_], H).
elemento_en_posicion(Pos, [_|Resto], Elem) :-
    Pos > 0,
    Pos1 is Pos - 1,
    elemento_en_posicion(Pos1, Resto, Elem).

% Seleccionar una palabra al azar de la base
seleccionar_palabra_random(PalabraSeleccionada) :-
    findall(P, palabra(P), Lista),
    length(Lista, N),
    random(0, N, Idx),
    elemento_en_posicion(Idx, Lista, PalabraSeleccionada).

% Leer palabra oculta en modo 2 jugadores
leer_palabra_oculta(Palabra) :- 
    writeln('Jugador 2, NO MIRE LA PANTALLA'),
    writeln('Jugador 1, escribe la palabra oculta (solo letras minusculas) y presiona ENTER:'),
    read_line_to_string(user_input, Palabra),
    string_length(Palabra, Len),
    length(Asteriscos, Len),
    maplist(=('*'), Asteriscos),
    string_chars(AsteriscosStr, Asteriscos),

    % Simular limpieza de pantalla para que el que juega no vea la palabra
    forall(between(1, 40, _), nl),

    format('Palabra registrada: ~s~n', [AsteriscosStr]),
    writeln('Comienza el juego...').


% Mostrar asteriscos de la misma longitud de la palabra
mostrar_asteriscos(Palabra) :-
    string_length(Palabra, L),
    generar_asteriscos(L),
    nl.

generar_asteriscos(0).
generar_asteriscos(N) :-
    N > 0,
    write('*'),
    N1 is N - 1,
    generar_asteriscos(N1).

% Leer opción numérica en el menú
leer_opcion(OpcionNum) :-
    read_line_to_string(user_input, Str),
    number_string(OpcionNum, Str).

% Validar que la palabra contenga solo letras minúsculas
validar_palabra(Palabra) :-
    string_chars(Palabra, Letras),
    forall(member(C, Letras), (char_type(C, alpha), char_type(C, lower))).

% Mostrar la palabra con guiones para letras no adivinadas
mostrar_palabra([], _):- nl.
mostrar_palabra([L|R], Adivinadas) :-
    (   member(L, Adivinadas)
    ->  write(L)
    ;   write('_')
    ),
    write(' '),
    mostrar_palabra(R, Adivinadas).

% Verificar si la palabra está completa
palabra_completa([], _).
palabra_completa([L|R], Adivinadas) :-
    member(L, Adivinadas),
    palabra_completa(R, Adivinadas).

% Solicitar una letra al jugador
solicitar_letra(L) :-
    write('Ingresa UNA letra minuscula: '),
    read_line_to_string(user_input, S),
    string_lower(S, Low),
    string_chars(Low, [L|_]).

% Inicializar estado (limpiar letras adivinadas y setear intentos)
inicializar_estado(Max) :-
    retractall(letra_adivinada(_)),
    retractall(intentos_restantes(_)),
    asserta(intentos_restantes(Max)).

% Dibujos del muñeco en 8 etapas (0..7 fallos)
dibujar(0) :-
    writeln('  +---+'),
    writeln('  |   |'),
    writeln('      |'),
    writeln('      |'),
    writeln('      |'),
    writeln('      |'),
    writeln('========='), nl.
dibujar(1) :-
    writeln('  +---+'),
    writeln('  |   |'),
    writeln('  O   |'),
    writeln('      |'),
    writeln('      |'),
    writeln('      |'),
    writeln('========='), nl.
dibujar(2) :-
    writeln('  +---+'),
    writeln('  |   |'),
    writeln('  O   |'),
    writeln('  |   |'),
    writeln('      |'),
    writeln('      |'),
    writeln('========='), nl.
dibujar(3) :-
    writeln('  +---+'),
    writeln('  |   |'),
    writeln('  O   |'),
    writeln(' /|   |'),
    writeln('      |'),
    writeln('      |'),
    writeln('========='), nl.
dibujar(4) :-
    writeln('  +---+'),
    writeln('  |   |'),
    writeln('  O   |'),
    writeln(' /|\\  |'),
    writeln('      |'),
    writeln('      |'),
    writeln('========='), nl.
dibujar(5) :-
    writeln('  +---+'),
    writeln('  |   |'),
    writeln('  O   |'),
    writeln(' /|\\  |'),
    writeln(' /    |'),
    writeln('      |'),
    writeln('========='), nl.
dibujar(6) :-
    writeln('  +---+'),
    writeln('  |   |'),
    writeln('  O   |'),
    writeln(' /|\\  |'),
    writeln(' / \\  |'),
    writeln('      |'),
    writeln('========='), nl.
dibujar(7) :-
    writeln('  +---+'),
    writeln('  |   |'),
    writeln('  X   |   <-- ¡Colgado!'),
    writeln(' /|\\  |'),
    writeln(' / \\  |'),
    writeln('      |'),
    writeln('========='), nl.

% ====== LÓGICA DEL JUEGO ======

jugar(PalabraAtom) :-
    atom_chars(PalabraAtom, Palabra),
    inicializar_estado(7),
    ciclo_juego(Palabra, PalabraAtom).

ciclo_juego(Palabra, PalabraAtom) :-
    intentos_restantes(Intentos),
    findall(C, letra_adivinada(C), Adivinadas),
    Fallos is 7 - Intentos,
    dibujar(Fallos),
    writeln('Palabra:'),
    mostrar_palabra(Palabra, Adivinadas),
    format('Intentos restantes: ~d~n', [Intentos]),
    (
        palabra_completa(Palabra, Adivinadas) ->
            writeln('Felicidades - Adivinaste la palabra.'), preguntar_reiniciar
    ;
        Intentos =< 0 ->
            write('Has perdido - La palabra era: '), writeln(PalabraAtom), preguntar_reiniciar
    ;
        solicitar_letra(L),
        (
            member(L, Adivinadas) ->
                writeln('Ya ingresaste esa letra.'), ciclo_juego(Palabra, PalabraAtom)
        ;
            (   member(L, Palabra) ->
                asserta(letra_adivinada(L)),
                writeln('CORRECTO'), ciclo_juego(Palabra, PalabraAtom)
            ;
                asserta(letra_adivinada(L)),
                writeln('INCORRECTO'),
                intentos_restantes(Prev), New is Prev - 1,
                retract(intentos_restantes(_)),
                asserta(intentos_restantes(New)),
                ciclo_juego(Palabra, PalabraAtom)
            )
        )
    ).

% Preguntar si se desea reiniciar o salir
preguntar_reiniciar :-
    writeln(''),
    writeln('Deseas jugar de nuevo (s/n)'),
    read_line_to_string(user_input, R),
    string_lower(R, Resp),
    (   Resp = "s" -> iniciar
    ;   writeln('¡Gracias por jugar!'), halt
    ).

% ====== PUNTO DE ENTRADA ======

iniciar :-
    nl,
    writeln('========================================'),
    writeln('            JUEGO AHORCADO              '),
    writeln('========================================'), nl,
    writeln('1. Un jugador (palabra aleatoria)'),
    writeln('2. Dos jugadores (ingresar palabra oculta)'),
    writeln('3. Salir'),
    write('Selecciona una opcion (1/2/3): '),
    leer_opcion(Opc),
    nl,
    (   Opc =:= 1 ->
        seleccionar_palabra_random(P) ,
        atom_length(P, L),
        format('Se ha seleccionado una palabra de longitud ~d~n', [L]),
        jugar(P)
    ;   Opc =:= 2 ->
        leer_palabra_oculta(P2),
        jugar(P2)
    ;   Opc =:= 3 ->
        writeln('¡Hasta luego!'), halt
    ;   writeln('Opcion invalida, intenta de nuevo.'), iniciar
    ).
