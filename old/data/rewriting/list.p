val cons : i -> list -> list.

append X nil --> X.
append (append X Y) Z --> append X (append Y Z).
append nil L --> L.
append (cons X L) L2 --> cons X (append L L2).

% flatten nil --> nil.
% flatten (cons X L) --> append X (flatten L).
% flatten (append X Y) --> append (flatten X) (flatten Y).

rev nil --> nil.
rev (cons X L) --> append (rev L) (cons X nil).
rev (append X Y) --> append (rev Y) (rev X).
rev (rev X) --> X.

% vim:syntax=prolog
