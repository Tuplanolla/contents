# Formal Grammar

The grammar of wrapped two-column tables is not entirely trivial even though
one might expect that to be the case.
The formal grammar is context-free and
can be expressed with the following type-annotated productions.

    C -> E* : [(String, [String])]
    E -> K V : (String, [String])
    K -> ~W (~(S S | L)* ~W)? : String
    V -> S S X | L Y : [String]
    X -> S* ~W (~L* ~W)? L Z? : [String]
    Y -> S X | L Y : [String]
    Z -> S X | L Z? : [String]
    W -> L | S : String
    L -> R N | R | N : String
    S -> " " : String
    R -> "\r" : String
    N -> "\n" : String

The productions can be renamed and rearranged to get a better looking result.
Turns out there is only one word in the English language that
does not feature the same letter more than once,
contains all the terminal symbols S, R and N,
does not contain any of the ambiguous symbols I or O and
has a matching length.

    T -> H* : [(String, [String])]
    H -> U D : (String, [String])
    U -> ~P (~(S S | L)* ~P)? : String
    N -> "\n" : String
    D -> S S E | L C : [String]
    E -> S* ~P (~L* ~P)? L A? : [String]
    R -> "\r" : String
    C -> S E | L C : [String]
    L -> R N | R | N : String
    A -> S E | L A? : [String]
    P -> L | S : String
    S -> " " : String
