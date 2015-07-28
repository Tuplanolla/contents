# Formal Grammar

## Tables

The grammar of wrapped two-column tables is not entirely trivial even though
one might expect that to be the case.
The formal grammar is context-free and
can be expressed with the following type-annotated productions.

    T -> E* : [(String, [String])] -- Table
    E -> K V : (String, [String]) -- Entry
    K -> ~W (~(S S | L)* ~W)? : String -- Key
    V -> S S C | L F : [String] -- Value
    C -> S* ~W (~L* ~W)? L Z? : [String] -- ContinuedLine
    F -> S C | L F : [String] -- FiniteLine
    Z -> S C | L Z? : [String] -- ZeroLine
    W -> L | S : String -- WhiteSpace
    L -> R N | R | N : String -- LineBreak
    S -> " " : String -- Space
    R -> "\r" : String -- ReturnCarriage
    N -> "\n" : String -- NewLine

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
