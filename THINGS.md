# Formal Grammar

## Tables

The grammar of wrapped two-column tables is not entirely trivial even though
one might expect that to be the case.
The formal grammar is context-free and
can be expressed with the following type-annotated productions.

    T -> E* : [(String, [String])] -- Table
    E -> K V : (String, [String]) -- Entry
    K -> ~(W | D) (~(S S | L | D)* ~W)? : String -- Key
    V -> S S C | L P : [String] -- Value
    C -> S* ~W (~L* ~W)? L Z? : [String] -- ContinuedLine
    P -> S C | L P : [String] -- PositiveLine
    Z -> S C | L Z? : [String] -- ZeroLine
    W -> L | S : String -- WhiteSpace
    L -> R N | R | N : String -- LineBreak
    D -> F | B : String -- Delimiter
    S -> " " : String -- Space
    R -> "\r" : String -- ReturnCarriage
    N -> "\n" : String -- NewLine
    F -> "/" : String -- ForwardSlash
    B -> "\\" : String -- BackwardSlash

The productions can be renamed and rearranged to get a better looking result.
Turns out there is only one word in the English language that
does not feature the same letter more than once,
contains all the terminal symbols S, R and N,
does not contain any of the ambiguous symbols I or O and
has a matching length.
