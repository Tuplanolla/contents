# Formal Grammar

The grammar of wrapped two-column tables is context-free and
its productions can be arranged to form the anagram "big thunderclaps".

    B -> E | I[U] (S S C | L P)
    C -> S* D
    D -> I[T] L (A | B)
    P -> G[P]
    A -> G[A | B]
    L -> R N | R | N
    U -> ~(S S | R | N)
    H -> ~(S | R | N)
    T -> ~(R | N)
    I[X] -> H | H X* H
    G[X] -> L X | S+ D
    S -> ' '
    R -> '\r'
    N -> '\n'
    E -> eof

It can be expanded

    B -> E | (H | H U* H) (S S C | L P)
    C -> S* D
    D -> (H | H T* H) L (A | B)
    P -> L P | S+ D
    A -> L (A | B) | S+ D
    L -> R N | R | N
    U -> ~(S S | R | N)
    H -> ~(S | R | N)
    T -> ~(R | N)
    S -> ' '
    R -> '\r'
    N -> '\n'
    E -> eof

and converted into LL(2) form.

    B -> E | H (S S C | L P | U* H (S S C | L P))
    C -> S* D
    D -> H (L | T* H L) (A | B)
    P -> L P | S+ D
    A -> L (A | B) | S+ D
    L -> R N | R | N
    U -> ~(S S | R | N)
    H -> ~(S | R | N)
    T -> ~(R | N)
    S -> ' '
    R -> '\r'
    N -> '\n'
    E -> eof
