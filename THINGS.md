# Formal Grammar

The grammar of wrapped two-column tables is context-free, so
it can be written with the first-order productions

    A -> E | (P | P K* P) (S S C | N F)
    C -> S* (P | P V* P) N (Z | A)
    Z -> (N | S+ (P | P V* P) N) (Z | A)
    F -> N F | S+ (P | P V* P) N (Z | A)
    K -> ~(S S | R | N)
    V -> ~(R | N)
    P -> ~(S | R | N)
    L -> R N | R | N
    S -> ' '
    R -> '\r'
    N -> '\n'
    E -> eof

or the equivalent higher-order productions

    A -> E | W[K] (S S C | N F)
    C -> S* D
    D -> W[V] N (Z | A)
    Z -> G[Z | A]
    F -> G[F]
    K -> ~(S S | R | N)
    V -> ~(R | N)
    P -> ~(S | R | N)
    L -> R N | R | N
    W[X] -> P | P X* P
    G[X] -> N X | S+ D
    S -> ' '
    R -> '\r'
    N -> '\n'
    E -> eof

that can then be arranged into

    B -> E | I[U] (S S C | N H)
    I[X] -> P | P X* P
    G[X] -> N X | S+ D
    T -> ~(R | N)
    H -> G[H]
    U -> ~(S S | R | N)
    N -> '\n'
    D -> I[T] N (A | B)
    E -> eof
    R -> '\r'
    C -> S* D
    L -> R N | R | N
    A -> G[A | B]
    P -> ~(S | R | N)
    S -> ' '

which is oddly satisfying.
