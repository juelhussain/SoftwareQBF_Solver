fof(low_up_is_exhaustive, axiom,
    (![X]:(lowairspace(X)|uppairspace(X)))).

fof(filter_equiv, conjecture, (
    (![X]:(((a_d_app(X) & lowairspace(X))|(dub_app(X) & lowairspace(X))
    |uppairspace(X))&
    (~military(X)|~milregion(X))))
    <=>
    (![X]:((uppairspace(X) | dub_app(X) | a_d_app(X)) &
    (~milregion(X) | ~military(X)))))).
