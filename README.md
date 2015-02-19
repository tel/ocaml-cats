Cats—Signatures of the category theoretic style
===============================================

Abandon hope all ye who enter here. Cats is going to start out just
*shameless* and we'll see what happens.

(I did this entirely to get away with creating a signature called
`Covariant`.)

Goals and Thoughts
------------------

Signatures, rather type classes, like these are basically required for
Haskell. In particular, because of how Haskell handles nominal typing
around these things we need types to not just "expose functionality
which is monadic" but to actually instantiate *exactly* the same
notion of "Monad" that everyone else agrees to use.

This is, I suppose, more type safe but rather brittle.

In ML we can avoid this issue. Nobody *has* to use these classes and
changing them will not *wreck havoc* across code bases.

But they're nice. They're functor fodder, can be composed to build
more sophisticated signatures, and it'd be nice to have agreement upon
the names of things so that things are *more convenient* across the
board.

These abstractions have also proven their weight in Haskell. They're
useful tools to construct your types around. When you realize and
prove that some type of yours instantiates like 30 of these then
you've *accomplished something* with your design: it's likely to be
reasonable.

### Library design

Right now this is a big grab bag. In fact, worse, it's literally a
single file because I'm not terrifically adroit at pulling these
things apart without wrecking the documentation.

*Documentation is vital.* The impact of this library will be judged
not purely by its use (which can easily be very minimal!) but instead
by the mindshare taken up through use of these signatures, these
types, these names. Good documentation will allow this to serve as a
teaching reference as well as an eminently sharable document.

There should be frequent links, frequent examples (`Tutorial`
modules?), and also a readable, explorable exterior.

One possible endpoint for this monolith is to be shattered into many
smaller packages. This style of package modularity has been very
successful in Haskell (although it has raised issues with
packaging). If a design can be established that allows for the
decomposition of this monolith then it will be easier to depend upon
the resulting smaller pieces. That's a big win!

### Signature design

In Haskell there are two major forces on type classes which do not
exist here: *canonicity* and *confluence*. In particular, Haskell's
design of typeclasses must satisfy that each type has one "right"
instantiation of a typeclass.

Fundamentally, this is a big step away from the mathematical home of
these signatures. In some cases it is true that there is only one,
e.g., `Covariant` behavior for a type, but generally we don't speak of
`list` as being a `Covariant` functor but instead the tuple (`list`,
`List.map`). This is important because it's often easy for one type to
have multiple, say, monoids.

In Haskell, we solve this problem by multiplication of
types---`newtypes` allow the introduction of cheap type synonyms and
isomorphisms between them but it means we have to port around these
isomophism witnesses all over.

ML modules capture the genuine mathematical structure more
faithfully. Modularity and module substitutivity mean that
representing all of the various ways that something, e.g., is a
`Monoid` is pretty easy. More pertinently for `cats`, it means that we
don't have to reify the signature subtyping hierarchy exactly---we
just express interesting points in it using a consistent naming scheme
and substitutivity will handle the rest.
