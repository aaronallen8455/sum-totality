# Sum Totality

Given a sum type with an instance of Generic, This package lets you define
producers for that sum type which the type checker expects to cover all
of its constructors.

For example,

```haskell
data T = First Int
       | Second Bool
       | Third String
       deriving Generic

parseT :: Parser T
parseT
  = getAlt
  . total
  . add First (Alt parseInt)
  . add Second (Alt parseBool)
  . add Third (Alt string)
  $ end

```

If the parser were missing a branch for one of the constructors, you will get a
type error. Unfortunately the type errors are usually not very helpful but may
be improved in future versions.

The `SumTotality` module exports a simplified interface that expects all
constructors of the sum type to take a single argument. The
`SumTotality.NArity` module exports an interface that works with constructors
of any arity but has worse type errors.

Note that totality is only guaranteed if each constructor has different
arguments, otherwise you could potentially replace the constructor argument to
one of the `add`s with a different constructor with the same type.
