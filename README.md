NestedFunctor
=============

Nested composition of functors with a type index tracking nesting.

```Haskell
data Flat (x :: * -> *)
data Nest (o :: *) (i :: * -> *)

data Nested fs a where
   Flat :: f a -> Nested (Flat f) a
   Nest :: Nested fs (f a) -> Nested (Nest fs f) a
```

A `Nested fs a` is the composition of all the layers mentioned in `fs`, applied to an `a`. Specifically, the `fs` parameter is a sort of snoc-list holding type constructors of kind `(* -> *)`. The outermost layer appears as the parameter to `Flat`; the innermost layer appears as the rightmost argument to the outermost `Nest`. For instance:

```Haskell
                  [Just ['a']]   :: [Maybe [Char]]
             Flat [Just ['a']]   :: Nested (Flat []) (Maybe [Char])
       Nest (Flat [Just ['a']])  :: Nested (Nest (Flat []) Maybe) [Char]
 Nest (Nest (Flat [Just ['a']])) :: Nested (Nest (Nest (Flat []) Maybe) []) Char
```
