# derive-ttg

This is a Haksell library that generates TTG(trees that grow) patterns with template Haskell. For data type:

```haskell
data Lam a = Var String a
           | App { t1 :: (Lam a) , t2::(Lam a), ext :: a}  
           | Abs String (Lam a) a
         deriving Eq
```

We can generate a new `Lam` type with decoration types, related constraint types and pattern synonyms.

```haskell
   derive_ttg ''Lam []
  ======>
    data LamX (a_aRb :: ghc-prim:GHC.Types.Type) eps_a2vI
      = VarX (XVar eps_a2vI) String a_aRb |
        AppX {appX :: (XApp eps_a2vI),
              t1X :: (LamX eps_a2vI a_aRb),
              t2X :: (LamX eps_a2vI a_aRb),
              extX :: a_aRb} |
        AbsX (XAbs eps_a2vI) String (LamX eps_a2vI a_aRb) a_aRb |
        LamX (XLam eps_a2vI)
    type family XVar eps
    type family XApp eps
    type family XAbs eps
    type family XLam eps
    type ForallXLam (phi :: ghc-prim:GHC.Types.Type
                            -> Constraint) esp =
        (phi (XVar esp), phi (XApp esp), phi (XAbs esp), phi (XLam esp))

    derive_simple_decorator ''Lam "Parse" [('App, ''Bool)] ''()
  ======>
    type LamParse a_a2z3 = LamX a_a2z3
    data Parse
    type instance XApp Parse = Bool
    type instance XVar Parse = ()
    type instance XAbs Parse = ()
    type instance XLam Parse = ()

    derive_simple_decorator ''Lam "TC" [(''Lam, ''Bool)] ''()
  ======>
    type LamTC a_a2Gx = LamX a_a2Gx
    data TC
    type instance XLam TC = Bool
    type instance XVar TC = ()
    type instance XApp TC = ()
    type instance XAbs TC = ()
```