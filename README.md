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
```

`Lam` may use other types that need extension, you need to put the name into the second argument list. 

```haskell
data Lam a = Val (Expr a)
           | App (Lam a) (Expr a)
           | Abs String (Expr a)
         deriving Eq
data Expr a = EVar String
           | EInt Int
           | EApp (Expr a) (Expr a)   
           | EAdd (Expr a) (Expr a)
         deriving Eq

derive_ttg ''Expr []
derive_ttg ''Lam [''Expr]
```

`Expr` should be transformed into `ExprX`

```haskell
   derive_ttg ''Expr []
  ======>
    data ExprX eps_a2J6 (a_a1hq :: ghc-prim:GHC.Types.Type)
      = EVarX (XEVar eps_a2J6 a_a1hq) String |
        EIntX (XEInt eps_a2J6 a_a1hq) Int |
        EAppX (XEApp eps_a2J6 a_a1hq) (ExprX eps_a2J6 a_a1hq) (ExprX eps_a2J6 a_a1hq) |
        EAddX (XEAdd eps_a2J6 a_a1hq) (ExprX eps_a2J6 a_a1hq) (ExprX eps_a2J6 a_a1hq) |
        ExprX (XExpr eps_a2J6 a_a1hq)
    type family XEVar eps a_a1hq
    type family XEInt eps a_a1hq
    type family XEApp eps a_a1hq
    type family XEAdd eps a_a1hq
    type family XExpr eps a_a1hq
    type ForallXExpr (phi :: ghc-prim:GHC.Types.Type
                             -> Constraint) esp a_a1hq =
        (phi (XEVar esp a_a1hq), phi (XEInt esp a_a1hq),
         phi (XEApp esp a_a1hq), phi (XEAdd esp a_a1hq),
         phi (XExpr esp a_a1hq))
vars:[a_6989586621679014717]
/haskell/derive-ttg/test/Spec.hs:23:1-25: Splicing declarations
    derive_ttg ''Lam [''Expr]
  ======>
    data LamX eps_a2LK (a_a1hr :: ghc-prim:GHC.Types.Type)
      = ValX (XVal eps_a2LK a_a1hr) (ExprX eps_a2LK a_a1hr) |
        AppX (XApp eps_a2LK a_a1hr) (LamX eps_a2LK a_a1hr) (ExprX eps_a2LK a_a1hr) |
        AbsX (XAbs eps_a2LK a_a1hr) String (ExprX eps_a2LK a_a1hr) |
        LamX (XLam eps_a2LK a_a1hr)
    type family XVal eps a_a1hr
    type family XApp eps a_a1hr
    type family XAbs eps a_a1hr
    type family XLam eps a_a1hr
    type ForallXLam (phi :: ghc-prim:GHC.Types.Type
                            -> Constraint) esp a_a1hr =
        (phi (XVal esp a_a1hr), phi (XApp esp a_a1hr),
         phi (XAbs esp a_a1hr), phi (XLam esp a_a1hr))
```

You can put related types together by using `derive_ttgs [''Lam, ''Expr ]`


```haskell
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