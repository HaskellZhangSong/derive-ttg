{-# LANGUAGE TemplateHaskell,KindSignatures,TypeFamilies,ConstraintKinds #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Data.Derive.TTG
    ( derive_decrator, derive_simple_decorator, derive_ttgs )
-- import Language.Haskell.TH
-- import GHC.Exts (Constraint)
import Data.Void
data Lam a = Val (Expr a)
           | App (Lam a) (Expr a)
           | Abs String (Expr a)
         deriving Eq
data Expr a = EVar String
           | EInt Int
           | EApp (Expr a) (Expr a)   
           | EAdd (Expr a) (Expr a)
         deriving Eq


derive_ttgs [''Lam, ''Expr ]

data Ps
-- We can decorate App with Bool type
derive_simple_decorator ''Lam ''Ps [('App, ''Bool)] ''()

-- It's a bit tricky here to use type @Lam@ name as the data constructor name we generated with @derive_ttg ''Lam []@
-- derive_simple_decorator ''Lam "TC" [(''Lam, ''Bool)] ''() 

data Ds 
derive_decrator ''Lam ''Ds [('Val, \[v] -> return v),
                            ('App, \_ -> [t| ((), (Char, Bool))|]), 
                            ('Abs, \_ -> [t| ((), (String, Bool)) |]),
                            (''Lam, \[v] -> [t| ((), ($(return v), Bool)) |])] [t|()|]


main :: IO ()
main = putStrLn "Test suite not yet implemented"
