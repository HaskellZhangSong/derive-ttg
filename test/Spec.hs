{-# LANGUAGE TemplateHaskell,KindSignatures,TypeFamilies,ConstraintKinds #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Data.Derive.TTG
import Language.Haskell.TH
import GHC.Exts (Constraint)
data Lam a = Var String a
           | App { t1 :: (Lam a) , t2::(Lam a), ext :: a}  
           | Abs String (Lam a) a
         deriving Eq

derive_ttg ''Lam []

-- We can decorate App with Bool type
derive_simple_decorator ''Lam "Parse" [('App, ''Bool)] ''()

-- It's a bit tricky here to use type @Lam@ name as the data constructor name we generated with @derive_ttg ''Lam []@
derive_simple_decorator ''Lam "TC" [(''Lam, ''Bool)] ''() 


main :: IO ()
main = putStrLn "Test suite not yet implemented"
