module Data.Derive.TTG.Utils (
    getTyVarBndrName
    , appTypes
    , isGadt
) where
import Language.Haskell.TH
import Data.List (foldl1')
import Data.Generics

getTyVarBndrName :: TyVarBndr flag -> Name
getTyVarBndrName (PlainTV n _) = n
getTyVarBndrName (KindedTV n _ _) = n

appTypes :: [Type] -> Type
appTypes [] = error "no type to apply"
appTypes xs = foldl1' AppT xs 

isConGadt :: Con -> Bool
isConGadt (GadtC _ _ _)    = True
isConGadt (RecGadtC _ _ _) = True
isConGadt _                = False

isGadt :: Data a => a -> Bool
isGadt = everything (||) (mkQ False isConGadt)