module Data.Derive.TTG.Utils (
    getTyVarBndrName, appTypes
) where
import Language.Haskell.TH
import Data.List (foldl1')
getTyVarBndrName :: TyVarBndr flag -> Name
getTyVarBndrName (PlainTV n _) = n
getTyVarBndrName (KindedTV n _ _) = n

appTypes :: [Type] -> Type
appTypes [] = error "no type to apply"
appTypes xs = foldl1' AppT xs 