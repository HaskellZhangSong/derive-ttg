{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE  MultiParamTypeClasses #-}
module Data.Derive.TTG(
    derive_ttg
  , derive_ttgs
  , derive_simple_decorator
)
where
import Language.Haskell.TH
import Data.List ( (\\), nub ) 
-- paper url: https://simon.peytonjones.org/trees-that-grow/
-- a stackoverflow user: https://stackoverflow.com/questions/75268118/how-can-i-remove-all-the-boilerplate-introduced-by-trees-that-grow
import Data.Generics
import Data.Char
import GHC.Exts (Constraint)
import Control.Monad

type VarName = Name
type TypeName = Name

extType :: [Name] -> VarName -> Type -> Type
extType names eps (ConT name) | name `elem` names 
    = let ext_name = suffixX name in AppT (ConT ext_name) (VarT eps)
extType _ _ t = t

genericExtType :: Data a => [Name] -> VarName -> a -> a
genericExtType names esp = everywhere $ mkT (extType names esp)

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

prefixX, suffixX, lowerSuffixX:: Name -> Name
prefixX n = mkName $ "X" ++ nameBase n
suffixX n = mkName $ nameBase n ++ "X"
lowerSuffixX n = mkName $ map toLower (nameBase n) ++ "X"

extendCon :: [Name] -> Con -> VarName -> [Con]
extendCon names (NormalC name ts) esp 
    = let 
        new_con_name = suffixX name;
        ext_ty       = AppT (ConT (prefixX name)) (VarT esp); 
        new_tys      = genericExtType names esp ts
        in  [NormalC new_con_name ((defaultBang, ext_ty) : new_tys)]

extendCon names (RecC name vbts)   esp 
    = let 
        new_con_name = suffixX name;
        ext_ty       = AppT (ConT (prefixX name)) (VarT esp); 
        new_tys      = map (\(n, b, t) -> (lowerSuffixX n,b,t)) (genericExtType names esp vbts)
        in [RecC new_con_name ((lowerSuffixX name, defaultBang, ext_ty) : new_tys)]

-- Note: Operator constructor cannot be extended
extendCon names (InfixC t1 name t2)  esp 
    = let 
        new_con_name = mkName $ nameBase name
        l_ty = genericExtType (names \\ [name]) esp t1
        r_ty = genericExtType (names \\ [name]) esp t2
        in [InfixC l_ty new_con_name r_ty]

extendCon names (ForallC tvb context con)  esp 
    = [ForallC tvb context (head (extendCon names con esp))]

extendCon names (GadtC ns ts t)    esp = 
    [let
        new_con_name = suffixX name;
        ext_ty       = AppT (ConT (prefixX name)) (VarT esp); 
        new_tys      = genericExtType names esp ts in
        GadtC [new_con_name] ((defaultBang, ext_ty) : new_tys) t| name <- ns]

extendCon names (RecGadtC ns vbts t) esp = 
    [let 
        new_con_name = suffixX name;
        ext_ty       = AppT (ConT (prefixX name)) (VarT esp); 
        new_tys      = map (\(n, b, ty) -> (lowerSuffixX n,b,ty)) (genericExtType names esp vbts) in
        RecGadtC [new_con_name] ((lowerSuffixX name, defaultBang, ext_ty) : new_tys) t| name <- ns]

extendCons :: [Name] -> [Con] -> VarName -> [Con]
extendCons names cons vn = concatMap (\x -> extendCon names x vn) cons

genTypeFamily :: Name -> Dec
genTypeFamily name = OpenTypeFamilyD (TypeFamilyHead (prefixX name) [(PlainTV (mkName "eps") BndrReq)] NoSig Nothing)

getDataConNames :: Con -> [Name]
getDataConNames (NormalC name _)  = [name]
getDataConNames (RecC name _)     = [name]
getDataConNames (InfixC _ name _) = [name]
getDataConNames (ForallC _ _ con) = getDataConNames con
getDataConNames (GadtC ns _ _)    = ns
getDataConNames (RecGadtC ns _ _) = ns

generateTypeFamilies :: [Con] -> [Dec]
generateTypeFamilies cons = concatMap (\con -> map genTypeFamily (getDataConNames con)) cons

getAllConNames :: [Con] -> [Name]
getAllConNames cons = concatMap getDataConNames cons

generateConstraint :: TypeName -> [Con] -> Dec
generateConstraint tn cons = 
    let
        phi              = mkName "phi"
        esp_name         = mkName "esp"
        esp_var          = VarT esp_name
        type_family_con  = map (\n -> AppT (VarT phi) $ AppT (ConT $ prefixX n) esp_var) (getAllConNames cons);
        constraint_tuple = foldl' AppT (TupleT (length type_family_con)) type_family_con;
        constraint_type  = KindedTV phi BndrReq (AppT (AppT ArrowT StarT) (ConT ''Constraint));
        esp_par          = PlainTV esp_name BndrReq
        in TySynD (mkName $ "ForallX" ++ nameBase tn) [constraint_type, esp_par] constraint_tuple

derive_ttg :: Name   -- ^ Type name that needs transformation of ttg
           -> [Name] -- ^ Type name list that needs to extend an extra argument in the transformed type name
           -> Q [Dec]
derive_ttg tn tns = do
    info <- reify tn
    case info of
        TyConI (DataD context name tvbs kind cons _) -> do
            let ext_name = mkName $ nameBase name ++ "X"
            ep <- newName "eps"
            let extended_cons = cons ++ [NormalC tn []]
            let constraint_type = generateConstraint tn extended_cons
            let type_families = generateTypeFamilies extended_cons
            let ext_cons = extendCons (nub (tn:tns)) extended_cons ep
            let ty_params = (PlainTV ep BndrReq : tvbs)
            let extended_type = DataD context ext_name ty_params kind ext_cons []
            return $ extended_type : type_families ++ [constraint_type]
        _ -> error $ show tn ++ " is not a data defined type and cannot be extended"

derive_ttgs :: [Name] -> Q [Dec]
derive_ttgs names = fmap concat $ mapM (\n -> derive_ttg n names) names

derive_simple_decorator :: Name -- ^ type name
                        -> Name -- ^ stage name
                        -> [(Name, Name)] -- ^ type instance function and decorator type
                        -> Name -- ^ default type
                        -> Q [Dec]
derive_simple_decorator tn sn tiets default_type
    = do
        info <- reify tn
        case info of
            TyConI (DataD _ _ tvbs _ cons _) -> do
                var_names <- replicateM (length tvbs) (newName "a")
                let vars = map (\v -> PlainTV v BndrReq) var_names
                let type_def = foldl' AppT (ConT (suffixX tn)) $ map (\v -> (VarT v)) var_names
                let ty_syn = TySynD (mkName (nameBase tn ++ nameBase sn)) vars type_def
                let make_ty_inst (name, ty) = TySynInstD $ TySynEqn Nothing (AppT (ConT (prefixX name)) (ConT sn)) (ConT ty)
                let ty_syn_eqns = map make_ty_inst tiets
                let default_con_names = zip ((getAllConNames cons ++ [tn]) \\ map fst tiets) (repeat default_type)
                let default_ty_syn_eqns = map make_ty_inst default_con_names
                return $ ty_syn : ty_syn_eqns ++ default_ty_syn_eqns
            _ -> undefined