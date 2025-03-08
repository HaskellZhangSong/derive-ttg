{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module Data.Derive.TTG(
    derive_ttg
  , derive_ttgs
  , derive_simple_decorator
  , derive_decrator
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
import Data.Derive.TTG.Utils
type VarName = Name
type TypeName = Name
type ConName = Name

extType :: [Name] -> VarName -> Type -> Type
extType names eps (ConT name) | name `elem` names 
    = let ext_name = suffixX name in AppT (ConT ext_name) (VarT eps)
extType _ _ t = t

genericExtType :: Data a => [Name] -> VarName -> a -> a
genericExtType names esp = everywhere $ mkT (extType names esp)

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

prefixX, suffixX, lowerSuffixX :: Name -> Name
prefixX n = mkName $ "X" ++ nameBase n
suffixX n = mkName $ nameBase n ++ "X"
lowerSuffixX n = mkName $ map toLower (nameBase n) ++ "X"

extendCon :: IsGadt -> [VarName] -> [Name] -> Con -> VarName -> [Con]
extendCon _ ty_para_names names (NormalC name ts) esp 
    = let 
        new_con_name = suffixX name;
        ext_ty       = appTypes $ ConT (prefixX name) : (VarT esp): map VarT ty_para_names; 
        new_tys      = genericExtType names esp ts
        in  [NormalC new_con_name ((defaultBang, ext_ty) : new_tys)]

extendCon _ ty_para_names names (RecC name vbts)   esp 
    = let 
        new_con_name = suffixX name;
        ext_ty       = appTypes $ ConT (prefixX name) : (VarT esp) : map VarT ty_para_names; 
        new_tys      = map (\(n, b, t) -> (lowerSuffixX n,b,t)) (genericExtType names esp vbts)
        in [RecC new_con_name ((lowerSuffixX name, defaultBang, ext_ty) : new_tys)]

-- Note: Operator constructor cannot be extended
extendCon _ _ _ (InfixC _ name _) _ 
    = error $ "Cannot extend operator:" ++ show name

-- FIX: if is_gadt then should extend more variables
extendCon is_gadt _ names (ForallC tvb context con) esp 
    = if is_gadt
        then let esp_tvb = (PlainTV esp SpecifiedSpec)
                in [ForallC (esp_tvb : tvb) context (head (extendCon is_gadt (map getTyVarBndrName tvb) names con esp))]
        else [ForallC tvb context (head (extendCon is_gadt (map getTyVarBndrName tvb) names con esp))]

extendCon _ ty_para_names names (GadtC ns ts t) esp = 
    [let
        new_con_name = suffixX name;
        ext_ty       = appTypes $ (ConT (prefixX name)) : VarT esp : map VarT ty_para_names; 
        new_tys      = genericExtType names esp ts 
        ret_ty       = genericExtType names esp t in 
        GadtC [new_con_name] ((defaultBang, ext_ty) : new_tys) (genericExtType names esp ret_ty)| name <- ns]

extendCon _ ty_para_names names (RecGadtC ns vbts t) esp = 
    [let
        new_con_name = suffixX name;
        ext_ty       = appTypes $ (ConT (prefixX name)) : VarT esp : map VarT ty_para_names; 
        new_tys      = map (\(n, b, ty) -> (lowerSuffixX n,b,ty)) (genericExtType names esp vbts)
        ret_ty       = genericExtType names esp t in
        RecGadtC [new_con_name] ((lowerSuffixX name, defaultBang, ext_ty) : new_tys) ret_ty| name <- ns]

type IsGadt = Bool
extendCons :: IsGadt -> [VarName] -> [TypeName] -> [Con] -> VarName -> [Con]
extendCons is_gadt ty_para_names names cons vn = if is_gadt
                                                    then
                                                        let vars = map (snd. getDataConNames) cons
                                                            in [res | (con, vs) <- zip cons vars, res <- extendCon is_gadt vs names con vn]
                                                    else
                                                        concatMap (\x -> extendCon is_gadt ty_para_names names x vn) cons

genTypeFamily :: [VarName] -> Name -> Dec
genTypeFamily vns name = 
    let var_params = map (\n -> PlainTV n BndrReq) vns
        eps_var    = PlainTV (mkName "eps") BndrReq
        in OpenTypeFamilyD (TypeFamilyHead (prefixX name) (eps_var : var_params) NoSig Nothing)

getDataConNames :: Con -> ([ConName], [VarName])
getDataConNames (NormalC name _)  = ([name], [])
getDataConNames (RecC name _)     = ([name], [])
getDataConNames (InfixC _ name _) = ([name], [])
getDataConNames (ForallC tvbs _ con)  = let (ns, vars) = getDataConNames con
                                            in (ns, map getTyVarBndrName tvbs)
getDataConNames (GadtC ns _ _)    = (ns, [])
getDataConNames (RecGadtC ns _ _) = (ns, [])

generateTypeFamilies :: IsGadt -> [VarName] -> [Con] -> [Dec]
generateTypeFamilies gadt ty_params_names cons =
    if not gadt
        then concatMap (\con -> map (genTypeFamily ty_params_names) (fst $ getDataConNames con)) cons
        else concatMap (\(cs, vns) -> map (genTypeFamily vns) cs) (map getDataConNames cons)

getAllConNames :: [Con] -> [Name]
getAllConNames cons = concatMap (fst. getDataConNames) cons

generateConstraint :: IsGadt -> [VarName] -> TypeName -> [Con] -> Dec
generateConstraint gadt vns tn cons =
    let
        phi              = mkName "phi"
        esp_name         = mkName "esp"
        esp_var          = VarT esp_name
        type_family_con  = if not gadt
                                then map (\n -> AppT (VarT phi) $ appTypes $ (ConT $ prefixX n) : esp_var : map VarT vns) (getAllConNames cons)
                                else map (\(n, vs) -> AppT (VarT phi) $ appTypes $ (ConT $ prefixX n) : esp_var : map VarT (map (mkName.show) vs)) 
                                                [(n,vs)| (ns, vs) <- map getDataConNames cons, n <- ns]
        constraint_tuple = foldl' AppT (TupleT (length type_family_con)) type_family_con;
        constraint_type  = KindedTV phi BndrReq (AppT (AppT ArrowT StarT) (ConT ''Constraint));
        esp_par          = PlainTV esp_name BndrReq
        vns_gadt         = if not gadt then vns else nub $ map (mkName.show) (concatMap snd $ map getDataConNames cons)
        vars             = map (\n -> PlainTV n BndrReq) vns_gadt
        in TySynD (mkName $ "ForallX" ++ nameBase tn) (constraint_type : esp_par : vars) constraint_tuple

derive_ttg :: Name   -- ^ Type name that needs transformation of ttg
           -> [TypeName] -- ^ Type name list that needs to extend an extra argument in the transformed type name
           -> Q [Dec]
derive_ttg tn tns = do
    info <- reify tn
    case info of
        TyConI d@(DataD context name tvbs kind cons _) -> do
            let ext_name = mkName $ nameBase name ++ "X"
            let is_gadt = isGadt d
            ep <- newName "eps"
            let extended_cons = if not $ isGadt d
                                    then cons ++ [NormalC tn []]
                                    else let gadt_ty_var_names = (map getTyVarBndrName tvbs)
                                             gadt_ty_var_binds = (fmap.fmap) (\_ -> SpecifiedSpec) tvbs
                                             ret_type = appTypes (ConT tn : map VarT gadt_ty_var_names)
                                            in cons ++
                                                [ForallC gadt_ty_var_binds []
                                                    (GadtC [tn] [] ret_type)]
            let ty_params = (PlainTV ep BndrReq : tvbs)
            let ty_params_names = map getTyVarBndrName tvbs
            let ext_cons = extendCons is_gadt ty_params_names (nub (tn:tns)) extended_cons ep
            let extended_type = DataD context ext_name ty_params kind ext_cons []
            let constraint_type = generateConstraint is_gadt ty_params_names tn extended_cons
            -- handle GADT
            let type_families = generateTypeFamilies is_gadt ty_params_names extended_cons
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
                let var_types = map (\v -> (VarT v)) var_names
                let type_def = appTypes $ ConT (suffixX tn) : ConT sn : var_types
                let ty_syn = TySynD (mkName (nameBase tn ++ nameBase sn)) vars type_def
                let make_ty_inst (name, ty) = TySynInstD $ 
                                                TySynEqn Nothing (appTypes $ ConT (prefixX name) : ConT sn : var_types) (ConT ty)
                let ty_syn_eqns = map make_ty_inst tiets
                let default_con_names = zip ((getAllConNames cons ++ [tn]) \\ map fst tiets) (repeat default_type)
                let default_ty_syn_eqns = map make_ty_inst default_con_names
                return $ [ty_syn] ++ ty_syn_eqns ++ default_ty_syn_eqns
            i -> error $ show (ppr i) ++ " cannot be extended as trees that grow"

derive_decrator :: Name -- ^ type name
                -> Name -- ^ stage name
                -> [(Name, Q Type)]
                -> Q Type
                -> Q [Dec]
derive_decrator tn sn tiets default_type
    = do
        info <- reify tn
        let var_names = (map (mkName. (\x -> x:[])) ['a'..'z']) -- Just limit it to 26 
        case info of
            TyConI (DataD _ _ tvbs _ cons _) -> do
                let vars = map (\v -> PlainTV v BndrReq) (take (length tvbs) var_names)
                let var_types = map (\v -> (VarT v)) (take (length tvbs) var_names)
                let type_def = appTypes $ ConT (suffixX tn) : ConT sn : var_types
                let ty_syn = TySynD (mkName (nameBase tn ++ nameBase sn)) vars type_def
                let make_ty_inst (name, ty) = do 
                                    rhs_ty <- ty
                                    return $ TySynInstD $ 
                                                TySynEqn Nothing (appTypes $ ConT (prefixX name) : ConT sn : var_types) rhs_ty
                let default_con_names = zip ((getAllConNames cons ++ [tn]) \\ map fst tiets) (repeat default_type)
                ty_syn_eqns <- sequence $ map make_ty_inst tiets
                default_ty_syn_eqns <- sequence $ map make_ty_inst default_con_names
                return $ [ty_syn] ++ ty_syn_eqns ++ default_ty_syn_eqns
            i ->  error $ show (ppr i) ++ " cannot be extended as trees that grow"