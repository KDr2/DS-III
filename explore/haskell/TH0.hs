{-# LANGUAGE TemplateHaskell #-}

module TH0 where

import Language.Haskell.TH

mklengthTH :: Q [Dec]
mklengthTH = return [typesig, FunD lengthTH [eq1, eq2]]
    where
        lengthTH = mkName "lengthTH"
        typesig = SigD lengthTH
            (ForallT
                [PlainTV (mkName "a") SpecifiedSpec]
                []
                (ArrowT `AppT` (AppT ListT (VarT $ mkName "a")) `AppT` (ConT ''Int))
            )
        eq1 = Clause
            [ConP '[] [] []]
            (NormalB (LitE $ IntegerL 0))
            []
        eq2 = Clause
            [InfixP (VarP $ mkName "x") '(:) (VarP $ mkName "xs")]
            (NormalB (
                InfixE
                    (Just (LitE $ IntegerL 1))
                    (VarE '(+))
                    (Just $ (VarE lengthTH) `AppE` (VarE $ mkName "xs"))
            ))
            []


-- myFunc :: Q [Dec]
-- myFunc = return mklengthTH
