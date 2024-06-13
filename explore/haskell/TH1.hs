{-# LANGUAGE TemplateHaskell #-}

import TH0;

mklengthTH;

main = print $ lengthTH [1..100]
