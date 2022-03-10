{-# LANGUAGE TemplateHaskell #-}

module QuickCheckAuto where

import Rename (runTestsRename)
import Substitutionen (runTestsSubst)
import Unifikation

prop_1 = runTestsSubst

prop_2 = runTestsRename

prop_3 = runTestsUnify

return []

runTests = $quickCheck