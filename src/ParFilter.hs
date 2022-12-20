module ParFilter
    ( parFilter
    ) where

import GHC.Conc (numCapabilities)
import Control.DeepSeq(force)
import Control.Parallel.Strategies(rpar, rseq, runEval)

parFilter f xs = 
    let chunkSize = 1 + (div (length xs) numCapabilities)
    in concat $ chunkFilter f xs chunkSize

chunkFilter f xs chunkSize
    | xs == [] = []
    | otherwise = let (xs1, xs2) = splitAt chunkSize xs
                      filteredBB = runEval $ do
                        filteredA <- (rpar (force (filter f xs1)))
                        filteredB <- (rpar (force (chunkFilter f xs2 chunkSize)))
                        _ <- rseq filteredB
                        return (filteredA:filteredB)
                  in filteredBB
