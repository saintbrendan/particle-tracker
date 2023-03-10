module ParFilter
  ( parFilter,
  )
where

import Control.DeepSeq (NFData, force)
import Control.Parallel.Strategies (rpar, rseq, runEval)
import GHC.Conc (numCapabilities)

parFilter :: (Eq a, Control.DeepSeq.NFData a) => (a -> Bool) -> [a] -> [a]
parFilter f xs =
  let chunkSize = 1 + (div (length xs) numCapabilities)
   in concat $ chunkFilter f xs chunkSize

chunkFilter :: (Eq a, Control.DeepSeq.NFData a) => (a -> Bool) -> [a] -> Int -> [[a]]
chunkFilter f xs chunkSize
  | xs == [] = []
  | otherwise =
      let (xs1, xs2) = splitAt chunkSize xs
          filteredTracks = runEval $ do
            filteredA <- (rpar (force (filter f xs1)))
            filteredB <- (rpar (force (chunkFilter f xs2 chunkSize)))
            _ <- rseq filteredB
            return (filteredA : filteredB)
       in filteredTracks
