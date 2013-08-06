module EAFIT.CB0081.Data.NFA.Examples where

import EAFIT.CB0081.Data.NFA ( NFA(MkNFA) )
import Data.Set              ( fromList, Set, singleton )

states :: Set Int
states = fromList [0,1,2,3,4]

symbols :: Set Char
symbols = fromList "ab"

initialState :: Int
initialState = 0

finalStates :: Set Int
finalStates = fromList [3]

errorState :: Set Int
errorState = singleton 5

nonemptyNFA :: NFA Int Char
nonemptyNFA = MkNFA states symbols delta initialState finalStates
  where
    delta :: Int -> Char -> Set Int
    delta 0 'a' = singleton 1
    delta 1 'b' = singleton 2
    delta 2 'a' = fromList [1,2]
    delta 3 'a' = singleton 4
    delta 2 'b' = singleton 3
    delta _ _   = errorState

emptyNFA :: NFA Int Char
emptyNFA = MkNFA states symbols delta initialState finalStates
  where
    delta :: Int -> Char -> Set Int
    delta 0 'a' = singleton 1
    delta 1 'b' = singleton 2
    delta 2 'a' = fromList [1,2]
    delta 3 'a' = singleton 4
    delta 2 'b' = sigleton 4
    delta _ _   = errorState
