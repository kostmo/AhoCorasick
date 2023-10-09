{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- BoolExpr unit tests
module Main where

import qualified Data.Map as M
import Data.Either (isRight)
import Data.List (isInfixOf)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

import Text.AhoCorasick

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Main" [
    testCase "Simplest example" $
        assertEqual "Found strings not equal!"
            ["she", "he", "hers"] example1
    , testCase "With data" $
        assertEqual "Found strings not equal!"
            [1, 0, 3] example2
    , testCase "Step-by-step state machine evaluation" $
        assertEqual "Steps were not equal!"
        [ ('u',[])
        , ('s',[])
        , ('h',[])
        , ('e',[(3,"she"),(2,"he")])
        , ('r',[])
        , ('s',[(4,"hers")])
        ]
        example3
    ]

example1 :: [String]
example1 = map pVal $ findAll simpleSM "ushers"
    where
    simpleSM = makeSimpleStateMachine ["he","she","his","hers"]

example2 :: [Int]
example2 = map pVal $ findAll sm "ushers" where
    sm = makeStateMachine [("he",0),("she",1),("his",2),("hers",3)]

example3 :: [(Char, [(Int, String)])]
example3 = next sm "ushers" where
    sm = makeSimpleStateMachine ["he","she","his","hers"]
    next _ [] = []
    next sm (s:n) = let (SMStepRes match nextSM) = stateMachineStep sm s in
        (s, match) : next nextSM n
