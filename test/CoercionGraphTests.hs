module CoercionGraphTests (coercionGraphTests) where

import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.TypeInference.CoercionGraph
import Test.Tasty
import Test.Tasty.HUnit

-- A ground nullary type constructor, e.g. `uint8`.
tc :: String -> Ty
tc n = TyCon (Name n) []

edge :: String -> String -> String -> CoercionEdge
edge s t w = CoercionEdge (tc s) (tc t) (Name w)

-- Build the graph and resolve the canonical path, collapsing a build failure to
-- Nothing (the build-failure cases are tested directly below).
path :: [CoercionEdge] -> String -> String -> Maybe [Name]
path es s t =
  case buildCoercionGraph es of
    Left _ -> Nothing
    Right g -> canonicalCoercionPath g (tc s) (tc t)

coercionGraphTests :: TestTree
coercionGraphTests =
  testGroup
    "CoercionGraph"
    [ testCase "edge read off a Coerce head" $
        coercionEdgeOf
          (Name "Coerce")
          (TyCon (Name "Pair") [tc "uint8", TyCon (Name "Proxy") [tc "uint16"]])
          @?= Just (tc "uint8", tc "uint16"),
      testCase "a non-Coerce head is not an edge" $
        coercionEdgeOf (Name "Eq") (tc "uint8") @?= Nothing,
      testCase "a direct edge is a length-1 path" $
        path [edge "a" "b" "f"] "a" "b" @?= Just [Name "f"],
      testCase "two covering edges compose to a 2-step path" $
        path [edge "a" "b" "f", edge "b" "c" "g"] "a" "c" @?= Just [Name "f", Name "g"],
      testCase "identity needs no coercion" $
        path [edge "a" "b" "f"] "a" "a" @?= Just [],
      testCase "no path yields Nothing" $
        path [edge "a" "b" "f"] "a" "c" @?= Nothing,
      testCase "canonical path is the shortest (a direct edge beats a composed one)" $
        path [edge "a" "b" "f", edge "b" "c" "g", edge "a" "c" "h"] "a" "c" @?= Just [Name "h"],
      testCase "a diamond resolves deterministically (shortest, tie-broken by node key)" $
        path
          [edge "a" "x" "f1", edge "x" "d" "f2", edge "a" "y" "g1", edge "y" "d" "g2"]
          "a"
          "d"
          @?= Just [Name "f1", Name "f2"],
      testCase "a duplicate edge with different witnesses is rejected (coherence)" $
        case buildCoercionGraph [edge "a" "b" "f", edge "a" "b" "g"] of
          Left _ -> pure ()
          Right _ -> assertFailure "expected duplicate-witness rejection",
      testCase "a duplicate edge with the same witness is accepted" $
        case buildCoercionGraph [edge "a" "b" "f", edge "a" "b" "f"] of
          Right _ -> pure ()
          Left e -> assertFailure ("unexpected rejection: " ++ e)
    ]
