--  Module      :  MCPrelude
--  Copyright   :  (c) Shaun Lee 2021
--  License     :  BSD-style (see the LICENSE file)
--
--  Maintainer  :  shaun@curlyfri.es
--  Stability   :  experimental
--  Portability :  portable
--
-- | This MCPrelude is a modified, PureScript version of the Haskell
-- | MCPrelude from the original Monad Challenges, which is a modified
-- | version of the Haskell Prelude designed specifically for The Monad
-- | Challenges.
module MCPrelude
  ( Seed
  , mkSeed
  , rand
  , toLetter
  , GreekData
  , greekDataA
  , greekDataB
  , salaries
  , firstNames
  , lastNames
  , cardRanks
  , cardSuits
  , module ExportArray
  , module ExportCodeUnits
  , module ExportFoldable
  , module ExportInt
  , module ExportPrelude
  , module ExportTraversable
  , module ExportStringUtils
  , module ExportTuple
  ) where

import Prelude
import Data.Array ((..), filter, range, concat, concatMap, take, drop, takeWhile, dropWhile, length, span, replicate, cons, (:), snoc, zip, zipWith) as ExportArray
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (foldl, foldr, and, or, any, all, sum, product, elem, notElem) as ExportFoldable
import Data.Int (toNumber) as ExportInt
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray) as ExportCodeUnits
import Data.String.Utils (lines, words) as ExportStringUtils
import Data.Traversable (scanl, scanr) as ExportTraversable
import Data.Tuple (Tuple(..), fst, snd) as ExportTuple
import Data.Tuple (Tuple(..))
import Prelude ((||), (&&), disj, conj, not, otherwise, Ordering(LT, GT, EQ), class Eq, (==), (/=), eq, notEq, class Ord, compare, min, max, comparing, (<), (<=), (>), (>=), class Bounded, top, bottom, class EuclideanRing, div, mod, lcm, gcd, identity, const, flip, map, ($), (<>), (*), (+), (-), (/), (<<<), (>>>), (#), (<$>), class Show, show, class Semiring, zero, one, add, mul) as ExportPrelude

-- | A Seed for the pseudo-random generator `rand`
newtype Seed
  = Seed Int

instance showSeed :: Show Seed where
  show (Seed n) = "Seed " <> show n

mkSeed :: Int -> Seed
mkSeed n = Seed n

m :: Int
m = 0x7FFF

rand :: Seed -> Tuple Seed Int
rand (Seed s) = Tuple (Seed s') s'
  where
  s' = (s * 16807) `mod` m

toLetter :: Int -> Char
toLetter v = case go v of
  Nothing -> ' '
  Just c -> c
  where
  go = fromCharCode <<< (_ + toCharCode 'a') <<< (_ `mod` 26)

type GreekData
  = Array (Tuple String (Array Int))

greekDataA :: GreekData
greekDataA =
  [ Tuple "alpha" [ 5, 10 ]
  , Tuple "beta" [ 0, 8 ]
  , Tuple "gamma" [ 18, 47, 60 ]
  , Tuple "delta" [ 42 ]
  ]

greekDataB :: GreekData
greekDataB =
  [ Tuple "phi" [ 53, 13 ]
  , Tuple "chi" [ 21, 8, 191 ]
  , Tuple "psi" []
  , Tuple "omega" [ 6, 82, 144 ]
  ]

salaries :: Array (Tuple String Int)
salaries =
  [ Tuple "alice" 105000
  , Tuple "bob" 90000
  , Tuple "carol" 85000
  ]

firstNames :: Array String
firstNames = [ "alice", "bob", "carol", "dave" ]

lastNames :: Array String
lastNames = [ "doe", "jones", "smith" ]

cardRanks :: Array Int
cardRanks = [ 2, 3, 4, 5 ]

cardSuits :: Array String
cardSuits = [ "H", "D", "C", "S" ]
