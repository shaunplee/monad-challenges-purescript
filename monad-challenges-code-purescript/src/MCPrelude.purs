module MCPrelude
  ( Seed
  , mkSeed
  , rand
  , toLetter
  , module ExportCodeUnits
  , module ExportFoldable
  , module ExportPrelude
  ) where

import Prelude
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (foldl, foldr) as ExportFoldable
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray) as ExportCodeUnits
import Prelude (($), (*), (+), (-), (/), (<<<), (>>>), map) as ExportPrelude

newtype Seed
  = Seed Int

instance showSeed :: Show Seed where
  show (Seed n) = "Seed " <> show n

mkSeed :: Int -> Seed
mkSeed n = Seed n

m :: Int
m = 0x7FFF

rand :: Seed -> { newVal :: Int, newSeed :: Seed }
rand (Seed s) = { newVal: s', newSeed: Seed s' }
  where
  s' = (s * 16807) `mod` m

toLetter :: Int -> Char
toLetter v = case go v of
  Nothing -> ' '
  Just c -> c
  where
  go = fromCharCode <<< (_ + toCharCode 'a') <<< (_ `mod` 26)
