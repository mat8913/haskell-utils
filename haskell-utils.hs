--------------------------------------------------------------------------------
-- To the extent possible under law, the author(s) have dedicated all copyright
-- and related and neighboring rights to this software to the public domain
-- worldwide. This software is distributed without any warranty.
--
-- You should have received a copy of the CC0 Public Domain Dedication along
-- with this software. If not, see
-- <http://creativecommons.org/publicdomain/zero/1.0/>.
--------------------------------------------------------------------------------

import Control.Monad ((>=>))
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty(..), (<|))


takeLength :: [b] -> [a] -> [a]
takeLength []    _      = []
takeLength _     []     = []
takeLength (_:i) (x:xs) = x : takeLength i xs

compareLength :: [a] -> [b] -> Ordering
compareLength []     []     = EQ
compareLength []     (_:_)  = LT
compareLength (_:_)  []     = GT
compareLength (_:xs) (_:ys) = compareLength xs ys

splitBefore :: (a -> Bool) -> [a] -> [[a]]
splitBefore f = foldr helper []
  where
    helper x acc = (if f x then ([]:) else id) $ uncurry (:) $ case acc of
      (xs:xss) -> ((x : xs), xss)
      []       -> ([x]     , [] )

insertBeforeEach :: a -> [a] -> [a]
insertBeforeEach x = concatMap ((x:) . pure)

splitRemove :: [Maybe a] -> [[a]]
splitRemove = foldr helper []
  where
    helper x acc = uncurry (:) $ case x of
      Nothing -> ([], acc)
      Just x' -> case acc of
        (xs:xss) -> (x' : xs, xss)
        []       -> ([x'], [])

removeSuffix :: Eq a => a         -- ^ Suffix
                     -> [a]       -- ^ Full list
                     -> Maybe [a] -- ^ Prefix
removeSuffix _ []     = Nothing
removeSuffix s [x]    = if x == s then Just [] else Nothing
removeSuffix s (x:xs) = (x:) <$> removeSuffix s xs

stripSuffix :: Eq a => [a]       -- ^ Suffix
                    -> [a]       -- ^ Full list
                    -> Maybe [a] -- ^ Prefix
stripSuffix = foldr (>=>) Just . map removeSuffix . reverse

showUnlinesList :: Show a => [a] -> String
showUnlinesList []     = "[\n]"
showUnlinesList (x:xs) = '[' : ' ' : shows x (showl xs)
  where
    showl []     = "\n]"
    showl (y:ys) = '\n' : ',' : ' ' : shows y (showl ys)

partitionLengths :: Integral a => [Bool] -> NonEmpty a
partitionLengths = foldr helper (1 :| [])
  where
    helper True  acc     = 1 <| acc
    helper False (a:|cc) = a+1 :| cc
