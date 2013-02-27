module Test.QuickCheck.Assertions (
  binAsrt
  , (>?)
  , (<?)
  , (?>)
  , (?<)
  , (>=?)
  , (<=?)
  , (?>=)
  , (?<=)
  , (~==?)
  , (?~==)
  ) where

import Test.QuickCheck.Property
import Data.AEq



binAsrt ::    String -- ^ The reason of fail
           -> Bool   -- ^ If True then test pass
           -> Result -- ^ The result with fail reason
binAsrt fmt pred = if pred
                   then succeeded
                   else failed {reason = fmt}


(==?), (?==) :: (Eq a, Show a) => a -> a -> Result
(?==) a b = binAsrt s (a == b)
  where
    s = ">>>>>>>>>>>>>> expected\n"
        ++ show a ++
        "\n>>>>>>>>>>>>>> but got\n"
        ++ show b

(==?) = flip (==?)


binOrdering :: (Show a, Ord a)
               => (Ordering -> Bool) -- ^ Check if comparsion is good
               -> String            -- ^ Human-readable representation of operation
               -> a
               -> a
               -> Result
binOrdering pred fmt a b = binAsrt s (pred $ compare a b)
  where
    s = ">>>>>>>>>>>>>> the value\n"
        ++ show a ++
        "\n>>>>>>>>>>>>>> should be " ++ fmt ++ " than value\n"
        ++ show b
        
(>?), (<?), (?>), (?<), (>=?), (<=?), (?>=), (?<=) :: (Show a, Ord a) => a -> a -> Result

-- | Left argument is greater than right
(?>) a b = binOrdering (== GT) "greater" a b
-- | Left argument is less then right
(?<) a b = binOrdering (== LT) "less" a b
-- | Right argument is less then left
(>?) = flip (?<)
-- | Right argument is greater than left
(<?) = flip (?>)
-- | Left argument is greater or equal to right
(?>=) a b = binOrdering (\x -> x == EQ || x == GT) "greater or equal" a b
-- | Left argument is less or equal to right
(?<=) a b = binOrdering (\x -> x == EQ || x == LT) "less or equal" a b
-- | Right argument is less or equal to left
(>=?) = flip (?<=)
-- | Right argument is greater or equal to left
(<=?) = flip (?>=)

(?~==), (~==?) :: (AEq a, Show a) => a -> a -> Result
-- | Left value is almost equal to right
(?~==) a b = binAsrt s (a ~== b)
  where
    s = ">>>>>>>>>>>>>> The value\n"
        ++ show a ++
        "\n>>>>>>>>>>>>>> should be almost equal to\n"
        ++ show b
-- | Right value is almost equal to left
(~==?) = flip (?~==)
