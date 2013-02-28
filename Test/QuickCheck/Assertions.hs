{- | Module provides convenient functions to do some assertions in QuickCheck properties with pretty printed reasons.
For example you can do something like that:

> module Main where
> 
> import Test.Hspec
> import Test.Hspec.QuickCheck
> import Test.QuickCheck.Assertions
> import Test.QuickCheck.Property
> 
> someProp :: Int -> Int -> Result
> someProp a b = (a ?> b)
> 
> someOtherProp :: Double -> Double -> Result
> someOtherProp a b = (a ?== b)
>                
> main = hspec $ describe "failing test" $ do
>   prop "must fail" $ someProp
>   prop "must fail again" $ someOtherProp

And receive pretty printed fail message when testing:

> failing test
>   - must fail FAILED [1]                    
>   - must fail again FAILED [2]                             
> 
> 1) failing test must fail FAILED
> *** Failed! (after 1 test): 
> >>>>>>>>>>>>>> the value
> 0
> >>>>>>>>>>>>>> should be greater than value
> 0
> 0
> 0
> 
> 
> 2) failing test must fail again FAILED
> *** Failed! (after 2 tests and 4 shrinks): 
> >>>>>>>>>>>>>> expected
> 0.0
> >>>>>>>>>>>>>> but got
> 1.0
> 0.0
> 1.0

Ok, not very well printed, but better than nothing.
-}
module Test.QuickCheck.Assertions (
  binAsrt
  , (?==)
  , (==?)
  , (/=?)
  , (?/=)
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
binAsrt fmt pre = if pre
                   then succeeded
                   else failed {reason = fmt}


(==?), (?==) :: (Eq a, Show a) => a -> a -> Result
(?==) a b = binAsrt s (a == b)
  where
    s = ">>>>>>>>>>>>>> expected\n" -- very stupid formater for now
        ++ show a ++
        "\n>>>>>>>>>>>>>> but got\n"
        ++ show b

(==?) = flip (?==)

(?/=), (/=?) :: (Eq a, Show a) => a -> a -> Result
(?/=) a b = binAsrt s (a /= b)
  where
    s = ">>>>>>>>>>>>>> expected the value\n"
        ++ show a ++
        "\n>>>>>>>>>>>>>> should not equal to\n"
        ++ show b

(/=?) = flip (?/=)


binOrdering :: (Show a, Ord a)
               => (Ordering -> Bool) -- ^ Check if comparsion is good
               -> String            -- ^ Human-readable representation of operation
               -> a
               -> a
               -> Result
binOrdering pre fmt a b = binAsrt s (pre $ compare a b)
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
