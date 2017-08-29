{-# LANGUAGE ScopedTypeVariables #-}

module Uncanny.BitMask where

import Uncanny.Prelude

buildEnumList :: forall a . (Bounded a, Enum a) => [a]
buildEnumList =
  foldr (\i ac -> toEnum i : ac) [] l
  where
    -- build power of 2 list from 0 to data type maxBound
    l = takeWhile (<= fromEnum (maxBound :: a)) (0 : iterate (*2) 1)

maskHasDataType :: Enum a => Int32 -> a -> Bool
maskHasDataType bm p = 0 /= bm .&. fromIntegral (fromEnum p)

bitMaskToDataType :: forall a . (Bounded a, Enum a) => Maybe Int32 -> Maybe [a]
bitMaskToDataType (Just bm) =
  Just $ foldr f [] buildEnumList
  where
    f t ac | maskHasDataType bm t = t : ac
           | otherwise = ac
bitMaskToDataType _ = Nothing

dataTypeToBitMask :: Enum a => Maybe [a] -> Maybe Int32
dataTypeToBitMask (Just []) = Nothing
dataTypeToBitMask (Just ps) =
  Just . fromIntegral $ foldr (\t bm -> bm .|. fromEnum t) 0 ps
dataTypeToBitMask _ = Nothing
