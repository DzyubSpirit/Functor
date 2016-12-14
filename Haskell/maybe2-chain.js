{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

class Chain a r where
  chain :: a -> r

instance Chain a a where 
  chain = id

instance Chain b r => Chain a ((a -> b) -> r) where
  chain x f = chain $ f x

instance (Functor f, Chain (f b) r) => Chain (f a) ((a -> b) -> r) where
  chain v f = chain $ fmap f v

myId :: Bool -> Bool
myId = id

f1, f2, f3 :: Int -> Int
f1 = (* 4)
f2 = (^ 2)
f3 = (+ 3)

startVal1, startVal2 :: Maybe Int
startVal1 = Just 4
startVal2 = Nothing

main = do
  printMaybe (chain startVal1 f1 f2 f3 :: Maybe Int)
  printMaybe (chain startVal2 f1 f2 f3 :: Maybe Int)

printMaybe :: Show a => Maybe a -> IO ()
printMaybe (Just x) = print x
printMaybe Nothing  = putStrLn "NaN"
