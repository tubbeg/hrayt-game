module GameTime where

import System.Clock

getMonoTime :: IO TimeSpec
getMonoTime = getTime Monotonic

oneSec :: TimeSpec
oneSec = 1000
{-
Haskell isn't always perfect. Documentation is not great, or maybe not even good. The tooling
could be a lot better. Some operators are borrowed from mathematics, but they do not make it
easy to read (lisp is way better at syntax). But if there's one thing that works really well
then it's type classes and polymorphism. This would normally be a huge pain in other
programming languages -}
diffMonoTime :: TimeSpec -> TimeSpec -> TimeSpec
diffMonoTime t1 t2 = t1 - t2

isTimeToUpdateGame :: TimeSpec -> Bool
isTimeToUpdateGame n 
  | n >= 1000 = True
  | otherwise = False