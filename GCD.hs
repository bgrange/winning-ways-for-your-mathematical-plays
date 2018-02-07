module Etude2 where
import Euterpea
import HSoM.Examples.MoreMusic
import Control.Monad.List



listToMusic :: [Int] -> Int -> Music Pitch
listToMusic pitches n = line $ map (\x -> note en (pitch (x `mod` 12 + n))) pitches



gcd :: Music (Pitch,Volume)
gcd = 
	let 
      multiples k i = listToMusic [k*x | x <- [0..]] i
      sevensHi = cut 3 $ multiples 7 40
      sevensLo = cut 3 $ multiples 7 38
      fives = cut 6 $ multiples 5 92
      threes = cut 16 $ multiples 3 76 
 	in	(addVolume 126 $ sevensHi :+: sevensLo :+: sevensHi :+: sevensLo)
      :=: 
      (addVolume 100 $ threes)
      :=:
      (addVolume 100 $ times 24 qnr :+: fives)





