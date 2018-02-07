module Primal where
import Euterpea
import HSoM.Examples.MoreMusic
import Control.Monad.List

primes :: [Int]
primes = sieve (2 : [3, 5..])
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]


listToMusic :: [Int] -> Int -> Music Pitch
listToMusic pitches n = line $ map (\x -> note en (pitch (x `mod` 12 + n))) pitches


primePitches = listToMusic (take 112 primes) 40

threetwo :: PercussionSound -> PercussionSound -> Music Pitch
threetwo ps1 ps2 =
  let p1 = perc ps1 (1/2)
      p2 = perc ps2 (1/3)
  in instrument Percussion $
         (p1 :+: p1)
     :=: (p2 :+: p2 :+: p2)


fourthree :: PercussionSound -> PercussionSound -> Music Pitch
fourthree ins1 ins2 =
  let ht = perc ins1 (1/3)
      lt = perc ins2 (1/4)
  in instrument Percussion $
         (ht :+: ht :+: ht)
     :=: (lt :+: lt :+: lt :+: lt)


--steadyNow ps1 ps2 =
--	let p1 = perc ps1 (1/4)
--	    p2 = perc ps2 (1/4)
--    in instrument Percussion $
--        (p1 :+: p1 :+: p1 :+: p1) :=:
--        (p2 :+: p2 :+: p2 :+: p2)


goneWild :: PercussionSound -> PercussionSound -> PercussionSound -> Music (Pitch,Volume)
goneWild ps1 ps2 ps3 =
	let p1 = perc ps1
	    p2 = perc ps2
	    p3 = perc ps3
    in instrument Percussion $
    	(addVolume 75 (snr :+: p2 den :+: p2 den :+: p2 sn)) :=:
        (addVolume 100 (p1 den :+: p1 sn :+: p1 den :+: p1 sn)) :=:
        (addVolume 127 (times 2 $ p3 qn))



primal :: Music (Pitch,Volume)
primal = 
	let pt1 = times 2 $ threetwo RideBell AcousticBassDrum
	    pt2 = times 2 $ fourthree RideBell AcousticBassDrum
	    pt3 = times 2 $ threetwo AcousticBassDrum RideBell
	    pt4 = times 2 $ fourthree AcousticBassDrum RideBell
	in
	(addVolume 126 $ primePitches) 
	        :=: (times 1 $
                ((addVolume 100 (pt1 :+: pt2 :+: pt3 :+: pt4))
					:+:
				 (times 4 $ (goneWild RideBell ElectricSnare AcousticBassDrum))
				   :+: (addVolume 10 wnr) :+:
				 (times 4 $ (goneWild RideBell ElectricSnare AcousticBassDrum))
     				 :+: (addVolume 10 wnr)
			   	))