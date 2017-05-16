module Crypto.Protocol.SPAKE2.Groups
  (
  ) where

gcd :: Integer -> Integer -> Integer
gcd a b | b > a = gcd b a
        | b == 0 = a
        | otherwise = gcd b (a `mod` b)

-- egcd x y = (g, x, y)
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 a = (b, 0, 1)
egcd a b = let (q, r) = b `divMod` a
               (g, x, y) = egcd r a
           in
             (g, y - q * x, x)

class (Show a, Eq a) => IntegerMultGroup a where
  mult :: a -> a -> a
  identity :: a -> a
  invert :: a -> a
  groupEq :: a -> a -> Bool

data IntegerModPElem = IntegerModPElem
                        { p :: Integer  -- modulus
                        , x :: Integer  -- value
                        } deriving (Eq)

instance IntegerMultGroup IntegerModPElem where
  mult (IntegerModPElem m x) (IntegerModPElem n y) | n == m =
                                                       IntegerModPElem m ((x*y) `mod` m)
                                                   | otherwise = error "Cannot multiply elements from two separate groups"
  -- https://en.wikipedia.org/wiki/Modular_multiplicative_inverse#Extended_Euclidean_algorithm
  -- https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
  -- x = inv of b (mod m) =>  x * b `mod` m == 1
  invert (IntegerModPElem m x) = let (g, x, _) = egcd x m
                                 in
                                   if g == 1
                                   then x `mod` m
                                   else error "cannot invert element in the group"
  identity (IntegerModPElem m _) = IntegerModPElem m 1
  groupEq (IntegerModPElem m _) (IntegerModPElem n _) = m == n 


-- sub groups
data SubgroupGen a = SubgroupGen
                     { g :: a       -- generator element for a group
                     , q :: Integer -- order
                     } deriving (Eq, Show)


-- This 1024-bit group originally came from the J-PAKE demo code,
-- http://haofeng66.googlepages.com/JPAKEDemo.java . That java code
-- recommended these 2048 and 3072 bit groups from this NIST document:
-- http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/DSA2_All.pdf

-- L=1024, N=160
I1024 = SubgroupGen { g = IntegerModPElem
                      {
                        p = 0xE0A67598CD1B763BC98C8ABB333E5DDA0CD3AA0E5E1FB5BA8A7B4EABC10BA338FAE06DD4B90FDA70D7CF0CB0C638BE3341BEC0AF8A7330A3307DED2299A0EE606DF035177A239C34A912C202AA5F83B9C4A7CF0235B5316BFC6EFB9A248411258B30B839AF172440F32563056CB67A861158DDD90E6A894C72A5BBEF9E286C6B
                      , x =  g=0xD29D5121B0423C2769AB21843E5A3240FF19CACC792264E3BB6BE4F78EDD1B15C4DFF7F1D905431F0AB16790E1F773B5CE01C804E509066A9919F5195F4ABC58189FD9FF987389CB5BEDF21B4DAB4F8B76A055FFE2770988FE2EC2DE11AD92219F0B351869AC24DA3D7BA87011A701CE8EE7BFE49486ED4527B7186CA4610A75
                      }
                    , q = 0xE950511EAB424B9A19A2AEB4E159B7844C589C4F
                    }


