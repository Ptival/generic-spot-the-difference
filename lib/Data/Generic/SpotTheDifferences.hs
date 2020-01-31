{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |

module Data.Generic.SpotTheDifferences (
  ) where

import Data.Text.Prettyprint.Doc
import GHC.Generics hiding (C, R)
import Generics.Deriving.Eq

class GDiffable p where
  gSpotTheDifferences :: p a -> p a -> Doc ()

class Diffable a where
  spotTheDifferences :: a -> a -> Doc ()

  default spotTheDifferences :: (Generic a, GDiffable (Rep a)) => a -> a -> Doc ()
  spotTheDifferences a b = gSpotTheDifferences (from a) (from b)

groundDifferences :: (Eq a, Show a) => a -> a -> Doc ()
groundDifferences a b =
  let prettyShow = pretty . show in
  if a == b
  then hsep [ pretty "`-", prettyShow a ]
  else foundTheDifferences (prettyShow a) (prettyShow b)

foundTheDifferences :: Doc () -> Doc () -> Doc ()
foundTheDifferences old new =
  vcat [ hsep [ pretty "`- [OLD]", old ]
       , hsep [ pretty "   [NEW]", new ]
       ]

data A
  = AA Int String Int Bool
  | AB Bool Bool
  deriving (Diffable, Eq, Generic, GEq, Show)

data B
  = BA Int A String A Bool A A
  | BB A Bool
  deriving (Diffable, Eq, Generic, GEq, Show)

data R = RConstructor
  { f1 :: Int
  , f2 :: A
  , f3 :: Bool
  }
  deriving (Diffable, Eq, Generic, GEq, Show)

data C
  = CA Bool B B R Bool B
  deriving (Diffable, Eq, Generic, GEq, Show)

a1 :: A
a1 = AA 23 "noise" 42 True

a2 :: A
a2 = AA 23 "noise" 44 True

a3 :: A
a3 = AA 23 "noice" 44 True

r1 :: R
r1 = RConstructor 48 a1 True

r2 :: R
r2 = RConstructor 48 a3 False

b1 :: B
b1 = BA 76 a1 "beep boop" a1 False a1 a1

b2 :: B
b2 = BA 76 a1 "beep boop" a1 False a2 a1

b3 :: B
b3 = BA 76 a1 "beep boop" a1 False a2 a3

c1 :: C
c1 = CA False b1 b1 r1 True b1

c2 :: C
c2 = CA False b1 b2 r2 True b3

test = spotTheDifferences c1 c2

instance (GDiffable f, GDiffable g, GEq' f, GEq' g) => GDiffable (f :*: g) where
  gSpotTheDifferences (f1 :*: g1) (f2 :*: g2) =
    vcat
    [ gSpotTheDifferences f1 f2
    , gSpotTheDifferences g1 g2
    ]

instance (GDiffable f, GDiffable g) => GDiffable (f :+: g) where
  gSpotTheDifferences (L1 f) (L1 g) = gSpotTheDifferences f g
  gSpotTheDifferences (R1 f) (R1 g) = gSpotTheDifferences f g
  gSpotTheDifferences (L1 f) (R1 g) = foundTheDifferences (pretty "L") (pretty "R")
  gSpotTheDifferences (R1 f) (L1 g) = foundTheDifferences (pretty "R") (pretty "L")

instance (GDiffable f) => GDiffable (M1 i c f) where
  gSpotTheDifferences a b = gSpotTheDifferences (unM1 a) (unM1 b)

instance (GDiffable f) => Diffable (M1 i c f p) where
  spotTheDifferences a b = gSpotTheDifferences (unM1 a) (unM1 b)

branch doc = hsep [ pretty "`-", doc ]

instance {-# OVERLAPPING #-} (Constructor c, GDiffable f, GEq' f) => GDiffable (C1 c f) where
  gSpotTheDifferences a b =
    if geq' a b
    then branch $ hsep [ pretty $ conName a, pretty "... [SAME] (C1)" ]
    else nest 3 $ vcat [ branch . pretty $ conName a
                       , gSpotTheDifferences (unM1 a) (unM1 b)
                       ]

instance {-# OVERLAPPING #-} (Datatype d, GDiffable f, GEq' f) => GDiffable (D1 d f) where
  gSpotTheDifferences a b = gSpotTheDifferences (unM1 a) (unM1 b)
  --  if geq' a b
  --  then branch . pretty $ datatypeName a ++ "... [SAME]"
  --  else nest 3 $ vcat [ branch $ hsep [ pretty "::", pretty $ datatypeName a ]
  --                     , gSpotTheDifferences (unM1 a) (unM1 b)
  --                     ]

instance {-# OVERLAPPING #-} (GDiffable f, GEq' f, Selector s) => GDiffable (S1 s f) where
  gSpotTheDifferences a b =
    if geq' a b
    then if selName a == ""
         then branch $ hsep [ pretty "... [SAME] (S1)" ]
         else branch $ hsep [ pretty $ selName a, pretty "= ... [SAME] (S1)" ]
    else if selName a == ""
         then gSpotTheDifferences (unM1 a) (unM1 b)
         else nest 3 $ vcat [ branch $ hsep [ pretty $ selName a, pretty "=" ]
                            , gSpotTheDifferences (unM1 a) (unM1 b)
                            ]

instance (Diffable c) => GDiffable (K1 i c) where
  gSpotTheDifferences a b = spotTheDifferences (unK1 a) (unK1 b)

instance                     Diffable Bool   where
  spotTheDifferences = groundDifferences
instance                     Diffable Int    where
  spotTheDifferences = groundDifferences
instance {-# OVERLAPPING #-} Diffable String where
  spotTheDifferences = groundDifferences
