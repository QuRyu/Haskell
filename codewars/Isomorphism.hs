-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd 

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

-- isomorphism is transitive
-- (a -> b, b -> a) -> (b -> c, c -> b) -> (a -> c, c -> a)
trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

-- We can combine isomorphism:
-- (a -> b, b -> a) -> (b -> c, c -> b) -> ((a, c) -> (b, d), (b, d) -> (a, c))
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = 
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
-- (a -> b, b -> a) -> ([a] -> [b], [b] -> [a])
isoList (ab, ba) = (map ab, map ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
-- (a -> b, b -> a) -> (Maybe a -> Maybe b, Maybe b -> Maybe a)
isoMaybe (ab, ba) = (fmap ab, fmap ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
-- (a -> b, b -> a) -> (b -> c, c -> b) -> (Either a c -> Either b d, Either b d -> Either a c)
isoEither (ab, ba) (cd, dc) = (fmapL, fmapR)
    where 
      fmapL (Left a)  = Left $ ab a 
      fmapL (Right c) = Right $ cd c 
      fmapR (Left b)  = Left $ ba b 
      fmapR (Right d) = Right $ dc d 

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
-- (a -> b, b -> a) -> (b -> c, c -> b) -> ((a -> c) -> (b -> d), (b -> d) -> (a -> c))
isoFunc (ab, ba) (cd, dc) = ((. ba), (. ab))

-- Going another way is hard (and is generally impossible)
-- isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
-- Remember, for all valid ISO, converting and converting back
-- Is the same as the original value.
-- You need this to prove some case are impossible,
-- And try to state why it is impossible (in the comment)

-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Try to state why in the comment here.
-- Suppose we have isoUnEither,
-- ..., 
-- Impossible,
-- We cannot have isoUnEither
-- Again, this wont be tested.

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
-- (ISO a b -> ISO b a, ISO b a -> ISO a b)
-- (((a -> b), (b -> a)) -> ((b -> a), (a -> b)), ((b -> a), (a -> b)) -> ((a -> b), (b -> a)))
isoSymm = (\(abL, baL) -> (baL, abL), \(baR, abR) -> (abR, baR))
