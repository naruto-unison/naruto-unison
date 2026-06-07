{-# LANGUAGE PatternSynonyms #-}
module NonNull
    ( NonNull(toNullable)
    , pattern (:|)
    , fromNullable, fromNullableEx
    , ncons, nuncons, nsnoc, nunsnoc
    , head, tail, last, init
    , ofoldMap1, ofold1, ofoldr1, ofoldl1'
    , foldMap1, fold1, foldr1, foldl1'
    , maximum, minimum, maximumBy, minimumBy
    , groupBy, group
    ) where
import Prelude hiding (foldr1, head, tail, last, init, maximum, minimum)
import Data.MonoTraversable
import Data.Sequences hiding (group, groupBy)
import qualified Data.Sequences as Sequences
import GHC.Stack (HasCallStack)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Zip (MonadZip)
import Data.Data
import GHC.Generics
import GHC.Exts
import Data.Aeson (FromJSON, ToJSON)

newtype NonNull f a = NonNull { toNullable :: f a } deriving (Applicative, Functor, Monad, MonadFix, MonadZip, Foldable, Traversable, Semigroup, Eq, Ord, Data, Generic, IsList, Read, Show, GrowingAppend, MonoFoldable, MonoFunctor, MonoPointed, SemiSequence, FromJSON, ToJSON)

type instance Element (NonNull f a) = Element (f a)

instance MonoTraversable (f a) => MonoTraversable (NonNull f a) where
    otraverse f (NonNull xs) =  NonNull <$> otraverse f xs
    {-# INLINE otraverse #-}

-- | 'ncons' and 'nuncons'
pattern (:|) :: ∀ f a. IsSequence (f a) => Element (f a) -> f a -> NonNull f a
pattern x :| xs <- (uncons . toNullable -> Just (x, xs)) where
    x :| xs = NonNull (cons x xs)
infixr 5 :|
{-# INLINE (:|) #-}
{-# COMPLETE (:|) #-}

expectJust :: ∀ a. HasCallStack => Maybe a -> a
expectJust (Just x) = x
expectJust Nothing  = error "NonNull contained a null sequence"
{-# INLINE expectJust #-}

fromNullable :: ∀ f a. MonoFoldable (f a) => f a -> Maybe (NonNull f a)
fromNullable xs
  | onull xs  = Just (NonNull xs)
  | otherwise = Nothing
{-# INLINE fromNullable #-}

fromNullableEx :: ∀ f a. MonoFoldable (f a) => f a -> NonNull f a
fromNullableEx = expectJust . fromNullable
{-# INLINE fromNullableEx #-}

ncons :: ∀ f a. SemiSequence (f a) => Element (f a) -> f a -> NonNull f a
ncons x xs = NonNull (cons x xs)
{-# INLINE ncons #-}

nuncons :: ∀ f a. IsSequence (f a) => NonNull f a -> (Element (f a), f a)
nuncons (NonNull xs) = expectJust (uncons xs)
{-# INLINE nuncons #-}

nsnoc :: ∀ f a. SemiSequence (f a) => f a -> Element (f a) -> NonNull f a
nsnoc xs x = NonNull (snoc xs x)
{-# INLINE nsnoc #-}

nunsnoc :: ∀ f a. IsSequence (f a) => NonNull f a -> (f a, Element (f a))
nunsnoc (NonNull xs) = expectJust (unsnoc xs)
{-# INLINE nunsnoc #-}

head :: ∀ f a. MonoFoldable (f a) => NonNull f a -> Element (f a)
head = headEx . toNullable
{-# INLINE head #-}

tail :: ∀ f a. IsSequence (f a) => NonNull f a -> (f a)
tail = tailEx . toNullable
{-# INLINE tail #-}

last :: ∀ f a. MonoFoldable (f a) => NonNull f a -> Element (f a)
last = lastEx . toNullable
{-# INLINE last #-}

init :: ∀ f a. IsSequence (f a) => NonNull f a -> (f a)
init = initEx . toNullable
{-# INLINE init #-}

ofoldMap1 :: ∀ f a m. (MonoFoldable (f a), a ~ Element (f a), Semigroup m)
          => (a -> m) -> NonNull f a -> m
ofoldMap1 f = ofoldMap1Ex f . toNullable
{-# INLINE ofoldMap1 #-}

ofold1 :: ∀ f a. (MonoFoldable (f a), a ~ Element (f a), Semigroup a)
       => NonNull f a -> a
ofold1 = ofoldMap1 id
{-# INLINE ofold1 #-}

ofoldr1 :: ∀ f a. (MonoFoldable (f a), a ~ Element (f a))
        => (a -> a -> a) -> NonNull f a -> a
ofoldr1 f = ofoldr1Ex f . toNullable
{-# INLINE ofoldr1 #-}

ofoldl1' :: ∀ f a. (MonoFoldable (f a), a ~ Element (f a))
         => (a -> a -> a)
         -> NonNull f a
         -> a
ofoldl1' f = ofoldl1Ex' f . toNullable
{-# INLINE ofoldl1' #-}

foldMap1 :: ∀ f a m. (MonoFoldable (f a), a ~ Element (f a), Semigroup m)
          => (a -> m) -> NonNull f a -> m
foldMap1 = ofoldMap1
{-# INLINE foldMap1 #-}

fold1 :: ∀ f a. (MonoFoldable (f a), a ~ Element (f a), Semigroup a)
       => NonNull f a -> Element (f a)
fold1 = ofold1
{-# INLINE fold1 #-}

foldr1 :: ∀ f a. (MonoFoldable (f a), a ~ Element (f a))
        => (a -> a -> a) -> NonNull f a -> a
foldr1 = ofoldr1
{-# INLINE foldr1 #-}

foldl1' :: ∀ f a. (MonoFoldable (f a), a ~ Element (f a))
         => (a -> a -> a) -> NonNull f a -> a
foldl1' = ofoldl1'
{-# INLINE foldl1' #-}

maximum :: ∀ f a. (MonoFoldable (f a), a ~ Element (f a), Ord a)
        => NonNull f a -> a
maximum = maximumEx . toNullable
{-# INLINE maximum #-}

minimum :: ∀ f a. (MonoFoldable (f a), a ~ Element (f a), Ord a)
        => NonNull f a -> a
minimum = minimumEx . toNullable
{-# INLINE minimum #-}

maximumBy :: ∀ f a. (MonoFoldable (f a), a ~ Element (f a))
          => (a -> a -> Ordering) -> NonNull f a -> a
maximumBy cmp = maximumByEx cmp . toNullable
{-# INLINE maximumBy #-}

minimumBy :: ∀ f a. (MonoFoldable (f a), a ~ Element (f a))
          => (a -> a -> Ordering) -> NonNull f a -> a
minimumBy cmp = minimumByEx cmp . toNullable
{-# INLINE minimumBy #-}

groupBy :: ∀ f a. (IsSequence (f a), a ~ Element (f a))
        => (a -> a -> Bool) -> f a -> [NonNull f a]
groupBy f xs = NonNull <$> Sequences.groupBy f xs
{-# INLINE groupBy #-}

group :: ∀ f a. (IsSequence (f a), a ~ Element (f a), Eq a)
      => f a -> [NonNull f a]
group xs = NonNull <$> Sequences.group xs
{-# INLINE group #-}
