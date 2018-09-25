-- | A collection of @'Ninja' -> 'Ninja'@ transformations.
module Game.Ninja where

import StandardLibrary

import qualified Data.Sequence as Seq

import Game.Structure
import Game.Functions

-- Applies a 'Status'. Deletes older matching 'Statuses' if 'Nonstacking'.
addStatus :: Status -> Ninja -> Ninja
addStatus st n@Ninja{..}
  | Single `elem` statusClasses st && has (statusL st) (statusSrc st) n = n
  | otherwise = n { nStatuses = nStatuses' }
  where 
    st' = st { statusClasses = filter (InvisibleTraps /=) $ statusClasses st }
    addCount x = x { statusCount = statusCount x + statusCount st }
    nStatuses'
      | Nonstacking `elem` statusClasses st = 
          st' : filter (not . lEq st') nStatuses
      | otherwise = case find (== st') nStatuses of
          Nothing    -> st' : nStatuses
          Just match -> addCount match : delete match nStatuses

-- Replicates 'addStatus'.
addStacks :: Int -> Text -> Int -> Skill -> Slot -> Slot -> Ninja -> Ninja
addStacks dur' l i skill@Skill{..} src c = addStatus $ 
    Status i l root src c skill [] (Unremovable : classes) [] dur dur
  where 
    dur    = copyDur copying . incr $ sync dur'
    root   = copyRoot skill src

-- | Passes the user's 'nId' to 'addStacks'.
addOwnStacks :: Int -> Text -> Int -> Int -> Int -> Ninja -> Ninja
addOwnStacks dur' l s v i n@Ninja{..} = addStacks dur' l i skill nId nId n
  where 
    skill = characterSkills nCharacter !! s !! v

-- | Adds to an element in 'nCooldowns'.
alterCd :: Int -- ^ Skill index (0-3)
        -> Int -- ^ 'Variant' index in 'characterSkills' of 'nCharacter'
        -> Int -- ^ Amount added
        -> Ninja -> Ninja
alterCd s v cd n@Ninja{..} = n { nCooldowns = adjustCd s v (+ cd) nCooldowns}

-- | Subtracts from 'nHealth'.
attack :: Int -> Ninja -> Ninja
attack hp n@Ninja{..} = n { nHealth = healthBound (minHealth n) $ nHealth - hp }

-- | Deletes matching 'Channel's in 'nChannels'.
cancelChannel :: Text -> Ninja -> Ninja
cancelChannel l n@Ninja{..} = n 
    { nChannels = filter ((l /=) . label . channelSkill) nChannels }  

-- | Deletes matching 'Status'es in 'nStatuses'.
clear :: Text -> Slot -> Ninja -> Ninja
clear l src n@Ninja{..} = n 
    { nStatuses = filter (not . lMatch l src) nStatuses }

-- | Deletes matching 'Trap's in 'nTraps'.
clearTrap :: Text -> Slot -> Ninja -> Ninja
clearTrap l src n@Ninja{..} = n 
    { nTraps = filter (not . lMatch l src) nTraps }

-- | Removes harmful effects. Does not work if the target has 'Plague'.
cure :: (Effect -> Bool) -> Ninja -> Ninja
cure match n@Ninja{..} = n { nStatuses = mapMaybe cure' nStatuses }
  where 
    keep Reveal = True
    keep a      = helpful a || not (match a)
    cure' status@Status{..}
      | statusSrc == nId = Just status
      | null statusEfs  = Just status
      | Unremovable `elem` statusClasses = Just status
      | is Plague n = Just status
      | not $ any keep statusEfs = Nothing
      | otherwise = Just status { statusEfs = filter keep statusEfs }

-- | Cures 'Bane' 'Status'es.
cureBane :: Ninja -> Ninja
cureBane n@Ninja{..}
  | is Plague n = n
  | otherwise   = cure cured n { nStatuses = filter uncured nStatuses }
  where 
    cured (Afflict _)  = True
    cured _            = False
    uncured Status{..} = Bane `notElem` statusClasses || nId == statusSrc

-- \ While concluding 'runTurn', prevents refreshed 'Status'es from having 
-- doubled effects due to there being both an old version and a new version.
decrStats :: Ninja -> Ninja
decrStats n@Ninja{..} = n { nStatuses = expire <$> nStatuses }
  where 
    expire status@Status{..} 
      | statusDur == 1 = status { statusEfs = [] }
      | otherwise     = status

-- | Applies 'decrTurn' to all of a 'Ninja's 'TurnBased' elements.
decr :: Ninja -> Ninja
decr n@Ninja{..} = case findMatch nStatuses of
        Just (Snapshot n') -> decr n' -- TODO
        _ -> n { nDefense   = mapMaybe decrTurn nDefense 
              , nStatuses  = mapMaybe decrTurn nStatuses
              , nBarrier   = mapMaybe decrTurn nBarrier
              , nFace      = mapMaybe decrTurn nFace     
              , nChannels  = mapMaybe decrTurn newChans 
                             ++ mapMaybe decrTurn nChannels
              , nTags      = mapMaybe decrTurn nTags
              , nTraps     = mapMaybe decrTurn nTraps    
              , newChans   = []
              , nVariants  = decrTurn'                  <$> nVariants 
              , nCopied    =        (>>= decrTurn)       <$> nCopied 
              , nCooldowns = ((max 0 . subtract 1) <$>) <$> nCooldowns 
              , nParrying  = []
              }
  where 
    findMatch          = find match . reverse . concatMap statusEfs .
                         filter ((<= 2) . statusDur)
    match (Snapshot _) = True
    match _            = False
    decrTurn' xs = case mapMaybe decrTurn $ toList xs of
        x:xs' -> x :| xs'
        []    -> noVariant :| []

kill :: Bool -- ^ Can be prevented by 'Endure'
     -> Ninja 
     -> Ninja
kill endurable n@Ninja{..}
  | endurable = n { nHealth = minHealth n }
  | otherwise = n { nHealth = 0, nStatuses = preventRes : nStatuses }
  where 
    preventRes = Status 1 "dead" nId nId nId newSkill [Plague] [] [] (-1) (-1)

-- | Decreases the duration of matching 'Status'es.
hasten :: Int -> Text -> Slot -> Ninja -> Ninja
hasten dur = prolong (-dur)

prolong :: Int -> Text -> Slot -> Ninja -> Ninja
-- | Extends the duration of matching 'Status'es.
prolong dur l src n@Ninja{..} = n
    { nStatuses = mapMaybe (prolong' dur l src) nStatuses }
         
-- | Extends the duration of a single 'Status'.
prolong' :: Int -> Text -> Slot -> Status -> Maybe Status
prolong' dur l src status@Status{..} 
  | statusDur == 0 || not (lMatch l src status) = Just status
  | statusDur' <= 0                            = Nothing
  | otherwise = Just status { statusDur    = statusDur'
                            , statusMaxDur = max statusMaxDur statusDur'
                            }
              -- TODO figure out why the fuck this works
    where 
      statusDur' = statusDur + dur'
      dur'       
        | odd statusDur == even dur = dur
        | dur < 0                   = dur + 1
        | otherwise                 = dur - 1
 
-- | Removes all friendly effects.
purge :: Ninja -> Ninja
purge n@Ninja{..}
  | is Enrage n = n
  | otherwise   = n { nStatuses = doPurge <$> nStatuses }
  where 
    canPurge ef = helpful ef || not (sticky ef)
    doPurge st@Status{..}
      | Unremovable `notElem` statusClasses && any canPurge statusEfs = st
          { statusEfs = filter canPurge statusEfs }
      | otherwise = st

-- | Resets the duration of matching 'Status'es to their 'statusMaxDur'.
refresh :: Text -> Slot -> Ninja -> Ninja
refresh l src n@Ninja{..} = n { nStatuses = f <$> nStatuses }
  where 
    f st@Status{..} 
      | lMatch l src st = st { statusDur = statusMaxDur }
      | otherwise       = st

-- | Deletes one matching 'Status'.
removeStack :: Text -> Ninja -> Ninja
removeStack l n@Ninja{..} = 
    n { nStatuses = deleteStats 1 (lMatch l nId) nStatuses }

-- | Replicates 'removeStack'.
removeStacks :: Text -> Int -> Slot -> Ninja -> Ninja
removeStacks l i src n@Ninja{..} =
    n { nStatuses = deleteStats i (lMatch l src) nStatuses }

-- | Removes matching self-applied 'Status'es.
removeOwn :: Text -> Int -> Ninja -> Ninja
removeOwn l _ n@Ninja{..} = clear l nId n

-- | Sets an element in 'nCooldowns' to 0.
reset :: Int -- ^ Skill index (0-3)
      -> Int -- ^ 'Variant' index in 'characterSkills' of 'nCharacter'
      -> Ninja -> Ninja
reset s v n@Ninja{..} = n { nCooldowns = insertCd s v 0 nCooldowns }

-- | Sets 'nCooldowns' to 'S.empty'.
resetAll :: Ninja -> Ninja
resetAll n@Ninja{..} = n { nCooldowns = mempty }

-- | Resets 'nCharges' to four @0@s.
resetCharges :: Ninja -> Ninja
resetCharges n = n { nCharges = Seq.replicate 4 0 }

-- | Removes 'nHealth' down to a minimum.
sacrifice :: Int -> Int -> Ninja -> Ninja
sacrifice minh' hp n@Ninja{..} = n { nHealth = healthBound minh $ nHealth - hp }
  where 
    minh = min minh' $ minHealth n

-- Sets 'nHealth'.
setHealth :: Int -> Ninja -> Ninja
setHealth hp n = n { nHealth = healthBound 0 hp }

-- | Removes 'OnCounter' 'Trap's.
unCounter :: Ninja -> Ninja
unCounter n@Ninja{..} = n 
    { nTraps = [ p | p@Trap {trapTrigger = OnCounter _} <- nTraps ] }

-- | Resets matching 'nVariants'.
unVariant :: Text -> Ninja -> Ninja
unVariant l n@Ninja{..} = n { nVariants = f <$> nVariants }
  where 
    f = ensure . filter ((l /=) . variantL) . toList
    ensure []     = noVariant :| []
    ensure (x:xs) = x :| xs

-- | Updates an element in 'nCooldowns'. If 'True', also increments 'nCharges'.
updateCd :: Bool -> Int -> Skill -> Int -> Ninja -> Ninja
updateCd True a skill s n@Ninja{..} = 
    (updateCd False a skill s n) { nCharges = Seq.adjust' (+ 1) s nCharges }
    
updateCd False a Skill{..} s n@Ninja{..}
   | copied copying = n
   | cd == 0         = n
   | otherwise      = n { nCooldowns = insertCd s vari cd' nCooldowns }
  where 
    cd'                  = sync $ cd + 1 + getSnare n + a
    vari                 = variantCD . head $ nVariants `Seq.index` s
    copied NotCopied     = False
    copied (Shallow _ _) = True
    copied (Deep _ _)    = False
