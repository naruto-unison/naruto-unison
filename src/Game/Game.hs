{-# LANGUAGE OverloadedLists #-}

-- | 'Game' handling, primarily through 'Transform's.
--
-- If a status Transform ends in an apostrophe, it uses a custom display name.
-- If a trap Transform ends in an apostrophe, it is hidden from both players.
-- 
-- A common pattern in higher-order Transforms is @(Int -> Transform) -> Int@.
-- This modifies the amount passed to a Transform that requires an amount,
-- such as 'damage' (how much?) or 'applyDur' (how long?).
-- @(Int -> Transform)@ is the function, 
-- and the following @Int@ is the base amount.
--
-- For example, @withI "Example Status" 25 damage 50@ results in 
-- "damage 50, and an additional 25 if the user has Example Status."
-- This way, @withI "Example Status" 25@
-- looks like an adverb applied to @damage 50@,
-- even though @damage@ and @50@ are actually two separate arguments of @withI@.

module Game.Game
    ( 
    -- * Handler interface
      censor, forfeit
    -- * Turn handling
    , act, runTurn, updateChakra
    -- * Higher-order 'Transform' operations
    -- ** Operators
    , (§), (•), (°)
    -- ** Application
    , self, alliedTeam, enemyTeam, everyone
    -- ** Conditional
    , ifChan, ifHealthI, ifHealthU, ifI, ifU, ifnotI, ifnotU
    , ifStacks, ifnotStacks, killThen
    -- ** Scaling
    , perAffected, perDead, perDef, perHelpful
    , perI, perI', perU, perHealthI, perHealthU
    , withI, withU, withInvulnU, withChan
    -- * 'Transform's
    , delay, factory, vary, vary', varyLoadout, identity, identity'
    -- ** 'Chakras'
    , drain, gain, steal
    -- ** 'Channel'
    , interrupt, prolongChannel
    -- ** Combat
    , afflict, damage, execute, pierce, heal, leech, restore, sacrifice
    -- ** Copying
    , copyAll, copyLast, teach, teachOne
    -- ** Destructible
    , addDefense, bar, defend, defend', demolish, demolish'
    -- ** Lifted
    , alterCd, cancelChannel, cure, cureAll, cureBane, cureStun
    , kill, kill'
    , purge, refresh, remove, removeStack, removeStacks
    , reset, resetAll, resetCharges, setHealth
    -- ** 'Status'
    , addStack, addStacks, addStacks'
    , apply, apply', applyDur, applyDur', applyX, applyWith
    , bomb, bomb', bombWith, bombWith'
    , flag, flag', tag, tag', hide, hide'
    , snapshot
    -- *** Modification
    , hasten, prolong, setFace
    -- ** 'Trap'
    , onBreak, onBreak', selfBreak, removeTrap
    , trap, trap', trapFrom, trapFrom', trapPer, trapPer', trapWith
    -- * Invulnerability
    , invuln, invuln1, invuln'
    -- * Specialized
    , commandeer, kabuto
    ) where

import StandardLibrary

import qualified Data.Either           as Either
import qualified Data.List.NonEmpty    as NonEmpty
import qualified Data.Sequence         as Seq
import qualified Data.HashSet          as Set
import qualified Data.Text             as Text
import qualified System.Random         as Random
import qualified System.Random.Shuffle as Shuffle

import qualified Game.Ninja as N

import Calculus
import Game.Structure
import Game.Functions

-- * HANDLER INTERFACE

-- | Hides the opponent's team information and 'Invisible' data.
censor :: Player -> Game -> Game
censor player game@Game{..} = censorPlayer player game 
    { gameChakra = in2 (player == PlayerB) 0 gameChakra }

censorPlayer :: Player -> Game -> Game
censorPlayer player game = alter (censorNinja game player <$>) game

censorNinja :: Game -> Player -> Ninja -> Ninja
censorNinja game player n@Ninja{..}
  | alliedP player nId = n'
  | is Reveal n        = n'
  | otherwise          = n' 
      { nCooldowns = mempty
      , nCharges   = mempty
      , nVariants  = Seq.replicate 4 [noVariant]
      , nChannels  = filter ((not . (Invisible `elem`)) . classes . channelSkill) 
                     nChannels
      , nLastSkill = Nothing
      }
  where 
    n' = n { nStatuses = mapMaybe mst nStatuses
           , nTraps    = [ p | p@Trap{..} <- nTraps
                             , alliedP player trapSrc 
                               || Invisible `notElem` trapClasses 
                               || is Reveal (gameNinja trapSrc game)
                             ]
           }
    mst st@Status{..} 
      | alliedP player statusSrc = Just st
      | Invisible `elem` statusClasses 
        && not (is Reveal $ gameNinja statusC game) = Nothing
      | otherwise = case statusEfs of
          []       -> Just st
          [Reveal] -> Nothing
          _        -> Just st { statusEfs = delete Reveal statusEfs }

-- | Kills all 'Ninja's of a 'Player', ending the game.
forfeit :: Player -> Game -> Game
forfeit player = alter (forfeitN player <$>)

forfeitN :: Player -> Ninja -> Ninja
forfeitN player n@Ninja{..}
  | alliedP player nId = n { nHealth = 0 }
  | otherwise          = n

-- * TURN HANDLING

-- | The game engine's main function. Performs 'Act's and 'Channel's; 
-- applies effects from 'Bomb's, 'Barrier's, 'Delay's, and 'Trap's;
-- decrements all 'TurnBased' data; adds 'ChannelTag's;
-- and resolves 'Chakras' for the next turn.
runTurn :: Player -> [Act] -> Random.StdGen -> Game -> Game
runTurn player actions stdGen game = yieldVictor game'Full
  where 
    rs         = unfoldl Random.split stdGen
    rs'        = toList rs
    game'Acts  = foldl' (act []) game . zip actions $ Right <$> rs'
    chans      = getAllChannels player game'Acts
    game'Chan  = doDeaths .
                 doDelays .
                 doTraps .
                 turnPerTrigger player game .
                 doBarriers player .
                 doBombs Remove game .
                 decrStats .
                 turnTrigger player .
                 foldl' (act []) game'Acts . zip chans $ Right <$> rs'
    decrDelays = filter ((0 /=) . getDur) . mapMaybe decrTurn . gameDelays
    game'End   = doBombs Done game .
                 doBombs Expire game'Chan $
                 decr game'Chan { gameDelays = decrDelays game'Chan }
    vs         = opponent player
    game'Next  = updateChakra vs 
                 (take teamSize . Random.randomRs (0, 3) $ head rs) .
                 doDrain (rs !! 1) .
                 doDeaths .
                 doAfflicts player $
                 game'End { gamePlaying = vs }
    game'Clear = game'Next { gameDrain = (0, 0)
                           , gameSteal = (0, 0)
                           , gameTraps = mempty
                           }
    game'Futur = censor vs game'Clear
    chansNext  = getAllChannels vs game'Futur
    game'Ghost = foldl' (act []) game'Futur $
                 zip chansNext $ Right <$> NonEmpty.drop (teamSize * 2) rs  
    game'Full  = addTags game'Clear game'Ghost
    getChannels n = map (fromChannel n) . filter ((1 /=) . getDur) $
                    nChannels n
    getAllChannels p Game{..} = concatMap getChannels (alives p gameNinjas)
    decr g@Game{..} = g { gameNinjas = N.decr <$> gameNinjas }
    decrStats g@Game{..} = g { gameNinjas = N.decrStats <$> gameNinjas }
    
doDelays :: Game -> Game
doDelays game@Game{..} = foldl' (flip delayEf) game $
                         filter ((<= 1) . delayDur) gameDelays

doBombs :: Bomb -> Game -> Game -> Game
doBombs bombType game game' = foldl' (&) game' efs
  where 
    efs = concat $ Seq.zipWith boom (gameNinjas game) (gameNinjas game')
    boom n n' = doBomb bombType (nId n) <$> nStatuses n \\ nStatuses n'

doBomb :: Bomb -> Slot -> Status -> Game -> Game
doBomb bombType t Status{..} game = foldl' detonate game statusBombs
  where 
    detonate game' (b, f)
      | bombType == b = wrapEffect [Trapped] 
                        f statusSkill statusSrc statusSrc game' t
      | otherwise     = game'

doBarriers :: Player -> Game -> Game
doBarriers player game@Game{..} = foldl' doBarrier game barriers
  where 
    barriers = concatMap (map head . groupBy lEq . sort . nBarrier) gameNinjas
    doBarrier game' Barrier{..}
      | barrierDur == 1           = barrierDone barrierAmount game'
      | alliedP player barrierSrc = barrierWhile game'
      | otherwise                 = game'

doTraps :: Game -> Game
doTraps game@Game{..} = foldl' (&) game {gameTraps = mempty} gameTraps

doDeaths :: Game -> Game
doDeaths game = foldl' checkDeath game allSlots

doAfflicts :: Player -> Game -> Game
doAfflicts player game = foldl' (doAfflict player) game allSlots

doAfflict :: Player -> Game -> Slot -> Game
doAfflict player game t
  | aff == 0         = game
  | not $ isAlive nt = game
  | otherwise        = r' (N.attack aff) game t
  where 
    nt  = gameNinja t game
    aff = getNet player game nt

doDrain :: Random.StdGen -> Game -> Game
doDrain rand game@Game{..} = game { gameChakra = gameChakra' }
  where 
    gameChakra'            = (chakraA2 { rand = 0}, chakraB2 { rand = 0 })
    (chakraA0, chakraB0)   = gameChakra
    (randA, randB)         = Random.split rand
    (gainB, loseA, gainA') = processDrain True  game randA
    (gainA, loseB, gainB') = processDrain False game randB
    chakraA1               = chakraA0 + χ (gainA ++ gainA')
    chakraB1               = chakraB0 + χ (gainB ++ gainB')
    chakraA2               = chakraA1 - χ loseA
    chakraB2               = chakraB1 - χ loseB

processDrain :: Bool -> Game -> Random.StdGen 
             -> ([ChakraType], [ChakraType], [ChakraType])
processDrain isFst Game{..} rand
  | draining == 0 = (stolen, stolen, [])
  | draining > 0  = (stolen, stolen ++ drained, [])
  | otherwise     = (stolen, stolen, drained)
  where 
    chakras             = unχ . out2 isFst $ gameChakra
    draining            = out2 isFst gameDrain
    (stolen, shuffled') = splitAt (out2 isFst gameSteal) shuffled
    drained             = take    (abs draining)         shuffled'
    shuffled 
      | null chakras = chakras
      | otherwise    = Shuffle.shuffle' chakras (length chakras) rand

-- | Uses random integers to give a 'Player' chakras at the start of their turn.
updateChakra :: Player -> [Int] -> Game -> Game
updateChakra player chakras game@Game{..} = game { gameChakra = upd gameChakra }
  where 
    upd = byPar (fromEnum player) $ χGain player (toEnum <$> chakras) gameNinjas

addTags :: Game -> Game -> Game
addTags game game' = game { gameNinjas = gameNinjas' }
  where 
    gameNinjas'   = Seq.zipWith addGhost (gameNinjas game) (gameNinjas game')
    ghost ct      = decrTurn ct { tagGhost = True }
    unghost ct    = ct { tagGhost = False }
    addGhost n n' = n { nTags = unghost <$> nTags n' ++ mapMaybe ghost (nTags n') }

-- * ACTING

-- | Performs a single 'Act'. 
-- If Right, this is a new action, 
-- in which case random targets are chosen by 'StdGen'.
-- If Left, this is a continuation of some previous action,
-- in which case its random targets carry over from the previous action.
act :: [Affected] -> Game -> (Act, Either (Slot, Slot) Random.StdGen) -> Game
act affected' game'Swap (Act c s t, rando)
  | not $ isAlive n               = game
  | notChan s                     = game
  | Channeled `elem` affected     = game'
  | require skill == Unusable     = game
  | lack $ oldChakra - cost skill = game
  | otherwise                     = case snareTrap skill n of
      Just (n', sn) -> setNinja c (safeSnare sn s n') game
      _             -> game' { gameChakra = f $ gameChakra game' }
  where 
    f           = bySlot c (— cost skill)
    n           = gameNinja c game'Swap
    skill'      = getSkill n s
    (skill, affected'Swap, game) = case triggerSwap (classes skill') n of
        Just swapped -> ( swapSkill swapped skill'
                        , Swapped : affected'
                        , swapGame swapped
                        )
        Nothing   -> (skill', affected', game'Swap)
    oldChakra   = outSlot c $ gameChakra game
    hascharge   = charges skill > 0
    game'Cd     = updateCd hascharge 0 skill c s game
    affected    = Either.isRight s ? (Channeled :) $ affected'Swap
    game'F      = doEffects affected skill c c t rando game'Cd
    game'       = trigger affected skill c game'Cd .
                  Either.isLeft s ? addChannels (channel skill) c skill t $
                  game'F
    notChan (Left _)   = False
    notChan (Right s') = Channeled `elem` affected 
                         && not (isChanneling (label s') n)
    swapGame swapped = setNinja c n { nStatuses = delete swapped $ nStatuses n } 
                       game'Swap
    safeSnare sn (Left s') 
      | s' <= 3    = N.updateCd hascharge sn skill s'
    safeSnare _  _ = id
    
addChannels :: Channeling -> Slot -> Skill -> Slot -> Game -> Game
addChannels Instant _        _                      _ = id
addChannels chan    channelC channelSkill@Skill{..} channelT 
  | dur == 1 || dur == 2 = id
  | otherwise            = fn channelC addChans
  where 
    addChans n@Ninja{..} = n { newChans = Channel{..} : newChans }
    channelRoot          = copyRoot channelSkill channelC
    dur                  = copyDur copying . (+1) . sync $ getDur chan
    channelDur           = setDur dur chan

updateCd :: Bool -> Int -> Skill -> Slot -> Either Int Skill -> Game -> Game
updateCd charge a skill c (Left s) = fn c $ N.updateCd charge a skill s
updateCd _      _ _     _ _        = id

-- | Adds a 'Trap' to 'gameTraps'.
entrap :: Slot -> Seq TrapTransform -> Game -> Game
entrap src trapped game@Game{..} = game
    { gameTraps = gameTraps ++ (flip ($ 0) src <$> trapped) }

-- | Directly performs a 'Trap'.
entrap' :: Slot -> Seq TrapTransform -> Game -> Game
entrap' src trapped game = foldl' (&) game (flip ($ 0) src <$> trapped)

-- | After 'act', the new game state is compared to the old game state
-- and differences trigger 'Trap's.
trigger :: [Affected] -> Skill -> Slot 
        -> Game -- ^ New
        -> Game -- ^ Old
        -> Game
trigger affected Skill{..} c game game'Pre
  | Channeled `elem` affected = game'
  | not (null counttr)   = entrap c counttr $
                           fn c N.unCounter game { gameNinjas = ns'Cp }
  | otherwise            = game'F
  where 
    n       = gameNinja c game
    n'Pre   = gameNinja c game'Pre
    dmgTot  = sum $ 
              Seq.zipWith healthLost (gameNinjas game) (gameNinjas game'Pre)
    n'      = n'Pre { nTraps = updatePer <$> nTraps n'Pre }
    game'   = setNinja c n' game'Pre
    ns'Cp   = Seq.zipWith cop (gameNinjas game) (gameNinjas game')
    als     = Seq.fromList [ (a, a') | a  <- allies  c game 
                                     | a' <- allies  c game' 
                                     ]
    ens     = Seq.fromList [ (a, a') | a  <- enemies c game 
                                     | a' <- enemies c game' 
                                     ]
    allNs   = [ (nt, nt') | nt  <- gameNinjas game,  nId nt  /= c 
                          | nt' <- gameNinjas game', nId nt' /= c
                          ]
    chk     = outSlot' c
    chakraF = chk (gameDrain game') /= 0 || chk (gameSteal game') /= 0
    harmful = not (null harmed) || nBarrier n /= nBarrier n'
    counttr = getTraps True (OnCounter Uncounterable) n'
            ++ classTrs (harmful && Uncounterable `notElem` classes) OnCounter classes n'
    healed  = [ nt' | (nt, nt') <- als, nHealth nt < nHealth nt' ]
    helped  = [ nt' | (nt, nt') <- als, nt /= nt' ]
    harmed  = [ nt' | (nt, nt') <- ens, nt /= nt' ]
    dmgEns  = [ nt' | (nt, nt') <- allNs, nHealth nt > nHealth nt' ]
    damaged = [ nt' | (nt, nt') <- allNs, nHealth nt > nHealth nt' ]
    stunned = [ nt' | (nt, nt') <- ens, is' Stun nt', not $ is' Stun nt ]
    addFlags = (game'Pre /= game) ? Set.insert Acted . 
               harmful ? Set.insert Harmed
    n'Acted = n' { nFlags = addFlags $ nFlags n' }
    n'Taunt = case ( harmed, getTaunting n' ) of
                ( [Ninja{nId}], Just (dur, st@Status{..}) ) -> 
                  let st' = Status 1 statusL statusSrc nId nId
                            statusSkill [Taunt] statusClasses [] 
                            (sync dur) (sync dur)
                  in n'Acted { nStatuses = st' : delete st (nStatuses n')} 
                _                 -> n'Acted
    game'T  = setNinja c n'Taunt game' 
    trapsF  = join $ join
              [ getTrapsFrom OnDamage n' <$> damaged
              , getTrapsFrom OnStun   n' <$> stunned
              , getTrapsFrom OnHarm   n' <$> harmed
              ] :: Seq (Game -> Game)
    trapsC  = join
              [ counttr
              , getTrapsTo chakraF              OnChakra   n'
              , getTrapsTo (not $ null dmgEns)  OnDamage   n'
              , getTrapsTo (not $ null stunned) OnStun     n'
              , getTrapsTo (was Immune (n', n)) OnImmune   n'
              , getTrapsTo harmful              OnHarm     n'
              , classTrs (game'Pre /= game)     OnAction   classes n'
              ]
    trapsN  = join $ join
              [ getTraps True OnHelped          <$> helped
              , getTraps True OnHealed          <$> healed
              , getTraps True (OnStunned Multi) <$> stunned
              , classTrs True OnHarmed classes  <$> harmed
              , classTrs True OnDamaged classes <$> damaged
              , onStunned                       <$> stunned
              ]
    trapsP  = getTrackTraps True TrackDamage n'
            ++ getPerTraps (not $ null dmgEns) PerDamage dmgTot n'
    game'Tr = entrap c trapsN $
              entrap c trapsC game'T { gameTraps = gameTraps game'T ++ trapsP }
    game'F  = foldl' (&) game'Tr trapsF
    onStunned nt = classTrs True OnStunned (getStun nt) nt
    cop t t' = t { nCopied = nCopied t' }
    was ef (nt, nt') = is' ef nt' && not (is' ef nt)
    updatePer p@Trap{..} = case trapTrigger of
        TrackDamage -> p { trapTrack = dmgTot + trapTrack }
        _           -> p

healthLost :: Ninja -> Ninja -> Int
healthLost n n' = max 0 $ nHealth n - nHealth n'

getTurnPerTraps :: Player -> Ninja -> Ninja -> Seq (Game -> Game)
getTurnPerTraps player n n'
  | hp < 0 && alliedP player nid       = getPerTraps True PerHealed (-hp) n'
  | hp > 0 && not (alliedP player nid) = getPerTraps True PerDamaged hp n'
  | otherwise                          = mempty
  where 
    nid = nId n'
    hp = nHealth n - nHealth n'

turnPerTrigger :: Player -> Game -> Game -> Game
turnPerTrigger player game game' = game'Track
  where 
    gameNinjas' = Seq.zipWith trackTurn (gameNinjas game) (gameNinjas game')
    trapsAll    = concat $
                  Seq.zipWith (getTurnPerTraps player) (gameNinjas game) gameNinjas'
    game'Track  = game' { gameNinjas = gameNinjas'
                        , gameTraps  = trapsAll ++ gameTraps game'
                        }

turnTrigger :: Player -> Game -> Game
turnTrigger player game@Game{..} = game { gameTraps = trapsAll ++ gameTraps }
  where 
    trapsAll = gameNinjas >>= getTurnTraps
    getTurnTraps n@Ninja{..}
      | not $ alliedP player nId = mempty
      | otherwise = (\t -> t 0 nId)
                    <$> getTrapsTo (Acted `notElem` nFlags) OnNoAction n
                     ++ getTrapsTo (Harmed `notElem` nFlags) OnNoHarm n

trackTurn :: Ninja -> Ninja -> Ninja
trackTurn n n' = n' { nTraps = updatePer <$> nTraps n' }
  where 
    dmgdTot = healthLost n n'
    updatePer p@Trap{..} = case trapTrigger of
        TrackDamaged -> p { trapTrack = dmgdTot + trapTrack }
        _            -> p
      
-- | If the 'nHealth' of a 'Ninja' reaches 0, 
-- they are either resurrected by triggering 'OnRes'
-- or they die and trigger 'OnDeath'.
-- If they die, their 'Soulbound' effects are canceled.
checkDeath :: Game -> Slot -> Game
checkDeath game t
  | nHealth nt > 0 = game
  | not $ null res = entrap' t res $ fn t resN game
  | otherwise      = alter (map unr) .
                     entrap' t (getTraps True OnDeath nt) $ fn t dieN game
  where 
    nt       = gameNinja t game
    resN nt' = nt' { nHealth = 1
                   , nTraps = filter ((OnRes /=) . trapTrigger) $ nTraps nt'
                   }
    dieN nt' = nt' { nTraps = filter ((OnDeath /=) . trapTrigger) $ nTraps nt' }
    res      = getTraps (not $ is Plague nt) OnRes nt
    unr n     = n { nStatuses = [ st | st@Status{..} <- nStatuses n 
                                     , (t /= statusSrc && t /= statusC) 
                                       || Soulbound `notElem` statusClasses
                                     ]
                  , nTraps = [ tp | tp@Trap{..} <- nTraps n
                                  , t /= trapSrc || Soulbound `notElem` trapClasses 
                                  ]
                  , nChannels = filter ((t /=) . channelT) $ nChannels n
                  }

broken :: Ninja -> Ninja -> (Ninja, Seq TrapTransform)
broken n n' = 
    ( n' { nTraps = filter ((`notElem` broke) . trapTrigger) $ nTraps n' }
    , [ trapEf | Trap{..} <- nTraps n', trapTrigger `elem` broke ]   
    )
  where 
    broke = OnBreak <$> nub (defenseL <$> nDefense n) 
                     \\ nub (defenseL <$> nDefense n')

-- * PROCESSING

doEffects :: [Affected] -> Skill -> Slot -> Slot -> Slot 
          -> Either (Slot, Slot) Random.StdGen -> Game -> Game
doEffects affected skill@Skill{..} src c t rando game =
    foldl' (&) game .
    zipWith (doEffect affected skill src c t) (splitRands rando) $
    (Channeled `notElem` affected) ? (start ++) $ effects
  where 
    splitRands a@(Left _)     = repeat a
    splitRands (Right stdGen) = Right <$> toList (unfoldl Random.split stdGen)

doEffect :: [Affected] -> Skill -> Slot-> Slot -> Slot 
         -> Either (Slot, Slot) Random.StdGen -> (Target, Transform) 
         -> Game -> Game
doEffect affected skill' src c t rando (target, f) game
  | Countered `notElem` affected                  = done
  | Parrying `elem` affected && target == Ally   = game
  | Parrying `notElem` affected && target `elem` anyone = game
  | otherwise                             = done       
  where
    anyone = [Self, Allies, Enemies, Everyone] :: [Target]
    skill = case target of
        Self -> skill' { classes = Unshifted : classes skill' }
        _    -> skill'
    done = foldl' (wrapEffect affected f skill src c) game $
           choose (rAlly, rEnemy) target c t   
    (rAlly, rEnemy) = chooseRands (gameNinja src game) skill game c rando

chooseRands :: Ninja -> Skill -> Game -> Slot 
            -> Either (Slot, Slot) Random.StdGen -> (Maybe Slot, Maybe Slot)
chooseRands _ _              _        _ (Left a)        = both Just a 
chooseRands n skill@Skill{..} Game{..} c (Right stdGen) = (rAlly, rEnemy) 
  where 
    targets = map nId . filter (targetable skill n n) $ toList gameNinjas
    (livingAllies, livingEnemies) = partition (allied c) targets
    (rAlly, stdGen')              = pick stdGen livingAllies
    (rEnemy, _)                   = pick stdGen' livingEnemies

wrapEffect :: [Affected] -> Transform -> Transform
wrapEffect affected f skill@Skill{..} src c game t
  | gameMock game                     = game'Do
  | Direct `elem` classes                  = game'Do
  | Applied `elem` affected                = game'Do
  | classes `intersects` getInvincible nt  = game'T
  | Trapped `elem` affected                = game'Do
  | not $ targetable skill' nSrc n nt = game'T
  | skill `elem` nParrying nt              = game'T
  | not new                           = game'Do
  | is Uncounter nt                   = game'Post
  | otherwise = case allow Redirected $? redir classes nt of
      Just red -> wrapEffect (Redirected : affected) 
                  f skill src (tOrC red) game'Mimic red
      Nothing    -> case allow Redirected $? reapply classes nt of
        Just red -> wrapEffect (Redirected : affected) 
                    f skill src (tOrC red) game'Shift red
        Nothing    -> case allow Countered $? parry skill nt of
          Just (nt', Status{..}, f') -> wrapEffect (Trapped : affected) f' 
                                       statusSkill statusSrc statusSrc 
                                       (setNinja t nt' game'Mimic) c
          Nothing -> case allow Reflected $? reflect classes n nt of
            Just nt'  -> wrapEffect (Reflected : affected) f skill src t 
                        (setNinja t nt' game'Mimic) c
            Nothing   -> case allow Countered $? counter classes n nt of
              Just nt' -> setNinja t nt' game'Mimic
              Nothing  -> let (nt', p) = broken nt (gameNinja t game'Post) in
                          entrap c p $ setNinja t nt' game'Post
  where 
    nt         = gameNinja t game
    nt'T
      | new       = nt { nFlags = Set.insert Targeted $ nFlags nt }  
      | otherwise = nt
    game'T     = setNinja t nt'T game
    skill' 
      | Countered `elem` affected = skill { classes = Bypassing : classes }
      | otherwise            = skill
    new        = not $ [Applied, Channeled, Delayed, Trapped] `intersects` affected
    cTag       = ChannelTag (copyRoot skill src) src skill False 3
    (nt'C, fc) 
      | t == src || Instant == channel             = (nt'T, False)
      | [Applied, Disrupted] `intersects` affected = (nt'T, False)
      | not $ isChanneling label n                 = (nt'T, False)
      | otherwise = (nt'T { nTags = cTag : nTags nt'T }, True)
    game'Tag   = fc ? setNinja t nt'C $ game'T
    n          = gameNinja c game'T
    nSrc       = gameNinja src game'T
    noharm     = allied c t && allied src t
    harm       = not noharm
    shifted    = [Redirected, Reflected, Swapped] `intersects` affected 
    addClasses = (shifted && not ([Unshifted, Shifted] `intersects` classes)) ?
                    (Shifted :) .
                 (Trapped `elem` affected && BaseTrap `notElem` classes) ?
                    (Direct :)
    game'Shift = wrapEffect (Redirected : affected) f skill 
                  { classes = Unshifted : classes } src c game'Mimic t
    skill'Do   = skill { classes = addClasses classes }
    game'Do    = f skill'Do src c game'Mimic t
    onlyDmgNs  = Seq.zipWith onlyDmg (gameNinjas game'Mimic) 
                                    (gameNinjas game'Do)
    game'Post  
      | is Silence nSrc = game'Mimic { gameNinjas = onlyDmgNs }
      | otherwise       = game'Do
    allow aff  = harm && not (is AntiCounter n) && aff `notElem` affected
    tOrC red
      | allied red t = c
      | otherwise    = t
    setLast n' = n' { nLastSkill = Just skill }
    game'Mimic 
      | new       = fn c setLast . 
                    foldl' (doCopy True Shallow c skill) game'Tag $ 
                    copy classes n harm
      | otherwise = game'Tag
    onlyDmg nx nx' = nx { nHealth   = min (nHealth nx') (nHealth nx) 
                        , nStatuses = nStatuses nx'
                        }
-- * HIGHER-ORDER

-- ** COMPOSITION

-- | '$' with higher fixity.
infixr 2 §
(§) :: ∀ a b. (a -> b) -> a -> b
(§) = ($)
{-# INLINE (§) #-}

-- | Function composition.
infixl 1 •
(•) :: Transform -> Transform -> Transform
(f • g) skill src c game t = g skill src c (f skill src c game t) t
{-# INLINE (•) #-}

-- | Same as '•', but with higher fixity.
infixl 3 °
(°) :: Transform -> Transform -> Transform
(°) = (•)
{-# INLINE (°) #-}

-- ** APPLICATION

-- | Directly applies an effect to the original source 
-- without checking anything (invulnerability, requirements, etc.)
self :: Transform -> Transform
self f skill src c game t = f skill src t game c

-- | Directly applies an effect to all allies of the original source 
-- without checking anything (invulnerability, requirements, etc.)
alliedTeam :: Transform -> Transform
alliedTeam f skill src c game _ = 
    foldl' (wrapEffect [Applied] f skill src c) game $ allySlots src

-- | Directly applies an effect to all enemies of the original source 
-- without checking anything (invulnerability, requirements, etc.)
enemyTeam :: Transform -> Transform
enemyTeam f skill src c game _ =
    foldl' (wrapEffect [Applied] f skill src c) game $ enemySlots src

-- | Directly applies an effect to all characters 
-- without checking anything (invulnerability, requirements, etc.)
everyone :: Transform -> Transform
everyone f skill src c game _ = 
    foldl' (wrapEffect [Applied] f skill src c) game allSlots

-- ** CONDITIONAL

-- | User 'has'
ifI :: Text -> Transform -> Transform
ifI l f skill src c game@Game{..} t
  | gameMock                       = f skill src c game t
  | has l src n || isChanneling l n = f skill src c game t
  | otherwise                      = game
  where
    n = gameNinja src game

-- | 'not' 'ifI'
ifnotI :: Text -> Transform -> Transform
ifnotI l f skill src c game@Game{..} t
  | gameMock                             = f skill src c game t
  | not $ has l src n || isChanneling l n = f skill src c game t
  | otherwise                            = game
  where 
    n = gameNinja src game

-- | Target 'has'
ifU :: Text -> Transform -> Transform
ifU l f skill src c game@Game{..} t
  | gameMock                     = f skill src c game t
  | has l src $ gameNinja t game = f skill src c game t
  | otherwise                    = game

-- | 'not' 'ifU'
ifnotU :: Text -> Transform -> Transform
ifnotU l f skill src c game@Game{..} t
  | gameMock                           = f skill src c game t
  | not . has l src $ gameNinja t game = f skill src c game t
  | otherwise                          = game

-- | User 'numStacks' exceeds a threshold
ifStacks :: Text -> Int -> Transform -> Transform
ifStacks l i f skill src c game@Game{..} t
  | gameMock                                 = f skill src c game t
  | numStacks l src (gameNinja src game) >= i = f skill src c game t
  | otherwise                                = game

-- | 'not' 'ifStacks'
ifnotStacks :: Text -> Int -> Transform -> Transform
ifnotStacks l i f skill src c game@Game{..} t
  | gameMock                                 = f skill src c game t
  | numStacks l src (gameNinja src game) < i = f skill src c game t
  | otherwise                                = game

-- | User 'isChanneling'
ifChan :: Text -> Transform -> Transform
ifChan l f skill src c game@Game{..} t
  | gameMock                            = f skill src c game t
  | isChanneling l $ gameNinja src game = f skill src c game t
  | otherwise                           = game

-- | User 'nHealth' is within a range
ifHealthI :: Int -> Int -> Transform -> Transform
ifHealthI minHp maxHp f skill src c game@Game{..} t
  | gameMock                = f skill src c game t
  | hp >= minHp && hp <= maxHp = f skill src c game t
  | otherwise               = game
  where 
    hp = nHealth $ gameNinja c game

-- | Target 'nHealth' is within a range
ifHealthU :: Int -> Int -> Transform -> Transform
ifHealthU minHp maxHp f skill src c game@Game{..} t
  | gameMock                = f skill src c game t
  | hp >= minHp && hp <= maxHp = f skill src c game t
  | otherwise               = game
  where 
    hp = nHealth $ gameNinja t game

-- | Kills and performs a 'Transform' on the user if the target is killed.
-- Has no effect if the target is invulnerable, has 'Endure', etc.
killThen :: Transform -> Transform
killThen f skill src c game@Game{..} t
  | gameMock                          = wrapEffect [] f skill src c game' src
  | not . isAlive $ gameNinja t game' = wrapEffect [] f skill src c game' src
  | otherwise                         = game'
  where 
    game' = kill skill src c game t

-- ** SCALING

-- | User 'has'
withI :: Text -> Int -> (Int -> Transform) -> Int -> Transform
withI l amount f base skill src c game@Game{..} t
  | total == 0 = game
  | otherwise  = f total skill src c game t
  where 
    total = has l src (gameNinja src game) ? (amount +) $ base

-- | Target 'has'
withU :: Text -> Int -> (Int -> Transform) -> Int -> Transform
withU l amount f base skill src c game t
  | total == 0 = game
  | otherwise  = f total skill src c game t
  where 
    total = has l src (gameNinja t game) ? (+ amount) $ base

-- | Target is 'Immune' to any 'Class'
withInvulnU :: Int -> (Int -> Transform) -> Int -> Transform
withInvulnU amount f base skill src c game t
  | total == 0 = game
  | otherwise  = f total skill src c game t
  where 
    total = (not . null . getImmune $ gameNinja t game) ? (amount +) $ base

-- | User 'isChanneling'
withChan :: Text -> Int -> (Int -> Transform) -> Int -> Transform
withChan l amount f base skill src c game t
  | total == 0 = game
  | otherwise  = f total skill src c game t
  where 
    total = isChanneling l (gameNinja src game) ? (+ amount) $ base

-- | User 'numStacks'
perI :: Text -> Int -> (Int -> Transform) -> Int -> Transform
perI l amount f base skill src c game t
  | total == 0 = game
  | otherwise  = f total skill src c game t
  where 
    total = base + amount * numStacks l src (gameNinja src game)

-- | User 'numStacks' divided by a denominator
perI' :: Text -> Int -> Int -> (Int -> Transform) -> Int -> Transform
perI' l amount denom f base skill src c game t
  | total == 0 = game
  | otherwise  = f total skill src c game t
  where 
    total = base + amount * numStacks l src (gameNinja src game) `quot` denom

-- | Target 'numStacks'
perU :: Text -> Int -> (Int -> Transform) -> Int -> Transform
perU l amount f base skill src c game t
  | total == 0 = game
  | otherwise  = f total skill src c game t
  where 
    total = base + amount * numStacks l src (gameNinja t game)

-- | User 'nHealth'
perHealthI :: (Int -> Int) -> (Int -> Transform) -> Int -> Transform
perHealthI ofhp f base skill src c game =
    f ((base +) . ofhp . nHealth $ gameNinja src game) skill src c game

-- | Target 'nHealth'
perHealthU :: (Int -> Int) -> (Int -> Transform) -> Int -> Transform
perHealthU ofhp f base skill src c game t =
    f ((base +) . ofhp . nHealth $ gameNinja t game) skill src c game t

-- | Number of enemies matching 'has'
perAffected :: Text -> Int -> (Int -> Transform) -> Int -> Transform
perAffected l amt f base skill src c game@Game{..} =
    f (base + amt * total) skill src c game
  where 
    total = length $ filter (has l src) gameNinjas

-- | Sum of user's matching 'Defense' 
perDef :: Text -> (Int -> Transform) -> Int -> Transform
perDef l f amt skill src c game = f (amt * total) skill src c game
  where 
    total = sum [ defenseAmount | Defense{..} <- nDefense $ gameNinja c game
                                , defenseSrc == src
                                , defenseL == l
                                ]

-- | Number of helpful 'Status'es and 'Defense's on the user that they did not
-- cause themselves. 
perHelpful :: Int -> (Int -> Transform) -> Int -> Transform
perHelpful amt f base skill src c game = f (base + amt * total) skill src c game
  where 
    Ninja{..} = gameNinja src game
    total      = length stats + length defs
    stats = nubBy lEq [ st | st@Status{..} <- nStatuses
                           , any helpful statusEfs
                           , nId /= statusSrc
                           , allied nId statusSrc
                           , Hidden `notElem` statusClasses
                          ]
    defs  = nubBy lEq [ d | d@Defense{..} <- nDefense 
                          , nId /= defenseSrc
                          , allied nId defenseSrc
                          ]
  
-- | Number of dead allies.
perDead :: Int -> (Int -> Transform) -> Int -> Transform
perDead amt f base skill src c game@Game{..} =
    f (base + amt * total) skill src c game
  where 
    total = length [ nId | Ninja{..} <- gameNinjas
                         , nId /= c, allied nId c
                         , nHealth == 0
                         ]

-- * TRANSFORMS

-- ** LIFTED

-- | Applies a 'Ninja' transformation to the target.
r' :: (Ninja -> Ninja) -> ( Game -> Slot -> Game)
r' f game t = fn t f game

rt :: (Ninja -> Ninja) -> Slot -> Game -> Slot -> Game
rt = const . r'

r :: (Ninja -> Ninja) -> Transform
r = const . const . const . r'

-- | 'N.alterCd'
alterCd :: Int -> Int -> Int -> Transform
alterCd s v = r . N.alterCd s v

-- | 'N.cancelChannel'
cancelChannel :: Text -> Transform
cancelChannel = r . N.cancelChannel

-- | 'N.cure'
cure :: (Effect -> Bool) -> Transform
cure match = r $ N.cure match

cureAll :: Transform
cureAll = cure $ const True

-- | 'N.cureBane'
cureBane :: Transform
cureBane = r N.cureBane

cureStun :: Transform
cureStun = cure cured
  where 
    cured (Stun _) = True
    cured _        = False

-- | 'N.kill'
kill :: Transform
kill = const . const . const $ r' (N.kill True)
-- | 'N.kill''
kill' :: Transform
kill' = const . const . const $ r' (N.kill False)

-- | 'N.purge'
purge :: Transform
purge = r N.purge

-- | 'N.refresh'
refresh :: Text -> Transform
refresh l _ = rt . N.refresh l

-- | 'N.clear'
remove :: Text -> Transform
remove l _ src = rt $ N.clear l src

-- | 'N.removeStack'
removeStack :: Text -> Transform
removeStack = r . N.removeStack

-- | 'N.removeStacks'
removeStacks :: Text -> Int -> Transform
removeStacks l i _ = rt . N.removeStacks l i

-- | 'N.reset'
reset :: Text -> Text -> Transform
reset l = r . N.reset l

-- | 'N.resetAll'
resetAll :: Transform
resetAll = r N.resetAll

-- | 'N.resetCharges'
resetCharges :: Transform
resetCharges = r N.resetCharges

-- | 'N.setHealth'
setHealth :: Int -> Transform
setHealth = r . N.setHealth

-- *** BASE

-- | Adds a 'Delay'.
delay :: Int -> Transform -> Transform
delay dur' f' skill@Skill{..} src c game@Game{..} t
    | gameMock     = wrapEffect [Delayed] f' skill src c game t
    | past copying = game
    | otherwise    = game { gameDelays = Delay c skill f dur : gameDelays }
  where 
    dur      = incr $ sync dur'
    f game' = wrapEffect [Delayed] f' skill src c game' t
    past (Shallow _ d) = dur > d
    past (Deep    _ d) = dur > d
    past NotCopied     = False

-- | Factory resets a 'Ninja' with 'ninjaReset'.
factory :: Transform
factory = r ninjaReset

varyFull :: Bool -> Int -> Text -> Text -> Transform
varyFull from dur l variant skill src c game t =
    safe (gameNinja t game) l variant skill src c game t
  where
    safe = skillSafe identity (unsafeVary from dur)

-- | Adds a 'Variant' with a different duration as its originating 'Skill'.
vary' :: Int -> Text -> Text -> Transform
vary' = varyFull False

-- | Adds a 'Variant' that lasts as long as the skill.
vary :: Text -> Text -> Transform
vary l variant skill@Skill{..} = case getDur channel of
    1   -> const $ const const
    dur -> varyFull True dur l variant skill

-- | Adds a 'Variant' by index.
unsafeVary :: Bool -> Int -> Int -> Int -> Transform
unsafeVary variantFrom dur s variantV skill _ _ game t = case copying skill of
    Shallow _ _ -> game
    _           -> setNinja t n' game 
  where 
    variantDur = copyDur (copying skill) . sync $ incr dur
    n          = gameNinja t game
    variantVCD = varicd $ getSkill' n s variantV
    variantL = case channel skill of
        Instant -> ""
        _       -> label skill
    n'         
      | variantDur <= 0 = 
        n { nVariants = Seq.update s [Variant{..}] $ nVariants n }
      | otherwise = 
        n { nVariants = Seq.adjust' (Variant{..} <|) s $ nVariants n }

-- | Used for 'Character's who swap between different sets of skills, 
-- such as Chōji and Tenten (S).
varyLoadout :: Int  -- ^ Base offset added to the first 'Variant' slot
            -> Int  -- ^ Base offset added to the second 'Variant' slot
            -> Int  -- ^ Base offset added to the third 'Variant' slot
            -> Bool -- ^ Whether to affect the fourth 'Variant' slot at all
            -> Int  -- ^ Counter added to all 'Variant' slots 
            -> Transform -- ^ Recalculates the 'Variant's of a target 'Ninja'
varyLoadout a b c d i = d ? (• unsafeVary False 0 3 i)
                      $ unsafeVary False 0 0 (i + a) 
                      • unsafeVary False 0 1 (i + b)
                      • unsafeVary False 0 2 (i + c)

-- | 'id'.
identity :: Transform
identity _ _ _ game _ = game

-- | 'const' 'identity'.
identity' :: ∀ a. a -> Transform
identity' = const identity

-- ** CHAKRAS

gain :: [ChakraType] -> Transform
gain chakras _ _ _ game@Game{..} t = game
    { gameChakra = bySlot t (+ χ nonrands) gameChakra
    , gameDrain  = bySlot t (— length rands) gameDrain
    }
  where (rands, nonrands) = partition (== Rand) chakras

drain :: Int -> Transform
drain amount _ _ _ game@Game{..} t
  | is Enrage $ gameNinja t game = game
  | otherwise = game { gameDrain = bySlot t (+ amount) gameDrain }

steal :: Int -> Transform
steal amount _ _ _ game@Game{..} t
  | is Enrage $ gameNinja t game = game
  | otherwise = game { gameSteal = bySlot t (+ amount) gameSteal }

-- ** CHANNELS

interrupt :: Transform
interrupt _ _ _ game t
  | is Enrage nt = game
  | otherwise    = doDisrupts nt' disr $ setNinja t nt' game
  where 
    nt   = gameNinja t game
    disr = filter disrupted $ nChannels nt
    nt'  = nt { nChannels = filter (not . disrupted) $ nChannels nt }
    disrupted Channel {channelDur = (Control _)} = True
    disrupted Channel {channelDur = (Action _)}  = True
    disrupted _                                  = False

doDisrupt :: Ninja -> Game -> Channel -> Game
doDisrupt Ninja{..} game Channel{..} = foldl' (flip f) game disr
  where 
    l    = label channelSkill
    disr =  (Self,  r $ N.unVariant l)
          : (Enemy, remove l) 
          : disrupt channelSkill
    f    = doEffect [Channeled, Disrupted]
            channelSkill nId nId channelT (Left (channelT, channelT))

doDisrupts :: ∀ o. (MonoFoldable o, Element o ~ Channel)
           => Ninja -> o -> Game -> Game
doDisrupts n chans game = foldl' (doDisrupt n) game chans

disruptAll :: Slot -> [Effect] -> Game -> Slot -> Game
disruptAll t fs game c
  | allied t c = game
  | null disr  = game
  | otherwise  = doDisrupts n disr $
                 setNinja c n { nChannels = nChannels n \\ disr } game
  where 
    n      = gameNinja c game
    disr   = filter disrupted $ nChannels n
    immune = any (\cla -> [Immune cla, Invincible cla] `intersects` fs) . classes
    disrupted Channel{..} = case channelDur of
        Control _ -> t == channelT && immune channelSkill
        _         -> False

prolongChannel :: Int -> Text -> Transform
prolongChannel dur l = r $ \n@Ninja{..} -> n { nChannels = f <$> nChannels }
  where 
    f ch@Channel{..} 
      | getDur ch <= 0          = ch
      | label channelSkill /= l = ch
      | otherwise = 
          setDur (copyDur (copying channelSkill) $ getDur ch + sync dur) ch

-- ** COMBAT

data Attack 
    = AttackAfflict 
    | AttackPierce 
    | AttackDamage 
    | Demolish
    deriving (Eq)

attack :: Attack -> Int -> Transform
attack _   0  _ _  _     game _ = game
attack atk hp Skill{..} src c game t
  | classes' `intersects` getInvincible nt = game
  | not direct && is (Stun class') n = game
  | hp'tSt <= 0                      = game
  | atk == AttackAfflict             = tr $ fn t (N.attack hp'tSt) game
  | atk == Demolish || hp'Def <= 0   = tr $ setNinja t nt'Def game'
  | hp'Def == 0                      = game'
  | otherwise = tr $ setNinja t (N.attack hp'Def nt'Def) game'
  where 
    direct   = Direct `elem` classes
    n        = gameNinja c game
    nt       = gameNinja t game
    class' = case atk of
        AttackAfflict -> Affliction
        _             -> NonAffliction
    classes' = class' : classes
    hp'cSt   
      | direct    = toRational hp
      | otherwise = (getScale classes' n *) . toRational $
                    hp + getLink src nt + getStrengthen classes' n 
    reduces  = atk == AttackDamage && not (is Pierce n) && not (is Expose nt)
    reduced  = (— toRational (getReduce classes' nt)) .
               (* getWard classes' nt)
    hp'tSt   = truncate .
                (— toRational (getReduce [Affliction] nt)) .
                reduces ? reduced .
                (* getWard [Affliction] nt) .
                (atk /= AttackAfflict && not direct) ? 
                (— toRational (getWeaken classes' n)) $
                hp'cSt + toRational (getBleed classes' nt)
    damaged = case atk of
        AttackAfflict -> hp'tSt > 0
        _             -> hp'Def > 0
    tr       = damaged ? entrap c (getTraps True (OnDamaged class') nt)
    game'    
      | direct    = game
      | otherwise = setNinja c n { nBarrier = barrier' } game
    nt'Def   = nt { nDefense = defense' }
    (hp'Bar, barrier') = absorbBarrier hp'tSt (nBarrier n)
    (hp'Def, defense') 
      | direct    = absorbDefense hp'tSt (nDefense nt)
      | otherwise = absorbDefense hp'Bar (nDefense nt)

absorbDefense :: Int -> [Defense] -> (Int, [Defense])
absorbDefense hp (d@Defense{..} : defs)
  | defenseAmount > hp = (0, d { defenseAmount = defenseAmount - hp } : defs)
  | otherwise          = absorbDefense (hp - defenseAmount) defs
absorbDefense hp [] = (hp, [])

absorbBarrier :: Int -> [Barrier] -> (Int, [Barrier])
absorbBarrier hp (barr@Barrier{..} : barrs)
  | barrierAmount > hp = (0, barr { barrierAmount = barrierAmount - hp } : barrs)
  | otherwise          = absorbBarrier (hp - barrierAmount) barrs
absorbBarrier hp [] = (hp, [])

-- | Damage that cannot be reduced and penetrates 'Barrier' and 'Defense'.
afflict :: Int -> Transform
afflict = attack AttackAfflict
-- | Damage that cannot be reduced.
pierce  :: Int -> Transform
pierce  = attack AttackPierce
-- | Ordinary damage.
damage  :: Int -> Transform
damage  = attack AttackDamage

-- | Kills the target if their 'nHealth' goes below a threshold.
execute :: (Int -> Transform) -> Int -> Transform
execute attackType amount skill src c game t
  | executed  = flag' "executed" skill src c attacked src
  | otherwise = attacked
  where 
    attacked = attackType amount skill src c game t
    executed = not . isAlive $ gameNinja t attacked

-- | Adds to 'nHealth'.
heal :: Int -> Transform
heal hp _ src _ game t
  | not $ is Plague nt = setNinja t nt { nHealth = nHealth' } game
  | otherwise          = game
  where 
    nt       = gameNinja t game
    hp'      = getBoost src nt * hp + getBless (gameNinja src game)
    nHealth' = healthBound (minHealth nt) $ hp' + nHealth nt

-- | Steals 'nHealth' from the target.
leech :: Int -> (Int -> Transform) -> Transform
leech hp f skill src c game t = f hp' skill src c 
                                (setNinja t (N.attack hp' nt) game) t
  where 
    nt  = gameNinja t game
    hp' = min hp $ nHealth nt

-- | Restores a percentage of 'nHealth'.
restore :: Int -> Transform
restore percent _ src _ game t
  | not $ is Plague nt = setNinja t nt { nHealth = hp } game
  | otherwise          = game
  where 
    nt      = gameNinja t game
    tHealth = nHealth nt
    hp      = healthBound (minHealth nt) $
              tHealth + getBoost src nt * percent * (100 - tHealth) 
              `quot` 100
            + getBless (gameNinja src game)

-- | Removes 'nHealth' from the user.
sacrifice :: Int -> Int -> Transform
sacrifice minhp hp _ c _ game t 
  | c == t && is ImmuneSelf (gameNinja t game) = game
  | otherwise = r' (N.sacrifice minhp hp) game t

-- ** COPYING

copyAll :: Int -> Transform
copyAll dur _ src _ game t = fn src copier game
  where 
    nt    = gameNinja t game
    dur'  = sync  . (dur < 0) ? (+1) $ dur
    cpDur = sync dur - 1
    copier n = n { nCopied = Seq.fromList $ cp <$> getSkills nt }
    cp skill = Just $
               Copied skill { copying = Deep (copyRoot skill t) cpDur } dur'

-- | Copies the target's 'nLastSkill'.
copyLast :: Int -> Int -> Transform
copyLast dur s Skill{..} _ c game t = case nLastSkill $ gameNinja t game of
    Nothing     -> game
    Just tSkill -> doCopy False Shallow t tSkill game (c, label, s, sync dur)

doCopy :: Bool -> (Slot -> Int -> Copying) -> Slot -> Skill -> Game 
       -> (Slot, Text, Int, Int) -> Game
doCopy clear' cop src skill game (c, l, s, dur) = 
    clear' ? alter (N.clear l c <$>) $ fn c copier game
  where 
    copied   = Just . Copied skill' . sync . (dur < 0) ? (+1) $ dur
    skill'   = skill { cost    = 0
                      , cd      = 0
                      , copying = cop src (sync dur - 1)
                      }                     
    copier n = n { nCopied = Seq.update s copied $ nCopied n }

-- | Copies the user's 'Skill's onto the target.
teach :: Int -> (Slot -> Int -> Copying) -> Int -> Transform
teach dur cop s _ src _ game t = fn t copier game
  where 
    skill    = getSkill (gameNinja src game) (Left s)
    skill'   = skill { copying = cop src (sync dur - 1) }
    copied   = Just . Copied skill' . sync . (dur < 0 ) ? (+1) $ dur
    copier n = n { nCopied = Seq.replicate 4 copied }

-- | Copies a user's 'Skill' onto the target.
teachOne :: Int -> Int -> (Slot -> Int -> Copying) -> Int -> Transform
teachOne dur s' cop s _ src _ game = r' copier game
  where 
    skill    = getSkill (gameNinja src game) (Left s)
    skill'   = skill { copying = cop src (sync dur - 1) }
    copied   = Just . Copied skill' . sync . (dur < 0 ) ? (+1) $ dur
    copier n = n { nCopied = Seq.update s' copied $ nCopied n }
  
-- ** DESTRUCTIBLE

-- | Adds to preexisting 'Defense'.
addDefense :: Text -> Int -> Transform
addDefense l amt _ src _ game t = case find (lMatch l src) nDefense of
    Nothing -> game
    Just defense@Defense{..} -> flip (setNinja t) game $
        nt { nDefense = defense { defenseAmount = defenseAmount + amt }
                      : deleteBy lEq defense nDefense 
           }
  where 
    nt@Ninja{..} = gameNinja t game

-- | Creates a 'Barrier'.
bar :: Int -> (Int -> Transform) -> Transform -> Int -> Transform
bar dur done during barrierAmount' skill@Skill{..} barrierSrc c game t
  | barrierAmount == 0 = game
  | single             = game
  | barrierAmount < 0  = damage (-barrierAmount) skill barrierSrc c game c
  | otherwise          = setNinja t nt { nBarrier = barr : barrier } game
  where 
    barrierL       = label
    nt             = gameNinja t game
    barrierAmount  = barrierAmount' + getBuild (gameNinja barrierSrc game)
    barrierDur     = copyDur copying $ sync dur
    barr           = Barrier{..}
    single         = Single `elem` classes && any (lEq barr) (nBarrier nt)
    barrier        = (Nonstacking `elem` classes) ? filter (not . lEq barr) $
                     nBarrier nt
    barrierWhile g = wrapEffect [Channeled, Trapped] 
                      during skill barrierSrc c g t
    barrierDone amount g
      | barrierDur < sync dur = g
      | otherwise = wrapEffect [Trapped] 
                    (done amount) skill barrierSrc c g t

defend' :: Text -> Int -> Int -> Transform
defend' l dur defenseAmount' skill@Skill{..} defenseSrc c game t
  | defenseAmount == 0 = game
  | single             = game
  | defenseAmount < 0  = damage (-defenseAmount) skill defenseSrc c game t
  | otherwise          = setNinja t nt { nDefense = defense : defenses } game
  where 
    defenseL      = defaultL l skill
    nt            = gameNinja t game
    defenseAmount = getBoost defenseSrc nt * defenseAmount' 
                  + getBuild (gameNinja defenseSrc game)
    defenseDur    = copyDur copying . incr $ sync dur
    defense       = Defense{..}
    single        = Single `elem` classes && any (lEq defense) (nDefense nt)
    defenses      = (Nonstacking `elem` classes) ? 
                    filter (not . lEq defense) $
                    nDefense nt

-- | Creates a 'Defense'.
defend :: Int -> Int -> Transform
defend = defend' ""

-- | Removes the user's 'Barrier' and the target's 'Defense'.
demolish :: Transform
demolish _ _ c game@Game{..} t = fn c unBarrier $ fn t unDefense game
    where 
      unDefense n = n { nDefense = [] }
      unBarrier n = n { nBarrier = [] }

-- | Damage that only affects destructibles, not health.
demolish' :: Int -> Transform
demolish' = attack Demolish

-- ** STATUSES

-- | 'N.hasten'.
hasten :: Int -> Text -> Transform
hasten dur l Skill{..} src = rt $ N.hasten (sync dur) l src

-- | 'N.prolong'.
prolong :: Int -> Text -> Transform
prolong dur l Skill{..} src = rt $ N.prolong (copyDur copying $ sync dur) l src

-- | Adds a 'Face' with a duration.
setFace :: Int -> Transform
setFace dur skill@Skill{..} src c game t = case copying of
    NotCopied -> r setFace' skill src c game t
    _         -> game
  where 
    setFace' n = n { nFace = Face label src (sync dur) : nFace n }

applyFull :: [Class] -> Bool -> [(Bomb, Transform)] -> Text -> Int -> [Effect] 
          -> Transform
applyFull clas bounced statusBombs' l unthrottled fs statusSkill@Skill{copying} 
          statusSrc statusC game t
  | not (null fs) && unthrottled /= 0 && dur == 0 = game
  | null fs && Shifted `elem` clas             = game
  | already && (bounced || isSingle)           = game
  | already && Extending `elem` statusClasses  = setNinja t nt'Extend game
  | not (null fs) && null statusEfs            = game
  | not bounced                                = game'Bounce
  | otherwise                                  = game'Stat
  where 
    dur      
      | Direct `elem` clas = unthrottled
      | otherwise          = unthrottled - getThrottle fs n * signum unthrottled
    statusCount   = 1
    statusRoot    = copyRoot statusSkill statusSrc
    statusDur     = copyDur copying . incr $ sync dur
    statusMaxDur  = statusDur
    already       = has statusL statusSrc nt
    statusBombs   
      | statusDur <= incr (sync dur) = statusBombs'
      | otherwise                    = []
    statusL       = defaultL l statusSkill
    isSingle      = statusL == label statusSkill && Single `elem` clas
    n             = gameNinja statusSrc game 
    nt            = gameNinja t game
    selfApplied   = statusSrc == statusC && statusC == t
    extending     = N.prolong' statusDur l statusRoot
    nt'Extend     = nt { nStatuses = mapMaybe extending $ nStatuses nt}
    statusEfs     = bounced ? filter (not . isDmg) . 
                    is Silence n ? filter isDmg $ 
                    filterEffects nt fs
    statusClasses = nub .
                    any bind fs ? (Soulbound :) .
                    (selfApplied && any (not . helpful) fs) ? (Unremovable :) $
                    clas ++ classes statusSkill
    disrupted     
      | is Enrage nt || is Focus nt = []
      | otherwise                   = filter disr $ nChannels nt
    disruptCtrl = [ ch | ch@Channel {channelDur = (Control _)} <- disrupted ]
    nt'           = nt { nChannels = nChannels nt \\ disruptCtrl }
    game'Disr     = doDisrupts nt' disrupted $ setNinja t nt' game
    game'Interr   = foldl' (disruptAll t statusEfs) game'Disr allSlots
    game'Stat     = fn t (N.addStatus Status{..}) game'Interr
    game'Bounce   
      | selfApplied = game'Stat
      | otherwise   = foldl' bounce game'Stat . delete statusSrc $ getShare nt
    bounce        = applyFull [] True statusBombs l dur fs statusSkill 
                    statusSrc statusC
    disr Channel{..} = any (`elem` statusEfs) $ Stun <$> classes channelSkill
    bind (Redirect _) = True
    bind _            = False
    isDmg (Afflict a) = a > 0
    isDmg _           = False

applyWith' :: [Class] -> Text -> Int -> [Effect] -> Transform
applyWith' classes = applyFull classes False []
apply' :: Text -> Int -> [Effect] -> Transform
apply' = applyWith' []
applyWith :: [Class] -> Int -> [Effect] -> Transform
applyWith classes = applyWith' classes ""
-- | Adds a 'Status' to the target.
apply :: Int -> [Effect] -> Transform
apply = apply' ""
  
-- 'apply' with a scaled duration.
applyDur :: [Effect] -> Int -> Transform
applyDur = flip apply

applyDur' :: Text -> [Effect] -> Int -> Transform
applyDur' l = flip (apply' l)

-- 'apply' with a scaled amount.
applyX :: Int -> (Int -> Effect) -> Int -> Transform
applyX dur constructor i = apply dur [constructor i]

addStacks' :: Int -> Text -> Int -> Transform
addStacks' _    _ 0  _    _   _ = const
addStacks' dur li i skill src c = r' $ N.addStacks dur li i skill src c
-- 'apply' replicated.
addStacks :: Text -> Int -> Transform
addStacks = addStacks' 0
addStack :: Transform
addStack skill@Skill{..} = addStacks label 1 skill    

bombWith' :: [Class] -> Text -> Int -> [Effect] -> [(Bomb, Transform)] 
          -> Transform
bombWith' classes l dur fs bombs = applyFull classes False bombs l dur fs
bombWith :: [Class] -> Int -> [Effect] -> [(Bomb, Transform)] -> Transform
bombWith classes = bombWith' classes ""
bomb' :: Text -> Int -> [Effect] -> [(Bomb, Transform)] -> Transform
bomb' = bombWith' []
-- Applies a 'Status' that causes a 'Transform' when it ends, 
-- either by expiring or by being cured.
bomb :: Int -> [Effect] -> [(Bomb, Transform)] -> Transform
bomb = bomb' ""

flag' :: Text -> Transform
flag' l = applyWith' [Hidden, Unremovable, Nonstacking] l (-1) []
-- Applies a hidden 'Status' with no effects that immediately expires.
flag :: Transform
flag = flag' ""

tag' :: Text -> Int -> Transform
tag' l dur = applyWith' [Unremovable, Nonstacking] l dur []
-- Applies a 'Status' with no effects, used as a marker for other 'Skill's.
tag :: Int -> Transform
tag = tag' ""

hide' :: Text -> Int -> [Effect] -> Transform
hide' = applyWith' [Unremovable, Hidden]
-- Applies a 'Hidden' 'Status'.
hide :: Int -> [Effect] -> Transform
hide = hide' ""

-- Saves the user's current state as a 'Snapshot', which can later be restored.
snapshot :: Int -> Transform
snapshot dur' skill@Skill{..} src c = r' $ \n@Ninja{..} -> 
    N.addStatus (status n) n
  where 
    dur      = copyDur copying . incr $ sync dur'
    status n = Status 1 label src src c skill [Snapshot n] 
                (Unremovable : Nonstacking : classes) [] dur dur

-- ** TRAPS

-- | Applies an 'OnBreak' trap for the 'Skill' used.
onBreak :: Transform -> Transform
onBreak f skill@Skill{..} src c game t
  | not $ hasDefense label src $ gameNinja t game = game
  | otherwise = trap' 0 (OnBreak label) f skill src c game t

-- | Default 'onBreak': remove 'Status'es and 'Channel's that match 'label'.
onBreak' :: Transform
onBreak' skill@Skill{..} = onBreak (remove label • cancelChannel label) skill

-- | 'onBreak' with 'self'
selfBreak :: Transform
selfBreak skill@Skill{..} =
    onBreak (self $ remove label • cancelChannel label) skill

removeTrap :: Text -> Transform
removeTrap l skill src = rt . N.clearTrap l $ copyRoot skill src

trapFull :: TrapType -> [Class] -> Int -> Trigger -> (Int -> Transform) 
         -> Transform
trapFull trapType clas dur trapTrigger f skill@Skill{..} trapSrc' _ game t
--  | trapSrc' /= c    = game TODO: Should reflecting not cause traps?
  | p `elem` nTraps nt = game
  | otherwise     = setNinja t nt' game
  where 
    trapDur      = copyDur copying . incr $ sync dur
    trapL        = label
    nt           = gameNinja t game
    trapSrc      = copyRoot skill trapSrc'
    trapClasses  = nub $ clas ++ (invis <$> classes)
    trapTrack    = 0
    trapDesc     = desc
    p          = Trap{..}
    nt'          = nt { nTraps = nTraps nt |> p }
    trapEf a c g = wrapEffect [Trapped] (f a) skill trapSrc trapSrc g $ 
                   case trapType of
                       TrapFrom -> c
                       _        -> t
    invis InvisibleTraps = Invisible
    invis a              = a

-- | Adds a 'TrapTo' 'Trap'.
trap :: Int -> Trigger -> Transform -> Transform
trap = trapWith TrapTo []
trap' :: Int -> Trigger -> Transform -> Transform
trap' = trapWith TrapTo [Hidden]

-- | Adds a 'TrapFrom' 'Trap'.
trapFrom :: Int -> Trigger -> Transform -> Transform
trapFrom = trapWith TrapFrom []
trapFrom' :: Int -> Trigger -> Transform -> Transform
trapFrom' = trapWith TrapFrom [Hidden]

-- | Adds a 'TrapPer' 'Trap'.
trapPer  :: Int -> Trigger -> (Int -> Transform) -> Transform
trapPer  = trapFull TrapPer []
trapPer' :: Int -> Trigger -> (Int -> Transform) -> Transform
trapPer' = trapFull TrapPer [Hidden] 

trapWith :: TrapType -> [Class] -> Int -> Trigger -> Transform -> Transform
trapWith trapType clas dur tr f = trapFull trapType clas dur tr (const f)

-- * INVULNERABILITY SKILL

invuln1' :: Text -> Text -> [Class] -> [Transform] -> Skill
invuln1' label desc classes effects = 
    newSkill { label   = label
             , desc    = desc
             , classes = classes
             , cd      = 4
             , cost    = χ [Rand]
             , effects = (Self, apply 1 [Immune All]) : ((Self, ) <$> effects)
             }
invuln' :: Text -> Text -> [Class] -> [Transform] -> NonEmpty Skill
invuln' label desc classes effects = invuln1' label desc classes effects :| []

invuln1 :: Text -> Text -> [Class] -> Skill
invuln1 label name classes = invuln1' label desc classes []
  where 
    desc = name ++ " becomes invulnerable for 1 turn."

invuln :: Text -> Text -> [Class] -> NonEmpty Skill
invuln label name classes = invuln1 label name classes :| []

-- * SPECIALIZED

-- | Steals all of the target's beneficial effects.
commandeer :: Transform
commandeer _ _ c game t = setNinja t nt' $ setNinja c n' game
  where 
    n   = gameNinja c game
    nt  = gameNinja t game
    n'  = n  { nDefense  = nDefense nt ++ nDefense nt 
              , nBarrier  = []
              , nStatuses = mapMaybe gainHelpful (nStatuses nt) 
                            ++ nStatuses n
              }
    nt' = nt { nDefense  = [] 
              , nBarrier  = nBarrier n 
              , nStatuses = mapMaybe loseHelpful $ nStatuses nt
              }
    loseHelpful st@Status{..}
      | Unremovable `elem` statusClasses = Just st
      | null statusEfs              = Just st
      | not $ any lose statusEfs    = Just st
      | all lose statusEfs          = Nothing
      | otherwise = Just st { statusEfs = filter (not . lose) statusEfs }
    gainHelpful st@Status{..}
      | Unremovable `elem` statusClasses = Nothing
      | null statusEfs              = Nothing
      | not $ any lose statusEfs    = Nothing
      | all lose statusEfs          = Just st
      | otherwise = Just st { statusEfs = filter lose statusEfs }
    lose ef = helpful ef && not (sticky ef)

kabuto' :: Skill -> Ninja -> Ninja
kabuto' skill@Skill{..} n@Ninja{..} = 
    n { nStatuses = newmode : filter (not . getMode) nStatuses 
      , nVariants = Seq.fromList $ (:|[]) <$> [var', var, var, var]
      , nChannels = toList (init nChannels') |> swaps (last nChannels')
      }
  where 
    nChannels' = case nChannels of
                    x:xs -> x :| xs
                    []   -> Channel nId skill nId channel :| []
    sage       = " Sage"
    sLen       = length sage
    (mode, m)  = advance . maybe "" (dropEnd sLen . statusL) $
                 find getMode nStatuses
    var        = Variant m False "" False 0
    var'       = Variant (m + 1) False "" False 0
    ml         = mode ++ sage
    newmode    = Status 1 ml nId nId nId skill [] [Hidden, Unremovable] [] 0 0
    getMode st = statusSrc st == nId && sage == Text.takeEnd sLen (statusL st) 
    advance "Bloodline" = ("Genjutsu" , 2)
    advance "Genjutsu"  = ("Ninjutsu" , 3)
    advance "Ninjutsu"  = ("Taijutsu" , 4)
    advance _           = ("Bloodline", 1)
    swaps ch@Channel{..} = ch { channelSkill = channelSkill { label = ml } }

kabuto :: Transform
kabuto skill = r (kabuto' skill) skill
