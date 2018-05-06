-- | A collection of 'Transform's and other 'Game' → 'Game' transformations.
module Game.Game
 ( setFace
 , wait, wait'
 , apply, apply', applyWith, applyWith', applyX, applyDur, applyDur'
 , prolong, prolongChannel, hasten, refresh
 , tag, tag', hide, hide', flag, flag'
 , bomb, bomb', bombWith, bombWith', snapshot
 , remove, cureStun, cureBane, cure, cureAll, purgeAll, purge
 , addStack, addStacks, addStacks', removeStack, removeStacks
 , setHealth, heal, restore
 , copyLast, copyAll, teach, teachOne
 , sacrifice, afflict, pierce, damage, execute, leech
 , kill, kill', killThen
 , defend, defend', addDefense, demolish, demolish', commandeer, bar
 , factory, resetAll, reset, alterCd, resetCharges
 , interrupt
 , delay
 , trapWith, trap, trap', trapFrom, trapFrom', trapPer, trapPer', onBreak
 , removeTrap
 , gain, drain, steal
 , cancelChannel, vary, vary'
 , self, alliedTeam, enemyTeam, everyone
 , (°), (•)
 , ifI, ifU, ifnotI, ifnotU
 , ifChan, ifHealthI, ifHealthU, ifStacks, ifnotStacks, ifDef, withInvulnU
 , withI, withU, withChan
 , perI, perI', perU, perHealthI, perHealthU, perDef, perHelpful, perAffected
 , updateChakra, runTurn
 , forfeit
 , censor
 , invuln, invuln'
 , act
 , kabuto -- I do what I want!
 ) where

import qualified Data.Sequence as S
import qualified Data.Text     as T
import qualified Game.Ninja    as N

import Data.Either
import Data.Foldable
import Data.Function         ((&))
import Data.List
import Data.Maybe
import Data.Sequence         (Seq, adjust', update)
import Data.Text             (Text)
import System.Random
import System.Random.Shuffle 

import Calculus
import Core.Unicode
import Game.Structure
import Game.Functions

-- * INTERFACE

forfeitN ∷ Player → Ninja → Ninja
forfeitN player n@Ninja{..}
  | alliedP player nId = n { nHealth = 0 }
  | otherwise          = n

forfeit ∷ Player → Game → Game -- ^ Sets all on team to 0 health.
forfeit player = alter (forfeitN player <$>)

censorNinja ∷ Game → Player → Ninja → Ninja
censorNinja game player n@Ninja{..}
  | alliedP player nId = n'
  | is Reveal n        = n'
  | otherwise          = n' 
      { nCooldowns = ø
      , nCharges   = ø
      , nVariants  = S.replicate 4 [noVariant]
      , nChannels  = filter ((not ∘ ([Invisible, InvisibleTraps] ⩀)) ∘ classes ∘ channelSkill)
                     nChannels
      , nLastSkill = Nothing
      }
  where n' = n { nStatuses = mapMaybe mst nStatuses
               , nTraps    = S.filter iftr nTraps
               }
        iftr Trap{..}   = alliedP player trapSrc ∨ Invisible ∉ trapClasses
                        ∨ is Reveal (gameNinja trapSrc game)
        mst st@Status{..} | alliedP player statusC                   = Just st
                          | Invisible ∈ statusClasses 
                          ∧ not (is Reveal $ gameNinja statusC game) = Nothing
                          | statusEfs ≡ []                           = Just st
                          | statusEfs ≡ [Reveal]                     = Nothing
                          | otherwise                                = Just st 
                              { statusEfs = delete Reveal statusEfs }

censorPlayer ∷ Player → Game → Game
censorPlayer player game = alter (censorNinja game player <$>) game

-- | Hide private information and 'Invisible's from opponent.
censor ∷ Player → Game → Game
censor player game@Game{..} = censorPlayer player game 
    { gameChakra = in2 (player ≡ PlayerB) χØ gameChakra }

-- * TURN HANDLING

runTurn ∷ Player → [Act] → StdGen → Game → Game
runTurn player actions stdGen game = yieldVictor game'Full
  where rs'        = unfoldl (Just ∘ split) stdGen
        rs         = drop 2 rs'
        game'Acts  = foldl' (act []) game $ zip actions (map Right rs)
        chans      = getAllChannels player game'Acts
        game'Chan  = doDeaths
                   ∘ doDelays
                   ∘ doTraps
                   ∘ turnTrigger player game
                   ∘ doBarriers player
                   ∘ doBombs Remove game
                   ∘ decrStats
                   ∘ foldl' (act []) game'Acts $ zip chans (map Right rs)
        decrDelays = filter ((0 ≠) ∘ getDur) ∘ mapMaybe decrTurn ∘ gameDelays
        game'End   = doBombs Done game
                   ∘ doBombs Expire game'Chan
                   $ decr game'Chan { gameDelays = decrDelays game'Chan }
        vs         = opponent player
        game'Next  = updateChakra vs 
                     (take teamSize ∘ randomRs (0, 3) $ head rs)
                   ∘ doDrain (rs !! 1)
                   ∘ doAfflicts player
                   $ game'End { gamePlaying = vs }
        game'Clear = game'Next { gameDrain = (0, 0)
                               , gameSteal = (0, 0)
                               , gameTraps = ø
                               }
        game'Futur = censor vs game'Clear
        chansNext  = getAllChannels vs game'Futur
        game'Ghost = foldl' (act []) game'Futur
                   $ zip chansNext $ map Right (drop (teamSize * 2) rs)
        game'Full  = addGhosts game'Clear game'Ghost
        getChannels n = map (fromChannel n) ∘ filter ((1 ≠) ∘ getDur) 
                      $ nChannels n
        getAllChannels p Game{..} = concatMap getChannels (alives p gameNinjas)
        decr g@Game{..} = g { gameNinjas = N.decr <$> gameNinjas }
        decrStats g@Game{..} = g { gameNinjas = N.decrStats <$> gameNinjas }
        
        
doDelays ∷ Game → Game
doDelays game@Game{..} = foldl' (flip delayEf) game 
                       $ filter ((≤ 1) ∘ delayDur) gameDelays

doBombs ∷ Bomb → Game → Game → Game
doBombs bombType game game' = foldl' (&) game' efs
  where efs = concat $ S.zipWith boom (gameNinjas game) (gameNinjas game')
        boom n n' = map (doBomb bombType $ nId n) $ nStatuses n ∖ nStatuses n'

doBomb ∷ Bomb → Slot → Status → Game → Game
doBomb bombType t Status{..} game = foldl' detonate game statusBombs
  where detonate game' (b, f)
          | bombType ≡ b = wrapEffect [Trapped] 
                           f statusSkill statusSrc statusSrc game' t
          | otherwise    = game'

doBarriers ∷ Player → Game → Game
doBarriers player game@Game{..} = foldl' doBarrier game barriers
  where barriers = concatMap (map head ∘ groupBy lEq ∘ nBarrier) gameNinjas
        doBarrier game' Barrier{..}
          | barrierDur ≡ 1            = barrierDone barrierAmount game'
          | alliedP player barrierSrc = barrierWhile game'
          | otherwise                 = game'

doTraps ∷ Game → Game
doTraps game@Game{..} = foldl' (&) game {gameTraps = ø} gameTraps


doDeaths ∷ Game → Game
doDeaths game = foldl' checkDeath game allSlots

doAfflicts ∷ Player → Game → Game
doAfflicts player game = foldl' (doAfflict player) game allSlots

doAfflict ∷ Player → Game → Slot → Game
doAfflict player game t
  | aff ≡ 0          = game
  | not $ isAlive nt = game
  | otherwise        = r' (N.attack aff) game t
  where nt  = gameNinja t game
        aff = getNet player game nt

doDrain ∷ StdGen → Game → Game
doDrain rand game@Game{..} = game { gameChakra = gameChakra' }
  where gameChakra'            = (chakraA2 { rand = 0}, chakraB2 { rand = 0 })
        (chakraA0, chakraB0)   = gameChakra
        (randA, randB)         = split rand
        (gainB, loseA, gainA') = processDrain True  game randA
        (gainA, loseB, gainB') = processDrain False game randB
        chakraA1 = foldl' (addChakra 1)    chakraA0 (gainA ⧺ gainA')
        chakraB1 = foldl' (addChakra 1)    chakraB0 (gainB ⧺ gainB')
        chakraA2 = foldl' (addChakra (-1)) chakraA1 loseA
        chakraB2 = foldl' (addChakra (-1)) chakraB1 loseB

processDrain ∷ Bool → Game → StdGen → ([Int], [Int], [Int])
processDrain isFst Game{..} rand
  | draining ≡ 0 = (stolen, stolen, [])
  | draining > 0 = (stolen, stolen ⧺ drained, [])
  | otherwise    = (stolen, stolen, drained)
  where chakras             = listChakra ∘ out2 isFst $ gameChakra
        draining            = out2 isFst gameDrain
        (stolen, shuffled') = splitAt (out2 isFst gameSteal) shuffled
        drained             = take    (abs draining)         shuffled'
        shuffled | null chakras = chakras
                 | otherwise    = shuffle' chakras (length chakras) rand

updateChakra ∷ Player → [Int] → Game → Game
updateChakra player chakras game@Game{..} = game { gameChakra = upd gameChakra }
  where upd = byPar (fromEnum player) $ gainChakra player chakras gameNinjas

addGhosts ∷ Game → Game → Game
addGhosts game game' = game { gameNinjas = gameNinjas' }
  where gameNinjas' = S.zipWith addGhost (gameNinjas game) (gameNinjas game')
        addGhost n n' = n { nTags = map unghost (nTags n') ⧺ mapMaybe ghost (nTags n') }
        ghost ct   = decrTurn ct { tagGhost = True } -- TODO
        unghost ct = ct { tagGhost = False }

-- * ACTING

act ∷ [Affected] → Game → (Act, Either (Slot, Slot) StdGen) → Game
act affected' game'Swap (Act c s t, rando)
  | not $ isAlive n                = game
  | notChan s                      = game
  | Channeled ∈ affected           = game'
  | require skill ≡ Unusable       = game
  | lack $ oldChakra -~ cost skill = game
  | otherwise                      = case snareTrap skill n of
      Just (n', sn) → setNinja c (safeSnare sn s n') game
      _             → game' { gameChakra = f $ gameChakra game' }
  where f           = bySlot c (-~ cost skill)
        n           = gameNinja c game'Swap
        skill'      = getSkill n s
        (skill, affected'Swap, game) = case getSwap (classes skill') n of
            Just swap → ( swapSkill swap skill'
                        , Swapped : affected'
                        , swapGame swap
                        )
            Nothing   → (skill', affected', game'Swap)
        oldChakra   = outSlot c $ gameChakra game
        hascharge   = charges skill > 0
        game'Cd     = updateCd hascharge 0 skill c s game
        affected    = isRight s ? (Channeled :) $ affected'Swap
        game'F      = doEffects rando affected skill c c t game'Cd
        game'       = trigger affected skill c game'Cd
                    ∘ addChannels (isLeft s) (channel skill) c skill t
                    $ game'F
        notChan (Left _)   = False
        notChan (Right s') = Channeled ∈ affected 
                           ∧ not (isChanneling (label s') n)
        safeSnare sn (Left s') | s' ≤ 3    = N.updateCd hascharge sn skill s'
                               | otherwise = id
        safeSnare _  _         = id
        swapGame swap = setNinja c n { nStatuses = delete swap $ nStatuses n } 
                        game'Swap

addChannels ∷ Bool → Channeling → Slot → Skill → Slot → Game → Game
addChannels _    Instant _        _                      _ = id
addChannels True chan    channelC channelSkill@Skill{..} channelT 
  | dur ≡ 1 ∨ dur ≡ 2 = id
  | otherwise         = fn channelC addChans
  where addChans n@Ninja{..} = n { newChans = Channel{..} : newChans }
        channelRoot = cp channelSkill channelC
        dur         = copyDur copying ∘ sync $ getDur chan
        channelDur  = setDur dur chan
addChannels _    _       _        _                      _ = id

updateCd ∷ Bool → Int → Skill → Slot → Either Int Skill → Game → Game
updateCd charge a skill c (Left s) = fn c $ N.updateCd charge a skill s
updateCd _      _ _     _ _        = id

{-entrapPer ∷ Slot → Seq (TrapTransform, Int) → Game → Game
entrapPer src trapped game@Game{..} = game 
    { gameTraps = gameTraps ◇ ((\(f, a) → f a src) <$> trapped) }-}
entrap ∷ Slot → Seq TrapTransform → Game → Game
entrap src trapped game@Game{..} = game
    { gameTraps = gameTraps ◇ (flip ($ 0) src <$> trapped) }

entrap' ∷ Slot → Seq TrapTransform → Game → Game
entrap' src trapped game = foldl' (&) game $ (flip ($ 0) src <$> trapped)

trigger ∷ [Affected] → Skill → Slot → Game → Game → Game
trigger affected Skill{..} c game game'Pre
  | Channeled ∈ affected = game'
  | not (null counttr)   = fn c N.unCounter game { gameNinjas = ns'Cp }
  | otherwise            = game'Tr
  where n       = gameNinja c game
        n'Pre   = gameNinja c game'Pre
        dmgTot  = sum $ S.zipWith healthLost 
                  (gameNinjas game) (gameNinjas game'Pre)
        n'      = n'Pre { nTraps = updatePer <$> nTraps n'Pre }
        game'   = setNinja c n' game'Pre
        ns'Cp   = S.zipWith cop (gameNinjas game) (gameNinjas game')
        dmgTr   = getTrackTraps True TrackDamage n'
        als     = S.zip (S.fromList $ allies c game) 
                        (S.fromList $ allies c game')
        ens     = S.zip (S.fromList $ enemies c game) 
                        (S.fromList $ enemies c game')
        allNs   = S.zip (gameNinjas game) (gameNinjas game')
        chk     = outSlot' c
        chakraF = chk (gameDrain game') ≠ 0 ∨ chk (gameSteal game') ≠ 0
        harmful = not (null harmed) ∨ nBarrier n ≠ nBarrier n'
        counttr = getTraps True (OnCounter Uncounterable) n'
                ◇ classTrs (harmful ∧ Uncounterable ∉ classes) 
                                                OnCounter classes n'
        healed  = snd <$> S.filter (uncurry (<) ∘ both nHealth) als
        helped  = snd <$> S.filter (uncurry (≠))                als
        harmed  = snd <$> S.filter (uncurry (≠))                ens
        dmgEns  = snd <$> S.filter (uncurry (>) ∘ both nHealth) ens
        damaged = snd <$> S.filter (uncurry (>) ∘ both nHealth) allNs
        stunned = snd <$> S.filter (was Stun) ens
        trapsC  = counttr
                ◇ getTraps chakraF              OnChakra n'
                ◇ getTraps (not $ null dmgEns)  OnDamage n'
                ◇ getTraps (not $ null stunned) OnStun   n'
                ◇ getTraps (was Immune (n', n)) OnImmune n'
                ◇ getTraps harmful              OnHarm   n'
                ◇ classTrs (game'Pre ≠ game)    OnAction classes n' 
        trapsN  = seqConcat
                $ (getTraps True OnHelped          <$> helped)
                ◇ (getTraps True OnHealed          <$> healed)
                ◇ (getTraps True (OnStunned Multi) <$> stunned)
                ◇ (classTrs True OnHarmed classes  <$> harmed)
                ◇ (classTrs True OnDamaged classes <$> damaged)
                ◇ (onStunned                       <$> stunned)
        game'Tr = entrap c trapsN
                $ entrap c trapsC game' 
                  { gameTraps = gameTraps game' ◇ dmgTr }
        onStunned nt = classTrs True OnStunned (getStun nt) nt
        cop t t' = t { nCopied = nCopied t' }
        was ef (nt, nt') = is' ef nt' ∧ not (is' ef nt)
        updatePer trp@Trap{..} 
          | trapTrigger ≡ TrackDamage = trp { trapTrack = dmgTot + trapTrack }
          | otherwise                 = trp

healthLost ∷ Ninja → Ninja → Int
healthLost n n' = max 0 $ nHealth n - nHealth n'

getTurnTraps ∷ Player → Ninja → Ninja → Seq (Game → Game)
getTurnTraps player n n'
  | hp < 0 ∧ alliedP player (nId n') = getPerTraps True PerHealed (-hp) n'
  | hp > 0 ∧ not (alliedP player $ nId n') = getPerTraps True PerDamaged hp n'
  | otherwise = ø
  where hp = nHealth n - nHealth n'

turnTrigger ∷ Player → Game → Game → Game
turnTrigger player game game' = game'Track
  where gameNinjas' = S.zipWith trackTurn (gameNinjas game) (gameNinjas game')
        trapsAll    = seqConcat $ S.zipWith (getTurnTraps player) 
                      (gameNinjas game) gameNinjas'
        game'Track  = game' { gameNinjas = gameNinjas'
                            , gameTraps  = trapsAll ◇ gameTraps game'
                            }
trackTurn ∷ Ninja → Ninja → Ninja
trackTurn n n' = n' { nTraps = updatePer <$> nTraps n' }
  where dmgdTot = healthLost n n'
        updatePer trp@Trap{..}
          | trapTrigger ≡ TrackDamaged = trp { trapTrack = dmgdTot + trapTrack }
          | otherwise                  = trp
          
checkDeath ∷ Game → Slot → Game
checkDeath game t
  | nHealth nt > 0 = game
  | not $ null res = entrap' t res $ fn t resN game
  | otherwise      = alter (fmap unr)
                   ∘ entrap' t (getTraps True OnDeath nt) $ fn t dieN game
  where nt    = gameNinja t game
        resN nt' = nt' 
            { nHealth = 1
            , nTraps = S.filter ((OnRes ≠) ∘ trapTrigger) $ nTraps nt'
            }
        dieN nt' = nt' 
            { nTraps = S.filter ((OnDeath ≠) ∘ trapTrigger) $ nTraps nt' }
        res   = getTraps (not $ is Plague nt) OnRes nt
        unr n = n { nStatuses =   filter keepSt $ nStatuses n
                  , nTraps    = S.filter keepTr $ nTraps n
                  , nChannels =   filter keepCh $ nChannels n
                  }
        keepSt Status{..}  = (t ≠ statusSrc ∧ t ≠ statusC)
                           ∨ Soulbound ∉ statusClasses
        keepTr Trap{..}    = t ≠ trapSrc ∨ Soulbound ∉ trapClasses
        keepCh Channel{..} = t ≠ channelT

broken ∷ Ninja → Ninja → (Ninja, Seq TrapTransform)
broken n n' = (n'', trapEf <$> S.filter ((∈ trigs) ∘ trapTrigger) (nTraps n'))
  where broke = nub (map defenseL $ nDefense n) 
              ∖ nub (map defenseL $ nDefense n')
        trigs = map OnBreak broke
        n''   = n' { nTraps = S.filter match $ nTraps n' }
        match trp = trapTrigger trp ∉ trigs

-- * PROCESSING

doEffects ∷ Either (Slot, Slot) StdGen → [Affected] → Skill → Slot → Slot → Slot → Game 
          → Game
doEffects rando affected skill@Skill{..} src c t game 
    = foldl' (&) game 
    ∘ zipWith (doEffect affected skill src c t) (splitRands rando)
    $ (Channeled ∉ affected) ? (start ⧺) $ effects
  where splitRands a@(Left _)     = repeat a
        splitRands (Right stdGen) = map Right $ unfoldl (Just ∘ split) stdGen

doEffect ∷ [Affected] → Skill → Slot → Slot → Slot
         → Either (Slot, Slot) StdGen → (Target, Transform) → Game → Game
doEffect affected skill' src c t rando (target, f) game
  | Countered ∉ affected                                             = done
  | Parrying ∈ affected ∧ target ≡ Ally                              = game
  | Parrying ∉ affected ∧ target ∈ [Self, Allies, Enemies, Everyone] = game
  | otherwise                                                        = done       
  where skill | target ≡ Self = skill' { classes = Unshifted : classes skill' }
              | otherwise     = skill'
        done = foldl' (wrapEffect affected f skill src c) game
             $ choose (rAlly, rEnemy) target c t   
        (rAlly, rEnemy) = chooseRands (gameNinja src game) skill game c rando

chooseRands ∷ Ninja → Skill → Game → Slot → Either (Slot, Slot) StdGen 
            → (Maybe Slot, Maybe Slot)
chooseRands _ _              _        _ (Left a)        = both Just a 
chooseRands n skill@Skill{..} Game{..} c (Right stdGen) = (rAlly, rEnemy) 
  where targets = map nId ∘ filter (targetable skill n n) $ toList gameNinjas
        (livingAllies, livingEnemies) = partition (allied c) targets
        (rAlly, stdGen')              = safePick stdGen livingAllies
        (rEnemy, _)                   = safePick stdGen' livingEnemies

wrapEffect ∷ [Affected] → Transform → Transform
wrapEffect affected f skill@Skill{..} src c game t
  | [Applied, Trapped] ⩀ affected     = game'Do
  | not $ targetable skill' nSrc n nt = game
  | skill ∈ nParrying nt              = game
  | not new                           = game'Do
  | is Uncounter nt                   = game'Post
  | otherwise = case allow Redirected $? redirect classes nt of
      Just redir → wrapEffect (Redirected : affected) 
                   f skill src (tOrC redir) game'Mimic redir
      Nothing    → case allow Redirected $? reapply classes nt of
        Just redir → wrapEffect (Redirected : affected) 
                     f skill src (tOrC redir) game'Shift redir 
        Nothing    → case allow Countered $? parry skill nt of
          Just (nt', pSrc, a) → actParry' (Act pSrc (Left a) pSrc, rando)
                              ∘ actParry  (Act pSrc (Left a) c,    rando)
                              ∘ actParry  (Act pSrc (Left a) t,    rando)
                              $ setNinja t nt' game'Mimic
          Nothing → case allow Reflected $? reflect classes n nt of
            Just nt'  → wrapEffect (Reflected : affected) f skill src t 
                        (setNinja t nt' game'Mimic) c
            Nothing   → case allow Countered $? counter classes n nt of
              Just nt' → setNinja t nt' game'Mimic
              Nothing  → let (nt', trp) = broken nt (gameNinja t game'Post) in
                         entrap c trp $ setNinja t nt' game'Post
  where skill' | Countered ∈ affected = skill { classes = Bypassing : classes }
               | otherwise            = skill
        new        = not $ [Applied, Channeled, Delayed, Trapped] ⩀ affected
        cTag       = ChannelTag (cp skill src) src skill False 3
        nt'Start   = gameNinja t game
        (nt, setn) | t ≡ src ∨ Instant ≡ channel     = (nt'Start, False)
                   | [Applied, Disrupted] ⩀ affected = (nt'Start, False)
                   | not $ isChanneling label n      = (nt'Start, False)
                   | otherwise = 
                        (nt'Start { nTags = cTag : nTags nt'Start }, True)
        game'Tag   = setn ? setNinja t nt $ game
        n          = gameNinja c game
        nSrc       = gameNinja src game
        noharm     = allied c t ∧ allied src t
        harm       = not noharm
        shifted    = [Redirected, Reflected, Swapped] ⩀ affected 
        addClasses = (shifted ∧ not ([Unshifted, Shifted] ⩀ classes))
                   ? (Shifted :)
                   ∘ (Trapped ∈ affected ∧ BaseTrap ∉ classes)
                   ? (Direct :)
        game'Shift = wrapEffect (Redirected : affected) f skill 
                     { classes = Unshifted : classes } src c game'Mimic t
        skill'Do   = skill { classes = addClasses classes }
        game'Do    = f skill'Do src c game'Mimic t
        onlyDmgNs  = S.zipWith onlyDmg (gameNinjas game'Mimic) 
                                       (gameNinjas game'Do)
        game'Post  | is Silence nSrc = game'Mimic { gameNinjas = onlyDmgNs }
                   | otherwise       = game'Do
        rando      = Left (c, t)
        actParry   = flip $ act (Countered : affected)
        actParry'  = flip $ act (Countered : Parrying : affected)
        allow aff  = harm ∧ not (is AntiCounter n) ∧ aff ∉ affected
        tOrC redir | allied redir t = c
                   | otherwise      = t
        setLast n' = n' { nLastSkill = Just skill }
        game'Mimic | new       = fn c setLast
                               ∘ foldl' (doCopy True Shallow c skill) game'Tag
                               $ triggerCopy classes n harm
                   | otherwise = game'Tag
        onlyDmg nx nx' = nx { nHealth   = min (nHealth nx') (nHealth nx) 
                            , nStatuses = nStatuses nx'
                            }

-- * RUNNING ACTIONS

-- * ACTIONS

doCopy ∷ Bool → (Slot → Int → Copying) → Slot → Skill → Game 
       → (Slot, Text, Int, Int) → Game
doCopy clear' cop src skill game (c, l, s, dur) = clear' 
                                                ? alter (N.clear l c <$>) 
                                                $ fn c copy game
  where copied = Just ∘ Copied skill' ∘ sync ∘ (dur < 0) ? (+1) $ dur
        skill' = skill { cost    = χØ
                       , cd      = 0
                       , copying = cop src (sync dur - 1)
                       }                     
        copy n = n { nCopied = update s copied $ nCopied n }

teach ∷ Int → (Slot → Int → Copying) → Int → Transform
teach dur cop s _ src _ game t = fn t copy game
  where skill  = getSkill (gameNinja src game) (Left s)
        skill' = skill { copying = cop src (sync dur - 1) }
        copied = Just ∘ Copied skill' ∘ sync ∘ (dur < 0 ) ? (+1) $ dur
        copy n = n { nCopied = S.replicate 4 copied }

teachOne ∷ Int → Int → (Slot → Int → Copying) → Int → Transform
teachOne dur s' cop s _ src _ game = r' copy game
  where skill  = getSkill (gameNinja src game) (Left s)
        skill' = skill { copying = cop src (sync dur - 1) }
        copied = Just ∘ Copied skill' ∘ sync ∘ (dur < 0 ) ? (+1) $ dur
        copy n = n { nCopied = update s' copied $ nCopied n }

copyLast ∷ Int → Int → Transform
copyLast dur s Skill{..} _ c game t = case mSkill of
    Nothing     → game
    Just tSkill → doCopy False Shallow t tSkill game (c, label, s, sync dur)
  where mSkill = nLastSkill $ gameNinja t game

copyAll ∷ Int → Transform
copyAll dur _ _ c game t = fn c copy game
  where nt = gameNinja t game
        dur'  = sync  ∘ (dur < 0) ? (+1) $ dur
        cpDur = sync dur - 1
        copied skil = Just 
                    $ Copied skil { copying = Deep (cp skil t) cpDur } dur'
        copy n = n { nCopied = S.fromList ∘ map copied $ getSkills nt }

-- * HIGHER-ORDER

-- ** COMPOSITION

infixl 3 °
(°) ∷ Transform → Transform → Transform
(°) = (•)
{-# INLINE (°) #-}

infixl 1 •
(•) ∷ Transform → Transform → Transform
(f • g) skill src c game t = g skill src c (f skill src c game t) t
{-# INLINE (•) #-}

-- ** APPLICATION

-- | Directly applies an effect to the original source without going through 'wrapEffect'.
self ∷ Transform → Transform
self f skill src c game t = f skill src t game c

-- | Directly applies an effect to all allies of the original source without going through 'wrapEffect'.
alliedTeam ∷ Transform → Transform
alliedTeam f skill src c game _
    = foldl' (wrapEffect [Applied] f skill src c) game 
    $ allySlots src

-- | Directly applies an effect to all enemies of the original source without going through 'wrapEffect'.
enemyTeam ∷ Transform → Transform
enemyTeam f skill src c game _
    = foldl' (wrapEffect [Applied] f skill src c) game 
    $ enemySlots src

-- | Directly applies an effect to all characters without going through 'wrapEffect'.
everyone ∷ Transform → Transform
everyone f skill src c game _ = 
    foldl' (wrapEffect [Applied] f skill src c) game allSlots

-- ** CONDITIONAL

ifI ∷ Text → Transform → Transform
ifI l f skill src c game t
  | has l src n ∨ isChanneling l n = f skill src c game t
  | otherwise                      = game
  where n = gameNinja src game

ifnotI ∷ Text → Transform → Transform
ifnotI l f skill src c game t
  | not $ has l src n ∨ isChanneling l n = f skill src c game t
  | otherwise                            = game
  where n = gameNinja src game

ifU ∷ Text → Transform → Transform
ifU l f skill src c game t
  | has l src $ gameNinja t game = f skill src c game t
  | otherwise                    = game

ifnotU ∷ Text → Transform → Transform
ifnotU l f skill src c game t
  | not ∘ has l src $ gameNinja t game = f skill src c game t
  | otherwise                          = game

ifStacks ∷ Text → Int → Transform → Transform
ifStacks l i f skill src c game t
  | numStacks l src (gameNinja src game) ≥ i = f skill src c game t
  | otherwise                                = game

ifnotStacks ∷ Text → Int → Transform → Transform
ifnotStacks l i f skill src c game t
  | numStacks l src (gameNinja src game) < i = f skill src c game t
  | otherwise                               = game

ifDef ∷ Text → Transform → Transform
ifDef l f skill src c game t
  | hasDefense l src $ gameNinja src game = f skill src c game t
  | otherwise                             = game

ifChan ∷ Text → Transform → Transform
ifChan l f skill src c game t
  | isChanneling l $ gameNinja src game = f skill src c game t
  | otherwise                           = game

ifHealthI ∷ Int → Int → Transform → Transform
ifHealthI minHp maxHp f skill src c game t
  | hp ≥ minHp ∧ hp ≤ maxHp = f skill src c game t
  | otherwise               = game
  where hp = nHealth $ gameNinja c game

ifHealthU ∷ Int → Int → Transform → Transform
ifHealthU minHp maxHp f skill src c game t
  | hp ≥ minHp ∧ hp ≤ maxHp = f skill src c game t
  | otherwise               = game
  where hp = nHealth $ gameNinja t game

withI ∷ Text → Int → (Int → Transform) → Int → Transform
withI l amount f base skill src c game t
  | total ≡ 0 = game
  | otherwise = f total skill src c game t
  where total = has l src (gameNinja src game) ? (amount +) $ base

withU ∷ Text → Int → (Int → Transform) → Int → Transform
withU l amount f base skill src c game t
  | total ≡ 0 = game
  | otherwise = f total skill src c game t
  where total = has l src (gameNinja t game) ? (+ amount) $ base

withInvulnU ∷ Int → (Int → Transform) → Int → Transform
withInvulnU amount f base skill src c game t
  | total ≡ 0 = game
  | otherwise = f total skill src c game t
  where total = (not ∘ null ∘ getImmune $ gameNinja t game) ? (amount +) $ base

withChan ∷ Text → Int → (Int → Transform) → Int → Transform
withChan l amount f base skill src c game t
  | total ≡ 0 = game
  | otherwise = f total skill src c game t
  where total = isChanneling l (gameNinja src game) ? (+ amount) $ base

perI ∷ Text → Int → (Int → Transform) → Int → Transform
perI l amount f base skill src c game t
  | total ≡ 0 = game
  | otherwise = f total skill src c game t
  where total = base + amount * numStacks l src (gameNinja src game)

perI' ∷ Text → Int → Int → (Int → Transform) → Int → Transform
perI' l amount denom f base skill src c game t
  | total ≡ 0 = game
  | otherwise = f total skill src c game t
  where total = base + amount * numStacks l src (gameNinja src game) 
              ÷ denom

perU ∷ Text → Int → (Int → Transform) → Int → Transform
perU l amount f base skill src c game t
  | total ≡ 0 = game
  | otherwise = f total skill src c game t
  where total = base + amount * numStacks l src (gameNinja t game)

perHealthI ∷ (Int → Int) → (Int → Transform) → Transform
perHealthI ofhp f skill src c game 
    = f (ofhp ∘ nHealth $ gameNinja src game) skill src c game

perHealthU ∷ (Int → Int) → (Int → Transform) → Transform
perHealthU ofhp f skill src c game t
    = f (ofhp ∘ nHealth $ gameNinja t game) skill src c game t

perDef ∷ Text → (Int → Transform) → Int → Transform
perDef l f amt skill src c game = f (amt * def) skill src c game
  where def = sum [ defenseAmount | Defense{..} ← nDefense $ gameNinja c game
                                  , defenseSrc ≡ src
                                  , defenseL ≡ l
                                  ]

perHelpful ∷ Int → (Int → Transform) → Int → Transform
perHelpful amt f base skill src c game = f (base + amt * count) skill src c game
  where Ninja{..} = gameNinja src game
        count     = length (nubBy lEq $ filter stHelps  nStatuses)
                  + length (nubBy lEq $ filter defHelps nDefense)
        stHelps  Status{..}  = any helpful statusEfs 
                             ∧ nId ≠ statusSrc ∧ allied nId statusSrc
                             ∧ Hidden ∉ statusClasses
        defHelps Defense{..} = nId ≠ defenseSrc ∧ allied nId defenseSrc
  
perAffected ∷ Text → Int → (Int → Transform) → Int → Transform
perAffected l amt f base skill src c game@Game{..} 
    = f (base + amt * count) skill src c game
  where count = S.length $ S.filter (has l src) gameNinjas

-- ** LIFTING

r' ∷ (Ninja → Ninja) → ( Game → Slot → Game)
r' f game t = fn t f game

rt ∷ (Ninja → Ninja) → Slot → Game → Slot → Game
rt = const ∘ r'

r ∷ (Ninja → Ninja) → Transform
r = const ∘ const ∘ const ∘ r'

-- ** IMPORTS

alterCd ∷ Int → Int → Int → Transform
alterCd s v = r ∘ N.alterCd s v

cancelChannel ∷ Text → Transform
cancelChannel = r ∘ N.cancelChannel

cure ∷ (Effect → Bool) → Transform
cure match = r $ N.cure match

cureBane ∷ Transform
cureBane = r N.cureBane

kill ∷ Transform
kill = const ∘ const ∘ const $ r' (N.kill True)
kill' ∷ Transform
kill' = const ∘ const ∘ const $ r' (N.kill False)

killThen ∷ Transform → Transform
killThen f skill src c game t
  | not ∘ isAlive $ gameNinja t game' = wrapEffect [] f skill src c game' t
  | otherwise                         = game'
  where game' = kill skill src c game t

purge ∷ Transform
purge = r N.purge

purgeAll ∷ Transform
purgeAll = r N.purgeAll

refresh ∷ Text → Transform
refresh l _ = rt ∘ N.refresh l

remove ∷ Text → Transform
remove l _ src = rt $ N.clear l src

removeStack ∷ Text → Transform
removeStack = r ∘ N.removeStack

removeStacks ∷ Text → Int → Transform
removeStacks l i _ = rt ∘ N.removeStacks l i

reset ∷ Int → Int → Transform
reset s = r ∘ N.reset s

resetAll ∷ Transform
resetAll = r N.resetAll

resetCharges ∷ Transform
resetCharges = r N.resetCharges

-- * EFFECTS

wait ∷ Transform
wait _ _ _ game _ = game

wait' ∷ a → Transform
wait' = const wait

factory ∷ Transform
factory = r ninjaReset

vary ∷ Int → Int → Int → Transform
vary dur s variantV skill _ _ game t = case copying skill of
        Shallow _ _ → game
        _           → setNinja t n' game 
  where variantDur = copyDur (copying skill) ∘ sync $ incr dur
        n          = gameNinja t game
        variantVCD = varicd $ getSkill' n s variantV
        variantL   | channel skill ≡ Instant = ""
                   | otherwise               = label skill
        n'         | variantDur ≤ 0 = n 
                       { nVariants = update s [Variant{..}]    $ nVariants n }
                   | otherwise      = n
                       { nVariants = adjust' (Variant{..} :) s $ nVariants n }

vary' ∷ Int → Int → Transform
vary' s v skill@Skill{..} src c
  | dur ≡ 1   = const ∘ id
  | dur ≡ 0   = vary 0 s v skill src c
  | otherwise = vary (dur - 1) s v skill src c
  where dur = getDur channel

-- ** DELAYED

delay ∷ Int → Transform → Transform
delay dur' f' skill@Skill{..} src c game t
    | past copying = game
    | otherwise    = game { gameDelays = Delay c skill f dur : gameDelays game }
  where dur      = incr $ sync dur'
        f game' = wrapEffect [Delayed] f' skill src c game' t
        past (Shallow _ d) = dur > d
        past (Deep    _ d) = dur > d
        past NotCopied     = False

trapFull ∷ TrapType → [Class] → Int → Trigger → (Int → Transform) → Transform
trapFull trapType clas dur trapTrigger f skill@Skill{..} trapSrc' _ game t
--  | trapSrc' ≠ c    = game TODO: Should reflecting not cause traps?
  | trp ∈ nTraps nt = game
  | otherwise       = setNinja t nt' game
  where trapDur     = copyDur copying ∘ incr $ sync dur
        trapL       = label
        nt          = gameNinja t game
        trapSrc     = cp skill trapSrc'
        trapClasses = nub ∘ (clas ⧺) ∘ map invis $ classes
        trapTrack   = 0
        trapDesc    = desc
        trp         = Trap{..}
        nt'         = nt { nTraps = nTraps nt ▷ trp }
        trapEf amt from game' = wrapEffect [Trapped] (f amt) skill
                                trapSrc trapSrc game'
                              $ if trapType ≡ TrapFrom then from else t
        invis InvisibleTraps = Invisible
        invis a              = a

trapWith ∷ TrapType → [Class] → Int → Trigger → Transform → Transform
trapWith trapType clas dur tr f = trapFull trapType clas dur tr (const f)

trap ∷ Int → Trigger → Transform → Transform
trap = trapWith TrapTo []
trap' ∷ Int → Trigger → Transform → Transform
trap' = trapWith TrapTo [Hidden]

trapFrom ∷ Int → Trigger → Transform → Transform
trapFrom = trapWith TrapFrom []
trapFrom' ∷ Int → Trigger → Transform → Transform
trapFrom' = trapWith TrapFrom [Hidden]

trapPer  ∷ Int → Trigger → (Int → Transform) → Transform
trapPer  = trapFull TrapPer []
trapPer' ∷ Int → Trigger → (Int → Transform) → Transform
trapPer' = trapFull TrapPer [Hidden] 

removeTrap ∷ Text → Transform
removeTrap l skill src = rt ∘ N.clearTrap l $ cp skill src

onBreak ∷ Transform → Transform
onBreak f skill@Skill{..} src c game t
  | not $ hasDefense label src $ gameNinja t game = game
  | otherwise = trap' 0 (OnBreak label) f skill src c game t

-- ** STATUSES

hasten ∷ Int → Text → Transform
hasten dur l Skill{..} src = rt $ N.hasten (sync dur) l src

prolong ∷ Int → Text → Transform
prolong dur l Skill{..} src = rt $ N.prolong (copyDur copying $ sync dur) l src

prolongChannel ∷ Int → Text → Transform
prolongChannel dur l = r $ \n@Ninja{..} → n
    { nChannels = map f nChannels }
  where f ch@Channel{..} | getDur ch ≤ 0          = ch
                         | label channelSkill ≠ l = ch
                         | otherwise = setDur ( copyDur (copying channelSkill) 
                                              $ getDur ch + sync dur
                                              ) ch

setFace ∷ Int → Transform
setFace dur skill@Skill{..} src c game t
  | copying ≡ NotCopied = r setFace' skill src c game t
  | otherwise           = game
  where setFace' n = n { nFace = Face label src (sync dur) : nFace n }

doDisrupt ∷ Ninja → Game → Channel → Game
doDisrupt Ninja{..} game Channel{..} = foldl' (flip f) game disrupting
  where l          = label channelSkill
        disrupting = (Self,  r $ N.unVariant l)
                   : (Enemy, remove l) 
                   : disrupt channelSkill
        f = doEffect [Channeled, Disrupted]
            channelSkill nId nId channelT (Left (channelT, channelT))

doDisrupts ∷ Ninja → [Channel] → Game → Game
doDisrupts n chans game = foldl' (doDisrupt n) game chans

disruptAll ∷ Slot → [Effect] → Game → Slot → Game
disruptAll t fs game c
  | allied t c     = game
  | null disrupted = game
  | otherwise      = doDisrupts n disrupted 
                   $ setNinja c n { nChannels = nChannels n ∖ disrupted } game
  where n = gameNinja c game
        disrupted = filter disr $ nChannels n
        disr Channel{..} = (t ≡ channelT) 
                         ∧ (isControl channelDur)
                         ∧ ((any (∈ fs) ∘ map Immune ∘ classes $ channelSkill))

interrupt ∷ Transform
interrupt _ _ _ game t
  | is Enrage nt = game
  | otherwise    = doDisrupts nt' disrupted $ setNinja t nt' game
  where nt = gameNinja t game
        disrupted = filter disr $ nChannels nt
        nt'       = nt { nChannels = filter (not ∘ disr) $ nChannels nt }
        disr Channel{..} = isControl channelDur ∨ isAction channelDur

defaultL ∷ Text → Skill → Text
defaultL "" Skill{..} = label
defaultL l _ = l

-- channelskill, channelt, channeldur
applyFull ∷ [Class] → Bool → [(Bomb, Transform)] → Text → Int → [Effect] 
          → Transform
applyFull clas bounced statusBombs' l dur fs statusSkill@Skill{copying} statusSrc statusC game t
  | null fs ∧ Shifted ∈ clas             = game
  | already ∧ (bounced ∨ isSingle)       = game
  | already ∧ Extending ∈ statusClasses  = setNinja t nt'Extend game
  | not (null fs) ∧ null statusEfs       = game
  | not bounced                          = game'Bounce
  | otherwise                            = game'Stat
  where statusRoot    = cp statusSkill statusSrc
        statusDur     = copyDur copying ∘ incr $ sync dur
        statusMaxDur  = statusDur
        already       = has statusL statusSrc nt
        statusBombs   | statusDur ≤ incr (sync dur) = statusBombs'
                      | otherwise                   = []
        statusL       = defaultL l statusSkill
        isSingle      = statusL ≡ label statusSkill ∧ Single ∈ clas
        n             = gameNinja statusSrc game
        nt            = gameNinja t game
        selfApplied   = statusSrc ≡ statusC ∧ statusC ≡ t
        extending     = N.prolong' statusDur l statusRoot
        nt'Extend     = nt { nStatuses = mapMaybe extending $ nStatuses nt}
        statusEfs     = bounced ? filter (not ∘ isDmg) 
                      ∘ is Silence n ? filter isDmg 
                      $ filterEffects nt fs
        statusClasses = nub
                      ∘ any bind fs ? (Soulbound :)
                      ∘ (selfApplied ∧ any (not ∘ helpful) fs) ? (Unremovable :)
                      $ clas ⧺ classes statusSkill
        disrupted     = filter disr $ nChannels nt
        disruptCtrl   = filter (isControl ∘ channelDur) disrupted
        nt'           = nt { nChannels = nChannels nt ∖ disruptCtrl }
        game'Disr     = doDisrupts nt' disrupted $ setNinja t nt' game
        game'Interr   = foldl' (disruptAll t statusEfs) game'Disr allSlots
        game'Stat     = fn t (N.addStatus Status{..}) game'Interr
        game'Bounce   | selfApplied = game'Stat
                      | otherwise   = foldl' bounce game'Stat ∘ delete statusSrc 
                                    $ getShare nt
        bounce        = applyFull [] True statusBombs l dur fs statusSkill 
                        statusSrc statusC
        disr chan = not (is Enrage nt ∨ is Focus nt) 
                  ∧ (any (∈ statusEfs) ∘ map Stun ∘ classes $ channelSkill chan)
        bind (Redirect _) = True
        bind _            = False
        isDmg (Afflict a) = a > 0
        isDmg _           = False

applyWith' ∷ [Class] → Text → Int → [Effect] → Transform
applyWith' classes = applyFull classes False []
applyWith ∷ [Class] → Int → [Effect] → Transform
applyWith classes = applyWith' classes ""
apply' ∷ Text → Int → [Effect] → Transform
apply' = applyWith' []
apply ∷ Int → [Effect] → Transform
apply = apply' ""
  
applyDur ∷ [Effect] → Int → Transform
applyDur = flip apply

applyDur' ∷ Text → [Effect] → Int → Transform
applyDur' l = flip (apply' l)

bombWith' ∷ [Class] → Text → Int → [Effect] → [(Bomb, Transform)] → Transform
bombWith' classes l dur fs bombs = applyFull classes False bombs l dur fs
bombWith ∷ [Class] → Int → [Effect] → [(Bomb, Transform)] → Transform
bombWith classes = bombWith' classes ""
bomb' ∷ Text → Int → [Effect] → [(Bomb, Transform)] → Transform
bomb' = bombWith' []
bomb ∷ Int → [Effect] → [(Bomb, Transform)] → Transform
bomb = bomb' ""

flag' ∷ Text → Transform
flag' l = applyWith' [Hidden, Unremovable, Nonstacking] l (-1) []
        
flag ∷ Transform
flag = flag' ""

tag' ∷ Text → Int → Transform
tag' l dur = applyWith' [Unremovable, Nonstacking] l dur []

tag ∷ Int → Transform
tag = tag' ""

hide' ∷ Text → Int → [Effect] → Transform
hide' = applyWith' [Unremovable, Hidden]
hide ∷ Int → [Effect] → Transform
hide = hide' ""

snapshot ∷ Int → Transform
snapshot dur' skill@Skill{..} src c = r' 
                                    $ \n@Ninja{..} → N.addStatus (status n) n
  where dur = copyDur copying ∘ incr $ sync dur'
        status n = Status label src src c skill [Snapshot n] 
                   (Unremovable : classes) [] dur dur

applyX ∷ Int → (Int → Effect) → Int → Transform
applyX dur constructor i = apply dur [constructor i]

addStacks' ∷ Int → Text → Int → Transform
addStacks' _    _ 0  _    _   _ = const
addStacks' dur li i skill src c = r' $ N.addStacks dur li i skill src c
addStacks ∷ Text → Int → Transform
addStacks = addStacks' 0
addStack ∷ Transform
addStack skill@Skill{..} = addStacks label 1 skill    

cureStun ∷ Transform
cureStun = cure cured
  where cured (Stun _) = True
        cured _        = False

cureAll ∷ Transform
cureAll = cure $ const True

-- ** CHAKRA

gain ∷ [ChakraType] → Transform
gain chakras _ _ _ game@Game{..} t = game
    { gameChakra = bySlot t (+~ χ nonrands) gameChakra
    , gameDrain  = bySlot t (— length rands) gameDrain
    }
  where (rands, nonrands) = partition (≡ Rand) chakras

drain ∷ Int → Transform
drain amount _ _ _ game@Game{..} t
  | is Enrage $ gameNinja t game = game
  | otherwise = game { gameDrain = bySlot t (+ amount) gameDrain }

steal ∷ Int → Transform
steal amount _ _ _ game@Game{..} t
  | is Enrage $ gameNinja t game = game
  | otherwise = game { gameSteal = bySlot t (+ amount) gameSteal }

-- ** HEALTH

bar ∷ Int → (Int → Transform) → Transform → Int → Transform
bar dur done during barrierAmount' skill@Skill{..} barrierSrc c game t
  | barrierAmount ≡ 0 = game
  | single            = game
  | barrierAmount < 0 = damage (-barrierAmount) skill barrierSrc c game c
  | otherwise         = setNinja t nt { nBarrier = barr : barrier } game
  where barrierL      = label
        nt            = gameNinja t game
        barrierAmount = barrierAmount' + getBuild (gameNinja barrierSrc game)
        barrierDur    = copyDur copying $ sync dur
        barr          = Barrier{..}
        single        = Single ∈ classes ∧ any (lEq barr) (nBarrier nt)
        barrier       = (Nonstacking ∈ classes) ? filter (not ∘ lEq barr)
                      $ nBarrier nt
        barrierWhile game' = wrapEffect [Channeled, Trapped] 
                             during skill barrierSrc c game' t
        barrierDone amount game'
          | barrierDur < sync dur = game'
          | otherwise = wrapEffect [Trapped] 
                        (done amount) skill barrierSrc c game' t

defend' ∷ Text → Int → Int → Transform
defend' l dur defenseAmount' skill@Skill{..} defenseSrc c game t
  | defenseAmount ≡ 0 = game
  | single            = game
  | defenseAmount < 0 = damage (-defenseAmount) skill defenseSrc c game t
  | otherwise         = setNinja t nt { nDefense = def : defense } game
  where defenseL      = defaultL l skill
        nt            = gameNinja t game
        defenseAmount = getBoost defenseSrc nt * defenseAmount' 
                      + getBuild (gameNinja defenseSrc game)
        defenseDur    = copyDur copying ∘ incr $ sync dur
        def           = Defense{..}
        single        = Single ∈ classes ∧ any (lEq def) (nDefense nt)
        defense       = (Nonstacking ∈ classes) ? filter (not ∘ lEq def)
                      $ nDefense nt

defend ∷ Int → Int → Transform
defend = defend' ""

addDefense ∷ Text → Int → Transform
addDefense l amt _ src _ game t = 
    case find (lMatch l src) nDefense of
        Nothing → game
        Just defense@Defense{..} → flip (setNinja t) game
            $ nt { nDefense = defense { defenseAmount = defenseAmount + amt }
                            : deleteBy lEq defense nDefense 
                 }
  where nt@Ninja{..} = gameNinja t game

demolish ∷ Transform
demolish _ _ c game@Game{..} t = f game
    where unDefense n = n { nDefense = [] }
          unBarrier n = n { nBarrier = [] }
          f = fn c unBarrier ∘ fn t unDefense

demolish' ∷ Int → Transform
demolish' = attack Demolish

commandeer ∷ Transform
commandeer _ _ c game t = setNinja t nt' $ setNinja c n' game
  where n   = gameNinja c game
        nt  = gameNinja t game
        n'  = n  { nDefense  = nDefense nt ⧺ nDefense nt 
                 , nBarrier  = []
                 , nStatuses = mapMaybe gainHelpful (nStatuses nt) ⧺ nStatuses n
                 }
        nt' = nt { nDefense  = [] 
                 , nBarrier  = nBarrier n 
                 , nStatuses = mapMaybe loseHelpful $ nStatuses nt
                 }
        loseHelpful st@Status{..}
          | Unremovable ∈ statusClasses = Just st
          | null statusEfs              = Just st
          | not $ any losable statusEfs = Just st
          | all losable statusEfs       = Nothing
          | otherwise = Just st { statusEfs = filter (not ∘ losable) statusEfs }
        gainHelpful st@Status{..}
          | Unremovable ∈ statusClasses = Nothing
          | null statusEfs              = Nothing
          | not $ any losable statusEfs = Nothing
          | all losable statusEfs       = Just st
          | otherwise = Just st { statusEfs = filter losable statusEfs }
        losable ef = helpful ef ∧ not (sticky ef)

setHealth ∷ Int → Transform
setHealth = r ∘ N.setHealth

heal ∷ Int → Transform
heal hp _ src _ game t
  | is Plague nt = game
  | otherwise    = setNinja t nt { nHealth = nHealth' } game
  where nt       = gameNinja t game
        hp'      = getBoost src nt * hp + getBless (gameNinja src game)
        nHealth' = healthBound (minHealth nt) $ hp' + nHealth nt

restore ∷ Int → Transform
restore percent _ src _ game t
  | is Plague nt = game
  | otherwise    = setNinja t nt { nHealth = hp } game
  where nt      = gameNinja t game
        tHealth = nHealth nt
        hp      = healthBound (minHealth nt)
                $ tHealth + getBoost src nt * percent * (100 - tHealth) ÷ 100
                + getBless (gameNinja src game)

sacrifice ∷ Int → Int → Transform
sacrifice minhp hp _ c _ game t 
  | c ≡ t ∧ is ImmuneSelf (gameNinja t game) = game
  | otherwise                                = r' (N.sacrifice minhp hp) game t

leech ∷ Int → (Int → Transform) → Transform
leech hp f skill src c game t = f hp' skill src c 
                                (setNinja t (N.attack hp' nt) game) t
  where nt  = gameNinja t game
        hp' = min hp $ nHealth nt

absorbDefense ∷ Int → [Defense] → (Int, [Defense])
absorbDefense hp (def@Defense{..} : defs)
  | defenseAmount > hp = (0, def { defenseAmount = defenseAmount - hp } : defs)
  | otherwise          = absorbDefense (hp - defenseAmount) defs
absorbDefense hp [] = (hp, [])

absorbBarrier ∷ Int → [Barrier] → (Int, [Barrier])
absorbBarrier hp (barr@Barrier{..} : barrs)
  | barrierAmount > hp = (0, barr { barrierAmount = barrierAmount - hp } : barrs)
  | otherwise          = absorbBarrier (hp - barrierAmount) barrs
absorbBarrier hp [] = (hp, [])

data Attack = AttackAfflict 
            | AttackPierce 
            | AttackDamage 
            | Demolish
            deriving (Eq)

attack ∷ Attack → Int → Transform
attack _   0  _ _  _     game _ = game
attack atk hp Skill{..} src c game t
  | not direct ∧ is (Stun class') n = game
  | hp'tSt ≤ 0                      = game
  | atk ≡ AttackAfflict             = tr $ fn t (N.attack hp'tSt) game
  | atk ≡ Demolish ∨ hp'Def ≤ 0     = tr $ setNinja t nt'Def game'
  | hp'Def ≡ 0                      = game'
  | otherwise = tr $ setNinja t (N.attack hp'Def nt'Def) game'
  where direct   = Direct ∈ classes
        n        = gameNinja c game
        nt       = gameNinja t game
        class'   | atk ≡ AttackAfflict = Affliction
                 | otherwise           = NonAffliction
        classes' = class' : classes
        hp'cSt   | direct    = toRational hp
                 | otherwise = (getScale classes' n *) 
                             ∘ toRational
                             $ hp + getLink src nt + getStrengthen classes' n 
        reduces  = atk ≡ AttackDamage ∧ not (is Pierce n) ∧ not (is Expose nt)
        reduced  = (— toRational (getReduce classes' nt))
                 ∘ (* getWard classes' nt)
        hp'tSt   = truncate 
                 ∘ (— toRational (getReduce [Affliction] nt))
                 ∘ reduces ? reduced
                 ∘ (* getWard [Affliction] nt)
                 ∘ (atk ≠ AttackAfflict ∧ not direct) 
                 ? (— toRational (getWeaken classes' n))
                 $ hp'cSt + toRational (getBleed classes' nt)
        damaged  | atk ≡ AttackAfflict = hp'tSt > 0
                 | otherwise           = hp'Def > 0
        tr       = damaged ? entrap c (getTraps True (OnDamaged class') nt)
        game'    | direct    = game
                 | otherwise = setNinja c n { nBarrier = barrier' } game
        nt'Def   = nt { nDefense = defense' }
        (hp'Bar, barrier') = absorbBarrier hp'tSt (nBarrier n)
        (hp'Def, defense') | direct    = absorbDefense hp'tSt (nDefense nt)
                           | otherwise = absorbDefense hp'Bar (nDefense nt)

afflict ∷ Int → Transform
afflict = attack AttackAfflict
pierce  ∷ Int → Transform
pierce  = attack AttackPierce
damage  ∷ Int → Transform
damage  = attack AttackDamage

execute ∷ (Int → Transform) → Int → Transform
execute attackType amount skill src c game t
  | executed =  flag' "executed" skill src c attacked src
  | otherwise = attacked
  where attacked = attackType amount skill src c game t
        executed = not ∘ isAlive $ gameNinja t attacked

-- ** INVULNERABILITY SKILL

invuln' ∷ Text → Text → [Class] → [Transform] → [Skill]
invuln' label desc classes effects
    = [ newSkill { label   = label
                 , desc    = desc
                 , classes = classes
                 , cd      = 4
                 , cost    = χ [Rand]
                 , effects = (Self, apply 1 [Immune All]) : map (Self, ) effects
                 } 
      ]

invuln ∷ Text → Text → [Class] → [Skill]
invuln label name classes = invuln' label 
                            (name ☩ " becomes invulnerable for 1 turn.") 
                            classes []

-- * MVP ZONE

kabuto' ∷ Skill → Ninja → Ninja
kabuto' skill n@Ninja{..} = 
    n { nStatuses = newmode : filter (not ∘ findMode) nStatuses 
      , nVariants = S.fromList $ map (:[]) [var', var, var, var]
      , nChannels = init nChannels ⧺ [swap $ last nChannels]
      }
  where sage      = " Sage"
        sLen      = T.length sage
        (mode, m) = advance ∘ maybe "" (T.dropEnd sLen ∘ statusL) 
                  $ find findMode nStatuses
        var       = Variant m False "" 0
        var'      = Variant (m + 1) False "" 0
        ml        = mode ☩ sage
        newmode   = Status ml nId nId nId skill [] [Hidden, Unremovable] [] 0 0
        findMode st = statusSrc st ≡ nId ∧ sage ≡ T.takeEnd sLen (statusL st) 
        advance "Bloodline" = ("Genjutsu" , 2)
        advance "Genjutsu"  = ("Ninjutsu" , 3)
        advance "Ninjutsu"  = ("Taijutsu" , 4)
        advance _           = ("Bloodline", 1)
        swap ch@Channel{..} = ch { channelSkill = channelSkill { label = ml } }

kabuto ∷ Transform
kabuto skill = r (kabuto' skill) skill
