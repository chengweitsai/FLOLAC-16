{-# Language NoMonomorphismRestriction, GADTs #-}

-- Graphical models and the inference on them
-- Discrete case

module Discrete where

import qualified Data.Map as M
import System.Random
import Control.Monad (ap)

-- ------------------------------------------------------------------------
-- Representing distributions
-- (Tagless-final aproach)

-- running example: flipping a fair coin twice and obtaining
-- the probability the two outcomes are different

xor :: Bool -> Bool -> Bool
xor x y = not (x == y)

-- We need to represent graphical models like Grass and the two-coin model
-- Draw the model.
-- We need to formally define what the graphical model really means.
-- It means the joint distribution:
-- Pr(Coin1=x,Coin2=y,HeadTail=t) =
--  {chain rule}
-- Pr(Coin1=x) * Pr(Coin2=y,HeadTail=t|Coin1=x) =
--  {chain rule}
-- Pr(Coin1=x) * Pr(Coin2=y|Coin1=x) * Pr((x = not y)=t|Coin1=x,Coin2=y)
--  {conditional independence, from the graph}
-- Pr(Coin1=x) * Pr(Coin2=y) * Pr((x = not y)=t|Coin1=x,Coin2=y)


-- Let's define operations we need
type Prob = Double

-- First attempt

-- Suppose (d t) represents the probability distribution, in some yet
-- to be defined way. We parameterized it by the type of samples:
-- we really want to distinguish coin flip (producing Booleans) from
-- die roll or the normal distribution

class Dist' d where
  bern'  :: Prob -> d Bool

  -- To represent terms like Pr(t=(xor true false))
  -- which is 1 if t is indeed true, or 0 otherwise.
  -- Pr(a=x)
  dirac' :: a -> d a

  -- How to represent the conditional distribution Pr(X|P)?
  -- (for some parameters P)?
  -- Obviously, like functions P -> Pr(X)

  -- How to represent the chain law
  -- Pr(X=x,Y=y) = PR(X=x) * Pr(Y=y|X=x)
  chain' :: d a -> (a -> d b) -> d (a,b)

twocoins' = chain' (bern' 0.5) $ \x ->
            chain' (bern' 0.5) $ \y ->
            dirac' (xor x y)
-- what is the type?
-- How is the conditional independence expressed?

-- How to compute the probability if in two successive tosses,
-- there is one head, one tail?

-- Integrate out
-- Sum_x Sum_y Pr(Coin1=x,Coin2=y,HeadTail)
-- What this means...
-- [((False,(False,False)),0.25),
--  ((False,(True,True)),0.25),
--  ((True,(False,True)),0.25),
--  ((True,(True,False)),0.25)]


-- The summation is bothersome. There should be a better way.
-- Let's combine sampling with chain
-- Pr(Y=y) = Sum_x Pr(X=x,Y=y) = Sum_x Pr(X=x) * Pr(Y=y|X=x)
-- This has a nice ring to it: building a  new distribution from
-- others.

class Dist'' d where
  bern''  :: Prob -> d Bool

  dirac'' :: a -> d a

  chain'' :: d a -> (a -> d b) -> d b


twocoins'' = chain'' (bern'' 0.5) $ \x ->
             chain'' (bern'' 0.5) $ \y ->
             dirac'' (xor x y)


-- What is the inferred type?

-- We haven't lost anything

twocoins1'' = chain'' (bern'' 0.5) ( \x ->
             chain'' (bern'' 0.5)  ( \y ->
             dirac'' (x,(y,(xor x y)))))

-- The notation is a bit ungainly though...

--
-- What a happy coincidence!

class Monad d => Dist d where
  bern :: Prob -> d Bool
  failure :: d a         -- we see the need for this when it comes to
                         -- conditioning

twocoins = do
  x <- bern 0.5
  y <- bern 0.5
  return $ xor x y

twocoins1 = do
  x <- bern 0.5
  y <- bern 0.5
  return $ (x,(y,xor x y))

-- This is just the syntactic sugar. GHC re-writes it into the form
-- twocoins' that we saw before.

-- Again, what is the type of twocoins?

-- Let us implement another model, as an exercise:
-- die roll; throw n dice
-- Draw the model

class Dist d => Die d where
  die :: d Int   -- in Agda, Finite 6. But we do Int for now

 -- dieroll n = undefined
dieroll :: (Functor d, Die d, Dist d) => Int -> d Int
dieroll n = fmap sum $ sequence $ replicate n die  --   $ : 右結合

_ = runExact (dieroll 3)

-- Implementing grass model

grass_fwd = do
  rain         <- bern 0.3
  sprinkler    <- bern 0.5
  grass_is_wet <- nor 0.9 0.8 0.1 rain sprinkler
  return grass_is_wet

-- noisy-or function
nor :: Double -> Double -> Double -> Bool -> Bool -> Dst Bool
nor strengthX strengthY noise x y =
  bern $ 1 - nnot (1-strengthX) x * nnot (1-strengthY) y * (1-noise)

-- noisy not function
nnot :: Num a => a -> Bool -> a
nnot p True  = p
nnot p False = 1

_ = runExact grass_fwd

-- Implement uniformly given bern
-- Demonstrate that the implementation is correct
-- Prove it
-- Point: recursive implementation

uniformly :: Ord a => [a] -> Dst a
-- exwecise only using
uniformly [] = failure
uniformly [a] = return a
uniformly lst@(h:t) = do
  f <- bern (1/fromIntegral (length lst))
  if f then return h else uniformly t


_ = runExact $ uniformly ([]::[Int])
_ = runExact $ uniformly [1]
_ = runExact $ uniformly [1,2]
_ = runExact $ uniformly [1..10]

{- Blood type example (simple version)

From PRISM documentation,
1.2 Basic probabilistic inference and parameter learning

``Let us pick up another example that models the inheritance mechanism
of human's ABO blood type. As is well-known, a human's blood type
(phenotype) is determined by his/her genotype, which is a pair of two
genes (A, B or O) inherited from his/her father and mother. For
example, when one's genotype is AA or AO (OA), his/her phenotype will
be type A. In a probabilistic context, on the other hand, we consider
a pool of genes, and let pa , pb and po denote the frequencies of gene
A, B and O in the pool, respectively ( pa + pb + po = 1). When random
mating is assumed, the frequencies of phenotypes, namely, PA , PB , PO
and PAB , are computed by Hardy- Weinberg's law [13]: PA = pa^2 + 2 pa
po , P B = pb^2 + 2 pb po , PO = po^2 and PAB = 2 pa pb.''

?- prob(bloodtype(a)).
Probability of bloodtype(a) is: 0.360507016168634

http://rjida.meijo-u.ac.jp/prism/download/prism22.pdf
-}

data Gene = GA | GB | GO
data BloodType = A | B | O | AB

{- Monty Hall Problem
https://en.wikipedia.org/wiki/Monty_Hall_problem

    Suppose you're on a game show, and you're given the choice of three doors:
    Behind one door is a car; behind the others, goats. You pick a door, say
    No. 1, and the host, who knows what's behind the doors, opens another door,
    say No. 3, which has a goat. He then says to you, "Do you want to pick door
    No. 2?" Is it to your advantage to switch your choice?
-}

monty_hall :: Bool -> Dst Bool
-- monty_hall = undefined
monty_hall shoudlSwitch = do
  car <- uniformly doors
  mydoors <- uniformly doors
  hostdoor <- uniformly $ del car . del mydoors $ doors
  if shoudlSwitch
    then let [remaining] = del hostdoor . del mydoors  $ doors in
          return $ remaining == car
    else return $ mydoors == car
  where
  doors = [1, 2, 3]
  uniformly xs = Single [(x,p) |
                            let p = 1/fromIntegral(length xs),
                        x <- xs]

del :: Eq a => a -> [a] -> [a]
del x [] = []
del x (h:t) | x == h = del x t
            | otherwise = h : (del x t)

_ = runExact $ monty_hall False
_ = runExact $ monty_hall True

-- ------------------------------------------------------------------------
-- Conditioning
-- Let's go back to the running example (slightly more complicated):
-- We flip a fair coin thrice. We now *observed* that the
-- results of three flips are not all the same.
-- What is the (posterior) probability that the first coin came up head (True)?
-- (draw the model)

{-
 We want to compute
 Pr(Coin1=x|TheSame=False)
    { Multiply/divide by Pr(TheSame=False) }
 = Pr(Coin1=x|TheSame=False) * Pr(TheSame=False) / Pr(TheSame=False)
    {Chain rule; obtaining the Joint distribution}
 = Pr(Coin1=x,TheSame=False) / Pr(TheSame=False)
    {Integrating out}
 = Sum_y Sum_z Pr(Coin1=x,Coin2=y,Coin3=z,TheSame=False) /
   Sum_x Sum_y Sum_z Pr(Coin1=x,Coin2=y,Coin3=z,TheSame=False)

How to find the above numerator and denominator from the joint distribution:
  False False False  True     1/8
  False False True   False    1/8
  False True False   False    1/8
  False True  True   False    1/8
  True  False False  False    1/8
  True  False True   False    1/8
  True  True  False  False    1/8
  True  True  True   True     1/8

First, remove all rows where TheSame is not False

  False False True   False    1/8
  False True False   False    1/8
  False True  True   False    1/8
  True  False False  False    1/8
  True  False True   False    1/8
  True  True  False  False    1/8

Then integrate out Coin2 and Coin3 (do Sum_y and Sum_z)

  False    False    3/8
  True     False    3/8

for the last factor, we just sum up: 6/8
The desired result

  False    False    1/2
  True     False    1/2

-}

-- How to program this
-- Recall, the joint distribution corresponds to a model

joint3:: Dst (Bool,Bool,Bool,Bool)
joint3 = do
  coin1 <- bern 0.5
  coin2 <- bern 0.5
  coin3 <- bern 0.5
  same  <- return (coin1 == coin2 && coin2 == coin3)
  return (coin1,coin2,coin3,same)

_ = runExact joint3

-- Pr(Coin1=x|Same=Fasle)
one_not_same = do
  (coin1,coin2,coin3,same) <- joint3
  if same then failure  -- erasing rows
     else return coin1  -- coin2 and coin3 are integrated out

_ = runExact one_not_same

-- But we didn't compute the term in the denominator.
-- How to do that?   need to normalize

_ = normalize $ runExact one_not_same

normalize :: PT a -> PT a
normalize pt = map (map_snd (/nf)) pt
 where nf = sum $ map snd pt

-- Eldest daughter puzzle
-- A family has exactly two kids, one of them is a girl.
-- What is the chance the older is a girl?
girl :: Dst Bool
girl = do
  girl1 <- bern 0.5
  girl2 <- bern 0.5
  if girl1 || girl2 then return girl1 else failure

-- _ = runExact girl
-- _ = normalize $ runExact girl

-- Grass model

grass_wet = do
  rain         <- bern 0.3
  sprinkler    <- bern 0.5
  grass_is_wet <- nor 0.9 0.8 0.1 rain sprinkler
  -- return (rain, sprinkler,grass_is_wet)
  if not grass_is_wet then failure else return (rain, sprinkler)

-- Pr(Rain|Wet)
--__ = normalize . runExact $ do { (r,s) <- grass_wet; return r}

-- The result matches what we had before. Much simpler this time, right?
-- Pr(Sprinkler|Wet)
--__ = normalize . runExact $ do { (r,s) <- grass_wet; return s}



-- Fair coin tosses given arbitrarily biased coin (von Neumann trick)
-- Show/Prove the correctness
-- (recursion, rejection)

fair_coin :: Dst Bool -> Dst Bool
fair_coin c = undefined

_ = runExact $ fair_coin (bern 0.2)

-- Drunken coin example:
-- very large depth and very low probability of acceptance

-- Drunken coin example: coin flipping by a drunk.
-- Because the person flipping the coin is drunk, most of the time
-- the result of flipping is a lost/dropped coin

drunk_coin :: Dst Bool
drunk_coin = do
  c <- bern 0.5
  lost <- bern 0.9
  if lost then failure else return c

-- Compute AND of n tosses of the drunk coin
dcoin_and n = fmap (foldr1 (&&)) . sequence $ replicate n drunk_coin

-- _ = runExact $ dcoin_and 10
-- [(False,9.990234374999978e-11),(True,9.765624999999978e-14)]

-- Can we implement more efficiently?  *** by accumalate only one at each chain


-- (shorthand and) Why the False probability changes?

-- Alarm puzzle
-- http://aima.eecs.berkeley.edu/slides-pdf/chapter14a.pdf
alarm :: Dst Bool
alarm = do
  burglary <- bern 0.001
  earthQuake <- bern 0.003
  a <- case (burglary , earthQuake) of
          (True, True)    -> bern 0.95
          (True, False)   -> bern 0.94
          (False, True)   -> bern 0.29
          (False, False)  -> bern 0.001
  j <- if a then bern 0.9 else bern 0.05
  m <- if a then bern 0.7 else bern 0.01
  if (j && not m ) then return burglary else failure
_ = normalize $ runExact alarm
-- p5 and 6
-- See examples on pp19 and 20


-- HMM: Hidden Markov Model
{-
  States represent position of a 1-dimensional random walk on 1..8

  State transition: one position to left/right with prob. 0.3 each;
  stay in state with prob. 0.4 (0.7 for boundary states)

  Observable: two states 'l' and 'r'. Prob. of observation=l in state
  k is 1-(k-1)/7
-}

newtype HMMState = HS{unHS:: Int} deriving (Show,Eq,Ord)
instance Bounded HMMState where
  minBound = HS 1
  maxBound = HS 8

instance Enum HMMState where
  succ (HS i) = HS (i+1)
  pred (HS i) = HS (i-1)

-- Transition Probabilities
transitions :: HMMState -> PT HMMState
transitions st = case () of
  _ | st == minBound -> [(st,0.7),(succ st,0.3)]
  _ | st == maxBound -> [(st,0.7),(pred st,0.3)]
  _                  -> [(st,0.4),(succ st,0.3),(pred st,0.3)]

-- Observations
data HMMObs = L | R
-- Tabulate the observation probabilities: of observing L
-- We use the numeric values directly from the Primula code
l_observation_prob :: M.Map HMMState Prob
l_observation_prob = M.fromList $
   zip [minBound..maxBound]
       [1.0, 0.85714, 0.71428, 0.57142, 0.42857, 0.28571, 0.14285, 0.0]

-- The evolution function: compute the state for the next step
evolve :: HMMState -> Dst HMMState
evolve = categorical . transitions

hmmobserve :: HMMState -> Dst HMMObs
hmmobserve st =
  case (M.!) l_observation_prob st of
    0 -> return R
    1 -> return L
    p -> categorical [(L,p), (R,1-p)]

-- Queries
-- Run the model for N steps, asserting observations

evolve_with_evidence :: Int -> (Int -> HMMState -> Dst ()) -> Dst HMMState
evolve_with_evidence n evidence = do
  st() <- uniformly ([minBound .. maxBound :: HMMState])
  let evolveCheck st i = do
    s <- categorical . runExact $ st
    evidence i s
    evolve s
  foldl evolveCheck (return st()) [1..n]
{-
do_evolve n evidence =
  let st0 = uniformly [minBound..maxBound] in
  let rec iter i n st =
    if i > n then st
    else let () = evidence st i in		(* check the evidence *)
         iter (succ i) n (evolve st)
  in iter 1 n st0
-}

-- ------------------------------------------------------------------------
-- Implementation
-- It is quite tricky; and those who want to know the tricky bit,
-- can ask later...

-- I show the implementation that you haven't seen: it is fully
-- above the board and faithfully implements specification.

-- Probability Table
type PT a = [(a,Prob)]  -- Ord a

map_fst :: (a -> b) -> (a,c) -> (b,c)
map_fst f (x,y) = (f x,y)

map_snd :: (a -> b) -> (c,a) -> (c,b)
map_snd f (x,y) = (x,f y)


-- The representation of distributions
data Dst a where
  Single :: PT a -> Dst a
  Chain  :: PT b -> (b -> Dst a) -> Dst a

instance Functor Dst where
   fmap f (Single pt) = Single (map (map_fst f) pt)
   fmap f (Chain d k) = Chain d (fmap f . k)

instance Applicative Dst where
  pure = return
  (<*>) = ap

instance Monad Dst where
  return x = Single [(x,1)]

  -- Exercise: why is this correct?
  Single []      >>= k = Single []
  Single [(x,1)] >>= k = k x    -- Nothing to sum over
  Single pt      >>= k = Chain pt k

  -- Exercise: justify this
  -- Sum_y (Sum_X Pr(X=x) * Pr(Y=y|X=x)) * Pr(Z=z|X=x)
  -- Sum_y (Sum_X Pr(X=x) * Pr(Y=y|X=x) * Pr(Z=z|X=x))
  -- Sum_X (Sum_Y Pr(X=x) * Pr(Y=y|X=x) * Pr(Z=z|X=x))
  -- Sum_X Pr(X=x) * (Sum_Y  Pr(Y=y|X=x) * Pr(Z=z|X=x))
  -- Sum_X Pr(X=x) * (Sum_Y  Pr(Z=z|X=x))
  Chain pt k1 >>= k = Chain pt (\x -> k1 x >>= k)


-- The handler of the Dst effect: Exact inference
runExact :: Ord a => Dst a -> PT a
runExact (Single x) = x
runExact (Chain pt k) =
  M.toList . M.fromListWith (+) .
   concatMap (\ (x,p) -> map (map_snd (*p)) $ runExact (k x)) $ pt


instance Dist Dst where
  bern p  = Single [(True, p), (False,1-p)]
  failure = Single []

instance Die Dst where
  die = Single $ [ (x,1/6) | x <- [1..6] ]

_ = runExact twocoins

-- A different handler: sampling

runSample :: Int -> Dst a -> [a]
runSample n m = go (mkStdGen 17) n m
  where
    go g 0 _ = []
    go g n (Single []) = go g (n-1) m
    go g n (Single pt) =
      let (x,g1) = select g pt in x:go g1 (n-1) m
    go g n (Chain [] k) = go g (n-1) m
    go g n (Chain pt k) =
      let (x,g1) = select g pt in go g1 n (k x)

    select g [(x,1)] = (x,g)
    select g pt      =
      let (r,g1) = randomR (0::Double,1) g
          x = fst . head . dropWhile (\ (_,p) -> r > p) $
              scanl1 (\ (_,p1) (x,p2) -> (x,p1+p2)) pt
      in (x,g1)

_ = runSample 10 twocoins

_ = let n = 20 in (/ fromIntegral n) . fromIntegral . length . filter id $ runSample n grass_fwd

categorical :: PT a -> Dst a
categorical = Single

-- ------------------------------------------------------------------------
