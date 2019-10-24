module Deck where

  import Cards
  import Control.Monad
  import Data.List
  import System.Random
  
  -- | We deal each hand from a new deck.  Hands can be any size as long as we
  -- have enough cards in the deck for each player
  data Deck = Deck {
    handSize :: Int,
    deck :: [Card]
  }
  
  -- | Deal n cards each to m players.
  deal :: Int -> Int -> [Card] -> [[Card]]
  deal n m = take m . map (take n) . iterate (drop n)
  
  sortedDeck :: [Card]
  sortedDeck = Card <$> [Spade ..] <*> [Two ..]
  
  shuffleList :: [a] -> IO [a]
  shuffleList l = do
    i <- replicateM (length l) (randomIO :: IO Int)
    return $ map snd $ sortOn fst $ zip i l
  
  shuffledDeck :: IO [Card]
  shuffledDeck = shuffleList sortedDeck

  {-
  This is a custom random number generator. It works by using the shuffledDeck as an entropy.
  It will get the last half of the cards from shuffledDeck and multiply the values of the ranks where
  face cards will act as negatives so that the random number generated will have a greater range.

  This further strengthens the randomness as a new seed is created using the randomly generated
  shuffledDeck function as an entropy.

  It will then zip the sortedDeck with the list of seeds and sort them by value.
  -}

  -- Gets integer values out from the card, face cards will be set to -1 to allow for greater range.
  getVal :: Card -> Integer
  getVal (Card _ rank) = val
    where 
      val = if head (show rank) `elem` "2345678910" then read (show rank) :: Integer else -1
  
  -- Creates the random seed from a list of cards by multiplying them together.
  liftRank :: [Card] -> Integer
  liftRank [] = 1
  liftRank (x:xs) = (getVal x) * (liftRank xs)

  -- Applies liftRank to the last half of the cards obtained from shuffledDeck and sets it as the seed.
  -- When provided with the sortedDeck (or any deck of cards) it will return the shuffled deck.
  customShuffle :: [Card] -> IO [Card]
  customShuffle deck = do
    seed <- replicateM (length deck) (liftRank <$> drop ((length deck) `div` 2) <$> shuffledDeck)
    return $ map snd $ sortOn fst $ zip seed deck

  customShuffledDeck :: IO [Card]
  customShuffledDeck = customShuffle sortedDeck