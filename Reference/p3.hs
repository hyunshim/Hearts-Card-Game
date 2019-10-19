
module Player (
    playCard,
    makeBid
)
where

import Hearts.Types
import Cards

cardsInTrick :: [(Card, PlayerId)] -> [Card]
cardsInTrick trick = map fst trick


getMemory :: Maybe ([(Card, PlayerId)], String) -> String
getMemory Nothing = ""
getMemory (Just a) = snd a

getSuit :: Card -> Suit
getSuit (Card suit _) = suit

getPreviousCards :: Maybe ([(Card, PlayerId)], String) -> [Card]
getPreviousCards previous_state = cardsInTrick $ getCards previous_state
    where
        getCards Nothing = []
        getCards (Just a) = fst a

cardsOfSuit :: Suit -> [Card] ->  [Card]
cardsOfSuit suit cards = filter ((suit ==) . getSuit) cards

sortCards :: [Card] -> [Card]
sortCards [] = []
sortCards (pivot:cards) = left cards ++ [pivot] ++ right cards
    where
        left = part (<pivot)
        right = part (>=pivot)
        part = (sortCards.) . filter

playPointCard :: [Card] -> Card
playPointCard player_cards
    | elem (Card Spade Queen) player_cards = (Card Spade Queen)
    | otherwise = (last $ sortCards player_cards)

newPlayer :: PlayFunc
newPlayer player_id player_cards current_trick previous_state
    | elem (Card Club Two) player_cards = ((Card Club Two), memory ++ player_id) 

    | null current_trick = ((head $ sortCards player_cards), memory ++ heartsPlayed) 

    | not $ null $ cardsOfSuit current_trick_suit player_cards = ((head $ cardsOfSuit current_trick_suit player_cards), heartsPlayed)

    | (null $ cardsOfSuit current_trick_suit player_cards) && elem 'H' (show memory) = (playPointCard player_cards, heartsPlayed)
            
    | otherwise = ((head $ sortCards player_cards), heartsPlayed)
        where 
            trick_cards = cardsInTrick current_trick
            current_trick_suit = getSuit (last trick_cards)
            memory = getMemory previous_state
            heartsPlayed = if elem 'H' (show $ getPreviousCards previous_state) then "H" else ""

playCard :: PlayFunc
playCard player_id player_cards current_trick previous_state = newPlayer player_id player_cards current_trick previous_state

makeBid :: BidFunc
makeBid = undefined