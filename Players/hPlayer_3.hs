-- | Write a report describing your design and strategy here.
module Player (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Hearts.Types
import Cards
import Data.Maybe
-- import Debug.Trace

{-
    1)  IF leading suit is a card and I have cards of the same suit:
        - IF I have cards lower than the highest card of the same suit:
            - Play the highest that I can play that is lower than the highest card of the same suit in the current trick
                - Ex: Trick contains [C3, C10], the best card I can play is C9 if I have it
        - ELSE IF I don't have cards lower
            - Play the highest card that I can play of the same suit
        
        ELSE IF leading suit is a card I do not have:
            - Play the highest point card if point cards have been broken
            - Otherwise play highest card of any suit
        

    2) IF I am the last to play:
        - IF there are no point cards, play the highest card (try to win)

    3) IF I need to start a trick:
        - Play the lowest non-point card possible
            - If there are two cards with the same value, play from the suit with the least number of cards


-}



-- type Trick = [Play]
-- type Play = (Card, String, PlayerId)
-- We want [Card], therefore we must only get Card from Play by using "first" defined in Types.hs
-- [(Card, PlayerId)] should be Trick I think (but it works as below)
cardsInTrick :: [(Card, PlayerId)] -> [Card]
cardsInTrick trick = map fst trick

-- Gets the memory out of previous_state
getMemory :: Maybe ([(Card, PlayerId)], String) -> String
getMemory Nothing = ""
getMemory (Just a) = snd a

-- Gets the suit of the given Card
getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- Gets a list of cards in the previous trick
getPreviousCards :: Maybe ([(Card, PlayerId)], String) -> [Card]
getPreviousCards previous_state = cardsInTrick $ getCards previous_state
    where
        getCards Nothing = []
        getCards (Just a) = fst a

-- Gets all cards of a given Suit
cardsOfSuit :: Suit -> [Card] ->  [Card]
cardsOfSuit suit cards = filter ((suit ==) . getSuit) cards

-- Gets all cards not of a given Suit
cardsNotOfSuit :: Suit -> [Card] ->  [Card]
cardsNotOfSuit suit cards = filter ((suit /=) . getSuit) cards

-- Sorts cards with order provided in Cards.hs
-- obtained from FIT2102 lecture slides and changed into point free form
sortCards :: [Card] -> [Card]
sortCards [] = []
sortCards (pivot:cards) = left cards ++ [pivot] ++ right cards
    where
        left = part (<pivot)
        right = part (>=pivot)
        part = (sortCards.) . filter

-- Plays a point card in hand if available
-- Play priority: SQ > Highest Heart > Lowest Heart > First card in hand
playPointCard :: ([Card] -> Card) -> [Card] -> Card
playPointCard func player_cards
    | elem (Card Spade Queen) player_cards = (Card Spade Queen)
        -- If SQ in hand --> Play SQ
    | otherwise = (func $ sortCards player_cards)
        -- Else --> Play the highest point card
        -- If I don't have a point card, it will play the last card in hand


highestOfLowest :: [(Card, PlayerId)] -> Suit -> [Card] -> Card
highestOfLowest current_trick current_trick_suit player_cards
    | null valid_cards || (length current_trick == 3 && not point_cards) = 
        if (length player_cards_suit > 1 && ((last $ sortCards $ player_cards_suit) == (Card Spade Queen))) 
            then last $ init $ sortCards $ player_cards_suit 
            else last $ sortCards $ player_cards_suit
        -- If I don't have a card lower than the highest OR if I am last to play and there are no point cards
            -- If the highest card of that suit is SQ --> play the 2nd highest card
            -- Else --> Play the highest card

    | otherwise = last $ sortCards $ filter (highest_trick_suit>) player_cards_suit
        -- If I have a card lower than the highest --> play the highest card that is lower than the highest card of that suit in the trick
        where
            player_cards_suit = cardsOfSuit current_trick_suit player_cards
            highest_trick_suit = maximum $ cardsOfSuit current_trick_suit (cardsInTrick current_trick)
            valid_cards = filter (highest_trick_suit>) player_cards_suit
            point_cards = (||) (elem (Card Spade Queen) $ cardsInTrick current_trick) (elem 'H' $ show $ cardsInTrick current_trick)

-- noLeadingSuit :: [(Card)] -> String -> Card
-- noLeadingSuit player_cards memory
--     | (Card Spade Queen) `elem` player_cards = (Card Spade Queen)
--     | 'H' `elem` (show memory) = playPointCard head player_cards
--     | (length $ non_point_suit_cards) > 0 = (last $ non_point_suit_cards)
--     | otherwise = last $ sortCards player_cards
--         where non_point_suit_cards = sortCards $ cardsNotOfSuit Heart $ cardsNotOfSuit Spade player_cards

playCard :: PlayFunc
playCard player_id player_cards current_trick previous_state
    | elem (Card Club Two) player_cards = ((Card Club Two), memory ++ player_id) 
        -- If C2 in hand --> Play C2

    -- | (null current_trick) || (null $ cardsOfSuit current_trick_suit player_cards) =
    --     if (elem 'H' (show memory)) 
    --         then (playPointCard head player_cards, memory ++ heartsPlayed ++ queenPlayed)
    --         else if (((length $ cardsNotOfSuit Heart $ cardsNotOfSuit Spade player_cards) > 0) && (elem 'Q' (show memory)))
    --             then ((last $ sortCards $ cardsNotOfSuit Heart $ cardsNotOfSuit Spade player_cards), memory ++ heartsPlayed ++ queenPlayed) 
    --             else ((head $ sortCards player_cards), memory ++ heartsPlayed ++ queenPlayed) 
    --     -- If I'm the first to play || If I don't have a card of the leading suit
    --         -- If Hearts has been broken --> Play the lowest point card
    --         -- Else --> play the lowest non-point card

    | (null current_trick) = 
        if (elem 'H' (show memory))
            then (playPointCard head player_cards, memory ++ heartsPlayed ++ queenPlayed)
            else if ((length non_point_suit_cards) > 0)
                then ((last $ non_point_suit_cards), memory ++ heartsPlayed ++ queenPlayed)
                else ((head $ sortCards player_cards), memory ++ heartsPlayed ++ queenPlayed) 
        -- If I'm the first to play

    | (null $ cardsOfSuit current_trick_suit player_cards) = 
        if (Card Spade Queen) `elem` player_cards 
            then ((Card Spade Queen), memory ++ heartsPlayed ++ queenPlayed)
            else  if (elem 'H' (show memory))
                then (playPointCard head player_cards, memory ++ heartsPlayed ++ queenPlayed)
                else if ((length non_point_suit_cards) > 0)
                    then ((last $ non_point_suit_cards), memory ++ heartsPlayed ++ queenPlayed)
                    else ((last $ sortCards player_cards), memory ++ heartsPlayed ++ queenPlayed) 
        -- If I don't have a card of the leading suit

    | not $ null $ cardsOfSuit current_trick_suit player_cards = ((highestOfLowest current_trick current_trick_suit player_cards), heartsPlayed)
        -- If I have a card of the leading suit --> play the highest card of the leading suit that is lower than the highest card on the trick with the same suit

    | otherwise = ((head $ sortCards player_cards), memory ++ heartsPlayed ++ queenPlayed)
        -- Else, just play the first card in hand
        where 
            trick_cards = cardsInTrick current_trick
            current_trick_suit = getSuit (last trick_cards)
            memory = getMemory previous_state
            non_point_suit_cards = sortCards $ cardsNotOfSuit Heart $ cardsNotOfSuit Spade player_cards
            heartsPlayed = if elem 'H' (show $ getPreviousCards previous_state) then "H" else ""
            queenPlayed = if elem 'Q' (show $ getPreviousCards previous_state) then "Q" else ""
            --new_mem = memory ++ heartsPlayed ++ queenPlayed 

                -- If Hearts has been played before --> Append 'H' to memory
{-
type PlayFunc
    =  PlayerId                             -- ^ this player's Id so they can identify themselves in the bids and tricks
    -> [Card]                               -- ^ the player's current hand
    -> [(Card, PlayerId)]                   -- ^ cards played in the current trick, i.e., previous players cards for this trick
    -> Maybe ([(Card, PlayerId)], String)   -- ^ ([all the cards played in the previous trick], "memory") 
                                            -- ^ in the first trick, this will be Nothing, then it will report the cards played and the memory returned by the player
    -> (Card, String)                       -- ^ should return: (chosen Card, "new memory"); the memory is a string    
-} 

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined