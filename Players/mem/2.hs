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
import Debug.Trace

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
-- func can either be 'last' or 'head' to determine whether to play highest or lowest card
playPointCard :: [Card] -> Card
playPointCard player_cards
    | elem (Card Spade Queen) player_cards = (Card Spade Queen)
        -- If SQ in hand --> Play SQ
    | otherwise = last $ sortCards player_cards
        -- Else --> Play the highest point card
        -- If I don't have a point card, it will play the last card in hand


highestOfLowest :: [(Card, PlayerId)] -> Suit -> [Card] -> Card
highestOfLowest current_trick current_trick_suit player_cards
    | (null valid_cards) || ((length current_trick == 3) && (not point_cards)) = 
        if (length player_cards_suit > 1 && ((last player_cards_suit) == (Card Spade Queen))) 
            then last $ init $ player_cards_suit 
            else last player_cards_suit
        -- If I don't have a card lower than the highest OR if I am last to play and there are no point cards
            -- If the highest card of that suit is SQ --> play the 2nd highest card
            -- Else --> Play the highest card

    | otherwise = last $ valid_cards
        -- If I have a card lower than the highest --> play the highest card that is lower than the highest card of that suit in the trick
        where
            player_cards_suit = sortCards $ cardsOfSuit current_trick_suit player_cards
            highest_trick_suit = maximum $ cardsOfSuit current_trick_suit (cardsInTrick current_trick)
            valid_cards = filter (highest_trick_suit>) player_cards_suit
            point_cards = (||) (elem (Card Spade Queen) $ cardsInTrick current_trick) (elem 'H' $ show $ cardsInTrick current_trick)

noLeadingSuit :: [Card] -> [Card] -> [Card] -> Card
noLeadingSuit player_cards hearts_played all_cards_played
    | length all_cards_played == 0 && (Card Spade Queen) `elem` player_cards = (Card Spade Queen)
        -- If it is not the first trick && SQ in hand --> play SQ
    | length hearts_played > 0 = last $ sortCards player_cards
        -- If hearts has been broken --> play the highest heart
    | length non_point_suit_cards > 0 = (last $ non_point_suit_cards)
        -- If club or diamond cards in hand --> play the highest of those cards
    | otherwise = last $ sortCards player_cards
        -- Otherwise --> play the highest hand possible
        where non_point_suit_cards = sortCards $ cardsNotOfSuit Heart $ cardsNotOfSuit Spade player_cards

-- If:      hearts has been broken && I have cards of suit Heart that is lower than 7
-- Then:    play the lowest hearts
-- Else If: the lowest card is a queen of spades, and I have more than 1 spades
-- Then:    play the other spade card
-- Else:    play the lowest card in hand
firstToPlay :: [Card] -> [Card] -> Card
firstToPlay player_cards hearts_played
    | (length hearts_played) > 0 && length player_hearts > 0 = head player_hearts
    | head sorted_player_cards == (Card Spade Queen) && length player_spades > 1 = head $ tail sorted_player_cards
    | otherwise = head $ sortCards player_cards
        where 
            sorted_player_cards = sortCards player_cards
            player_hearts = sortCards $ cardsOfSuit Heart player_cards
            player_spades = sortCards $ cardsOfSuit Spade player_cards

-- trace ("prev: " ++ memory ++ " cards: " ++ show player_cards) 

data Memory = Memory {
    player_count :: Int,
    cards_played :: [Card]
} deriving (Show, Read, Eq)

updateMem :: Memory -> [Card] -> String
updateMem memory c_played = show $ memory { 
    cards_played = sortCards $ cards_played memory ++ c_played
}

playCard :: PlayFunc
playCard player_id player_cards current_trick previous_state
    | elem (Card Club Two) player_cards = trace (trace_str) ((Card Club Two), mem) 
        -- If C2 in hand --> Play C2

    | (null current_trick) = trace (trace_str) (firstToPlay player_cards hearts_played, mem)
        -- If I'm the first to play --> select card using firstToPlay

    | (null $ cardsOfSuit current_trick_suit player_cards) = trace (trace_str) (noLeadingSuit player_cards hearts_played all_cards_played, mem)
        -- If I don't have a card of the leading suit --> select card using noLeadingSuit

    | not $ null $ cardsOfSuit current_trick_suit player_cards = trace (trace_str) ((highestOfLowest current_trick current_trick_suit player_cards), mem)
        -- If I have a card of the leading suit --> select card using highestOfLowest

    | otherwise = trace (trace_str) ((head $ sortCards player_cards), mem)
        -- Else --> play the first card in hand
        where 
            trick_cards = cardsInTrick current_trick
            current_trick_suit = getSuit (last trick_cards)

            num_players = if (length player_cards == 13 && previous_state == Nothing) then "4" else "2"
            memory = getMemory previous_state -- HS
            mem = if memory == "" 
                then "Memory {player_count = " ++ num_players ++ ", cards_played = []}" 
                else updateMem (read memory :: Memory) (getPreviousCards previous_state)

            all_cards_played = cards_played (read mem :: Memory)
            heart_played = length (cardsOfSuit Heart all_cards_played) > 0
            queen_played = (Card Spade Queen) `elem` all_cards_played

            hearts_played = sortCards $ (cardsOfSuit Heart trick_cards) ++ (cardsOfSuit Heart all_cards_played)

            trace_str = "p_id: " ++ player_id ++ "\t\t\t" ++ show all_cards_played ++ show (cardsOfSuit Heart trick_cards)


            -- mem = if previous_state == Nothing then show $ Memory {player_count = 0, queen_played = False} else ""
            -- new_mem2 = if memory == "" then updateMem (read memory :: Memory) else memory ++ mem


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