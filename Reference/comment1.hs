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

-}


{-
    1)  IF leading suit is a card and I have cards of the same suit:
        - IF I have cards lower than the highest card of the same suit:
            - Play the highest that I can play that is lower than the highest card of the same suit in the current trick
                - Ex: Trick contains [C3, C10], the best card I can play is C9 if I have it
        - ELSE IF I don't have cards lower
            - Play the lowest card that I can play of the same suit
        
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

-- Sorts cards with order provided in Cards.hs
-- obtained from FIT2102 lecture slides and changed into point free form
sortCards :: [Card] -> [Card]
sortCards [] = []
sortCards (pivot:cards) = left cards ++ [pivot] ++ right cards
    where
        left = part (<pivot)
        right = part (>=pivot)
        part = (sortCards.) . filter

-- naivePlayer :: PlayFunc
-- naivePlayer player_id player_cards current_trick previous_state
--     | elem (Card Club Two) player_cards = ((Card Club Two), memory ++ player_id)
--         -- If player has Two of Clubs, they must play it

--     | not (null current_trick) && not (null (cardsOfSuit current_trick_suit player_cards)) = ((head $ cardsOfSuit current_trick_suit player_cards), memory)
--         -- If player has a card with the leading suit, play the first card of that leading suit

--     -- | not (null current_trick) && null (cardsOfSuit current_trick_suit player_cards) = ((head player_cards), memory)
--     --     -- If player does not have a card with the leading suit, play the first card in the hand                                                    %%% Must change so it doesn't play a point card
--     | otherwise = ((head $ sortCards player_cards), memory)
--         where 
--             trick_cards = cardsInTrick current_trick
--                 -- gets just the cards in the current trick

--             current_trick_suit = getSuit (last trick_cards)
--                 -- gets the suit of the current trick, player must follow this suit and can only break the suit if they have no cards of this suit.
            
--             memory = getMemory previous_state


-- Plays a point card in hand if available
-- Play priority: SQ > Highest Heart > Lowest Heart > First card in hand
playPointCard :: [Card] -> Card
playPointCard player_cards
    | elem (Card Spade Queen) player_cards = (Card Spade Queen)
        -- If SQ in hand --> Play SQ
    | otherwise = (last $ sortCards player_cards)
        -- Else --> Play the highest point card
        -- If I don't have a point card, it will play the last card in hand

newPlayer :: PlayFunc
newPlayer player_id player_cards current_trick previous_state
    | elem (Card Club Two) player_cards = ((Card Club Two), memory ++ player_id) 
    -- | elem (Card Club Two) player_cards = trace ("ID: " ++ player_id ++ "\t\tPrev State:" ++ show previous_state ++ "\t\t\tMem:" ++ show memory ++ "\t\t\tTrick: " ++ show current_trick) ((Card Club Two), memory ++ player_id) 
        -- If C2 in hand --> Play C2

    | null current_trick = ((head $ sortCards player_cards), memory ++ heartsPlayed) 
    -- | null current_trick = trace ("ID: " ++ player_id ++ "\t\tPrev State:" ++ show previous_state ++ "\t\t\tMem:" ++ show memory ++ "\t\t\tTrick: " ++ show current_trick) ((head $ sortCards player_cards), memory ++ heartsPlayed) 
        -- If I'm the first to play --> play the lowest non-point card

    | not $ null $ cardsOfSuit current_trick_suit player_cards = ((head $ cardsOfSuit current_trick_suit player_cards), heartsPlayed)
        -- If I have a card of the leading suit --> play the lowest card of the leading suit

    | (null $ cardsOfSuit current_trick_suit player_cards) && elem 'H' (show memory) = (playPointCard player_cards, heartsPlayed)
        -- If I don't have a card of the leading suit && point card has been played before --> play the highest point card
            
    | otherwise = ((head $ sortCards player_cards), heartsPlayed)
        -- Else, just play the first card in hand
        where 
            trick_cards = cardsInTrick current_trick
            current_trick_suit = getSuit (last trick_cards)
            memory = getMemory previous_state
            heartsPlayed = if elem 'H' (show $ getPreviousCards previous_state) then "H" else ""
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
playCard :: PlayFunc
playCard player_id player_cards current_trick previous_state = newPlayer player_id player_cards current_trick previous_state

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined