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


--import Debug.Trace

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

-- Gets all cards of a given Suit
cardsOfSuit :: Suit -> [Card] ->  [Card]
cardsOfSuit suit cards = filter ((suit ==) . getSuit) cards

-- obtained from FIT2102 lecture slides and changed into point free form
sortCards :: [Card] -> [Card]
sortCards [] = []
sortCards (pivot:cards) = left cards ++ [pivot] ++ right cards
    where
        left = part (<pivot)
        right = part (>=pivot)
        part = (sortCards.) . filter

naivePlayer :: PlayFunc
naivePlayer player_id player_cards current_trick previous_state
    | elem (Card Club Two) player_cards = ((Card Club Two), memory ++ player_id)
        -- If player has Two of Clubs, they must play it

    | not (null current_trick) && not (null (cardsOfSuit current_trick_suit player_cards)) = ((head $ cardsOfSuit current_trick_suit player_cards), memory)
        -- If player has a card with the leading suit, play the first card of that leading suit

    -- | not (null current_trick) && null (cardsOfSuit current_trick_suit player_cards) = ((head player_cards), memory)
    --     -- If player does not have a card with the leading suit, play the first card in the hand                                                    %%% Must change so it doesn't play a point card
    | otherwise = ((head $ sortCards player_cards), memory)
        where 
            trick_cards = cardsInTrick current_trick
                -- gets just the cards in the current trick

            current_trick_suit = getSuit (last trick_cards)
                -- gets the suit of the current trick, player must follow this suit and can only break the suit if they have no cards of this suit.
            
            memory = getMemory previous_state

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
playCard player_id player_cards current_trick previous_state = naivePlayer player_id player_cards current_trick previous_state

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined