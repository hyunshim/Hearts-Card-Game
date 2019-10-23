-- | Write a report describing your design and strategy here.
module Player (
    playCard,
    makeBid,
    naive
)
where

import Hearts.Types
import Cards

-- Create a custom record syntax to store all data from previous states
data Memory = Memory {
    player_count :: Int,
    cards_played :: [Card]
} deriving (Show, Read, Eq)


-- Update the memory and return as a string
updateMem :: Memory -> [Card] -> String
updateMem memory c_played = show $ memory { cards_played = sortCards $ cards_played memory ++ c_played }


-- Returns only the cards from the trick
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
cardsNotOfSuit :: Suit -> [Card] -> [Card]
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


-- Finds if there is a card in my hand that will guarantee a loss even if other players have not played yet
-- Plays the hand that has less lower hands playable than max cards playable in the leftover trick by counting cards that are lower than each of the cards in my hand
guaranteeLoss :: [Card] -> [Card] -> [Card] -> Card
guaranteeLoss [] _ _ = (Card Club Two)
guaranteeLoss (x:xs) hearts_not_played trick_cards
    | (length $ filter (x>) hearts_not_played) <= 2 - (length trick_cards) = x
        -- If there are more at most 2-(length trick_cards) cards that are smaller than the current card being tested ==> Play the card
    | otherwise = guaranteeLoss xs hearts_not_played trick_cards


--------------------
-- Main functions --
--------------------


-- Main goal: Plays an unbeatable Heart card (by counting) if possible
-- Finds a card of suit Hearts that can not lose the current trick even if other players have not played yet (explained in the function guaranteeLoss)
playHeart :: [Card] -> [Card] -> [Card] -> Card
playHeart player_cards hearts_played trick_cards
    | return_card /= (Card Club Two) = return_card
        -- If card chosen is of the specified suit ==> Play chosen suit card

    | length trick_cards == 0 = firstToPlay player_cards
        -- If I'm the first to play ==> Play the first card in hand

    | length (cardsOfSuit Heart player_cards) > 0 = last player_cards_hearts
        -- If I'm not the first to play ==> Play the highest card of the suit

    | (Card Spade Queen) `elem` player_cards = (Card Spade Queen)
        -- If I don't have a card with the leading suit && SQ in hand ==> Play SQ

    | otherwise = firstToPlay player_cards
        -- Else ==> Play the lowest card in my hand
        where
            sorted_player_cards = sortCards player_cards
            return_card = if length player_cards_hearts == 0 then (Card Club Two) else guaranteeLoss (reverse player_cards_hearts) hearts_not_played trick_cards
            player_cards_hearts = cardsOfSuit Heart sorted_player_cards
            hearts_not_played = filter (\x -> not (x `elem` (hearts_played ++ player_cards_hearts))) (Card <$> [Heart] <*> [Two ..])


-- Main goal: Plays the highest card that is lower than the highest card of that suit in the trick
highestOfLowest :: [(Card, PlayerId)] -> Suit -> [Card] -> Int -> Card
highestOfLowest current_trick current_trick_suit player_cards number_of_players
    | null valid_cards || (length current_trick == (number_of_players - 1) && not point_cards) = 
        if (length player_cards_suit > 1 && ((last player_cards_suit) == (Card Spade Queen))) 
            then last $ init $ player_cards_suit 
            else last player_cards_suit
        -- If I don't have a card lower than the highest || if I am last to play and there are no point cards
            -- If the highest card of that suit is SQ --> play the 2nd highest card
            -- Else --> Play the highest card

    | otherwise = last $ valid_cards
        -- If I have a card lower than the highest --> play the highest card that is lower than the highest card of that suit in the trick

        where
            player_cards_suit = sortCards $ cardsOfSuit current_trick_suit player_cards
            highest_trick_suit = maximum $ cardsOfSuit current_trick_suit (cardsInTrick current_trick)
            valid_cards = filter (highest_trick_suit>) player_cards_suit
            point_cards = (Card Spade Queen) `elem` (cardsInTrick current_trick) || (elem 'H' $ show $ cardsInTrick current_trick)


-- Main goal: When I have no cards of the leading suit, plays SQ if possible
noLeadingSuit :: [Card] -> [Card] -> [Card] -> Card
noLeadingSuit player_cards hearts_played all_cards_played
    | length all_cards_played /= 0 && (Card Spade Queen) `elem` player_cards = (Card Spade Queen)
        -- If it is not the first trick && SQ in hand ==> Play SQ

    | length hearts_played > 0 = last sorted_player_cards
        -- If hearts has been broken ==> Play the highest heart

    | length non_point_suit_cards > 0 = (last $ non_point_suit_cards)
        -- If Club or Diamond cards in hand ==> Play the highest of those cards

    | otherwise = last sorted_player_cards
        -- Else ==> Play the highest hand possible

        where 
            sorted_player_cards = sortCards player_cards
            non_point_suit_cards = sortCards $ cardsNotOfSuit Heart $ cardsNotOfSuit Spade player_cards


-- Main goal: Plays the lowest non-point suit if possible
firstToPlay :: [Card] -> Card
firstToPlay player_cards
    | length non_point_suit_cards > 0 = head non_point_suit_cards
        -- If I have cards of suits Club or Diamond ==> Play the lowest of those cards

    | head sorted_player_cards == (Card Spade Queen) && length player_spades > 1 = head $ tail sorted_player_cards
        -- If the lowest Spade card is SQ && I have more than 1 Spade card ==> Play the lowest of the other Spade cards

    | otherwise = head sorted_player_cards
        -- Else ==> Play the lowest card

        where 
            sorted_player_cards = sortCards player_cards
            player_spades = sortCards $ cardsOfSuit Spade player_cards
            non_point_suit_cards = sortCards $ cardsNotOfSuit Heart $ cardsNotOfSuit Spade player_cards


-- Steps taken to play a good hand (Also written within in-line comments)
-- 1) If player has C2, play C2
-- 2) Try playing an unbeatable Heart card if Hearts has been broken
-- 3) Play the lowest non-point card if first to play
-- 4) Play SQ or highest point card if I have no cards of the leading suit
-- 5) Play the highest card lower than the highest card in the current trick if i have a card of the leading suit
playCard :: PlayFunc
playCard _ player_cards current_trick previous_state
    | elem (Card Club Two) player_cards = ((Card Club Two), new_memory) 
        -- If C2 in hand ==> Play C2

    | (null current_trick || current_trick_suit == Heart) && (length hearts_played /= 0) = (playHeart player_cards hearts_played trick_cards, new_memory)
        -- (If I'm the first to play || current suit is Heart) && heart has been played before ==> Select card using playHeart

    | null current_trick = (firstToPlay player_cards, new_memory)
        -- If I'm the first to play and heart has not been played ==> Select card using firstToPlay

    | null $ cardsOfSuit current_trick_suit player_cards = (noLeadingSuit player_cards hearts_played all_cards_played, new_memory)
        -- If I don't have a card of the leading suit ==> Select card using noLeadingSuit

    | not $ null $ cardsOfSuit current_trick_suit player_cards = ((highestOfLowest current_trick current_trick_suit player_cards number_of_players), new_memory)
        -- If I have a card of the leading suit ==> Select card using highestOfLowest

    | otherwise = ((head $ sortCards player_cards), new_memory)
        -- Else ==> Play the first card in hand

        where 
            all_cards_played = cards_played (read new_memory :: Memory)
            number_of_players = player_count (read new_memory :: Memory)

            hearts_played = sortCards $ (cardsOfSuit Heart trick_cards) ++ (cardsOfSuit Heart all_cards_played)
            -- Gets the number of hearts played so far including the current trick

            trick_cards = cardsInTrick current_trick
            current_trick_suit = getSuit $ last trick_cards

            num_players = if (length player_cards == 13 && previous_state == Nothing) then "4" else "2"
            previous_memory = getMemory previous_state
            new_memory = if previous_memory == "" 
                then "Memory {player_count = " ++ num_players ++ ", cards_played = []}" 
                else updateMem (read previous_memory :: Memory) (getPreviousCards previous_state)
                -- Sets the initial memory if previous_state memory is empty; otherwise updates the memory
            

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined

naive :: PlayFunc
naive player_id player_cards current_trick previous_state
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