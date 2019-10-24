module Player (
    playCard,
    makeBid
)
where

import Hearts.Types
import Cards

{-
Introduction
    It has been pointed out that one of Haskell's advantages is that it is very similar to reading English.
    Hence, I have formatted this file so that it will be similar to reading a book.

    The main function will be at the top of the page and will call smaller functions which will give you a basic summary of what this file will do.
    The smaller functions will then be explained in the order given in the main function.
    The simple functions will be stated at the end of the file.

    All functions will have a brief introduction of its main goal and also step-by-step explanation of what it is doing in each line if necessary.

    All function and variable names should give you a rough idea on what it is and does.
        Functions will be named using camelCase.
        Variables will be named using snake_case.

    All simple functions have been made point-free to a degree that will still be understandable.

    A custom random deck generator has also been implemented to strengthen the deck generation and provide more accurate results.


Strategy
    This player prioritises bombarding other players with point cards such as the Queen of Spades or Heart cards.

    If a point card can be played, it will first attempt to play a point card that will 100% be taken by someone else.
        1) playHeart:       Plays a card that has less lower cards playable than 
                            max cards playable in the leftover trick by counting 
                            cards that are lower than each of the cards in my hand.

        2) noLeadingSuit:   Plays the highest value point card when I don't have
                            cards of the current trick suit.

        3) secondHighest:   Plays the highest card that is lower than the highest 
                            card of that suit in the trick.

    If a point card can't be played, it will get rid of non-point suits (Diamond and Club) to create a void suit.
    A void suit allows the player to play point cards using the noLeadingSuit function more frequently.
        1) firstToPlay:     Plays a non-point suit card to create a void suit.
                            It will also avoid playing the Queen of Spades.

        2) noLeadingSuit:   If a point card can't be played using this function,
                            it also acts as a double-up which tries to win the
                            trick if I am guaranteed that I won't win a point card.

    Memory is used to keep track of all the cards that have been played in previous tricks of the current round.
    This can be combined with the cards in the current trick to allow this player to count cards and play the best card possible.
-}



-- Create a custom record syntax to store all data from previous states
data Memory = Memory {
    player_count :: Int,
    cards_played :: [Card]
} deriving (Show, Read, Eq)



{-
playCard is the main function of our application.
Using the given parameters, it matches patterns and calls other functions to choose the best card for the current trick.
The patterns are matched in the order of highest priority to ensure that the best card that follows the rules is chosen.

A custom record syntax data was implemented over a pure string option so that it may be easier to update and read the required attributes.
This makes the addition of new attributes within the memory much easier compared to the string method.

The other functions that this functions call will be explained within the header of each of the respective functions.
-}
playCard :: PlayFunc
playCard _ player_cards current_trick previous_state
    | (Card Club Two) `elem` player_cards = ((Card Club Two), new_memory) 
        -- If C2 in hand ==> Play C2

    | (first_to_play || current_trick_suit == Heart) && (length hearts_played /= 0) = (playHeart player_cards hearts_played current_trick_cards number_of_players, new_memory)
        -- (If I'm the first to play || current suit is Heart) && heart has been played before ==> Select card using playHeart

    | first_to_play = (firstToPlay player_cards, new_memory)
        -- If I'm the first to play and heart has not been played ==> Select card using firstToPlay

    | no_leading_suit = (noLeadingSuit player_cards hearts_played all_cards_played, new_memory)
        -- If I don't have a card of the leading suit ==> Select card using noLeadingSuit
    
    | not no_leading_suit = ((secondHighest current_trick current_trick_suit player_cards number_of_players), new_memory)
        -- If I have a card of the leading suit ==> Select card using secondHighest

    | otherwise = ((head $ sortCards player_cards), new_memory)
        -- Else ==> Play the first card in hand

        where 
            first_to_play = null current_trick
            no_leading_suit = null $ cardsOfSuit current_trick_suit player_cards

            number_of_players = player_count (read new_memory :: Memory)

            all_cards_played = cards_played (read new_memory :: Memory)
            hearts_played = sortCards $ cardsOfSuit Heart (current_trick_cards ++ all_cards_played)
            -- Gets the number of hearts played so far including the current trick

            current_trick_cards = cardsInTrick current_trick
            current_trick_suit = getSuit $ last current_trick_cards

            previous_memory = getMemory previous_state
            new_memory = if previous_memory == "" 
                then "Memory {player_count = " ++ show (52 `div` (length player_cards)) ++ ", cards_played = []}" 
                else updateMem (read previous_memory :: Memory) (getPreviousCards previous_state)
                -- Sets the initial memory with the number of players if previous_state memory is empty; otherwise updates the memory



{-
playHeart is a function that attempts to play a Heart card that can't win the trick even if other players have not played yet.
Using the given parameters, it checks if I have a Heart card that can't win the trick by calling the function guaranteeLoss (logic will be explained in header of this function).
-}
playHeart :: [Card] -> [Card] -> [Card] -> Int -> Card
playHeart player_cards hearts_played current_trick_cards number_of_players
    | chosen_card /= (Card Club Two) = chosen_card
        -- If there is a card that can't win the trick ==> Play chosen card

    | length current_trick_cards == 0 = firstToPlay player_cards
        -- If I'm the first to play ==> Select card using firstToPlay

    | length (cardsOfSuit Heart player_cards) > 0 = last player_cards_hearts
        -- If I have cards of suit Heart ==> Play the highest Heart card (As I will win this trick anyway)

    | (Card Spade Queen) `elem` player_cards = (Card Spade Queen)
        -- If SQ in hand ==> Play SQ

    | otherwise = firstToPlay player_cards
        -- Else ==> Select card using firstToPlay
        -- It should be noted that this function call to firstToPlay will have a different impact to the call to firstToPlay on the second pattern guard in this function
        -- This is because of the additional restrictions filtered by the other pattern guards above this one

        where
            player_cards_hearts = cardsOfSuit Heart (sortCards player_cards)
            hearts_not_played = filter (\x -> not (x `elem` (hearts_played ++ player_cards_hearts))) (Card <$> [Heart] <*> [Two ..])
                -- Finds all the cards that have not been played yet or are not in my hand
            chosen_card = guaranteeLoss (reverse player_cards_hearts) hearts_not_played current_trick_cards number_of_players
                -- Calls guaranteeLoss to check if there is a card that can't win the trick



{-
firstToPlay is a function that plays a safe card that will not have too much impact on the round.
It aims to play a card of suits Club or Diamond so that I can get rid of them as soon as possible.
It also avoids playing SQ as the first play if SQ in hand so that I could use the SQ when I don't have a leading suit.
-}
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



{-
noLeadingSuit is a function that attempts to bombard other players with point cards with a priority given to SQ and then Heart cards in descending value.
If I can't play a Heart due to Hearts not being broken or I have no Heart cards left, it will get rid of a dangerous spade (Spade card higher than Queen).
-}
noLeadingSuit :: [Card] -> [Card] -> [Card] -> Card
noLeadingSuit player_cards hearts_played all_cards_played
    | length all_cards_played /= 0 && (Card Spade Queen) `elem` player_cards = (Card Spade Queen)
        -- If it is not the first trick && SQ in hand ==> Play SQ

    | length hearts_played > 0 = last sorted_player_cards
        -- If hearts has been broken ==> Play the highest heart
    
    | not $ null dangerous_spades = last dangerous_spades
        -- If I have dangerous spades (Spade cards higher than Queen) ==> Play highest of dangerous_spades

    | length non_point_suit_cards > 0 = (last $ non_point_suit_cards)
        -- If Club or Diamond cards in hand ==> Play the highest of those cards

    | otherwise = last sorted_player_cards
        -- Else ==> Play the highest hand possible

        where 
            dangerous_spades = filter ((Card Spade Queen)<) (cardsOfSuit Spade player_cards)
            sorted_player_cards = sortCards player_cards
            non_point_suit_cards = sortCards $ cardsNotOfSuit Heart $ cardsNotOfSuit Spade player_cards



{-
secondHighest is a function that attempts to play the highest card that is lower than the highest card of that suit in the trick
However, if I am last to play and there are no point cards in the current trick, it will play a winning card.
When playing the winning card, it will avoid playing SQ and cards of suit Heart.
-}
secondHighest :: [(Card, PlayerId)] -> Suit -> [Card] -> Int -> Card
secondHighest current_trick current_trick_suit player_cards number_of_players
    | null cards_LT_highest || (last_to_play && not point_cards) = 
        -- If I don't have cards lower than the highest || if I am last to play and there are no point cards    
        if (length player_cards_suit > 1 && ((last player_cards_suit) == (Card Spade Queen))) 
            -- If the highest card of that suit is SQ 
            then last $ init $ player_cards_suit 
                -- Play the 2nd highest card
            else last player_cards_suit
                -- Play the highest card
    
    | otherwise = last cards_LT_highest
        -- If I have a card lower than the highest --> play the highest card that is lower than the highest card of that suit in the trick

        where
            player_cards_suit = sortCards $ cardsOfSuit current_trick_suit player_cards
            highest_card = maximum $ cardsOfSuit current_trick_suit (cardsInTrick current_trick)
            cards_LT_highest = filter (highest_card>) player_cards_suit
                -- finds cards that are less than the highest of the current trick
            last_to_play = length current_trick == (number_of_players - 1)
            point_cards = (Card Spade Queen) `elem` (cardsInTrick current_trick) || (elem 'H' $ show $ cardsInTrick current_trick)



--------------------------
---- Simple functions ----
--------------------------


-- Finds if there is a card in my hand that will guarantee a loss even if other players have not played yet.
-- Plays a card that has less lower cards playable than max cards playable in the leftover trick by counting cards that are lower than each of the cards in my hand.
-- If it can't find a card that won't win the trick, it will return C2 as a default.
guaranteeLoss :: [Card] -> [Card] -> [Card] -> Int -> Card
guaranteeLoss [] _ _ _ = (Card Club Two)
guaranteeLoss (card:cards) hearts_not_played current_trick_cards number_of_players
    | will_lose && must_play_suit = card
        -- If it finds a card that won't win the trick ==> Play the card
    | otherwise = guaranteeLoss cards hearts_not_played current_trick_cards number_of_players
        -- Recursively call this function so that it goes through all the items
        where
            will_lose = (length $ filter (card>) hearts_not_played) <= (number_of_players-2) - (length current_trick_cards)
                -- Checks if there are less cards that are smaller than the current card being tested that have not been played yet
            must_play_suit = (length hearts_not_played >= ((number_of_players-1) - (length current_trick_cards)))
                -- Checks if there are enough Heart cards so that other players won't be able to play cards of different suits


-- Update the memory and return as a string
updateMem :: Memory -> [Card] -> String
updateMem memory c_played = show $ memory { cards_played = sortCards $ cards_played memory ++ c_played }


-- Returns only the cards from the trick
cardsInTrick :: [(Card, PlayerId)] -> [Card]
cardsInTrick = map fst


-- Gets the memory out of previous_state
getMemory :: Maybe ([(Card, PlayerId)], String) -> String
getMemory Nothing = ""
getMemory (Just a) = snd a


-- Gets the suit of the given Card
getSuit :: Card -> Suit
getSuit (Card suit _) = suit


-- Gets a list of cards in the previous trick
getPreviousCards :: Maybe ([(Card, PlayerId)], String) -> [Card]
getPreviousCards = cardsInTrick . getCards . (fst <$>)
    where
        getCards Nothing = []
        getCards (Just a) = a


-- Gets all cards of a given Suit
cardsOfSuit :: Suit -> [Card] ->  [Card]
cardsOfSuit suit = filter ((suit ==) . getSuit)


-- Gets all cards not of a given Suit
cardsNotOfSuit :: Suit -> [Card] -> [Card]
cardsNotOfSuit suit = filter ((suit /=) . getSuit)


-- Sorts cards with order provided in Cards.hs
-- obtained from FIT2102 lecture slides and changed into point free form
sortCards :: [Card] -> [Card]
sortCards [] = []
sortCards (pivot:cards) = left cards ++ [pivot] ++ right cards
    where
        left = part (<pivot)
        right = part (>=pivot)
        part = (sortCards.) . filter


-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined