-- Simple cards utility modules written in Haskell, all for fun
module Cards where
import System.Random

data Suit = Club | Diamond | Heart | Spade
	deriving (Eq, Ord, Show, Read, Enum)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
	deriving (Eq, Ord, Show, Read, Enum)

data Card = Card Suit Rank
	deriving (Show, Read)

instance Eq Card where
	Card sa ra == Card sb rb = (sa == sb) && (ra == rb)

instance Ord Card where
	Card sa ra < Card sb rb = sa < sb || (sa == sb && ra < rb)

type Deck = [Card]

makeDeck :: Deck
makeDeck = [ Card suit rank | suit <- [Club .. Spade], rank <- [Two, Three .. Ace] ]

shuffle :: (RandomGen r) => r -> Deck -> Deck
shuffle gen deck = shuffle' gen deck []
	where
		shuffle' _ [] shuffled = shuffled
		shuffle' g d  shuffled =
			let (k, g')   = randomR (0, length d - 1) g
			    (l, x:xs) = splitAt k d
			in shuffle' g' (l ++ xs) (x:shuffled)

