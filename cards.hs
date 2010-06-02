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

shuffle :: (RandomGen r) => r -> [x] -> [x]
shuffle gen a = shuffle' gen a []
	where
		shuffle' _ [] shuffled = shuffled
		shuffle' g a shuffled =
			let (k, g')   = randomR (0, length a - 1) g
			    (h, x:xs) = splitAt k a
			in shuffle' g' (h ++ xs) (x:shuffled)

