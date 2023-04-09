import Control.Applicative (liftA2)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid
import System.Environment

type Label = String

type Text = String

type Document = (Label, Text)

type Token = String

type Apriori = Map.Map Label Double

type Aposteriori = Map.Map Label (Map.Map Token Double)

data Classifier = Classifier
  { getApriori :: Apriori,
    getAposteriori :: Aposteriori
  }
  deriving (Show)

emptyClassifier :: Classifier
emptyClassifier = Classifier Map.empty Map.empty

countLabels :: (Num n) => [Document] -> Map.Map Label n
countLabels = foldr (\(label, _) -> Map.insertWith (+) label 1) Map.empty

trainApriori :: [Document] -> Apriori
trainApriori docs =
  let numberOfDocs = length docs
   in Map.map (/ fromIntegral numberOfDocs) $ countLabels docs

-- Concatenate all documents of the same label
concatDocuments :: [Document] -> Map.Map Label Text
concatDocuments = foldr insertDoc Map.empty
  where
    insertDoc (label, content) = Map.insertWith (\a b -> unwords [a, b]) label content

-- Naive tokenization for now
tokenize = words

countTokens :: (Num n) => [Token] -> Map.Map Token n
countTokens = foldr (\token -> Map.insertWith (+) token 1) Map.empty

tokenFrequency :: (Fractional f) => Text -> Map.Map Token f
tokenFrequency text =
  let tokens = tokenize text
      numberOfTokens = length tokens
   in Map.map (/ fromIntegral numberOfTokens) $ countTokens tokens

trainAposteriori :: [Document] -> Aposteriori
trainAposteriori docs = Map.map tokenFrequency $ concatDocuments docs

train :: [Document] -> Classifier
train = Classifier <$> trainApriori <*> trainAposteriori

lookupTokenFreq :: Aposteriori -> Label -> Token -> Double
lookupTokenFreq aPosteriori label token = fromMaybe 0 $ Map.lookup token (aPosteriori Map.! label)

-- Calculate the probability for a text to be of each label
score :: Classifier -> String -> Map.Map Label Double
score (Classifier aPriori aPosteriori) text =
  let tokens = tokenize text
      aPostProb label = product $ map (lookupTokenFreq aPosteriori label) tokens
      probability label aPrioProb = aPrioProb * aPostProb label
   in Map.mapWithKey probability aPriori

geqBy :: (Ord b) => (a -> b) -> a -> a -> a
geqBy f x y = if f x >= f y then x else y

classify :: Classifier -> String -> (Label, Double)
classify classifier text =
  let scores = score classifier text
   in Map.foldrWithKey (\key val acc -> geqBy snd (key, val) acc) ("(No labels defined)", 0) scores

linesToDocs :: [String] -> [Document]
linesToDocs (x : y : ys) = (x, y) : linesToDocs ys
linesToDocs _ = []

trainFromInput :: [String] -> Classifier
trainFromInput = train . linesToDocs

main = do
  (file : input) <- getArgs
  lines <- lines <$> readFile file
  let classifier = trainFromInput lines
  print $ classify classifier $ unwords input
