import Control.Applicative (liftA2)
import Data.List (nub)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import System.Environment

type Label = String

type Text = String

type Document = (Label, Text)

data Token = UNK | Token String deriving (Show, Eq, Ord)

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
  let numDocs = length docs
   in Map.map (/ fromIntegral numDocs) $ countLabels docs

-- Concatenate all documents of the same label
concatDocuments :: [Document] -> Map.Map Label Text
concatDocuments = foldr insertDoc Map.empty
  where
    insertDoc (label, content) = Map.insertWith (\a b -> unwords [a, b]) label content

-- Naive tokenization for now
tokenize = map Token . words

countTokens :: (Num n) => [Token] -> Map.Map Token n
countTokens = foldr (\token -> Map.insertWith (+) token 1) Map.empty

tokenFrequency :: (Fractional f) => Int -> Text -> Map.Map Token f
tokenFrequency vocabSize text =
  let tokens = UNK : tokenize text
      numTokens = length tokens
      relFreqSmooth count = (count + 1) / fromIntegral (numTokens + vocabSize)
   in Map.map relFreqSmooth $ countTokens tokens

countVocab :: [Document] -> Int
countVocab = length . nub . concatMap (tokenize . snd)

trainAposteriori :: [Document] -> Aposteriori
trainAposteriori docs =
  let vocabSize = countVocab docs
   in Map.map (tokenFrequency vocabSize) $ concatDocuments docs

train :: [Document] -> Classifier
train = Classifier <$> trainApriori <*> trainAposteriori

lookupTokenFreq :: Aposteriori -> Label -> Token -> Double
lookupTokenFreq aPosteriori label token =
  let tokenFrequencies = (aPosteriori Map.! label)
      unkFreq = (tokenFrequencies Map.! UNK)
   in fromMaybe unkFreq $ Map.lookup token tokenFrequencies

-- Calculate the probability for a text to be of each label
score :: Classifier -> String -> Map.Map Label Double
score (Classifier aPriori aPosteriori) text =
  let tokens = tokenize text
      aPostProb label = sum $ map (log . lookupTokenFreq aPosteriori label) tokens
      probability label labelProb = log labelProb + aPostProb label
   in Map.mapWithKey probability aPriori

geqBy :: (Ord b) => (a -> b) -> a -> a -> a
geqBy f x y = if f x >= f y then x else y

classify :: Classifier -> String -> (Label, Double)
classify classifier text =
  let scores = score classifier text
   in Map.foldrWithKey (\key val acc -> geqBy snd (key, val) acc) ("(No labels defined)", log 0) scores

linesToDocs :: [String] -> [Document]
linesToDocs (x : y : ys) = (x, y) : linesToDocs ys
linesToDocs _ = []

trainFromInput :: [String] -> Classifier
trainFromInput = train . linesToDocs

main = do
  (file : input) <- getArgs
  trainData <- lines <$> readFile file
  let classifier = trainFromInput trainData
  -- putStrLn $ fst $ classify classifier $ unwords input
  print $ classify classifier $ unwords input
