import Control.Applicative (liftA2)
import Data.Char (isAlpha, isSpace, toLower)
import Data.List (maximumBy, nub)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

type Label = String

type Text = String

type Document = (Label, Text)

data Token = UNK | Token String deriving (Show, Eq, Ord)

type Apriori = Map.Map Label Double

type Conditionals = Map.Map Label (Map.Map Token Double)

data Classifier = Classifier
  { getApriori :: Apriori,
    getConditionals :: Conditionals
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
tokenize :: String -> [Token]
tokenize = map Token . filter (not . null) . map (map toLower . filter isAlpha) . words

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

trainConditionals :: [Document] -> Conditionals
trainConditionals docs =
  let vocabSize = countVocab docs
   in Map.map (tokenFrequency vocabSize) $ concatDocuments docs

train :: [Document] -> Classifier
train = Classifier <$> trainApriori <*> trainConditionals

lookupTokenFreq :: Conditionals -> Label -> Token -> Double
lookupTokenFreq conditionals label token =
  let tokenFrequencies = (conditionals Map.! label)
      unkFreq = (tokenFrequencies Map.! UNK)
   in fromMaybe unkFreq $ Map.lookup token tokenFrequencies

-- Calculate the probability for a text to be of each label
score :: Classifier -> String -> Map.Map Label Double
score (Classifier apriori conditionals) text =
  let tokens = tokenize text
      condProbability label = sum $ map (log . lookupTokenFreq conditionals label) tokens
      probability label labelProb = log labelProb + condProbability label
   in Map.mapWithKey probability apriori

geqBy :: (Ord b) => (a -> b) -> a -> a -> a
geqBy f x y = if f x >= f y then x else y

classify :: Classifier -> String -> (Label, Double)
classify classifier text =
  let scores = Map.toList $ score classifier text
      scoresExp = map (exp <$>) scores
      scoresSum = sum . map snd $ scoresExp
   in (/ scoresSum) <$> maximumBy (\a b -> snd a `compare` snd b) scoresExp

linesToDocs :: [String] -> [Document]
linesToDocs = map $ break isSpace

trainFromInput :: [String] -> Classifier
trainFromInput = train . linesToDocs

main = do
  (filename : input) <- getArgs
  trainData <- lines <$> readFile filename
  let classifier = trainFromInput trainData
  print $ classify classifier $ unwords input
