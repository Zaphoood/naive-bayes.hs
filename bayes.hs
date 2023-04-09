import Data.Map qualified as Map
import GHC (concatDocs)

type Label = String

type Token = String

type Document = (Label, String)

type Apriori = Map.Map Label Double

type APosteriori = Map.Map Label (Map.Map Token Double)

data Classifier = Classifier
  { getAPriori :: Apriori,
    getAPosteriori :: APosteriori
  }
  deriving (Show)

emptyClassifier :: Classifier
emptyClassifier = Classifier Map.empty Map.empty

countLabels :: (Num a) => [Document] -> Map.Map Label a
countLabels = foldr (\(label, _) -> Map.insertWith (+) label 1) Map.empty

trainAPriori :: [Document] -> Apriori
trainAPriori docs =
  let numberOfDocs = length docs
   in Map.map (/ fromIntegral numberOfDocs) $ countLabels docs

-- Create map of lables to all documents of each label concatenated
concatDocuments :: [Document] -> Map.Map Label String
concatDocuments = foldr insertDoc Map.empty
  where
    insertDoc (label, content) = Map.insertWith (\a b -> unwords [a, b]) label content

-- Naive tokenization for now
tokenize = words

countTokens :: (Num a) => [Token] -> Map.Map Token a
countTokens = foldr (\token -> Map.insertWith (+) token 1) Map.empty

tokenFrequency :: (Fractional a) => String -> Map.Map Token a
tokenFrequency text =
  let tokens = tokenize text
      numberOfTokens = length tokens
   in Map.map (/ fromIntegral numberOfTokens) $ countTokens tokens

trainAPosteriori :: [Document] -> APosteriori
trainAPosteriori docs = Map.map tokenFrequency $ concatDocuments docs

train :: [Document] -> Classifier
train = Classifier <$> trainAPriori <*> trainAPosteriori
