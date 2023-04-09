# Naive Bayes classifier

A very _naive_ approach at text classification

## Requirements
 
 * ghc 9.2.5

## Set up

Build with ghc:
```
ghc --make ./bayes.hs
```

## Usage

Basic usage is
```
./bayes PATH QUERY
```
Where `PATH` is a path to a file with training data (see below for format), and `QUERY` is the text that should be classified.

### Training data

The training data file must only consist of lines of the following format:
```
<label> <text>
```

Example
```
ham Lol your always so convincing.
ham Oops, I'll let you know when my roommate's done
spam Thanks for your subscription to Ringtone UK your mobile will be charged £5/month Please confirm by replying YES or NO.
spam Please call our customer service representative on 0800 169 6031 between 10am-9pm as you have WON a guaranteed £1000 cash or £5000 prize!
...
```

## Limitations

Currently, the classifier is trained each time when running the program. For the future, support for saving and loading classifiers from disk is planned.
