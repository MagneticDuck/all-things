module DuckScript.SentenceID (
SentenceID,
parseSentenceWithID,
parsedToSentenceID,
stringToSentenceID,
TaggedVars,
matchesSentenceID,
getTaggedVarsWithID) where

import Data.List
import Data.Char
import Data.Maybe

import Control.Monad
import Text.Regex

import IndentParse
 
type SentenceStr = String
type PhraseStr = String 
type PhraseSequence = [PhraseStr] 

type PhrasePredicate = (PhraseStr -> Bool) 
type SentencePredicate = [PhrasePredicate]

getPossibleHeads :: PhrasePredicate -> String -> [PhraseStr]
getPossibleHeads predicate str =
    filter predicate $ inits str

breakIntoPhrases :: SentencePredicate -> String -> [PhraseSequence]
breakIntoPhrases [] _               = []
breakIntoPhrases predicateSequence str =
    let dropSequence phraseseq = drop (sum . map length $ phraseseq)
        sequenceSuccessors predicate phraseSequence =
            let possiblePhrases = getPossibleHeads predicate $ 
                    dropSequence phraseSequence str
            in map (: phraseSequence) possiblePhrases
        sequenceTransformers = map sequenceSuccessors predicateSequence
    in map (reverse . init) $ foldr1 (>=>) sequenceTransformers $ [""]
    
parseWholeSentence :: SentencePredicate -> String -> [PhraseSequence]
parseWholeSentence predicateSequence str =
   let isWholeSentence phraseSeq = (sum . map length $ phraseSeq) == length str 
   in filter isWholeSentence $ breakIntoPhrases predicateSequence str
   
data PhraseID = 
          AnythingPhrs       -- ^ 'AnythingPhrs' matches any string
        | CleanedPhrs String -- ^ 'CleanedPhrs' matches a string without non alpha-numeric or case
        | ExactPhrs String   -- ^ 'ExactPhrs' matches a string exactly
        | RegexPhrs String   -- ^ 'RegexPhrs' matches a regex pattern
        | DelimPhrs          -- ^ 'DelimPhrs' matches a series of non alpha-numberic characters
        | AlphaPhrs          -- ^ 'AlphaPhrs' matches a series of alpha-numeric characters
        | NullPhrs           -- ^ 'NullPhrs' matches a null string
        | AnyPhrs [PhraseID] -- ^ 'AnyPhrs' matches any of the phrases it contains
        | ConcatPhrs [PhraseID] -- ^ 'ConcatPhr' matches the sequence of phrases it contains
        | AndPhrs [PhraseID] -- ^ 'AndPhrs' matches a phrase that matches all of the phrases it contains
        | NotPhrs [PhraseID] -- ^ 'NotPhrs' matches anything but the phrase it contains
        deriving (Show, Eq)
        
data TaggedPhraseID = 
        TaggedPhraseID (Maybe String) PhraseID
        deriving (Show, Eq)
type SentenceID = [TaggedPhraseID]

cleanPhrase :: String -> String
cleanPhrase = unwords . (filter (not . null) . map (filter isAlphaNum . map toLower)) . words
   
matchesPhraseID :: PhraseID -> PhrasePredicate
matchesPhraseID phraseID str =
    case phraseID of
        AnythingPhrs    -> True
        CleanedPhrs phr -> cleanPhrase str == cleanPhrase phr
        ExactPhrs phr   -> str == phr
        RegexPhrs phr   -> isJust $ matchRegex (mkRegex phr) str
        DelimPhrs       -> (all (not . isAlphaNum) str) && (not . null $ str)
        AlphaPhrs       -> (all (isAlphaNum) str) && (not . null $ str)
        NullPhrs        -> null str
        AnyPhrs phrs   -> any (\phr -> matchesPhraseID phr str) phrs
        ConcatPhrs phrs-> 
            let predicates = map (\phr -> matchesPhraseID phr) phrs
            in (not . null) $ parseWholeSentence predicates str
        AndPhrs phrs -> and . map ($ str) $ map matchesPhraseID phrs
        NotPhrs phrs -> and . map ($ str) $ map matchesPhraseID phrs
        
parseSentenceWithID :: SentenceID -> SentenceStr -> [PhraseSequence]
parseSentenceWithID sentenceID str =
    let phraseIDs = map (\(TaggedPhraseID _ phraseID) -> phraseID) sentenceID
    in parseWholeSentence (map matchesPhraseID phraseIDs) str

parsedToPhraseID :: ParsedStructure -> PhraseID
parsedToPhraseID parsed = 
    case parsed of
        ParsedFolder headstr structures ->
            case headstr of
                "Any" -> 
                    AnyPhrs $ map parsedToPhraseID structures
                "Concat" ->
                    ConcatPhrs $ map parsedToPhraseID structures
                "And" ->
                    AndPhrs $ map parsedToPhraseID structures
                "Not" ->
                    NotPhrs $ map parsedToPhraseID structures
                _ -> error $ "PhraseID: unknown folder name: " ++ headstr
        ParsedString str ->
            case (head . words $ str) of
                "Anything" -> AnythingPhrs
                "Cleaned"  -> CleanedPhrs $ drop 8 str 
                "Exact"    -> ExactPhrs $ drop 6 str
                "Regex"    -> RegexPhrs $ drop 6 str
                "Delim"    -> DelimPhrs
                "Alpha"    -> AlphaPhrs
                "Null"     -> NullPhrs
                _          -> CleanedPhrs str
         
parsedToTaggedPhraseID :: ParsedStructure -> TaggedPhraseID
parsedToTaggedPhraseID parsed =
    case parsed of
        ParsedFolder headStr structures ->
            case headStr of
                "Tagged" ->
                    case head structures of
                        ParsedString tagname -> 
                            TaggedPhraseID (Just tagname) $ 
                                parsedToPhraseID . (head . tail) $ structures
                        _ -> error $ "TaggedPhraseID: unexpected folder as first structure"
                _ -> TaggedPhraseID Nothing $ parsedToPhraseID parsed
        ParsedString _ ->
            TaggedPhraseID Nothing $ parsedToPhraseID parsed

parsedToSentenceID :: ParsedStructure -> SentenceID
parsedToSentenceID parsed =
    case parsed of
        ParsedFolder headstr structures ->
            case headstr of
                "SentenceID" -> map parsedToTaggedPhraseID structures
                _ -> error $ "SentenceID: unknown folder name " ++ headstr
        _ -> error "SentenceID: unexpected string"
             
stringToSentenceID :: String -> SentenceID
stringToSentenceID str =
    parsedToSentenceID $ stringToParsedStructure str 
    
matchesSentenceID :: String -> SentenceID -> Bool
matchesSentenceID str sentenceID =
    isJust $ getTaggedVarsWithID str sentenceID
    
type TagString = String
type VarString = String
type TaggedVars = [(TagString, VarString)]

getTaggedVarsWithID :: String -> SentenceID -> Maybe TaggedVars
getTaggedVarsWithID str sentenceID = 
    let 
        varTags = map (\(TaggedPhraseID tag _) -> tag) sentenceID
        phraseSequence = 
            case parseSentenceWithID sentenceID str of
                [] -> []
                sequences -> head sequences
        tagVarToTuple tag var =
            case tag of
                Just tagstr -> (tagstr, var)
                Nothing     -> ("", var)
    in 
        case (zipWith tagVarToTuple) varTags phraseSequence of
            [] -> Nothing
            taggedvars -> Just (filter (\(a, _) -> not . null $ a) taggedvars)
