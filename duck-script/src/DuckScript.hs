module DuckScript (
ContextItem(..),
Context,
DuckScript,
runDuckScript,
stringToDuckScript,
) where

import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe

import Control.Applicative
import Control.Monad

import System.Random

import IndentParse
import DuckScript.SentenceID

type VarTag = String
type VarString = String

type LogName = String
type LogList = [String]

data ContextItem = 
    ContextVar VarTag VarString
    |ContextLog LogName LogList
    deriving (Eq, Show)
    
type Context = [ContextItem]

findContextVar :: Context -> VarTag -> Maybe VarString
findContextVar context tag =
    case filter isJust $ map itemToMaybe context of
       (x:_) -> x
       [] -> Nothing
    where
        itemToMaybe item =
            case item of
                ContextVar atag astring ->
                    if atag == tag then
                        Just $ astring
                    else
                        Nothing
                _ -> Nothing
                
contextVarExists :: Context -> VarTag -> Bool
contextVarExists context tag = isJust $ findContextVar context tag
                
findContextLog :: Context -> LogName -> Maybe LogList
findContextLog context name =
    case filter isJust $ map itemToMaybe context of
       (x:_) -> x
       [] -> Nothing
    where
        itemToMaybe item =
            case item of
               ContextLog aname alog ->
                    if aname == name then
                        Just $ alog
                    else
                        Nothing
               _ -> Nothing
               
contextLogExists :: Context -> VarTag -> Bool
contextLogExists context tag = isJust $ findContextLog context tag
    
data DuckPredicate =
    TruePred
    |VarExistsPred DuckScript
    |LogExistsPred DuckScript
    |ParsesPred DuckScript SentenceID
    |EqualsPred DuckScript DuckScript
    |NotPred [DuckPredicate]
    |AndPred [DuckPredicate]
    |OrPred [DuckPredicate]
    deriving (Show, Eq)
    
onelinerToDuckPredicate :: String -> DuckPredicate
onelinerToDuckPredicate str = 
    case str of
        "True" -> TruePred
        _ -> error $ "DuckPredicate: unknown string " ++ str
    
parsedToDuckPredicate :: ParsedStructure -> DuckPredicate
parsedToDuckPredicate parsed =
    case parsed of
        ParsedFolder name structures ->
            case name of
                "VarExists" ->
                    case structures of
                        [varname] ->
                            VarExistsPred $ parsedToDuckScript varname
                        _ -> error "DuckPredicate: folder VarExists expects one structure"
                "LogExists" ->
                    case structures of
                        [logname] ->
                            LogExistsPred $ parsedToDuckScript logname
                        _ -> error "DuckPredicate: folder LogExists expects one structure"
                "Parse" ->
                    case structures of
                        [sentence, sentenceid] ->
                            ParsesPred (parsedToDuckScript sentence) (parsedToSentenceID sentenceid)
                        _ -> error "DuckPredicate: folder Parse expects two structures"
                "Equals" ->
                    case structures of
                        [strA, strB] ->
                            EqualsPred (parsedToDuckScript strA) $
                                (parsedToDuckScript strB)
                        _ -> error "DuckPredicate: folder Equals expects one structure"
                "NotPred" ->
                    NotPred $ map parsedToDuckPredicate structures
                "AndPred" ->
                    AndPred $ map parsedToDuckPredicate structures
                "OrPred" ->
                    OrPred $ map parsedToDuckPredicate structures
                _ -> error $ "DuckPredicate: unknown folder name " ++ name
        ParsedString str -> onelinerToDuckPredicate str
                
type IfBlock = (DuckPredicate, DuckScript)
                
parsedToIfBlock :: ParsedStructure -> IfBlock
parsedToIfBlock parsed =
    case parsed of
        ParsedFolder name structures ->
            case name of
                "Block" ->
                    case structures of
                        [predicate, result] ->
                            (,) (parsedToDuckPredicate predicate) $
                                (parsedToDuckScript result)
                        _ -> error "CaseAlternitive: folder Block expects two structures"
                _ -> error $ "CaseAlternitive: unknown folder name " ++ name
        _ -> error "CaseAlternitive: unexpected string at top level"
                
functionPredicateToLiteral :: Context -> DuckPredicate -> IO Bool
functionPredicateToLiteral context predicate = 
    case predicate of
        TruePred -> return $ True
        VarExistsPred dscript ->
            (contextVarExists context) <$> (runDuckScript context dscript)
        LogExistsPred dscript ->
            (contextLogExists context) <$> (runDuckScript context dscript)
        ParsesPred dscript sentenceID ->
            (flip matchesSentenceID $ sentenceID) <$> (runDuckScript context dscript)
        EqualsPred dscriptA dscriptB ->
            (==) <$> (runDuckScript context dscriptA) <*> 
                (runDuckScript context dscriptB)
        NotPred predicates->
            (not . and) <$> (sequence . map (functionPredicateToLiteral context) $ predicates)
        AndPred predicates ->
            and <$> (sequence . map (functionPredicateToLiteral context) $ predicates)
        OrPred predicates ->
            or <$> (sequence . map (functionPredicateToLiteral context) $ predicates)
            
data ContextModifier =
    AddContextVar DuckScript DuckScript
    |AddContextLog DuckScript DuckScript
    deriving (Show, Eq)
    
modifyContext :: ContextModifier -> Context -> IO Context
modifyContext modifier context =
    case modifier of
        AddContextVar tag var -> do
            literaltag <- runDuckScript context tag
            literalvar <- runDuckScript context var
            return $ (ContextVar literaltag literalvar) : context
        AddContextLog name var -> do
            literalname <- runDuckScript context name
            literalvar <- runDuckScript context var
            case findContextLog context literalname of
                Just loglist -> 
                     let
                        oldlog = (ContextLog literalname loglist)
                    in
                        return $ (ContextLog literalname (literalvar : loglist)) : delete oldlog context
                Nothing -> undefined
            
data DuckScript =
    -- basic constructors
    LiteralScript String 
    |RandomScript [DuckScript] 
    |ConcatScript DuckScript DuckScript
    |ConcatListScript [DuckScript]
    |SequenceScript [DuckScript]
    -- basic functions, getting info from context
    |GetVarScript DuckScript 
    |GetLogIndexScript DuckScript DuckScript
    |GetLogLengthScript DuckScript
    -- language constructs
    |IfScript [IfBlock]
    -- changing the scope
    |LetScript [ContextModifier] DuckScript
    -- reading and writing context to disk
    |LoadContextFile FilePath DuckScript
    |SaveContextFile FilePath
    deriving (Show, Eq)
    
taggedVarsToModifier :: TaggedVars -> [ContextModifier]
taggedVarsToModifier taggedvars =
    let toD = LiteralScript
    in
        map (\(name, var) -> AddContextVar (toD name) (toD var)) taggedvars
    
runDuckScript :: Context -> DuckScript -> IO String
runDuckScript context funcstr= 
    fmap (init . unlines . filter (not . null) . lines) $ 
    case funcstr of
        LiteralScript str -> return str
        RandomScript scripts -> do
            literals <- sequence . map (runDuckScript context) $ scripts
            (literals !!) <$> randomRIO (0, length literals - 1)
        ConcatScript scriptA scriptB -> do
            literalA <- runDuckScript context scriptA
            literalB <- runDuckScript context scriptB
            return $ literalA ++ literalB
        ConcatListScript scripts -> do
            literals <- sequence $ map (runDuckScript context) scripts
            return $ concat literals
        SequenceScript scripts -> do
            literals <- sequence $ map (runDuckScript context) scripts
            return . init $ unlines literals
        GetVarScript vartag -> do
            literaltag <- runDuckScript context vartag
            case findContextVar context literaltag of
                Just varstr -> return $ varstr
                Nothing -> return $ "<" ++ literaltag ++ ">"
        GetLogIndexScript funcname funcind -> do
            literalname <- runDuckScript context funcname
            literalind <- runDuckScript context funcind
            case findContextLog context literalname of
                Just vars -> return $ vars !! (read literalind) 
                Nothing -> return $ "<" ++ literalname ++ " !! " ++ literalind ++ ">"
        GetLogLengthScript funcname -> do
            literalname <- runDuckScript context funcname
            case findContextLog context literalname of
                Just vars -> return . show $ length vars
                Nothing -> return $ "<" ++ "length of " ++ literalname ++ ">"
        IfScript alternitives ->
            let
                predicateMatches apred =
                    functionPredicateToLiteral context apred
            in do
                literalBools <- sequence $ map (predicateMatches . fst) alternitives
                case (alternitives !!) <$> elemIndex True literalBools of
                    Just ((ParsesPred sentenceScript sentenceid), result) -> do
                        literalSentence <- runDuckScript context sentenceScript
                        case getTaggedVarsWithID literalSentence sentenceid of
                            Just taggedvars ->
                                let varsmodifier = taggedVarsToModifier taggedvars
                                in
                                    runDuckScript context (LetScript varsmodifier result)
                            Nothing -> 
                                error $ "wow. what universe are you in? you..." ++
                                    " you defied the laws of logic as we know it"
                    Just (_, result) -> runDuckScript context result
                    Nothing -> error "<if statement incomplete>"
        LetScript modifiers script -> 
            let contextTransformers = map modifyContext modifiers
            in do
                newcontext <- foldl1 (>=>) contextTransformers $ context
                runDuckScript newcontext script
        LoadContextFile filepath script -> do
            newcontext <- fmap (++ context) (fileToContext filepath)
            runDuckScript newcontext script
        SaveContextFile filepath -> do
            writeContextFile filepath context
            return ""
            
                
parsedToContextVar :: ParsedStructure -> ContextItem
parsedToContextVar parsed =
    case parsed of
        ParsedFolder headstr structures ->
            case headstr of
                "Var" ->
                    case structures of
                        [varname, varstr] ->
                            ContextVar (parsedToString varname) (parsedToString varstr)
                        _ -> error "ContextItem: folder Var expects two structures"
                "Log" ->
                    case structures of
                        (logname:items) ->
                            ContextLog (parsedToString logname) $ map parsedToString items
                        _ -> error "ContextItem: folder Log expects more then two structures"
                _ ->
                    error $ "ContextItem: unknown folder name " ++ headstr
        ParsedString _ ->
            error "ContextItem: unknown string"

parsedToContext :: ParsedStructure -> Context
parsedToContext parsed =
    case parsed of
        ParsedFolder headstr structures ->
            case headstr of
                "Context" ->
                    map parsedToContextVar structures
                _ -> error $ "Context: unknown folder name " ++ headstr
        ParsedString str ->
            case str of
                "Empty" ->
                    []
                _ ->
                    error $ "Context: unknown string " ++ str
                
fileToContext :: FilePath -> IO Context
fileToContext path = (parsedToContext . stringToParsedStructure) <$> readFile path

contextItemToParsed :: ContextItem -> ParsedStructure
contextItemToParsed contextitem =
    case contextitem of
        ContextVar varname varstr ->
            ParsedFolder "Var" [(ParsedString varname), (ParsedString varstr)]
        ContextLog logname contents ->
            ParsedFolder "Log" ((ParsedString logname) : map ParsedString contents)

contextToParsed :: Context -> ParsedStructure
contextToParsed context = 
    ParsedFolder "Context" $ map contextItemToParsed context
    
writeContextFile :: FilePath -> Context -> IO ()
writeContextFile path context = undefined contextToParsed
    
takeWhile2 :: (a -> Bool) -> [[a]] -> [[a]]
takeWhile2 _ []  = []
takeWhile2 f xss =
    if all f $ head xss then
        (head xss) : takeWhile2 f (tail xss)
    else
        [takeWhile f (head xss)]
        
getHeadInquotes :: String -> Maybe String
getHeadInquotes str =
    let
        firstLine = head . lines $ str
    in
        case head firstLine of
            '"' ->
                let
                    blocks = splitOn "\\\"" $ drop 1 firstLine          
                in
                    Just . ('"':) . (++"\"") .  concat . intersperse "\\\"" $ 
                        takeWhile2 (not . (== '\"')) blocks
            _ -> Nothing
            
inquotesToLiteral :: String -> String
inquotesToLiteral str =
    let
        cleaned = tail . init $ str
        blocks = splitOn "\\\"" cleaned
    in
        concat $ intersperse "\"" blocks
            
onelinerToDuckScript :: String -> DuckScript
onelinerToDuckScript "" = LiteralScript ""
onelinerToDuckScript str =
    let firstLine = head . lines $ str
    in 
        case getHeadInquotes firstLine of
            Nothing ->
                case head firstLine of
                    '$' -> 
                        let
                            headStr =
                                (takeWhile isAlpha) . drop 1 $ firstLine
                            tailStr = drop (length headStr + 1) firstLine
                            headScript = GetVarScript . LiteralScript $ headStr
                        in
                            ConcatScript headScript (onelinerToDuckScript tailStr)
                    _  -> 
                        let 
                            headScript = 
                                takeWhile (not . (`elem` ['$', '"'])) str
                            tailStr = drop (length headScript) firstLine
                        in 
                            if null tailStr then 
                                LiteralScript $ headScript 
                            else 
                                ConcatScript (LiteralScript headScript) $
                                    (onelinerToDuckScript tailStr) 
            Just inquotes ->
                let
                    headScript = LiteralScript . inquotesToLiteral $ inquotes
                    tailStr = drop (length inquotes) firstLine
                in 
                    if null tailStr then
                        headScript
                    else
                        ConcatScript headScript (onelinerToDuckScript tailStr)
              
onelinerToContextModifier :: String -> Maybe ContextModifier
onelinerToContextModifier str =
    case head . words $ str of
        "AddVar" ->
            case tail . words $ str of
                [varname, varstr] ->
                    Just $ AddContextVar (LiteralScript varname) (LiteralScript varstr)
                _ -> Nothing
        _ -> Nothing
                
parsedToContextModifier :: ParsedStructure -> ContextModifier
parsedToContextModifier parsed =
    case parsed of
        ParsedFolder headstr structures ->
            case headstr of
                "Var" ->
                    case structures of
                        [varname, varstr] ->
                            AddContextVar (parsedToDuckScript varname) (parsedToDuckScript varstr)
                        _ -> error "ContextModifier: folder Var expects two structures"
                _ -> error $ "ContextModifier: unknown folder name " ++ headstr
        ParsedString str -> 
            case onelinerToContextModifier str of
                Just modifier -> modifier
                Nothing -> error $ "unknown string " ++ str
        
parsedToModifierList :: ParsedStructure -> [ContextModifier]
parsedToModifierList parsed =
    case parsed of
        ParsedFolder headstr structures ->
            case headstr of
                "Modify" ->
                    map parsedToContextModifier structures
                _ -> error $ "[ContextModifier]: unknown folder name " ++ headstr
        ParsedString _ -> error "[ContextModifier]: unknown string"
        
parsedToString :: ParsedStructure -> String
parsedToString parsed =
    case parsed of
        ParsedFolder _ _ ->
            error "String: unknown folder"
        ParsedString str ->
            str
               
parsedToDuckScript :: ParsedStructure -> DuckScript
parsedToDuckScript parsed = 
    case parsed of
        ParsedFolder headstr structures ->
            case headstr of
                "Random" ->
                    RandomScript . map parsedToDuckScript $ structures
                "Concat" ->
                    ConcatListScript $ map parsedToDuckScript structures
                "Sequence" ->
                    SequenceScript $ map parsedToDuckScript structures
                "GetVar" ->
                    case structures of
                        [vartag] ->
                            GetVarScript $ parsedToDuckScript vartag
                        _ -> error "DuckScript: folder GetVar expects one structure"
                "LogAtIndex" ->
                    case structures of
                        [logStruct, indexStruct] ->
                            GetLogIndexScript (parsedToDuckScript logStruct) $
                                (parsedToDuckScript indexStruct)
                        _ -> error "DuckScript: folder LogAtIndex expects two structures"
                "If" ->
                    IfScript $ map parsedToIfBlock structures                        
                "Let" ->
                    case structures of  
                        [modifiers, script] ->
                            LetScript (parsedToModifierList modifiers) (parsedToDuckScript script)
                        _ -> error $ "DuckScript: folder Let expects two structures"
                "LoadFile" ->
                    case structures of
                        [filepath, script] ->
                            LoadContextFile (parsedToString filepath) (parsedToDuckScript script)
                        _ -> error "DuckScript: folder ReadFile expects two structures"
                _ -> error $ "DuckScript: unknown folder name " ++ headstr
                
        ParsedString str -> onelinerToDuckScript str
        
stringToDuckScript :: String -> DuckScript
stringToDuckScript =
    parsedToDuckScript . stringToParsedStructure