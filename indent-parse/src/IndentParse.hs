{- |
    "IndentParse" exports a generic recursive data structure 'ParsedStructure' 
    definition and methods to read and write it from a string, using a basic "off-side" 
    indentation style as shown in "test.txt" in the "examples" directory of 
    this project on github
-}
module IndentParse (
ParsedStructure(..),
-- * reading and writing
stringToParsedStructure,
parsedStructureToString
) where

removeCommonIndent :: [String] -> [String]
removeCommonIndent strs =
    let commonIndent = minimum $ map (length . takeWhile (== ' ')) strs
    in map (drop commonIndent) strs

groupByIndentBlocks :: [String] -> [String]
groupByIndentBlocks dirtystrs
    | null dirtystrs =
        []
    | otherwise = 
        let indentVal = length . takeWhile (== ' ')
            strs = removeCommonIndent dirtystrs
        in
            if (length strs >= 2) && (length . takeWhile (== ' ') $ strs !! 1) == 2 then
                let 
                    firstPhrase =  
                        (head strs) : 
                            (takeWhile (\curline -> (indentVal curline) >= 2) $ 
                                tail strs)
                in 
                    (init $ unlines firstPhrase) :
                        (groupByIndentBlocks $ drop (length firstPhrase) strs)
            else
                (head strs) : (groupByIndentBlocks $ tail strs)
                
type FolderName = String
                
-- | ParsedStructure is a generic recursive data structure, with two contructors
data ParsedStructure =
    ParsedString String -- ^ 'ParsedString' is a single line
    |ParsedFolder FolderName [ParsedStructure] -- ^ 'ParsedFolder' is a name and a list of contents
        deriving (Show, Eq)
    
stripSpacesString :: String -> String
stripSpacesString str = 
    let dropWhileEqSpace = dropWhile (`elem` " \n")
    in
        reverse . dropWhileEqSpace . reverse . dropWhileEqSpace $ str
    
stripSpacesStructure :: ParsedStructure -> ParsedStructure
stripSpacesStructure (ParsedString str) = ParsedString $ stripSpacesString str
stripSpacesStructure (ParsedFolder name structures) =
    ParsedFolder (stripSpacesString name) $ map stripSpacesStructure structures
    
-- | 'stringToParsedStructure' takes a multiline string and reads it as a 'ParsedStructue'
stringToParsedStructure :: String -> ParsedStructure
stringToParsedStructure dirtystr = 
    stripSpacesStructure $ 
        let str = unlines . filter (not . null) . lines $ dirtystr
        in
            if (length . lines $ str) == 1 then
                ParsedString str
            else
                ParsedFolder (head . lines $ str) $     
                    map stringToParsedStructure . groupByIndentBlocks $
                        tail . lines $ str
             
-- | parsedStructureToString satisfies: stringToParsedStructure . parsedStructureToString == id
parsedStructureToString :: ParsedStructure -> String
parsedStructureToString structure = 
    let
        indentBy lvl linestr =
            reverse . take (length linestr + lvl) . (++ repeat ' ') . reverse $ linestr
    in
        unlines . filter (not . null) . lines $ 
            case structure of
                ParsedString str -> str
                ParsedFolder name structures ->
                    let
                        showLines = 
                            init . unlines . map (indentBy 2) . lines . parsedStructureToString
                        structureStrs = 
                            map showLines structures
                    in
                        init . unlines $ name : structureStrs