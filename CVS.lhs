> {-# LANGUAGE GADTs #-}
> import Parsing2
  
CVS (Crochet Validity Scrutinizer)
==================================


> data Stitch where 
>   SlipStitch    :: Integer -> Stitch -- slip stitch
>   SingleCrochet :: Integer -> Stitch -- single crochet
>   DoubleCrochet :: Integer -> Stitch -- double crochet 
>   TrebleCrochet :: Integer -> Stitch -- treble corchet 
>   Space         :: Integer -> Stitch -- space
>   Chain         :: Integer -> Stitch -- chain 
>   deriving (Show)
> 
> data Part where 
>   S         :: Stitch -> Part
>   Increase    :: Integer -> Stitch -> Part --CHANGED DEFINITION TO INC NUM(STITCH) -> OP X Y
>   Decrease    :: Integer -> Stitch -> Part --wiggliness on the definition
>   Remaining   :: Integer -> Part
>   FlipChain   :: Part -- flip chain 
>   Flip        :: Part -- flip the piece
>   PullThrough :: Part 
> --add stitches here.
>   deriving (Show)
> 

> --Maybe take a page from the way that IMP defined parts of ADTs to be more general.

> -- Change the way that dec is defined to Num(Tog(stitch)) and same for inc.

> --Another comprehensive ADT or editing Part to have stitches in it. is needed for parts and stitches together so that row can be defined as a list of the parts.

> type Row = [Part]
> data PatternError where 
>   ZeroWidth   :: PatternError
>   SpaceError  :: String -> PatternError
>   DecError    :: String -> PatternError 
>   IncError    :: String -> String -> PatternError -- old width, new width, error 
>   FlipEarly   :: PatternError 
>   BegSpace    :: PatternError 
>   NoTurnChain :: PatternError 
>   NoPull      :: PatternError  
>   deriving (Show)
> 
> showPatErr :: PatternError -> String 
> showPatErr ZeroWidth      = "The Width is Zero"
> showPatErr (SpaceError x) = "There are too many spaces in this row, there are : " ++ x
> showPatErr (DecError x)   = "Too many stitches have been combined, you tried to combine: " ++ x
> showPatErr (IncError x y) = "The new width is : " ++ y ++ "which is double or more than the old width, which is: " ++ x
> showPatErr FlipEarly      = "Flipped before end of row"
> showPatErr BegSpace       = "Can't start a row with a space"
> showPatErr NoTurnChain    = "There is no turning chain"
> showPatErr NoPull         = "There was no pull through at the end"
>
> -- important variables to keep track of 
> -- similarity to arith interpreter, so need to create an environment. take in the width and keep track of it through the environment
> -- Parsers 
> lexer :: TokenParser u
> lexer = makeTokenParser $
>   emptyDef
>   { reservedNames   = ["row:" ],     
>     reservedOpNames = ["ss","sc", "dc", "tc", "sp", "ch", "repeat", "inc", "tog", "remaining", "fc", "fl", "," ]}  
> 
> integer :: Parser Integer
> integer = getInteger lexer
>
> --Change this to work on Row
> -- parseProg :: Parser Prog 
> -- parseProg = parseStmt `sepBy` (reservedOp ";") YORGEY QUESTION
>
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
> 
> parens :: Parser a -> Parser a
> parens = getParens lexer
> 
> parseRow :: Parser Row
> parseRow = parsePart `sepBy` reservedOp ","
>
> parseStitch:: Parser Stitch
> parseStitch =
>       SlipStitch <$> (integer <* reservedOp "ss")
>   <|> SingleCrochet <$> (integer <* reservedOp "dc")
>   <|> DoubleCrochet <$> (integer <* reservedOp "dc")
>   <|> TrebleCrochet <$> (integer <* reservedOp "tc")
>   <|> Chain <$> (integer <* reservedOp "ch")
>   <|> Space <$> (integer <* reservedOp "sp")
>   <|> parens parseStitch
> 
> parsePart :: Parser Part
> parsePart =
>       S <$> parseStitch
>   <|> Increase <$> integer <*> parseStitch <* reservedOp "inc" 
>   <|> Decrease <$> integer <*> parseStitch <* reservedOp "tog"


> -- does this pull from our lexer?

> reserved, reservedOp :: String -> Parser ()
> reserved   = getReserved lexer
> reservedOp = getReservedOp lexer
> 
> checkSpace :: Stitch -> Bool 
> checkSpace (Space _) = True -- if there is a space this BAD  
> checkSpace _         = False-- no space at the beginning is GOOD 
> 
> checkTreble :: Stitch -> Stitch -> Bool 
> checkTreble (TrebleCrochet _) (SlipStitch    _)  = True  
> checkTreble (TrebleCrochet _) (SingleCrochet _)  = True
> checkTreble (SlipStitch    _) (TrebleCrochet _)  = True
> checkTreble (SingleCrochet _) (TrebleCrochet _)  = True
> checkTreble _ _ = False 
> 
> checkFlip :: Part -> Bool
> checkFlip Flip = True 
> checkFlip _ = False
> 
> checkFC :: Part -> Bool 
> checkFC (FlipChain) = True 
> checkFC _ = False 
> 
> checkPullThrough :: Part -> Bool 
> checkPullThrough (PullThrough) = True 
> checkPullThrough _ = False 
> 
> interpRow :: Row -> Either PatternError Bool  
> interpRow _ = undefined
>  

> -- STOLEN FROM MODULE 11
> {- run :: String -> IO ()
> run fileName = do
>   s <- readFile fileName
>   case parse impParser s of
>     Left err -> print err
>     Right p  -> j76
>       case checkProg M.empty p of
>         Left tyErr -> putStrLn (showTyError tyErr)
>         Right _    -> do
>           inp <- getContents
>           let es = interpProg p (initWorld inp)
>           putStr $ formatWorld es
>
> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>     []     -> putStrLn "Please provide a file name."
>     (fn:_) -> run fn
> -} 
 
