> import Parsing2
  
CVS (Crochet Validity Scrutinizer)
==================================


> {-# LANGUAGE GADTs #-}
> 
> data Stitch where 
>   SlipStitch    :: Int -> Stitch -- slip stitch
>   SingleCrochet :: Int -> Stitch -- single crochet
>   DoubleCrochet :: Int -> Stitch -- double crochet 
>   TrebleCrochet :: Int -> Stitch -- treble corchet 
>   Space         :: Int -> Stitch -- space
>   Chain         :: Int -> Stitch -- chain 
>   deriving (Show)
> 
> data Part where 
>   Repeat    :: Part
>   Increase  :: Part
>   Decrease  :: Part 
>   Remaining :: Part
>   FlipChain :: Part -- flip chain 
>   Flip      :: Part -- flip the piece
>   deriving (Show)
> 
> data Row where 
>   Flat      :: Stitch -> Part -> Part -> Row -- no increases, decrease, or anything crazy
>   Repeater  :: Part -> Stitch -> Part ->  Part-> Row 
>   TwoStitch :: Stitch -> Stitch -> Part -> Part -> Row -- always have t 
>   TwoType   :: Part -> Stitch -> Part -> Stitch -> Part -> Part -> Row 
> 
> data PatternError where 
>   ZeroWidth   :: PatternError
>   SpaceError  :: Int -> PatternError
>   DecError    :: Int -> PatternError 
>   IncError    :: Int -> Int -> PatternError -- old width, new width, error 
>   FlipEarly   :: PatternError 
>   BegSpace    :: PatternError 
>   NoTurnChain :: PatternError 
>   NoPull      :: PatternError  
>   deriving (Show)
> 
> showPatErr :: PatternError -> String 
> showPatErr (ZeroWidth)    = "The Width is Zero"
> showPatErr (SpaceError x) = "There are too many spaces in this row, there are : " ++ x
> showPatErr (DecError x)   = "Too many stitches have been combined, you tried to combine: " ++ x
> showPatErr (IncError x y) = "The new width is : " ++ y ++ "which is double or more than the old width, which is: " ++ x
> showPatErr (FlipEarly)    = "Flipped before end of row"
> showPatErr (BegSpace)     = "Can't start a row with a space"
> showPatErr (NoTurnChain)  = "There is no turning chain"
> showPatErr (NoPull)       = "There was no pull through at the end"
>
> -- Parsers 
> lexer :: TokenParser u
> lexer = makeTokenParser $
>   emptyDef
>   { reservedNames    = ["row:" ],     
>     reservedOpNames = ["ss","sc", "dc", "tc", "sp", "ch", "repeat", "inc", "dec", "remaining", "fc", "fl" ]}  
> 
> integer :: Parser Integer
> integer = getInteger lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
> 
> parens :: Parser a -> Parser a
> parens = getParens lexer
> -- does this pull from our lexer 
> reserved, reservedOp :: String -> Parser ()
> reserved   = getReserved lexer
> reservedOp = getReservedOp lexer
> 
> checkSpace :: Stitch -> Bool 
> checkSpace (Space) = True -- if there is a space this BAD  
> checkSpace _ = False      -- no space at the beginning is GOOD 
> 
> checkTreble :: Stitch -> Stitch -> Bool 
> checkTreble (TrebleCrochet SlipStitch) = True  
> checkTreble (TrebleCrochet SingleCrochet) = True
> checkTreble (SlipStitch TrebleCrochet ) = True
> checkTreble (SingleCrochet TrebleCrochet ) = True
> checkTreble _ _ = False 
> 
> interpRow :: Row -> Either PatternError Bool  
> interpRow (Flat s p1 p2) = undefined
> interpRow (TwoStitch s1 s2 p1 p2) = 
>     if s1 || s2 == tc then (if s2 || s1 == ss || sc then False else True) else True
>     -- if the first stitch or the second stitch is equal to a treble crochet then check to see if either the first stitch or the second stitch is a single crochet or a slip stitch 
>    
>
> -- STOLEN FROM MODULE 11
> run :: String -> IO ()
> run fileName = do
>   s <- readFile fileName
>   case parse impParser s of
>     Left err -> print err
>     Right p  ->
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

> 