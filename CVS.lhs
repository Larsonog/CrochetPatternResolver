> {-# LANGUAGE GADTs #-}
> import Parsing2
  
CVS (Crochet Validity Scrutinizer)
==================================

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
>   Increase    :: Stitch -> Int -> Part
>   Decrease    :: Stitch -> Int -> Part 
>   Remaining   :: Int -> Part
> --Maybe don't need flipchain or flip because it is guaranteed at the end of the rows. 
>   FlipChain   :: Part -- flip chain 
>   Flip        :: Part -- flip the piece
>   PullThrough :: Part 
> --add stitches here.
>   deriving (Show)
> 

> --Maybe take a page from the way that IMP defined parts of ADTs to be more general.

> -- Change the way that dec is defined to Num(Tog(stitch)) and same for inc.

> --Another comprehensive ADT or editing Part to have stitches in it. is needed for parts and stitches together so that row can be defined as a list of the parts.

> --change row so that it is just a list of parts instead of individual cases.

> data Row where -- environment 
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
> -- important variables to keep track of 
> width = 0   -- comes from the first row chain amount
> counter = 0 -- need to count how many stitches in row for 
> numStitches = 0 
> -- similarity to arith interpreter, so need to create an environment. take in the width and keep track of it through the environment
> -- Parsers 
> lexer :: TokenParser u
> lexer = makeTokenParser $
>   emptyDef
>   { reservedNames   = ["row:" ],     
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
> checkSpace (Space _) = True -- if there is a space this BAD  
> checkSpace _         = False-- no space at the beginning is GOOD 
> 
> checkTreble :: Stitch -> Stitch -> Bool 
> checkTreble (TrebleCrochet _) (SlipStitch    _)    = True  
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
> interpRow (Flat s p1 p2) = undefined
> interpRow (TwoStitch s1 s2 p1 p2) = undefined 
>  

> -- STOLEN FROM MODULE 11
> {- run :: String -> IO ()
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
> -} 
 