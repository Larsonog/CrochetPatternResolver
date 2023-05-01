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
>   deriving (Show, Eq)
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
>   deriving (Show, Eq)
> 

> --Maybe take a page from the way that IMP defined parts of ADTs to be more general.

> -- Change the way that dec is defined to Num(Tog(stitch)) and same for inc.

> --Another comprehensive ADT or editing Part to have stitches in it. is needed for parts and stitches together so that row can be defined as a list of the parts.

> type Row = [Part]
> data PatternError where 
>   ZeroWidth   :: PatternError
>   SpaceError  :: PatternError
>   DecError    :: String -> PatternError 
>   IncError    :: String -> String -> PatternError -- old width, new width, error 
>   FlipEarly   :: PatternError 
>   BegSpace    :: PatternError 
>   NoTurnChain :: PatternError 
>   NoPull      :: PatternError  
>   TrebleError :: PatternError
>   ProgFail    :: PatternError
>   deriving (Show)
> 
> showPatErr :: PatternError -> String 
> showPatErr ZeroWidth      = "The Width is Zero"
> showPatErr (SpaceError)   = "There are too many spaces in this row, there are over 5 consecutively."
> showPatErr (DecError x)   = "Too many stitches have been combined, you tried to combine: " ++ x
> showPatErr (IncError x y) = "The new width is : " ++ y ++ "which is double or more than the old width, which is: " ++ x
> showPatErr FlipEarly      = "Flipped before end of row"
> showPatErr BegSpace       = "Can't start a row with a space"
> showPatErr NoTurnChain    = "There is no turning chain"
> showPatErr NoPull         = "There was no pull through at the end"
> showPatErr ProgFail       = "Something went wrong within the program. Dunno about your pattern! Sorry!"
> showPatErr _              = "Uh I don't know what to do with this error, haven't accounted for it."
>
> -- important variables to keep track of 
> -- similarity to arith interpreter, so need to create an environment. take in the width and keep track of it through the environment
> -- Parsers 
> lexer :: TokenParser u
> lexer = makeTokenParser $
>   emptyDef
>   { reservedNames   = ["row:" ],     
>     reservedOpNames = ["ss","sc", "dc", "tc", "sp", "ch", "repeat", "inc", "tog", "remaining", "fc", "fl", ",", "pt"]}  
> 
> integer :: Parser Integer
> integer = getInteger lexer
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
>   <|> Remaining <$> integer <* reservedOp "remaining"
>   <|> FlipChain <$ reservedOp "fc"
>   <|> Flip <$ reservedOp "fl"
>   <|> PullThrough <$ reservedOp "pt"


> -- does this pull from our lexer?

> reserved, reservedOp :: String -> Parser ()
> reserved   = getReserved lexer
> reservedOp = getReservedOp lexer
> 

> checkTreble :: Part -> Part -> Bool 
> checkTreble (S (TrebleCrochet _)) (S (SlipStitch _))  = True  
> checkTreble (S (TrebleCrochet _)) (S (SingleCrochet _))  = True
> checkTreble (S (SlipStitch _)) (S (TrebleCrochet _))  = True
> checkTreble (S (SingleCrochet _)) (S (TrebleCrochet _))  = True
> checkTreble _ _ = False 
> 
> checkFlip :: Part -> Bool
> checkFlip Flip = True 
> checkFlip _ = False
> 
> checkSpace :: Part-> Bool
> checkSpace (S (Space 5))= True
> checkSpace _ = False
>
> parse2 :: Parser Part
> parse2 = whiteSpace *> parsePart <* eof

> checkFC :: Part -> Bool 
> checkFC FlipChain = True 
> checkFC _ = False 
> 
> checkPullThrough :: Part -> [Part] -> Bool 
> checkPullThrough PullThrough parts = if (last parts) == PullThrough then False else True
> checkPullThrough _ _ = True 
> 
> checkFlipChain :: Part -> [Part] -> Bool
> checkFlipChain FlipChain (x: parts) = if FlipChain == x then False else checkFlipChain FlipChain parts
> checkFlipChain _ _ = True
> 
> checkBegSpace :: Part -> [Part] -> Bool
> checkBegSpace (S(Space y ))(x: parts) = (S (Space y)) == x
> checkBegSpace _ _ = False 
>
> data Progress where
>   Working :: [Part] -> Progress
>   Done :: Bool -> Progress
>   Error :: PatternError -> Progress
> -- add the needed items for the environment to the Progress data type.
> -- Error ProgFail just accounts for the fact that the pattern may be vaild but something went wrong that isn't the users fault.

> step :: Progress -> Progress
> step (Working []) = Done True
> step (Done bool) = Done bool
> step (Working row) 
>   | checkBegSpace (S(Space 1)) row = Error BegSpace  -- works
>   | checkFlipChain FlipChain row = Error NoTurnChain -- works  CAN'T CHECK FLIPCHAIN AND PULL THROUGH AT SAME TIME
>   | checkPullThrough PullThrough row = Error NoPull  -- works 
> step (Working (x: row)) 
>   | checkSpace x = Error SpaceError
> step (Working (x:y: row))
>   | checkTreble x y = Error TrebleError 

> -- turn the row cases into a guard case instead. Fixes infinite loop.
> -- Need to change the S Space of CheckBegSpace because it doesn't catch all cases currently.
> step (Error e) = Error e
> step _ = Error ProgFail
>  

> steps :: Progress -> Progress
> steps (Working parts) = step (Working parts)
> steps (Done bool) = Done bool
> steps _ = Error ProgFail

> execute :: [Part] -> Progress
> execute parts = 
>    case step(Working parts) of 
>        Working [] -> Done True
>        Working parts' -> execute parts'
>        Done bool -> Done bool 
>        Error e -> Error e
> 
> run :: [Part] -> Either String Bool 
> run parts = 
>   case execute parts of 
>     Done True -> Right True 
>     Done False -> Right False -- we probably don't need it 
>     Error e -> Left (showPatErr e)   -- the cause of our problems
>     Working _ -> Right (False)