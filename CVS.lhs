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
> type Pattern = [Row]
> data PatternError where 
>   ZeroWidth   :: PatternError
>   SpaceError  :: PatternError
>   DecError    :: PatternError 
>   IncError    :: PatternError -- old width, new width, error 
>   FlipEarly   :: PatternError 
>   BegSpace    :: PatternError 
>   NoTurnChain :: PatternError 
>   NoPull      :: PatternError  
>   TrebleError :: PatternError
>   WidthSize   :: PatternError
>   ProgFail    :: PatternError
>   deriving (Show)
> 
> showPatErr :: PatternError -> String 
> showPatErr ZeroWidth      = "The Width is Zero"
> showPatErr SpaceError     = "There are too many spaces in this row, there are over 5 consecutively."
> showPatErr DecError       = "Too many stitches have been combined, you tried to combine more than two stitches together "
> showPatErr IncError       = "You tried to add too many stitches on top of this one" 
> showPatErr FlipEarly      = "Flipped before end of row"
> showPatErr BegSpace       = "Can't start a row with a space"
> showPatErr NoTurnChain    = "There is no turning chain"
> showPatErr NoPull         = "There was no pull through at the end"
> showPatErr TrebleError    = "You cannot have these two types next to each other."
> showPatErr WidthSize      = "The width is either too large or too small!"
> showPatErr ProgFail       = "Something went wrong within the program. Dunno about your pattern! Sorry!"
> showPatErr _              = "Uh I don't know what to do with this error, haven't accounted for it."
>
>
> showWidth :: Integer -> Integer -> String 
> showWidth o n = "the original width is: " ++ show(o) ++ " the new width is: " ++ show(n)
> 
> showBool :: Bool -> String 
> showBool True = "Great! Your pattern is valid!!!!!!!!!!!"
> showBool False = "Oh no! Your pattern is invalid! Sorry!"
> -- important variables to keep track of 
> -- similarity to arith interpreter, so need to create an environment. take in the width and keep track of it through the environment
> -- Parsers 
> lexer :: TokenParser u
> lexer = makeTokenParser $
>   emptyDef
>   { reservedOpNames = ["ss","sc", "dc", "tc", "sp", "ch", "repeat", "inc", "tog", "remaining", "fc", "fl", ",", "pt", ";"]}  
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
> parseRows :: Parser Pattern
> parseRows = parseRow `sepBy` reservedOp ";"
> -- because we have this we can now implement width and the pullthrough errors. 
>
>
> --Need to add a special symbol that is recognized as the change between rows, such as ; 
> -- SemiSep1 might be useful.
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
> checkSpace :: Part -> Bool
> checkSpace (S (Space x)) = if x <= 5 then True else False
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
> checkInc :: Part -> Bool
> checkInc (Increase y (SingleCrochet _)) = if y>2 then True else False
> checkInc (Increase y (DoubleCrochet _)) = if y>2 then True else False
> checkInc (Increase y (TrebleCrochet _)) = if y>2 then True else False
> checkInc (Increase y (SlipStitch    _)) = if y>2 then True else False
> checkInc  _ = False
>
> checkDec :: Part -> Bool
> checkDec (Decrease y (SingleCrochet _)) = if y>2 then True else False
> checkDec (Decrease y (DoubleCrochet _)) = if y>2 then True else False
> checkDec (Decrease y (TrebleCrochet _)) = if y>2 then True else False
> checkDec (Decrease y (SlipStitch    _)) = if y>2 then True else False
> checkDec  _ = False
> 
> checkChain :: Part -> Bool 
> checkChain (S(Chain x)) = True
> checkChain _ =  False
> 
> setUpOWid :: Part -> Integer
> setUpOWid (S(Chain x)) = x  
>
> setUpNWid :: Part -> Integer -> Integer
> setUpNWid (S(SingleCrochet x)) y = x + y
> setUpNWid (S(DoubleCrochet x)) y = x + y 
> setUpNWid (S(TrebleCrochet x)) y = x + y 
> setUpNWid (S(SlipStitch x)) y    = x + y 
> setUpNWid (S(Space x)) y         = x + y 
> setUpNWid (Increase x (_)) y     = x + y 
> setUpNWid (Decrease x (_)) y     = y - x
> setUpNWid _ y = y
> 
> checkWidth :: Integer -> Integer -> Bool
> checkWidth  o n
>   | n > 2 * o = True
>   | n < o `div` 2 = True 
> checkWidth _ _ = False
> 
> checkStitch :: Part -> Bool 
> checkStitch (Increase _ _) = True 
> checkStitch (Decrease _ _) = True
> checkStitch (S(SlipStitch _ )) = True
> checkStitch (S(SingleCrochet _ )) = True
> checkStitch (S(DoubleCrochet _ )) = True
> checkStitch (S(TrebleCrochet _ )) = True
> checkStitch _  = False
>

>
> data Progress where
>   Working :: Integer -> Integer -> [Part] -> Progress -- Add Integer -> Integer in the middle, Current Width -> Old Width. 
>   Done :: Bool -> Progress
>   Error :: PatternError -> Progress
> -- add the needed items for the environment to the Progress data type.
> -- Error ProgFail just accounts for the fact that the pattern may be vaild but something went wrong that isn't the users fault.
>

>
> step :: Progress -> Progress
> step (Working _ _[]) = Done True
> step (Done bool) = Done bool
> step (Working o n row)  -- o is old width, n is new width
>   | checkBegSpace (S(Space 1)) row = Error BegSpace  -- works
>   | checkFlipChain FlipChain row = Error NoTurnChain -- works  CAN'T CHECK FLIPCHAIN AND PULL THROUGH AT SAME TIME
>   | checkPullThrough PullThrough row = Error NoPull  -- works 
>   | checkWidth o n = Error WidthSize
> step (Working o n (x: row) ) 
>   | checkSpace x = Error SpaceError                  -- works 
>   | checkInc x  = Error IncError                     -- works 
>   | checkDec x = Error DecError                      -- works 
>   | checkChain x = Working (setUpOWid x) (setUpOWid x) row
>   | checkStitch x = Working o (setUpNWid x o) row
>   | checkWidth o n = Error WidthSize
> step (Working o n (x:y: row))
>   | checkTreble x y = Error TrebleError              -- works
>   | checkStitch x  && checkStitch y  = Working o (((setUpNWid x o) +(setUpNWid y o) )) row 
>   | checkWidth o n = Error WidthSize

> -- turn the row cases into a guard case instead. Fixes infinite loop.
> -- Need to change the S Space of CheckBegSpace because it doesn't catch all cases currently.
> step (Error e) = Error e
> step _ = Done True
>  

> steps :: Progress -> Progress
> steps (Working o n parts) = step (Working o n parts)
> steps (Done bool) = Done bool
> steps _ = Error ProgFail

> execute :: Integer-> Integer -> [Part] -> Progress
> execute o n parts = 
>    case step(Working o n parts) of 
>        Working o n [] -> Done True
>        Working o' n' parts' -> execute o' n' parts'
>        Done bool -> Done bool 
>        Error e -> Error e
> 
> run :: Integer -> Integer -> [Part] -> String 
> run o n parts = 
>   case execute o n parts of 
>     Done True -> showBool True 
>     Done False -> showBool False -- we probably don't need it 
>     Error e -> showPatErr e   -- the cause of our problems
>     Working o n _ -> showBool False