> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -Wall #-}
> import Parsing2

  
CVS (Crochet Validity Scrutinizer)
==================================
All of these are common stitches that a beginner would find and are the building blocks
of all patterns and more complex stitches. 

> data Stitch where 
>   SlipStitch    :: Integer -> Stitch -- slip stitch
>   SingleCrochet :: Integer -> Stitch -- single crochet
>   DoubleCrochet :: Integer -> Stitch -- double crochet 
>   TrebleCrochet :: Integer -> Stitch -- treble corchet 
>   Space         :: Integer -> Stitch -- space
>   Chain         :: Integer -> Stitch -- chain 
>   deriving (Show, Eq)
> 

Parts are different because they can involve or build upon a stitch or is used to finish off or
flip the work. 

> data Part where 
>   S           :: Stitch -> Part
>   Increase    :: Integer -> Stitch -> Part 
>   Decrease    :: Integer -> Stitch -> Part
>   FlipChain   :: Part 
>   Flip        :: Part 
>   PullThrough :: Part 
>   deriving (Show, Eq)
> 
> type Row = [Part] -- Row is a list of parts that a pattern can be built off of 
> type Pattern = [Row] -- Pattern is a list of rows 

The pattern errors we chose are the ones that beginners are most likely to encounter. While some of these 
are technically allowed and used in higher level projects, its bad technique for beginners and they are most 
most likely to mess up their piece. 

> data PatternError where 
>   ZeroWidth   :: PatternError 
>   SpaceError  :: PatternError
>   DecError    :: PatternError 
>   IncError    :: PatternError
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

The showBool function is just supposed to be a nice way to show the user whether or not the pattern is valid rather than just a boolean.

> showBool :: Bool -> String 
> showBool True = "Great! Your pattern is valid!!!!!!!!!!!"
> showBool False = "Oh no! Your pattern is invalid! Sorry!"

> -- Parsers 
> lexer :: TokenParser u
> lexer = makeTokenParser $
>   emptyDef
>   { reservedOpNames = ["ss","sc", "dc", "tc", "sp", "ch", "repeat", "inc", "tog", "remaining", "fc", "fl", ",", "pt"]}  

The names we chose in reservedOpNames for the stitches and parts are all the most common US terminology short hand for these pieces.

> integer :: Parser Integer
> integer = getInteger lexer
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
>   <|> FlipChain <$ reservedOp "fc"
>   <|> Flip <$ reservedOp "fl"
>   <|> PullThrough <$ reservedOp "pt"

> reserved, reservedOp :: String -> Parser ()
> reserved   = getReserved lexer
> reservedOp = getReservedOp lexer

All of the below functions are meant to check individual parts that can cause errors and fail the program if it finds them.

> checkTreble :: Part -> Part -> Bool 
> checkTreble (S (TrebleCrochet _)) (S (SlipStitch    _))  = True
> checkTreble (S (TrebleCrochet _)) (S (SingleCrochet _))  = True
> checkTreble (S (SlipStitch    _)) (S (TrebleCrochet _))  = True
> checkTreble (S (SingleCrochet _)) (S (TrebleCrochet _))  = True
> checkTreble _ _ = False
>
> 
> checkFlip :: Part -> Bool
> checkFlip Flip = True 
> checkFlip _ = False
> 
> checkSpace :: Part -> Bool
> checkSpace (S (Space x)) = x >= 5
> checkSpace _ = False
>
> parse2 :: Parser Part
> parse2 = whiteSpace *> parsePart <* eof

> checkFC :: Part -> Bool 
> checkFC FlipChain = True 
> checkFC _ = False 
>
> checkPullThrough :: Part -> Pattern -> Bool 
> checkPullThrough PullThrough [] = False
> checkPullThrough PullThrough pattern = last(last pattern) /= PullThrough
> checkPullThrough _ _ = True 
> 
> checkFlipChain :: Part -> [Part] -> Bool
> checkFlipChain FlipChain (x: parts) = if FlipChain == x then False else checkFlipChain FlipChain parts
> checkFlipChain _ _ = True
> 
> checkBegSpace :: Part -> [Part] -> Bool
> checkBegSpace (S(Space y ))(x: parts) = S (Space y) == x
> checkBegSpace _ _ = False 
>
> checkInc :: Part -> Bool
> checkInc (Increase y (SingleCrochet _)) = y>2
> checkInc (Increase y (TrebleCrochet _)) = y>2
> checkInc (Increase y (SlipStitch    _)) = y>2
> checkInc  _ = False
>
> checkDec :: Part -> Bool
> checkDec (Decrease y (SingleCrochet _)) = y>2
> checkDec (Decrease y (DoubleCrochet _)) = y>2
> checkDec (Decrease y (TrebleCrochet _)) = y>2
> checkDec (Decrease y (SlipStitch    _)) = y>2
> checkDec  _ = False
> 
> checkChain :: Part -> Bool 
> checkChain (S(Chain _)) = True
> checkChain _ =  False

All of the below functions are supposed to update or check the width for width related errors. 
 
> setUpOWid :: Part -> Integer
> setUpOWid (S(Chain x)) = x
> setUpOWid _ = 0
>
> setUpNWid :: Part -> Integer -> Integer
> setUpNWid (S(SingleCrochet x)) y = x + y
> setUpNWid (S(DoubleCrochet x)) y = x + y 
> setUpNWid (S(TrebleCrochet x)) y = x + y 
> setUpNWid (S(SlipStitch x)) y    = x + y 
> setUpNWid (S(Space x)) y         = x + y 
> setUpNWid (Increase x _) y       = x + y 
> setUpNWid (Decrease x _) y       = y - x
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

An ADT that is supposed to keep track of where and what the machine is doing at any given point and allows us to throw errors as needed.

> data Progress where
>   Working :: Integer -> Integer -> Pattern -> Progress 
>   Done :: Bool -> Progress
>   Error :: PatternError -> Progress


>
> step :: Progress -> Progress
> step (Working _ _[]) = Done True
> step (Done bool) = Done bool
> step (Working o n ((x:y:p):pattern))
>   | checkTreble x y = Error TrebleError   
>   | checkStitch x  && checkStitch y  = Working o (setUpNWid x o + setUpNWid y o) ((y:p):pattern) 
>   | checkWidth o n = Error WidthSize
> step (Working o n ((x:p):pattern) ) 
>   | checkSpace x = Error SpaceError                  
>   | checkInc x  = Error IncError                     
>   | checkDec x = Error DecError                      
>   | checkChain x = Working (setUpOWid x) (setUpOWid x) pattern
>   | checkStitch x = Working o (setUpNWid x o) pattern
>   | checkWidth o n = Error WidthSize
> step (Working o n (r:pattern))  -- o is old width, n is new width
>   | checkBegSpace (S(Space 1)) r = Error BegSpace
>   | checkFlipChain FlipChain r = Error NoTurnChain
>   | checkPullThrough PullThrough pattern = Error NoPull 
>   | checkWidth o n = Error WidthSize
> step (Working o n ([]:pattern)) = Working o n pattern


> step (Error e) = Error e
> step _ = Done True
>  

> steps :: Progress -> Progress
> steps (Working o n pattern) = step (Working o n pattern)
> steps (Done bool) = Done bool
> steps _ = Error ProgFail

> execute :: Integer-> Integer -> Pattern -> Progress
> execute o n pattern = 
>    case step(Working o n pattern) of 
>        Working _ _ [] -> Done True
>        Working o' n' pattern' -> execute o' n' pattern'
>        Done bool -> Done bool 
>        Error e -> Error e
> 
> run :: Integer -> Integer -> Pattern -> String 
> run o n pattern = 
>   case execute o n pattern of 
>     Done True -> showBool True 
>     Done False -> showBool False
>     Error e -> showPatErr e  
>     Working {}-> showBool False