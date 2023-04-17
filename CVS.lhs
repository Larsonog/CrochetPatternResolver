
CVS (Crochet Validity Scrutinizer)
==================================


> {-# LANGUAGE GADTs #-}
> 
> data Stitch where 
>   Ss :: Stitch -- slip stitch
>   Sc :: Stitch -- single crochet
>   Dc :: Stitch -- double crochet 
>   Tc :: Stitch -- treble corchet 
>   Sp :: Stitch -- space
>   Ch :: Stitch -- chain 
>   deriving (Show)
> 
> data Part where 
>   Repeat    :: Part
>   Inc       :: Part
>   Dec       :: Part 
>   Remaining :: Part
>   Fc        :: Part -- flip chain 
>   Fl        :: Part -- flip the piece
>   deriving (Show)