-- | Simple but usefull functions, those match no specific module.
module Youtan.Utils where

-- | Applies a function while the condition are met.
while :: ( a -> a -> Bool ) -> ( a -> a ) -> a -> a
while con f v = let step x y = if x `con` y then step ( f x ) x else x
                in step ( f v ) v
