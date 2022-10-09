module Stack (Stack (..),Stack.empty,Stack.singleton,pushl,push,pop,Stack.null,Stack.asList) where
import Prelude hiding (null)
import qualified Data.Foldable as Fold

-- https://stackoverflow.com/a/13971415/15205216
newtype Stack a = Stack [a]

instance Show a => Show (Stack a) where
  show (Stack a) = "Stack" ++ show a

empty :: Stack a
empty = Stack []

singleton :: a -> Stack a
singleton a = Stack [a]

pushl :: Foldable f => f a -> Stack a -> Stack a
pushl vl (Stack vs) = Stack (Fold.foldr (:) vs vl)

push :: a -> Stack a -> Stack a
push v (Stack vs) = Stack (v:vs)

pop :: Stack a -> (a, Stack a)
pop (Stack []) = error "Tried to pop from empty Stack"
pop (Stack (a:as)) = (a, Stack as)
                 
null :: Stack a -> Bool
null (Stack []) = True
null (Stack _) = False

asList :: Stack a -> [a]
asList (Stack as) = as

