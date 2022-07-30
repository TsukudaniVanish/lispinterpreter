{-# LANGUAGE  OverloadedStrings #-}
module Node where 
    import qualified Data.Text as T

    data Node a = Leaf a
        |  Tree (Node a) (Node a)
        deriving (Show, Read, Eq)

    instance Functor Node where
        fmap f (Leaf a) = Leaf (f a)
        fmap f (Tree y z) = Tree (fmap f y) (fmap f z) 

    instance Applicative Node where
        pure x = Leaf x
        
        Leaf f <*> Leaf x = Leaf (f x)
        Leaf f <*> (Tree y z) = Tree  (Leaf f <*> y) (Leaf f <*> z)
        (Tree ng nh) <*> Leaf x = Tree  (ng <*> Leaf x) (nh <*> Leaf x)
        (Tree ng nh) <*> (Tree ny nz) = Tree  (ng <*> ny) (nh <*> nz)

    
    instance Monad Node where
        Leaf x >>= f = f x
        Tree ny nz >>= f = Tree (ny >>= f) (nz >>= f)
    
    prettyPrint :: (Show a) => Node a -> Int -> T.Text
    prettyPrint (Leaf x) offset = T.concat (replicate offset " ") <> T.pack  (show x) <> "\n"
    prettyPrint (Tree ny nz) offset = 
        let 
            prefix =  T.concat (replicate offset " ")
        in seq prefix $ 
            prefix <> "Tree " <> "\n"
            <> prefix <> "Left:\n" <> prettyPrint ny (offset + 4)
            <> prefix <> "Right:\n" <> prettyPrint nz (offset + 4)  
    