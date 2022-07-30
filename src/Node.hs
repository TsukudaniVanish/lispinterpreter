{-# LANGUAGE  OverloadedStrings #-}
module Node where 
    import qualified Data.Text as T

    data Node a = Leaf a
        |  Tree a (Node a) (Node a)
        deriving (Show, Read, Eq)

    instance Functor Node where
        fmap f (Leaf a) = Leaf (f a)
        fmap f (Tree x y z) = Tree (f x) (fmap f y) (fmap f z) 

    instance Applicative Node where
        pure x = Leaf x
        
        Leaf f <*> Leaf x = Leaf (f x)
        Leaf f <*> (Tree x y z) = Tree (f x) (Leaf f <*> y) (Leaf f <*> z)
        (Tree f ng nh) <*> Leaf x = Tree (f x) (ng <*> Leaf x) (nh <*> Leaf x)
        (Tree f ng nh) <*> (Tree x ny nz) = Tree (f x) (ng <*> ny) (nh <*> nz)

    
    instance Monad Node where
        Leaf x >>= f = f x
        Tree x ny nz >>= f = case Leaf x >>= f of
            Leaf xx -> Tree xx (ny >>= f) (nz >>= f)
            Tree xx nxy nxz -> Tree xx (Tree xx (ny >>= f) nxy) (Tree xx (nz >>= f) nxz)
    
    prettyPrint :: (Show a) => Node a -> Int -> T.Text
    prettyPrint (Leaf x) offset = T.concat (replicate offset " ") <> T.pack  (show x) <> "\n"
    prettyPrint (Tree x ny nz) offset = 
        let 
            prefix =  T.concat (replicate offset " ")
        in seq prefix $ 
            prefix <> "Tree " <> T.pack (show x) <> "\n"
            <> prefix <> "Left:\n" <> prettyPrint ny (offset + 4)
            <> prefix <> "Right:\n" <> prettyPrint nz (offset + 4)  
    