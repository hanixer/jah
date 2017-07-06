module Pretty where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

-- Binary Tree
data Tree a      = Leaf a | Node (Tree a) (Tree a) deriving (Show)

-- Many child tree -- is this a Rose tree?
data MultiTree a = MLeaf a | MNode [MultiTree a] deriving (Show)

instance Pretty a => Pretty (Tree a) where
  pPrint (Leaf a)   = text "Leaf: " <> pPrint a
  pPrint (Node l r) = vcat [ text "Node:"
                             , nest 2 (pPrint l)
                             , nest 2 (pPrint r)]

instance Pretty a => Pretty (MultiTree a) where
  pPrint (MLeaf a)  = text "Leaf: " <> pPrint a
  pPrint (MNode ns) = vcat $ [ text "Node:" ] ++ map (nest 2 . pPrint) ns

exampleTree :: Tree Int
exampleTree =
  Node
    (Leaf 1)
    (Node
      (Leaf 2)
      (Leaf 3))

exampleMTree :: MultiTree Int
exampleMTree =
  MNode
    [ MLeaf 1
    , MNode [ MLeaf 2
            , MLeaf 3
            , MLeaf 4
            ]
    ]

renderShowTree :: String
renderShowTree = show exampleTree

renderPrettyTree :: String
renderPrettyTree = prettyShow exampleTree

renderPrettyMTree :: String
renderPrettyMTree = prettyShow exampleMTree
