module BinaryTree where

  data BinaryTree a =
    Leaf
    | Node (BinaryTree a)
           a
           (BinaryTree a) deriving (Eq, Ord, Show)



  mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
  mapTree _ Leaf = Leaf
  mapTree f (Node left a right) = Node left' (f a) right' where
    left'  = mapTree f left
    right' = mapTree f right

  testTree' :: BinaryTree Integer
  testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
  mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

  -- acceptance test for mapTree
  mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

  preorder :: BinaryTree a -> [a]
  preorder Leaf = []
  preorder (Node left a right) = a : preorder left ++ preorder right

  inorder :: BinaryTree a -> [a]
  inorder Leaf = []
  inorder (Node left a right) = preorder left ++ [a] ++ preorder right

  postorder :: BinaryTree a -> [a]
  postorder Leaf = []
  postorder (Node left a right) = postorder left ++ postorder right ++ [a]

  testTree :: BinaryTree Integer
  testTree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf))

  testPreorder :: IO ()
  testPreorder =
    if preorder testTree == [2, 1, 4, 3, 5] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."
  testInorder :: IO ()
  testInorder =
    if inorder testTree == [1, 2, 4, 3, 5] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."
  testPostorder :: IO ()
  testPostorder =
    if postorder testTree == [1, 3, 5, 4, 2] then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

  -- write fold for a binary tree
  -- could also fold left subtree first (since any traversal order is OK)
  foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
  foldTree f acc Leaf = acc
  foldTree f acc (Node left a right) = foldTree f foldedRightSubtree left where
    result = f a acc
    foldedRightSubtree = foldTree f result right
  
  -- rewrite mapTree using foldTree
  mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
  mapTree' f Leaf = Leaf
  mapTree' f (Node left a right) = undefined

  main :: IO()
  main = do
    testPreorder
    testInorder
    testPostorder