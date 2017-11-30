module Trie
  ( addSuffix
  , fromPatterns
  , match
  ) where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M

data Trie = Trie
  { trieValue :: Int
  , trieChildren :: M.Map Char Trie
  , trieEndOfLine :: Bool
  }

data Crumb = Crumb
  { crumbParent :: Trie
  , crumbLabel :: Char
  }

data Zipper = Zipper
  { zipperCurrent :: Trie
  , zipperCrumbs :: [Crumb]
  }

instance Show Trie where
  show = intercalate "\n" . showTrie
    where
      showTrie trie@(Trie value chrn _) =
        (M.elems $ M.mapWithKey (showEdge value) chrn) ++ (showChildren trie)
      showEdge source label (Trie dest _ _) =
        (show source) ++ "->" ++ (show dest) ++ ":" ++ [label]
      showChildren parent = concat $ map showTrie (chValues parent)
      chValues = M.elems . trieChildren

type NodeCount = Int

type TrieState = State NodeCount Trie

type ZipperState = State NodeCount Zipper

empty :: TrieState
empty = do
  count <- get
  put $ count + 1
  return (Trie count M.empty False)

findOrCreate :: Char -> Trie -> TrieState
findOrCreate label (Trie _ children _) =
  case M.lookup label children of
    Just child -> return child
    Nothing -> empty

addEdge :: Zipper -> Char -> ZipperState
addEdge (Zipper current crumbs) label = do
  child <- findOrCreate label current
  let crumb = Crumb current label
  let zipper' = Zipper child (crumb : crumbs)
  return zipper'

addSuffix :: Trie -> String -> TrieState
addSuffix trie [] = return trie
addSuffix trie cs = do
  zipper' <- foldM addEdge zipper cs
  return $ (zipperCurrent . zipperTop . endOfLine) zipper'
  where
    zipper = zipperNew trie

fromPatterns :: [String] -> Trie
fromPatterns patterns = evalState go 0
  where
    go = do
      trie <- empty
      foldM addSuffix trie patterns

zipperNew :: Trie -> Zipper
zipperNew trie = Zipper trie []

zipperTop :: Zipper -> Zipper
zipperTop z@(Zipper root []) = z
zipperTop (Zipper current (crumb:crumbs)) = zipperTop $ Zipper current' crumbs
  where
    current' = parent {trieChildren = children}
    children = M.insert label current (trieChildren parent)
    (Crumb parent label) = crumb

endOfLine :: Zipper -> Zipper
endOfLine (Zipper current crumbs) = Zipper current' crumbs
  where
    current' = current {trieEndOfLine = True}

find :: String -> Trie -> Maybe Int
find [] (Trie value _ _) = Just $ value
find (k:ks) (Trie _ children _) = do
  ct <- M.lookup k children
  find ks ct

prefixMatch :: String -> Trie -> Maybe String
prefixMatch text trie = go text trie ""
  where
    go _ (Trie _ _ True) path = Just path
    go (first:rest) (Trie value children _) path
      | (M.member first children) = go rest (children M.! first) (first : path)
      | otherwise = Nothing
    go _ _ _ = Nothing

match :: Trie -> String -> [Int]
match trie text = reverse $ go text trie [] 0
  where
    go [] _ positions _ = positions
    go text trie positions position =
      let positions' =
            case prefixMatch text trie of
              Just s -> position : positions
              Nothing -> positions
      in go (tail text) trie positions' (position + 1)
