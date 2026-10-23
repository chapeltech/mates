module Mates.Server.Condition (checkIf) where

import Data.Char (isSpace)
import Text.ParserCombinators.ReadP

-- A tagged resource has one or more alternative lists; each list is a
-- conjunction. Untagged lists apply to the request resource.
data Condition = State String | EntityTag String deriving (Eq, Show)

checkIf :: (String -> IO [String]) -> String -> String -> IO (Either String Bool)
checkIf states requestHref input = case [groups | (groups,rest) <- readP_to_S (header <* skipSpaces <* eof) input, null rest] of
  [] -> pure (Left "Invalid WebDAV If header")
  groups:_ -> do
    results <- mapM evaluateGroup groups
    pure (Right (and results))
  where
    evaluateGroup (href, alternatives) = do
      current <- states (if null href then requestHref else href)
      pure (any (all (\(positive, clause) -> positive == case clause of State value -> value `elem` current; EntityTag value -> value `elem` current)) alternatives)
    header = tagged +++ untagged
    untagged = do lists <- many1 alternative; pure [("", lists)]
    tagged = many1 $ do href <- token; lists <- many1 alternative; pure (href, lists)
    alternative = between (symbol '(') (symbol ')') (many1 condition)
    condition = do
      skipSpaces
      positive <- (string "Not" >> skipSpaces >> pure False) +++ pure True
      value <- (State <$> token) +++ (EntityTag <$> between (symbol '[') (symbol ']') (munch1 (/= ']')))
      skipSpaces
      pure (positive, value)
    token = between (symbol '<') (symbol '>') (munch1 (\c -> c /= '>' && not (isSpace c)))
    symbol c = skipSpaces *> char c <* skipSpaces
