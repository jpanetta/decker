{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de> --}
module Quiz
  ( renderQuizzes
  , renderQuestions
  ) where

import Common
import Control.Exception
import Debug.Trace as DT
import Filter
import Text.Pandoc
import Text.Pandoc.Walk

-- import Utilities as U
renderQuizzes :: Pandoc -> Decker Pandoc
renderQuizzes pandoc = do
  return $ walk renderQuiz pandoc

renderQuestions :: Pandoc -> Decker Pandoc
renderQuestions pandoc = return $ walk renderQuestion pandoc

renderQuestion :: Block -> Block
renderQuestion bl@(BulletList blocks@((firstBlock:_):(sndBlock:_):_)) =
  case (checkIfQuestion firstBlock, checkIfAnswer sndBlock) of
    (Just q, Just a) -> Div ("", ["freetextQ"], []) [Para $ questionForm q a]
    _ -> bl
renderQuestion block = block

toHtml :: String -> Inline
toHtml = RawInline (Format "html")

questionForm :: [Inline] -> [Inline] -> [Inline]
questionForm question answer =
  [toHtml "<form>"] ++
  question ++
  [LineBreak] ++
  [toHtml "<input type=\"text\" class=\"questionField\">"] ++
  -- 
  [LineBreak] ++
  [ toHtml $
    "<button type=\"button\" onclick=\"" ++
    "if (this.parentElement.getElementsByClassName('questionField')[0].value)" ++
    "{this.getElementsByClassName('freeAnswer')[0].style.display = 'block';" ++
    "this.parentElement.getElementsByClassName('questionField')[0].disabled = 'true';" ++
    "}\">"
  ] ++
  [Str "Answer:"] ++
  [Span ("", ["freeAnswer"], [("style", "display:none;")]) answer] ++
  [toHtml "</button>"] ++ [toHtml "</form>"]

checkIfQuestion :: Block -> Maybe [Inline]
checkIfQuestion (Para (Str "[?]":q)) = Just q
checkIfQuestion (Plain (Str "[?]":q)) = Just q
checkIfQuestion _ = Nothing

checkIfAnswer :: Block -> Maybe [Inline]
checkIfAnswer (Para (Str "[!]":a)) = Just a
checkIfAnswer (Plain (Str "[!]":a)) = Just a
checkIfAnswer _ = Nothing

-- renderMatching :: Block -> Block
-- checkIfMatching :: Block -> Bool
-- checkIfMatching (Para ((Str "["):Inline:Str "]")) = True
-- | Renders a quiz
-- A quiz is a bullet list in the style of a task list.
-- A div class survey is created around the bullet list
renderQuiz :: Block -> Block
-- BulletList which qualifies as survey
renderQuiz (BulletList blocks@((firstBlock:_):_))
  | checkIfQuiz firstBlock =
    Div ("", ["survey"], []) [BulletList (map renderAnswer blocks)]
-- Default pass through
renderQuiz block = block

-- | Checks if a block starts with [X] or [ ] to indicate a survey
checkIfQuiz :: Block -> Bool
checkIfQuiz (Para ((Str "[X]"):_)) = True
checkIfQuiz (Para ((Str "["):Space:(Str "]"):_)) = True
checkIfQuiz (Plain ((Str "[X]"):_)) = True
checkIfQuiz (Plain ((Str "["):Space:(Str "]"):_)) = True
checkIfQuiz (Plain ((Link nullAttr [] ('#':_, "")):Space:_)) = True
checkIfQuiz _ = False

-- | Renders a quiz answer 
-- Throws away the identifier and sourrounds the content with a div
-- The div has the class right or wrong according to how it was marked
renderAnswer :: [Block] -> [Block]
renderAnswer (prelude:rest) =
  [Div ("", "answer" : cls, []) (prelude' : (map renderTooltip rest))]
  where
    (cls, prelude') =
      case prelude of
        Para ((Str "[X]"):prest) -> (["right"], Para prest)
        Para ((Str "["):Space:(Str "]"):prest) -> (["wrong"], Para prest)
        Plain ((Str "[X]"):prest) -> (["right"], Para prest)
        Plain ((Str "["):Space:(Str "]"):prest) -> (["wrong"], Para prest)
        prest -> ([], prest)

-- if there is a bullet list create a div class tooltip around
-- if there are multiple bullet points, all but the first are thrown away
renderTooltip :: Block -> Block
renderTooltip (BulletList (content:_)) = Div ("", ["tooltip"], []) content
renderTooltip block = block
