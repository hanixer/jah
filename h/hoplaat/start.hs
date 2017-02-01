import Parser
import Control.Applicative ((<|>), some, many)

data Formula = Truen
             | Falsen
             | Atom String
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Imp Formula Formula
             | Iff Formula Formula
             | Forall String Formula
             | Exists String Formula
  deriving (Show)

stop :: Formula -> Bool
stop f = case f of
  Truen -> True
  Falsen -> False
  _ -> True

formulaExp :: Parser Formula
formulaExp = 
  atomExp <|>
  notExp <|>
  failp "formula expected"

trueExp :: Parser Formula
trueExp = string "true" >> return Truen

atomExp :: Parser Formula
atomExp = identifier >>= \var -> return $ Atom var

notExp :: Parser Formula
notExp = string "~" >> formulaExp >>= \fm -> return $ Not fm

data Token = VarTk String
           | NotTk
           | AndTk
           | OrTk
           | ImpTk
           | IffTk
  deriving (Show)

tokenParser :: Parser Token
tokenParser =
  (identifier >>= \v -> return $ VarTk v)
  <|>
  (string "~" >> return NotTk)
  <|>
  (string "&" >> return AndTk)
  <|>
  (string "|" >> return OrTk)
  <|>
  (string "==>" >> return ImpTk)
  <|>
  (string "<=>" >> return IffTk)

eee = (Atom "p" `And` Atom "q" `Imp` Atom "q" `And` Atom "p")