{

{-# LANGUAGE FieldSelectors #-}

module Frontend.Lexer (
  Alex,
  AlexPosn (..),
  alexGetInput,
  alexError,
  runAlex,
  alexMonadScan,
  Span (..),
  SpannedToken (..),
  Token (..),
) where

import Data.ByteString.Lazy.Char8 qualified as BS
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-

<0> $white+    ;

<0> let     { tok TLet }
<0> rec     { tok TRec }
<0> in      { tok TIn }
<0> if      { tok TIf }
<0> then    { tok TThen }
<0> else    { tok TElse }
<0> not     { tok TNot }
<0> true    { tok (TBool True) }
<0> false   { tok (TBool False) }

<0> ("Array.create" | "Array.make") { tok TArrayCreate }

<0> "+"     { tok TPlus }
<0> "-"     { tok TMinus }
<0> "+."    { tok TPlusDot }
<0> "-."    { tok TMinusDot }
<0> "*."    { tok TTimesDot }
<0> "/."    { tok TDivideDot }

<0> "="     { tok TEq }
<0> "<>"    { tok TNeq }
<0> "<"     { tok TLt }
<0> "<="    { tok TLe }
<0> ">"     { tok TGt }
<0> ">="    { tok TGe }

<0> "("     { tok TLPar }
<0> ")"     { tok TRPar }

<0> ","     { tok TComma }
<0> ";"     { tok TSemicolon }
<0> "."     { tok TDot }

<0> "->"    { tok TRightArrow }
<0> "<-"    { tok TLeftArrow }

<0> @id     { tokIdent }

{
data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex SpannedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ SpannedToken TEof (Span pos pos)

data Span = Span
  { start :: AlexPosn
  , stop :: AlexPosn
  }
  deriving (Eq, Show)

data Token
  = TIdent LByteString
  | TInt Integer
  | TBool Bool
  | TLet
  | TRec
  | TIn
  | TIf
  | TThen
  | TElse
  | TNot
  | TArrayCreate
  | TPlus
  | TMinus
  | TPlusDot
  | TMinusDot
  | TTimesDot
  | TDivideDot
  | TEq
  | TNeq
  | TLt
  | TLe
  | TGt
  | TGe
  | TLPar
  | TRPar
  | TComma
  | TSemicolon
  | TDot
  | TRightArrow
  | TLeftArrow
  | TEof
  deriving (Eq, Show)

data SpannedToken = SpannedToken
  { stToken :: Token
  , stSpan :: Span
  }
  deriving (Eq, Show)

mkSpan :: AlexInput -> Int64 -> Span
mkSpan (start, _, str, _) len = Span {start, stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tok :: Token -> AlexAction SpannedToken
tok ctor inp len =
  pure
    SpannedToken
      { stToken = ctor
      , stSpan = mkSpan inp len
      }

tokIdent :: AlexAction SpannedToken
tokIdent inp@(_, _, str, _) len =
  pure
    SpannedToken
      { stToken = TIdent $ BS.take len str
      , stSpan = mkSpan inp len
      }
}
