module LambdaSyntax.Token

open LambdaDomain.Location

type Token =
  | EofToken
  | BadToken
  | SpacesToken
  | CommentToken
  | IntToken
  | IdentToken
  | GreekToken
  | KeywordToken
  | PunToken

type TokenData = Token * string * Range
