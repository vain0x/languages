module Linear.Token

open Linear.Location

type Token =
  | EofToken
  | BadToken
  | SpacesToken
  | CommentToken
  | IntToken
  | StringToken
  | IdentToken
  | KeywordToken
  | PunToken

type TokenData = Token * string * Range
