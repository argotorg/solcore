module Solcore.Backend.NameEncoding
  ( encodeBackendName,
    encodeSpecialisedName,
    encodeTypeIdentity,
  )
where

import Data.Char (isAlpha, isAlphaNum, ord)
import Data.List (isInfixOf)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty

-- | Encode a source-level name as a Hull/Yul identifier.
--
-- Plain backend-safe names are preserved because entry points and inline
-- assembly refer to names such as @main@ by their source spelling. Encoded
-- names use the reserved "$$" namespace; a plain name containing that marker
-- is escaped, so source and compiler-generated names cannot forge each other.
encodeBackendName :: Name -> String
encodeBackendName (Name name)
  | isBackendIdentifier name && not ("$$" `isInfixOf` name) = name
  | otherwise = "$$N" ++ encodeSegment name
encodeBackendName qualified@QualName {} =
  "$$Q" ++ encodeNamePayload qualified

-- | Form the backend name of a specialised declaration. Source identity and
-- type-argument boundaries are both retained in the encoding.
encodeSpecialisedName :: Name -> [Ty] -> Name
encodeSpecialisedName name [] = Name (encodeBackendName name)
encodeSpecialisedName name types =
  Name
    ( "$$S"
        ++ encodeField (encodeBackendName name)
        ++ encodeList (map encodeTypeIdentity types)
    )

-- | Encode the complete structural identity of a type.
encodeTypeIdentity :: Ty -> String
encodeTypeIdentity (TyVar (TVar name)) =
  "V" ++ encodeField (encodeNamePayload name)
encodeTypeIdentity (TyVar (Skolem name)) =
  "K" ++ encodeField (encodeNamePayload name)
encodeTypeIdentity (Meta (MetaTv name)) =
  "M" ++ encodeField (encodeNamePayload name)
encodeTypeIdentity (TyCon name types) =
  "T"
    ++ encodeField (encodeNamePayload name)
    ++ encodeList (map encodeTypeIdentity types)

isBackendIdentifier :: String -> Bool
isBackendIdentifier [] = False
isBackendIdentifier (first : rest) =
  (isAlpha first || first == '_' || first == '$')
    && all
      (\char -> isAlphaNum char || char == '_' || char == '$')
      rest

-- Name constructors and qualification boundaries are retained explicitly.
-- Code points prevent source spelling from imitating structural delimiters.
encodeNamePayload :: Name -> String
encodeNamePayload (Name name) = "N" ++ encodeSegment name
encodeNamePayload (QualName qualifier leaf) =
  "Q" ++ encodeField (encodeNamePayload qualifier) ++ encodeSegment leaf

encodeSegment :: String -> String
encodeSegment value =
  show (length value)
    ++ "$"
    ++ concatMap (\char -> show (ord char) ++ "_") value

encodeField :: String -> String
encodeField value = show (length value) ++ "$" ++ value

encodeList :: [String] -> String
encodeList values =
  show (length values) ++ "$" ++ concatMap encodeField values
