{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Perm
import Text.Parsec.Prim
import Text.Parsec.String
import Control.Monad
import Data.String.Utils
import Data.Either.Unwrap
import Data.Either
import Data.List.Split as S
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Builder as BB
import Control.Parallel.Strategies


import Prelude

data Entry = Entry {
    first_name :: String
  , last_name :: String
  , nationality :: Maybe String
  , date_of_birth :: Maybe String
  , name_at_birth :: Maybe String
  , place_of_birth :: Maybe String
  , date_of_death :: Maybe String

  , address :: Maybe String
  , telephone :: Maybe String
  , fax :: Maybe String
  , email :: Maybe String
  , website :: Maybe String

  , family :: Maybe String
  , parentage :: Maybe String
  , extended_family :: Maybe String

  , education :: Maybe String
  , qualifications :: Maybe String
  , career :: Maybe String
  , honours :: Maybe String
  , achievements :: Maybe String
  , publications :: Maybe String

  , leisure_interests :: Maybe String
  , radio :: Maybe String
  , tv :: Maybe String
  , films :: Maybe String
  , music :: Maybe String
  , dance :: Maybe String
  , plays :: Maybe String
  , art_exhibitions :: Maybe String

} deriving (Show)

instance CSV.ToRecord Entry where
  toRecord (Entry {
      first_name, last_name, nationality, date_of_birth, name_at_birth, place_of_birth, date_of_death, address,
      telephone, fax, email, website, family, parentage, extended_family, education, qualifications, career, honours,
      achievements, publications, leisure_interests, radio, tv, films, music, dance, plays, art_exhibitions }) =
    CSV.record [
      CSV.toField $ first_name,
      CSV.toField $ last_name,

      CSV.toField $ nationality,
      CSV.toField $ date_of_birth,
      CSV.toField $ name_at_birth,
      CSV.toField $ place_of_birth,
      CSV.toField $ date_of_death,

      CSV.toField $ address,
      CSV.toField $ telephone,
      CSV.toField $ fax,
      CSV.toField $ email,
      CSV.toField $ website,

      CSV.toField $ family,
      CSV.toField $ parentage,
      CSV.toField $ extended_family,

      CSV.toField $ education,
      CSV.toField $ qualifications,
      CSV.toField $ career,
      CSV.toField $ honours,
      CSV.toField $ achievements,
      CSV.toField $ publications,

      CSV.toField $ leisure_interests,
      CSV.toField $ radio,
      CSV.toField $ tv,
      CSV.toField $ films,
      CSV.toField $ music,
      CSV.toField $ dance,
      CSV.toField $ plays,
      CSV.toField $ art_exhibitions]



firstNameParser :: Parser String
firstNameParser = do
  first_name <- manyTill anyChar (lookAhead endOfLine)
  endOfLine
  return first_name

lastNameParser :: Parser String
lastNameParser = do
  last_name <- manyTill anyChar (lookAhead (try paramsParser))
  return $ strip $ replace "\n" " " last_name

attributeParser :: String -> Parser String
attributeParser field = do
  string $ field ++ ": "
  value <- manyTill anyChar (lookAhead endOfLine)
  guard (length value > 0) <?> "value of " ++ "field"
  endOfLine
  return value

paramsParser = do
  params <- permute ((,,,,,,,,,,,,,,,,,,,,,,,,,,)
    <$?> (attribute "Nationality")
    <|?> (attribute "Date of Birth")
    <|?> (attribute "Name at Birth")
    <|?> (attribute "Place of Birth")
    <|?> (attribute "Date of Death")

    <|?> (attribute "Address")
    <|?> (attribute "Telephone")
    <|?> (attribute "Fax")
    <|?> (attribute "Email")
    <|?> (attribute "Website")

    <|?> (attribute "Family")
    <|?> (attribute "Parentage")
    <|?> (attribute "Extended Family")

    <|?> (attribute "Education")
    <|?> (attribute "Qualifications")
    <|?> (attribute "Career")
    <|?> (attribute "Honours")
    <|?> (attribute "Achievements")
    <|?> (attribute "Publications")

    <|?> (attribute "Leisure Interests")
    <|?> (attribute "Radio")
    <|?> (attribute "TV")
    <|?> (attribute "Films")
    <|?> (attribute "Music")
    <|?> (attribute "Dance")
    <|?> (attribute "Plays")
    <|?> (attribute "Art Exhibitions")
    )
  guard $ any (/= Nothing) $ toListLong params
  return params

  where
    attribute txt = (Nothing, liftM Just (try $ attributeParser txt))

toListLong (a, b, c, d, e, f, g, h, i, j, k, l, m, o, p, r, s, t, q, x, y, z, a1, b1, c1, d1, e1) = [a, b, c, d, e, f, g, h, i, j, k, l, m, o, p, r, s, t, q, x, y, z, a1, b1, c1, d1, e1]

uncurryLong fun (a, b, c, d, e, f, g, h, i, j, k, l, m, o, p, r, s, t, q, x, y, z, a1, b1, c1, d1, e1) =
  fun a b c d e f g h i j k l m o p r s t q x y z a1 b1 c1 d1 e1

entryParser :: Parser Entry
entryParser = do
  first <- firstNameParser
  last <- lastNameParser
  params <- paramsParser

  return $ uncurryLong (Entry first last) params

main = do
    content <- getContents
    let bios = map a (S.splitOn "\n\n\n" content) `using` parListChunk 64 rseq
    -- let records = CSV.encode bios


    B.writeFile "names.csv_" $ B.concat bios

  where
    a s = case b s of
      Right i -> CSV.encode [ i ]
      Left i -> error ( (show i) ++ (show s))

    b s = parse (entryParser) "" (s ++ "\n")



