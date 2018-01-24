{-# LANGUAGE OverloadedStrings #-}
module Database.Schema.Migrations.Filesystem.Serialize
    ( serializeMigration
    )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Time () -- for UTCTime Show instance
import Data.Maybe ( catMaybes )
import Data.Monoid ( (<>) )

import Database.Schema.Migrations.Migration
    ( Migration(..)
    )

type FieldSerializer = Migration -> Maybe ByteString

fieldSerializers :: [FieldSerializer]
fieldSerializers = [ serializeDesc
                   , serializeTimestamp
                   , serializeDepends
                   , serializeApply
                   , serializeRevert
                   ]

serializeDesc :: FieldSerializer
serializeDesc m =
    case mDesc m of
      Nothing -> Nothing
      Just desc -> Just $ "Description: " <> desc

serializeTimestamp :: FieldSerializer
serializeTimestamp m =
    case mTimestamp m of
        Nothing -> Nothing
        Just ts -> Just $ "Created: " <> (BSC.pack . show $ ts)

serializeDepends :: FieldSerializer
serializeDepends m = Just $ "Depends: " <> (BS.intercalate " " $ mDeps m)

serializeRevert :: FieldSerializer
serializeRevert m =
    case mRevert m of
      Nothing -> Nothing
      Just revert -> Just $ "Revert: |\n" <>
                     (serializeMultiline revert)

serializeApply :: FieldSerializer
serializeApply m = Just $ "Apply: |\n" <> (serializeMultiline $ mApply m)

commonPrefix :: ByteString -> ByteString -> ByteString
commonPrefix a b = fst $ BSC.unzip $ takeWhile (uncurry (==)) (BSC.zip a b)

commonPrefixLines :: [ByteString] -> ByteString
commonPrefixLines [] = ""
commonPrefixLines theLines = foldl1 commonPrefix theLines

serializeMultiline :: ByteString -> ByteString
serializeMultiline s =
    let sLines = BSC.lines s
        prefix = if BSC.head (commonPrefixLines sLines) == ' '
                   -- If the lines already have a common prefix that
                   -- begins with whitespace, no new prefix is
                   -- necessary.
                   then ""
                   -- Otherwise, use a new prefix of two spaces.
                   else " "

    in BSC.unlines $ map (prefix <>) sLines

serializeMigration :: Migration -> ByteString
serializeMigration m = BS.intercalate "\n" fields
    where
      fields = catMaybes [ f m | f <- fieldSerializers ]
