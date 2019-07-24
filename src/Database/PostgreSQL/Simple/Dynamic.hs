{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Database.PostgreSQL.Simple.Dynamic where

import           Control.Exception (Exception, toException)
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.Aeson (Value, (.=))
import           Data.ByteString (ByteString)
import           Data.Dynamic
import           Data.Int
import           Data.Text (Text)
import           Data.Time
import           Data.UUID (UUID)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow (FromRow(..), fieldWith)
import           Database.PostgreSQL.Simple.Internal (Conversion(..), Field(..), Row(..), RowParser(..))
import           Database.PostgreSQL.Simple.Ok (Ok(..))
import           System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.Aeson as JSON
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Database.PostgreSQL.LibPQ as PQ

-- | Infix lookup: @someRow `o` "columnName"@.
-- This combines two possible errors, no key, and value has unexpected
-- type.  To handle these separately, use 'lookup' and 'fromDynamic'
-- directly.
o :: Typeable a => M.Map Text Dynamic -> Text -> Maybe a
o m k = fromDynamic =<< M.lookup k m

-- | Exception thrown if an unknown Postgres type is encountered while decoding to Dynamic.
data DynamicDecodeError = DynamicDecodeError
    { dde_column :: !T.Text
    , dde_type :: !ByteString
    , dde_value :: !(Maybe ByteString)
    } deriving (Eq, Show, Typeable)

instance Exception DynamicDecodeError

instance JSON.ToJSON DynamicDecodeError where
    toJSON dde = JSON.object [ "column" .= dde_column dde, "type" .= enc (dde_type dde), "value" .= fmap enc (dde_value dde) ] where
      enc = T.decodeUtf8With T.lenientDecode

columnNameOrNumber :: Field -> Text
columnNameOrNumber field = case name field of
    Just name -> T.decodeUtf8With T.lenientDecode name
    Nothing -> T.pack (show c) where
        (PQ.Col c) = column field

dynamicParser :: FieldParser (T.Text, Dynamic)
dynamicParser field m_value = do
    let
        columnName = columnNameOrNumber field
        getField :: forall a. (FromField a, Typeable a) => Conversion Dynamic
        getField = toDyn <$> (fromField field m_value :: Conversion a)
    ty <- typename field
    -- TODO pattern match on values from Database.PostgreSQL.Simple.TypeInfo.Static first?
    -- Maybe faster / safer for known OIDs?
    value <- case ty of
        "bool" -> getField @Bool
        "boolean" -> getField @Bool -- I don't know if aliases can occur here
        "int2" -> getField @Int16
        "smallint" -> getField @Int16 -- documented alias
        "int4" -> getField @Int32
        "int8" -> getField @Int64
        "bigint" -> getField @Int64 -- documented alias
        "bit" -> getField @ByteString
        "varbit" -> getField @ByteString
        "bytea" -> getField @ByteString
        "json" -> getField @Value
        "jsonb" -> getField @Value
        "float4" -> getField @Float
        "float8" -> getField @Double
        "real" -> getField @Float -- documented alias
        "text" -> getField @Text
        "char"-> getField @Text
        "varchar"-> getField @Text
        "uuid" -> getField @UUID
        "timestamptz" -> getField @UTCTime
        "timestamp"-> getField @LocalTime
        "date" -> getField @Day
        "time" -> getField @TimeOfDay
        -- TODO interval
        _ -> Conversion (\_ -> return $ Errors [toException (DynamicDecodeError columnName ty m_value)])
    return (columnName, value)

-- | To extend the 'FromRow' instance with additional types (without contributing the addition upstream), you can use @fromRowToDynamicWith@:
-- >    newtype DbRow = DbRow (Map Text Dynamic)
-- >    instance FromRow DbRow where
-- >        fromRow = fromRowToDynamicWith (\field value -> do
-- >            ty <- typename field
-- >            m_value <- case ty of
-- >                "obscureType" -> fmap toDyn (fromField field m_value :: Conversion ObscureType)
-- >                _ -> DbRow <$> dynamicParser field value
-- >                                       )

fromRowToDynamicWith :: FieldParser (T.Text, Dynamic) -> RowParser (M.Map T.Text Dynamic)
fromRowToDynamicWith fieldParser = do
        (PQ.Col ncols) <- unsafeDupablePerformIO . PQ.nfields . rowresult <$> RP ask
        M.fromList <$> replicateM (fromIntegral ncols) (fieldWith fieldParser)

instance FromRow (M.Map T.Text Dynamic) where
    fromRow = fromRowToDynamicWith dynamicParser
