{-# LANGUAGE BangPatterns, DataKinds, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings
 #-}
{-# OPTIONS_GHC  -w #-}
module Proto.Vector_tile.Tile.Layer (Layer(..)) where
import Prelude ((+), (/), (++), (.), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.List as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Proto.Vector_tile.Tile.Feature as Vector_tile.Tile
import qualified Proto.Vector_tile.Tile.Value as Vector_tile.Tile

data Layer = Layer{version :: !(P'.Word32), name :: !(P'.Utf8), features :: !(P'.Seq Vector_tile.Tile.Feature),
                   keys :: !(P'.Seq P'.Utf8), values :: !(P'.Seq Vector_tile.Tile.Value), extent :: !(P'.Maybe P'.Word32),
                   ext'field :: !(P'.ExtField)}
             deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.ExtendMessage Layer where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.Mergeable Layer where
  mergeAppend (Layer x'1 x'2 x'3 x'4 x'5 x'6 x'7) (Layer y'1 y'2 y'3 y'4 y'5 y'6 y'7)
   = let !z'1 = P'.mergeAppend x'1 y'1
         !z'2 = P'.mergeAppend x'2 y'2
         !z'3 = P'.mergeAppend x'3 y'3
         !z'4 = P'.mergeAppend x'4 y'4
         !z'5 = P'.mergeAppend x'5 y'5
         !z'6 = P'.mergeAppend x'6 y'6
         !z'7 = P'.mergeAppend x'7 y'7
      in Layer z'1 z'2 z'3 z'4 z'5 z'6 z'7

instance P'.Default Layer where
  defaultValue = Layer 1 P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue (Prelude'.Just 4096) P'.defaultValue

instance P'.Wire Layer where
  wireSize ft' self'@(Layer x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 13 x'1 + P'.wireSizeReq 1 9 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeRep 1 9 x'4 +
             P'.wireSizeRep 1 11 x'5
             + P'.wireSizeOpt 1 13 x'6
             + P'.wireSizeExtField x'7)
  wirePutWithSize ft' self'@(Layer x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutReqWithSize 10 9 x'2, P'.wirePutRepWithSize 18 11 x'3, P'.wirePutRepWithSize 26 9 x'4,
             P'.wirePutRepWithSize 34 11 x'5, P'.wirePutOptWithSize 40 13 x'6, P'.wirePutReqWithSize 120 13 x'1,
             P'.wirePutExtFieldWithSize x'7]
        put'FieldsSized
         = let size' = Prelude'.fst (P'.runPutM put'Fields)
               put'Size
                = do
                    P'.putSize size'
                    Prelude'.return (P'.size'WireSize size')
            in P'.sequencePutWithSize [put'Size, put'Fields]
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown' P'.discardUnknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown' P'.discardUnknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             120 -> Prelude'.fmap (\ !new'Field -> old'Self{version = new'Field}) (P'.wireGet 13)
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{name = new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{features = P'.append (features old'Self) new'Field}) (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{keys = P'.append (keys old'Self) new'Field}) (P'.wireGet 9)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{values = P'.append (values old'Self) new'Field}) (P'.wireGet 11)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{extent = Prelude'.Just new'Field}) (P'.wireGet 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [16 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Layer) Layer where
  getVal m' f' = f' m'

instance P'.GPB Layer

instance P'.ReflectDescriptor Layer where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 120]) (P'.fromDistinctAscList [10, 18, 26, 34, 40, 120])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".vector_tile.Tile.Layer\", haskellPrefix = [], parentModule = [MName \"Vector_tile\",MName \"Tile\"], baseName = MName \"Layer\"}, descFilePath = [\"Vector_tile\",\"Tile\",\"Layer.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.version\", haskellPrefix' = [], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"version\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 120}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Just \"1\", hsDefault = Just (HsDef'Integer 1), isMapField = False, mapKeyVal = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.name\", haskellPrefix' = [], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing, isMapField = False, mapKeyVal = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.features\", haskellPrefix' = [], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"features\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".vector_tile.Tile.Feature\", haskellPrefix = [], parentModule = [MName \"Vector_tile\",MName \"Tile\"], baseName = MName \"Feature\"}), hsRawDefault = Nothing, hsDefault = Nothing, isMapField = False, mapKeyVal = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.keys\", haskellPrefix' = [], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"keys\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing, isMapField = False, mapKeyVal = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.values\", haskellPrefix' = [], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"values\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".vector_tile.Tile.Value\", haskellPrefix = [], parentModule = [MName \"Vector_tile\",MName \"Tile\"], baseName = MName \"Value\"}), hsRawDefault = Nothing, hsDefault = Nothing, isMapField = False, mapKeyVal = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.extent\", haskellPrefix' = [], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"extent\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Just \"4096\", hsDefault = Just (HsDef'Integer 4096), isMapField = False, mapKeyVal = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 16},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False, mapEntry = False}"

instance P'.TextType Layer where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Layer where
  textPut msg
   = do
       P'.tellT "version" (version msg)
       P'.tellT "name" (name msg)
       P'.tellT "features" (features msg)
       P'.tellT "keys" (keys msg)
       P'.tellT "values" (values msg)
       P'.tellT "extent" (extent msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'version, parse'name, parse'features, parse'keys, parse'values, parse'extent]) P'.spaces
       Prelude'.return (Prelude'.foldl' (\ v f -> f v) P'.defaultValue mods)
    where
        parse'version = Prelude'.fmap (\ v o -> o{version = v}) (P'.try (P'.getT "version"))
        parse'name = Prelude'.fmap (\ v o -> o{name = v}) (P'.try (P'.getT "name"))
        parse'features = Prelude'.fmap (\ v o -> o{features = P'.append (features o) v}) (P'.try (P'.getT "features"))
        parse'keys = Prelude'.fmap (\ v o -> o{keys = P'.append (keys o) v}) (P'.try (P'.getT "keys"))
        parse'values = Prelude'.fmap (\ v o -> o{values = P'.append (values o) v}) (P'.try (P'.getT "values"))
        parse'extent = Prelude'.fmap (\ v o -> o{extent = v}) (P'.try (P'.getT "extent"))
