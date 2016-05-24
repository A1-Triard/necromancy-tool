module Data.Tes3.Disassembler.Native where

#include <haskell>
import Data.Tes3
import Data.Tes3.Get
import Data.Tes3.Utils

writeT3FileSignature :: Text
writeT3FileSignature = "3SET\n"

writeT3FileHeader :: T3FileHeader -> Text
writeT3FileHeader (T3FileHeader version file_type author description refs)
  =  "VERSION " <> T.pack (show version) <> "\n"
  <> "TYPE " <> T.pack (show file_type) <> "\n"
  <> "AUTHOR " <> writeLine author
  <> "DESCRIPTION\n" <> writeLines description
  <> T.concat [writeNulledRun n <> " " <> T.pack (show z) <> "\n" | (T3FileRef n z) <- refs]

writeT3Field :: T3Field -> Text
writeT3Field (T3BinaryField sign d) = T.pack (show sign) <> " " <> T.pack (C.unpack (encode d)) <> "\n"
writeT3Field (T3StringField sign s) = T.pack (show sign) <> " " <> writeNulledLine s
writeT3Field (T3FixedStringField sign s) = T.pack (show sign) <> " " <> writeLine s
writeT3Field (T3MultilineField sign t) = T.pack (show sign) <> "\n" <> writeLines t
writeT3Field (T3RefField sign z n) = T.pack (show sign) <> " " <> T.pack (show z) <> " " <> writeLine n

writeT3Record :: T3Record -> Text
writeT3Record (T3Record sign gap fields)
  =  "\n" <> T.pack (show sign) <> (if gap == 0 then "" else " " <> T.pack (show gap)) <> "\n"
  <> T.concat [writeT3Field f | f <- fields]

conduitGet1 :: Monad m => Get e a -> ByteOffset -> ConduitM S.ByteString a m (Either (ByteOffset, Either String e) (ByteOffset, a))
conduitGet1 g base_offset = do
  go $ runGetIncremental base_offset g
  where
    go (G.Partial p) = do
      inp <- await
      go $ p inp
    go (G.Done unused offset result) = do
      yield result
      if SB.null unused
        then return ()
        else leftover unused
      return $ Right (offset, result)
    go (G.Fail _ offset err) = do
      return $ Left (offset, err)

conduitGetN :: (Monad m, Num n) => Get e a -> (ByteOffset, n) -> ConduitM S.ByteString a m (Either (ByteOffset, Either String e) (ByteOffset, n))
conduitGetN g (base_offset, n) = do
  go $ runGetIncremental base_offset g
  where
    go (G.Partial p) = do
      inp <- await
      go $ p inp
    go (G.Done unused offset result) = do
      yield result
      if SB.null unused
        then return ()
        else leftover unused
      return $ Right (offset, n + 1)
    go (G.Fail _ offset err) = do
      return $ Left (offset, err)

conduitRepeat :: Monad m => a -> (a -> ConduitM seq r m (Either e a)) -> ConduitM seq r m (Either e a)
conduitRepeat a0 produce =
  go a0
  where
    go an = do
      end <- N.null
      if end
        then return $ Right an
        else do
          p <- produce an
          case p of
            Left err -> return $ Left err
            Right an_1 -> go an_1

conduitRepeatE :: (Monad m, MonoFoldable seq) => a -> (a -> ConduitM seq r m (Either e a)) -> ConduitM seq r m (Either e a)
conduitRepeatE a0 produce =
  go a0
  where
    go an = do
      end <- N.nullE
      if end
        then return $ Right an
        else do
          p <- produce an
          case p of
            Left err -> return $ Left err
            Right an_1 -> go an_1

disassembly :: Monad m => ConduitM S.ByteString Text m (Either (ByteOffset, Either String String) ())
disassembly = runExceptT $ do
  (h, _) <- (hoistEither =<<) $ lift $ mapOutput (const writeT3FileSignature) $ conduitGet1 getT3FileSignature 0
  (r, (_, items_count)) <- (hoistEither =<<) $ lift $ mapOutput (writeT3FileHeader . fst) $ conduitGet1 getT3FileHeader h
  (f, n) <- (hoistEither =<<) $ lift $ mapOutput writeT3Record $ conduitRepeatE (r, 0) $ conduitGetN getT3Record
  if n /= items_count
    then hoistEither $ Left (f, Right $ "Records count mismatch: " ++ show items_count ++ " expected, but " ++ show n ++ " readed.")
    else return ()
