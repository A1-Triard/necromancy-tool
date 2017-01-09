module NecroInit.Cli.Native where

#include <haskell>
import Paths_necromancy_tool
import Control.Error.Extensions
import Data.Tes3
import Data.Tes3.Get

necroInitHelpHeader :: String
necroInitHelpHeader
  =  "Usage: A1_Necromancy_init [OPTION]...\n"
  ++ "Scan the game directory and prepare necessary data files and plugins.\n"

necroInitHelpFooter :: String
necroInitHelpFooter
  =  "\nReport bugs to <internalmike@gmail.com> (in English or Russian)."
  ++ "\nA1_Necromancy home page: <https://github.com/A1-Triard/necromancy>"

necroInitUsageErrorFooter :: String
necroInitUsageErrorFooter
  = "Try `A1_Necromancy_init --help' for more information."

data NecroInitOptions = NecroInitOptions
  { optRootDir :: Maybe String
  , optShowVersion :: Bool
  , optShowHelp :: Bool
  }

defaultNecroInitOptions :: NecroInitOptions
defaultNecroInitOptions = NecroInitOptions
  { optRootDir = Nothing
  , optShowVersion = False
  , optShowHelp = False
  }

necroInitOptionsDescr :: [OptDescr (NecroInitOptions -> NecroInitOptions)]
necroInitOptionsDescr =
  [ Option ['i'] ["install-path"] (ReqArg (\i o -> o {optRootDir = Just i}) "DIR") "the game install path"
  , Option ['V'] ["version"] (NoArg (\o -> o {optShowVersion = True})) "display the version number and exit"
  , Option ['h'] ["help"] (NoArg (\o -> o {optShowHelp = True})) "display this help and exit"
  ]

necroInitOptions :: [String] -> (NecroInitOptions, [String])
necroInitOptions args =
  let (options, tail, errors) = getOpt Permute necroInitOptionsDescr args in
  (foldl (flip id) defaultNecroInitOptions options, if null errors && not (null tail) then ["Unexpected trailing arguments.\n"] else errors)

necroInit :: IO ()
necroInit = do
  args <- getArgs
  process_options $ necroInitOptions args
  where
    process_options (options, errors)
      | optShowHelp options = putStrLn $ usageInfo necroInitHelpHeader necroInitOptionsDescr ++ necroInitHelpFooter
      | optShowVersion options = putStrLn $ "A1_Necromancy_init " ++ showVersion version
      | not $ null errors = hPutStrLn stderr $ concat errors ++ necroInitUsageErrorFooter
      | otherwise = printErrorsAndExit necroInitErrorText $ necroInitRun (optRootDir options)

printErrorsAndExit :: (e -> String) -> ExceptT e IO () -> IO ()
printErrorsAndExit error_text action = do
  result <- runExceptT action
  case result of
    Left e -> do
      handle (\x -> let _ = x :: IOError in return ()) $ hPutStrLn stderr $ error_text e
      exitFailure
    Right _ ->
      exitSuccess

necroInitErrorText :: IOError -> String
necroInitErrorText e
  | isUserError e = ioeGetErrorString e
  | isDoesNotExistError e = fromMaybe "" (ioeGetFileName e) ++ ": No such file or directory"
  | otherwise = ioeGetErrorString e

#ifdef mingw32_HOST_OS
dir :: String
dir = "\\"

getFullPath :: Maybe String -> FilePath -> FilePath
getFullPath Nothing path = path
getFullPath (Just game_dir) path =
  let game_path = process game_dir in
  if null game_path then path else game_path ++ "\\" ++ path
  where
    process = removeTrailingBackslashes . removeQuotes
    removeQuotes s
      | startswith "\"" s && endswith "\"" s = take (length s - 2) $ drop 1 $ s
      | otherwise = s
    removeTrailingBackslashes s =
      fromMaybe "" $ listToMaybe $ dropWhile (endswith "\\") $ iterate (\x -> take (length x - 1) x) s
#else
dir :: String
dir = "/"

getFullPath :: Maybe String -> FilePath -> FilePath
getFullPath Nothing path = path
getFullPath (Just game_dir) path
  | null game_dir = path
  | game_dir == "/" = "/" ++ path
  | otherwise = game_dir ++ "/" ++ path
#endif

fromCP1251 :: String -> String
fromCP1251 =
  map (\c -> fromMaybe c $ lookup c table)
  where
    table = zip ['\192'..'\255'] ['А'..'я']

catchIniParserError :: CF.CPError -> IOError
catchIniParserError (CF.ParseError s, loc) = userError $ "Invalid Morrrowind.ini (" ++ s ++ ", " ++ loc ++ ")."
catchIniParserError (CF.NoSection s, loc) = userError $ "Cannot find section " ++ s ++ " in Morrrowind.ini (" ++ loc ++ ")."
catchIniParserError (CF.NoOption s, loc) = userError $ "Cannot find option " ++ s ++ " in Morrrowind.ini (" ++ loc ++ ")."
catchIniParserError (_, loc) = userError $ "Unknown error while parsing Morrrowind.ini (" ++ loc ++ ")."

gameFilesSection :: String
gameFilesSection = "Game Files"

dataFiles :: String
dataFiles = "Data Files" ++ dir

npcsDir :: String
npcsDir = dataFiles ++ "NPCs"

npcsFiles :: String
npcsFiles = npcsDir ++ dir

data GameFile = GameFile
  { fileName :: String
  , fileTime :: UTCTime
  }

isMasterFile :: GameFile -> Bool
isMasterFile = endswith ".esm" . map toLower . fileName

isPluginFile :: GameFile -> Bool
isPluginFile = endswith ".esp" . map toLower . fileName

instance Eq GameFile where
  a == b = fileName a == fileName b

instance Ord GameFile where
  a <= b
    | isMasterFile a && isPluginFile b = True
    | isPluginFile a && isMasterFile b = False
    | fileTime a < fileTime b = True
    | fileTime a > fileTime b = False
    | otherwise = fileName a <= fileName b

getGameFiles :: Maybe String -> ExceptT IOError IO [String]
getGameFiles game_dir = do
  let ini_path = getFullPath game_dir "Morrowind.ini"
  bracketE (tryIO $ openFile ini_path ReadMode) (tryIO . hClose) $ \ini_file -> do
    tryIO $ hSetEncoding ini_file char8
    ini <- withExceptT catchIniParserError $ (hoistEither =<<) $ lift $ CF.readhandle CF.emptyCP { CF.optionxform = id } ini_file
    ini_files <- withExceptT catchIniParserError $ hoistEither $ CF.options ini gameFilesSection
    raw_files <- forM ini_files $ withExceptT catchIniParserError . hoistEither . CF.simpleAccess ini gameFilesSection
    return [fromCP1251 f | f <- raw_files]

getGameFileData :: Maybe String -> String -> ExceptT IOError IO GameFile
getGameFileData game_dir file_name = do
  m <- tryIO $ getModificationTime $ getFullPath game_dir $ dataFiles ++ file_name
  return $ GameFile file_name m

necroInitRun :: Maybe String -> ExceptT IOError IO ()
necroInitRun game_dir = do
  file_names <- getGameFiles game_dir
  files <- (sort <$>) $ forM file_names $ getGameFileData game_dir
  tryIO $ createDirectoryIfMissing True $ getFullPath game_dir npcsDir
  forM_ files $ \file -> do
    let file_path = getFullPath game_dir (dataFiles ++ fileName file)
    bracketE (tryIO $ openBinaryFile file_path ReadMode) (tryIO . hClose) $ \h -> do
      (a, b) <- runConduit $ (N.sourceHandle h =$= t3RecordsSource (fileName file)) `fuseBoth` t3RecordsSink game_dir (Hairs V.empty M.empty)
      case (a, b) of
        (Nothing, Right _) -> return ()
        (Just e, _) -> throwE e
        (_, Left e) -> throwE e

data Hairs = Hairs
  { hairs :: Vector String
  , hairsIndex :: Map String Int
  }

addHairs :: Hairs -> String -> (Int, Hairs)
addHairs h n =
  case M.lookup n (hairsIndex h) of
    Just i -> (i, h)
    Nothing -> (V.length (hairs h), Hairs (hairs h `V.snoc` n) (M.insert n (V.length (hairs h)) (hairsIndex h)))

t3RecordsSink :: MonadIO m => Maybe String -> Hairs -> ConduitM T3Record Void m (Either IOError Hairs)
t3RecordsSink game_dir hs =
  go hs
  where
  go h = do
    inp <- await
    case inp of
      Nothing -> return $ Right h
      Just (T3Record (T3Mark NPC_) _ fields) -> do
        case nameAndHairs fields of
          Just (name, knam) -> do
            let file_path = getFullPath game_dir $ npcsFiles ++ T.unpack name
            let (i, hn) = addHairs h (T.unpack knam)
            e <- liftIO $ tryIOError $ writeFile file_path $ "A1V1_Hairs_" ++ show i
            case e of
              Left r -> return $ Left r
              Right _ -> go hn
          Nothing -> go h
      _ -> go h

nameAndHairs :: [T3Field] -> Maybe (Text, Text)
nameAndHairs fields =
  let name = listToMaybe $ catMaybes $ map asName fields in
  let knam = listToMaybe $ catMaybes $ map asKnam fields in
  case (name, knam) of
    (Just n, Just k) -> Just (n, k)
    _ -> Nothing
  where
    asName (T3StringField (T3Mark NAME) n) = Just $ T.dropWhileEnd (== '\0') n
    asName _ = Nothing
    asKnam (T3StringField (T3Mark KNAM) n) = Just $ T.dropWhileEnd (== '\0') n
    asKnam _ = Nothing

t3RecordsSource :: Monad m => String -> ConduitM S.ByteString T3Record m (Maybe IOError)
t3RecordsSource file_name = do
  r1 <- skipGet file_name 0 getT3FileSignature
  case r1 of
    Left e -> return $ Just e
    Right offset1 -> do
      r2 <- skipGet file_name offset1 getT3FileHeader
      case r2 of
        Left e -> return $ Just e
        Right offset2 -> do
          go $ runGetIncremental offset2 $ getT3Record False
  where
    go (G.Partial p) = do
      inp <- await
      go $ p inp
    go (G.Done unused offset result) = do
      if SB.null unused
        then return ()
        else leftover unused
      yield result
      end <- N.null
      if end
        then return Nothing
        else go $ runGetIncremental offset $ getT3Record False
    go (G.Fail _ offset err) = do
      return $ Just $ formatError file_name offset err

skipGet :: Monad m => String -> ByteOffset -> Get String a -> ConduitM S.ByteString r m (Either IOError ByteOffset)
skipGet file_name base_offset g = do
  go $ runGetIncremental base_offset g
  where
    go (G.Partial p) = do
      inp <- await
      go $ p inp
    go (G.Done unused offset _) = do
      if SB.null unused
        then return ()
        else leftover unused
      return $ Right offset
    go (G.Fail _ offset err) = do
      return $ Left $ formatError file_name offset err

formatError :: String -> ByteOffset -> Either String String -> IOError
formatError name offset (Right e) = userError $ name ++ ": " ++ replace "{0}" (showHex offset "h") e
formatError name offset (Left e) = userError $ name ++ ": " ++ "Internal error: " ++ showHex offset "h: " ++ e

{-
withBinaryInputFile :: FilePath -> (Handle -> ExceptT IOError IO a) -> ExceptT IOError IO a
withBinaryInputFile "-" action = action stdin
withBinaryInputFile name action = bracketE (tryIO $ openBinaryFile name ReadMode) (tryIO . hClose) action

withBinaryOutputFile :: FilePath -> (Handle -> ExceptT IOError IO a) -> ExceptT IOError IO a
withBinaryOutputFile "-" action = action stdout
withBinaryOutputFile name action = bracketE (tryIO $ openBinaryFile name WriteMode) (tryIO . hClose) action

withTextInputFile :: FilePath -> (Handle -> ExceptT IOError IO a) -> ExceptT IOError IO a
withTextInputFile "-" action = action stdin
withTextInputFile name action = bracketE (tryIO $ openFile name ReadMode) (tryIO . hClose) action

withTextOutputFile :: FilePath -> (Handle -> ExceptT IOError IO a) -> ExceptT IOError IO a
withTextOutputFile "-" action = action stdout
withTextOutputFile name action = bracketE (tryIO $ openFile name WriteMode) (tryIO . hClose) action

handleT3Error :: FilePath -> String -> IOError
handleT3Error name e = userError $ showString name $ showString ": " e

necroInitDisassembly :: Bool -> (T3Sign -> Bool) -> Verboser -> FilePath -> ExceptT IOError IO ()
necroInitDisassembly adjust skip_record verbose name = do
  output_name <- hoistEither $ getDisassembliedFileName name
  tryIO $ verbose $ name ++ " -> " ++ output_name
  r <-
    withBinaryInputFile name $ \input -> do
      withTextOutputFile output_name $ \output -> do
        runConduit $ (N.sourceHandle input =$= disassembly adjust skip_record) `fuseUpstream` N.sinkHandle output
  case r of
    Right _ -> return ()
    Left (offset, err) -> do
      tryIO $ removeFile output_name
      case err of
        Right e -> throwE $ userError $ name ++ ": " ++ replace "{0}" (showHex offset "h") e
        Left e -> throwE $ userError $ name ++ ": " ++ "Internal error: " ++ showHex offset "h: " ++ e

necroInitAssemblyErrorText :: IOError -> String
necroInitAssemblyErrorText e
  | isUserError e = ioeGetErrorString e
  | otherwise = ioeGetErrorString e

necroInitAssembly :: Verboser -> FilePath -> ExceptT IOError IO ()
necroInitAssembly verbose name = do
  output_name <- hoistEither $ getAssembliedFileName name
  tryIO $ verbose $ name ++ " -> " ++ output_name ++ ".es?"
  r <-
    withTextInputFile name $ \input -> do
      withBinaryOutputFile (output_name ++ ".es_") $ \output -> do
        r <- runConduit $ (N.sourceHandle input =$= assembly) `fuseUpstream` N.sinkHandle output
        case r of
          Left e -> return $ Left e
          Right (file_type, n) -> do
            tryIO $ hSeek output AbsoluteSeek 320
            tryIO $ B.hPut output $ runPut $ putWord32le n
            return $ Right file_type
  case r of
    Left e -> do
      tryIO $ removeFile (output_name ++ ".es_")
      throwE $ userError $ name ++ ": " ++ "Parse error: " ++ e
    Right file_type -> do
      tryIO $ renameFile (output_name ++ ".es_") (output_name ++ fileSuffix file_type)
-}
