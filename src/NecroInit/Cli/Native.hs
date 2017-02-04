{-# LANGUAGE ForeignFunctionInterface #-}
module NecroInit.Cli.Native where

#include <haskell>
import Paths_necromancy_tool
import Control.Error.Extensions
import Data.Tes3
import Data.Tes3.Get
import Data.Tes3.Put
#ifdef mingw32_HOST_OS
import Foreign.C.Types
#endif

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
  , optHairsPlugin :: String
  , optShowVersion :: Bool
  , optShowHelp :: Bool
  }

defaultNecroInitOptions :: NecroInitOptions
defaultNecroInitOptions = NecroInitOptions
  { optRootDir = Nothing
  , optHairsPlugin = "A1_Necromancy_Hairs.esp"
  , optShowVersion = False
  , optShowHelp = False
  }

necroInitOptionsDescr :: [OptDescr (NecroInitOptions -> NecroInitOptions)]
necroInitOptionsDescr =
  [ Option ['i'] ["install-path"] (ReqArg (\i o -> o {optRootDir = Just i}) "DIR") "the game install path"
  , Option ['H'] ["hairs-plugin"] (ReqArg (\p o -> o {optHairsPlugin = p}) "NAME") "hairs plugin file name"
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
      | otherwise = printErrorsAndExit necroInitErrorText $ necroInitRun (optRootDir options) (optHairsPlugin options)

printErrorsAndExit :: (e -> String) -> ExceptT e IO () -> IO ()
printErrorsAndExit error_text action = do
  result <- runExceptT action
  case result of
    Left e -> do
      handle (\x -> let _ = x :: IOError in return ()) $ hPutStrLn stderr $ error_text e
      stop
      exitFailure
    Right _ -> do
      stop
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

stop :: IO ()
stop = do
  hPutStrLn stderr "Press any key..."
  void $ c_getch

foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
#else
dir :: String
dir = "/"

getFullPath :: Maybe String -> FilePath -> FilePath
getFullPath Nothing path = path
getFullPath (Just game_dir) path
  | null game_dir = path
  | game_dir == "/" = "/" ++ path
  | otherwise = game_dir ++ "/" ++ path

stop :: IO ()
stop = return ()
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
npcsDir = dataFiles ++ "MWSE"

npcsFiles :: String
npcsFiles = npcsDir ++ dir ++ "A1NPC_"

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

necroInitRun :: Maybe String -> String -> ExceptT IOError IO ()
necroInitRun game_dir plugin_name = do
  file_names <- getGameFiles game_dir
  files <- (sort <$>) $ forM file_names $ getGameFileData game_dir
  tryIO $ createDirectoryIfMissing True $ getFullPath game_dir npcsDir
  hr <- scanNPCs game_dir files
  hm <- scanBodyParts game_dir files $ M.fromList [(fst x, Nothing) | x <- M.toList hr]
  generateHairsPlugin game_dir plugin_name $ M.map mapModl hm
  where
    mapModl :: Maybe Text -> Text
    mapModl Nothing = "b\\B_N_Wood Elf_M_Hair_06.NIF"
    mapModl (Just x) = x

scanNPCs :: Maybe String -> [GameFile] -> ExceptT IOError IO (Map Text ())
scanNPCs game_dir files =
  go files M.empty
  where
    go [] hs = return hs
    go (file : tail) hs = do
      let file_path = getFullPath game_dir (dataFiles ++ fileName file)
      hr <- bracketE (tryIO $ openBinaryFile file_path ReadMode) (tryIO . hClose) $ \h -> do
        (a, b) <- runConduit $ (N.sourceHandle h =$= t3RecordsSource (fileName file)) `fuseBoth` npcsSink game_dir hs
        case (a, b) of
          (Nothing, Right hn) -> return hn
          (Just e, _) -> throwE e
          (_, Left e) -> throwE e
      go tail hr

scanBodyParts :: Maybe String -> [GameFile] -> Map Text (Maybe Text) -> ExceptT IOError IO (Map Text (Maybe Text))
scanBodyParts game_dir files p =
  go files p
  where
    go [] hs = return hs
    go (file : tail) hs = do
      let file_path = getFullPath game_dir (dataFiles ++ fileName file)
      hr <- bracketE (tryIO $ openBinaryFile file_path ReadMode) (tryIO . hClose) $ \h -> do
        (a, b) <- runConduit $ (N.sourceHandle h =$= t3RecordsSource (fileName file)) `fuseBoth` bodyPartsSink hs
        case (a, b) of
          (Nothing, Right hn) -> return hn
          (Just e, _) -> throwE e
          (_, Left e) -> throwE e
      go tail hr

generateHairsPlugin :: Maybe String -> String -> Map Text Text -> ExceptT IOError IO ()
generateHairsPlugin game_dir plugin_name hs = do
  let file_path = getFullPath game_dir $ dataFiles ++ plugin_name
  let
    file_header = putT3FileHeader $ T3FileHeader
      1067869798
      ESP
      "A1"
      ["Файл с париками, сгенерированный A1_Necromancy_init.exe. Вспомогательный плагин для A1_Necromancy.esp."]
      [ T3FileRef "Morrowind.esm\0" 79764287
      ]
  let
    plugin_header =
      [ T3Record (T3Mark SCPT) 0
        [ T3ScriptField (T3Mark SCHD) (T3ScriptHeader "A1V1_Hairs_sc" 1 1 0 96 11)
        , T3BinaryField (T3Mark SCDT) (decodeLenient $ C.pack "BgECCSBzAQAgPCA1MAUBcwEACCBzAQAgMSArJAEJAQUBcwEAAiAwBgEBCSBYIBAgPT0gMSQBCQFlPxM4AAATOGwAAjwGAQEKIGwBACA+IDUwMCQBCQEROOgDAABkPgEB")
        , T3MultilineField (T3Mark SCTX)
          [ "Begin A1V1_Hairs_sc"
          , ""
          , "short state"
          , "long temp"
          , ""
          , "if ( state < 50 )"
          , "  set state to state + 1"
          , "  return"
          , "endif"
          , ""
          , "set state to 0"
          , ""
          , "if ( MenuMode == 1 )"
          , "  return"
          , "endif"
          , ""
          , "setx temp to xGetCondition"
          , ""
          , "if (temp > 500 )"
          , "  return"
          , "endif"
          , ""
          , "xSetCondition 1000"
          , ""
          , "End"
          ]
        , T3MultiStringField (T3Mark SCVR) ["state", "temp"]
        ]
      ]
  let records = plugin_header ++ M.foldWithKey makePlugin [] hs
  let content = putT3FileSignature <> file_header <> B.concat [putT3Record x | x <- records]
  bracketE (tryIO $ openBinaryFile file_path WriteMode) (tryIO . hClose) $ \h -> do
    tryIO $ B.hPut h content
    tryIO $ hSeek h AbsoluteSeek 320
    tryIO $ B.hPut h $ runPut $ putWord32le $ fromIntegral $ length records
  tryIO $ setModificationTime file_path $ UTCTime (fromGregorian 2006 06 19) 0
  where
    makePlugin :: Text -> Text -> [T3Record] -> [T3Record]
    makePlugin n model records = makeBodyPart n model : makeArmor n : records
    makeBodyPart :: Text -> Text -> T3Record
    makeBodyPart n model =
      T3Record (T3Mark BODY) 0
        [ T3StringField (T3Mark NAME) (T.pack $ getBodyPartID n ++ ['\0'])
        , T3StringField (T3Mark MODL) (model `T.snoc` '\0')
        , T3BinaryField (T3Mark BYDT) (decodeLenient $ C.pack "AQAAAg==")
        ]
    makeArmor :: Text -> T3Record
    makeArmor n =
      T3Record (T3Mark ARMO) 0
        [ T3StringField (T3Mark NAME) (T.pack $ getArmorID n ++ ['\0'])
        , T3StringField (T3Mark MODL) "blas\\A1_hair.nif\0"
        , T3StringField (T3Mark FNAM) "Скальп\0"
        , T3StringField (T3Mark SCRI) "A1V1_Hairs_sc\0"
        , T3BinaryField (T3Mark AODT) (decodeLenient $ C.pack "AAAAAAAAgD8AAAAA6AMAAEsAAAAKAAAA")
        , T3StringField (T3Mark ITEX) "blas\\A1_paric.dds\0"
        , T3ByteField (T3Mark INDX) 1
        , T3StringField (T3Mark BNAM) (T.pack $ getBodyPartID n)
        ]

addHairs :: Map Text () -> Text -> Map Text ()
addHairs h n =
  case M.lookup n h of
    Just _ -> h
    Nothing -> M.insert n () h

bodyPartsSink :: MonadIO m => Map Text (Maybe Text) -> ConduitM T3Record Void m (Either IOError (Map Text (Maybe Text)))
bodyPartsSink hs =
  go hs
  where
  go h = do
    inp <- await
    case inp of
      Nothing -> return $ Right h
      Just (T3Record (T3Mark BODY) _ fields) -> do
        let name = fromMaybe "" $ getStringProperty (T3Mark NAME) fields
        let modl = fromMaybe "b\\B_N_Wood Elf_M_Hair_06.NIF" $ getStringProperty (T3Mark MODL) fields
        case M.lookup name h of
          Nothing -> go h
          Just _ -> go $ M.update (const $ Just $ Just modl) name h
      _ -> go h

npcsSink :: MonadIO m => Maybe String -> Map Text () -> ConduitM T3Record Void m (Either IOError (Map Text ()))
npcsSink game_dir hs =
  go hs
  where
  go h = do
    inp <- await
    case inp of
      Nothing -> return $ Right h
      Just (T3Record (T3Mark NPC_) _ fields) -> do
        let name = fromMaybe "" $ getStringProperty (T3Mark NAME) fields
        let knam = fromMaybe "" $ getStringProperty (T3Mark KNAM) fields
        let fnam = fromMaybe "" $ getStringProperty (T3Mark FNAM) fields
        let spells = getSpells fields
        let file_path = getFullPath game_dir $ npcsFiles ++ replace "-" "0" (show (getHashCode name))
        let hn = addHairs h knam
        e <- liftIO $ runExceptT $ bracketE (tryIO $ openBinaryFile file_path WriteMode) (tryIO . hClose) $ \hnpc -> do
          tryIO $ B.hPutStr hnpc $ B.pack [78, 80]
          tryIO $ B.hPutStr hnpc $ t3StringValue fnam
          tryIO $ B.hPutStr hnpc $ B.pack [0]
          tryIO $ hPutStr hnpc $ getArmorID knam
          forM_ spells $ \s -> do
            tryIO $ B.hPutStr hnpc $ B.pack [0, 83, 80]
            tryIO $ hPutStr hnpc $ T.unpack s
        case e of
          Left r -> return $ Left r
          Right _ -> go hn
      _ -> go h

getFileName :: String -> String
getFileName = intercalate "_" . take 2 . splitOn "-" . intercalate "_" . take 2 . splitOn "'" . intercalate "_" . take 4 . splitOn " "

getBodyPartID :: Text -> String
getBodyPartID = take 31 . ("A1V1B" ++) . replace " " "" . replace "'" "" . replace "_" "" . T.unpack

getArmorID :: Text -> String
getArmorID = take 31 . ("A1V1H" ++) . replace " " "" . replace "'" "" . replace "_" "" . T.unpack

getHashCode :: Text -> Int32
getHashCode = T.foldl (\h c -> h * 1664525 + fromIntegral (ord c) + 1013904223) 0

getStringProperty :: T3Sign -> [T3Field] -> Maybe Text
getStringProperty s fields =
  listToMaybe $ catMaybes $ map asStringField fields
  where
    asStringField (T3StringField ss n)
      | ss == s = Just $ T.dropWhileEnd (== '\0') n
      | otherwise = Nothing
    asStringField _ = Nothing

getSpells :: [T3Field] -> [Text]
getSpells fields =
  catMaybes $ map asStringField fields
  where
    asStringField (T3StringField (T3Mark NPCS) n) = Just $ T.dropWhileEnd (== '\0') n
    asStringField _ = Nothing

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
          end <- N.null
          if end
            then return Nothing
            else go $ runGetIncremental offset2 $ getT3Record False
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
