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
  , optPlugin :: String
  , optShowVersion :: Bool
  , optShowHelp :: Bool
  }

defaultNecroInitOptions :: NecroInitOptions
defaultNecroInitOptions = NecroInitOptions
  { optRootDir = Nothing
  , optPlugin = "A1_Hairs_V1.esp"
  , optShowVersion = False
  , optShowHelp = False
  }

necroInitOptionsDescr :: [OptDescr (NecroInitOptions -> NecroInitOptions)]
necroInitOptionsDescr =
  [ Option ['i'] ["install-path"] (ReqArg (\i o -> o {optRootDir = Just i}) "DIR") "the game install path"
  , Option ['p'] ["plugin-name"] (ReqArg (\p o -> o {optPlugin = p}) "NAME") "the plugin file name"
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
      | otherwise = printErrorsAndExit necroInitErrorText $ necroInitRun (optRootDir options) (optPlugin options)

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
  hm <- scanBodyParts game_dir files $ M.fromList [(x, Nothing) | x <- V.toList hr]
  generateHairsPlugin game_dir plugin_name $ V.map (mapModl hm) hr
  where
    mapModl :: Map Text (Maybe Text) -> Text -> Text
    mapModl m h =
      case M.lookup h m of
        Nothing -> error "mapModl"
        Just Nothing -> "b\\B_N_Wood Elf_M_Hair_06.NIF"
        Just (Just x) -> x

scanNPCs :: Maybe String -> [GameFile] -> ExceptT IOError IO (Vector Text)
scanNPCs game_dir files =
  hairs <$> go files (Hairs V.empty M.empty)
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

generateHairsPlugin :: Maybe String -> String -> Vector Text -> ExceptT IOError IO ()
generateHairsPlugin game_dir plugin_name hs = do
  let file_path = getFullPath game_dir $ dataFiles ++ plugin_name
  let
    file_header = putT3FileHeader $ T3FileHeader
      1067869798
      ESP
      "A1"
      ["Файл с париками, сгенерированный A1_Necromancy_init.exe."]
      [ T3FileRef "Morrowind.esm\0" 79764287
      ]
  let records = V.ifoldl makePlugin [] hs
  let content = putT3FileSignature <> file_header <> B.concat [putT3Record x | x <- records]
  bracketE (tryIO $ openBinaryFile file_path WriteMode) (tryIO . hClose) $ \h -> do
    tryIO $ B.hPut h content
    tryIO $ hSeek h AbsoluteSeek 320
    tryIO $ B.hPut h $ runPut $ putWord32le $ fromIntegral $ length records
  tryIO $ setModificationTime file_path $ UTCTime (fromGregorian 2006 06 19) 0
  where
    makePlugin :: [T3Record] -> Int -> Text -> [T3Record]
    makePlugin records i n = makeBodyPart i n : makeArmor i : records
    makeBodyPart :: Int -> Text -> T3Record
    makeBodyPart i n =
      T3Record (T3Mark BODY) 0
        [ T3StringField (T3Mark NAME) (T.pack $ hairsBodyPrefix ++ show i ++ ['\0'])
        , T3StringField (T3Mark MODL) (n `T.snoc` '\0')
        , T3BinaryField (T3Mark BYDT) (decodeLenient $ C.pack "AQAAAg==")
        ]
    makeArmor :: Int -> T3Record
    makeArmor i =
      T3Record (T3Mark ARMO) 0
        [ T3StringField (T3Mark NAME) (T.pack $ hairsPrefix ++ show i ++ ['\0'])
        , T3StringField (T3Mark MODL) "m\\misc_com_broom_01.nif\0"
        , T3StringField (T3Mark FNAM) "Волосы\0"
        , T3BinaryField (T3Mark AODT) (decodeLenient $ C.pack "AAAAAM3MzD0AAAAAZAAAABkAAAAKAAAA")
        , T3StringField (T3Mark ITEX) "m\\misc_com_broom_01.dds\0"
        , T3ByteField (T3Mark INDX) 1
        , T3StringField (T3Mark BNAM) (T.pack $ hairsBodyPrefix ++ show i)
        ]

data Hairs = Hairs
  { hairs :: Vector Text
  , hairsIndex :: Map Text Int
  }

addHairs :: Hairs -> Text -> (Int, Hairs)
addHairs h n =
  case M.lookup n (hairsIndex h) of
    Just i -> (i, h)
    Nothing -> (V.length (hairs h), Hairs (hairs h `V.snoc` n) (M.insert n (V.length (hairs h)) (hairsIndex h)))

hairsPrefix :: String
hairsPrefix = "A1V1_Hairs_"

hairsBodyPrefix :: String
hairsBodyPrefix = "A1V1_HairsBP_"

bodyPartsSink :: MonadIO m => Map Text (Maybe Text) -> ConduitM T3Record Void m (Either IOError (Map Text (Maybe Text)))
bodyPartsSink hs =
  go hs
  where
  go h = do
    inp <- await
    case inp of
      Nothing -> return $ Right h
      Just (T3Record (T3Mark BODY) _ fields) -> do
        case getProperty (T3Mark MODL) fields of
          Just (name, modl) -> do
            case M.lookup name h of
              Nothing -> go h
              Just _ -> go $ M.update (const $ Just $ Just modl) name h
          Nothing -> go h
      _ -> go h

npcsSink :: MonadIO m => Maybe String -> Hairs -> ConduitM T3Record Void m (Either IOError Hairs)
npcsSink game_dir hs =
  go hs
  where
  go h = do
    inp <- await
    case inp of
      Nothing -> return $ Right h
      Just (T3Record (T3Mark NPC_) _ fields) -> do
        case getProperty (T3Mark KNAM) fields of
          Just (name, knam) -> do
            let file_path = getFullPath game_dir $ npcsFiles ++ T.unpack name
            let (i, hn) = addHairs h knam
            e <- liftIO $ tryIOError $ writeFile file_path $ hairsPrefix ++ show i
            case e of
              Left r -> return $ Left r
              Right _ -> go hn
          Nothing -> go h
      _ -> go h

getProperty :: T3Sign -> [T3Field] -> Maybe (Text, Text)
getProperty s fields =
  let name = listToMaybe $ catMaybes $ map asName fields in
  let knam = listToMaybe $ catMaybes $ map asKnam fields in
  case (name, knam) of
    (Just n, Just k) -> Just (n, k)
    _ -> Nothing
  where
    asName (T3StringField (T3Mark NAME) n) = Just $ T.dropWhileEnd (== '\0') n
    asName _ = Nothing
    asKnam (T3StringField ss n)
      | ss == s = Just $ T.dropWhileEnd (== '\0') n
      | otherwise = Nothing
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
