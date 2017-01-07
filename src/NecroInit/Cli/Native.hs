module NecroInit.Cli.Native where

#include <haskell>
import Paths_necromancy_tool
import Control.Error.Extensions
import Data.Tes3
import Data.Tes3.Disassembler
import Data.Tes3.Assembler

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
getFullPath :: Maybe String -> FilePath -> FilePath
getFullPath Nothing path = path
getFullPath (Just game_dir) path
  | null game_dir = path
  | game_dir == "/" = "/" ++ path
  | otherwise = game_dir ++ "/" ++ path
#endif

catchIniParserError :: CF.CPError -> IOError
catchIniParserError (CF.ParseError s, loc) = userError $ "Invalid Morrrowind.ini (" ++ s ++ ", " ++ loc ++ ")."
catchIniParserError (CF.NoSection s, loc) = userError $ "Cannot find section " ++ s ++ " in Morrrowind.ini (" ++ loc ++ ")."
catchIniParserError (CF.NoOption s, loc) = userError $ "Cannot find option " ++ s ++ " in Morrrowind.ini (" ++ loc ++ ")."
catchIniParserError (_, loc) = userError $ "Unknown error while parsing Morrrowind.ini (" ++ loc ++ ")."

gameFilesSection :: String
gameFilesSection = "Game Files"

necroInitRun :: Maybe String -> ExceptT IOError IO ()
necroInitRun game_dir = do
  let ini_path = getFullPath game_dir "Morrowind.ini"
  bracketE (tryIO $ openFile ini_path ReadMode) (tryIO . hClose) $ \ini_file -> do
    tryIO $ hSetEncoding ini_file char8
    ini <- withExceptT catchIniParserError $ (hoistEither =<<) $ lift $ CF.readhandle CF.emptyCP { CF.optionxform = id } ini_file
    ini_files <- withExceptT catchIniParserError $ hoistEither $ CF.options ini gameFilesSection
    files <- forM ini_files $ withExceptT catchIniParserError . hoistEither . CF.simpleAccess ini gameFilesSection
    tryIO $ putStrLn $ intercalate "\n" files




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
