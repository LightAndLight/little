{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (many, optional, some, (<**>), (<|>))
import Control.Exception (throwIO)
import Control.Lens.Cons (_head, _last)
import Control.Lens.Setter (over)
import Control.Lens.TH (makePrisms)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState, modify)
import Control.Monad.State.Strict (runState)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Foldable (fold, traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Void (Void, absurd)
import qualified Options.Applicative as Options
import qualified System.Directory as Directory
import qualified System.IO
import qualified System.IO.Temp as Temporary
import qualified System.Process as Process
import qualified Xeno.DOM as Xeno

data Cli
    = Doc {docInput :: FilePath, docOutput :: Maybe FilePath}
    | Code {codeInput :: FilePath, codeOutput :: Maybe FilePath, codeExec :: Maybe String}

docParser :: Options.Parser Cli
docParser =
    Doc
        <$> Options.strArgument (Options.metavar "FILE")
        <*> optional (Options.strOption (Options.long "output" <> Options.short 'o' <> Options.metavar "FILE"))

codeParser :: Options.Parser Cli
codeParser =
    Code
        <$> Options.strArgument (Options.metavar "FILE")
        <*> optional (Options.strOption (Options.long "output" <> Options.short 'o' <> Options.metavar "DIR"))
        <*> optional (Options.strOption (Options.long "exec" <> Options.metavar "command"))

cliParser :: Options.Parser Cli
cliParser =
    Options.subparser (Options.command "doc" $ Options.info (docParser <**> Options.helper) Options.fullDesc)
        <|> Options.subparser (Options.command "code" $ Options.info (codeParser <**> Options.helper) Options.fullDesc)

data Path = Path FilePath [String]
    deriving (Eq, Show)

renderPath :: Path -> String
renderPath (Path file fragments) = file <> foldMap (\fragment -> ":" <> fragment) fragments

data Content a
    = CText ByteString
    | CFragment String
    | CExtend !a
    deriving (Eq, Show)

$(makePrisms ''Content)

data RunContent
    = Command
    | Output
    deriving (Eq, Show)

data PatchContent
    = PPath
    | PAction
    | PCode [Content Void]
    deriving (Eq, Show)

data Patch
    = Append Path [Content PatchContent]
    | Create Path [Content PatchContent]
    deriving (Eq, Show)

data Block
    = Text ByteString
    | Patch Patch
    | Run {runCommand :: [String], runContents :: [Content RunContent]}
    deriving (Eq, Show)

$(makePrisms ''Block)

newtype Document
    = Document [Block]
    deriving (Eq, Show)

renderContent :: (a -> String) -> Content a -> String
renderContent f content =
    case content of
        CText txt -> ByteString.Char8.unpack txt
        CFragment name -> "<<" <> name <> ">>"
        CExtend extend -> f extend

unescape :: ByteString -> ByteString
unescape input =
    case Parser.parseOnly go input of
        Left err -> error $ "escape sequence parse error: " <> err
        Right output -> output
  where
    go =
        (\prefix -> maybe prefix (prefix <>)) <$> Parser.takeWhile (/= '&')
            <*> optional ((<>) <$> escape <*> go)

    escape =
        Parser.char '&'
            *> ( "<" <$ Parser.string "lt"
                    <|> ">" <$ Parser.string "gt"
               )
                <* Parser.char ';'

decodeDocument :: Monad m => Xeno.Node -> m Document
decodeDocument document =
    case Xeno.name document of
        "document" ->
            Document <$> decodeBlocks document
        name ->
            error $ "expecting node " <> show ("document" :: String) <> ", got node " <> show name
  where
    decodeBlocks :: Monad m => Xeno.Node -> m [Block]
    decodeBlocks node =
        over
            (_last . _Text)
            (\txt -> Maybe.fromMaybe txt (ByteString.stripSuffix "\n" txt))
            . over
                (_head . _Text)
                (\txt -> Maybe.fromMaybe txt (ByteString.stripPrefix "\n" txt))
            <$> traverse decodeBlock (Xeno.contents node)

    decodeBlock :: Monad m => Xeno.Content -> m Block
    decodeBlock content =
        case content of
            Xeno.Text txt ->
                pure . Text $ unescape txt
            Xeno.Element element ->
                case Xeno.name element of
                    "patch" -> decodePatch element
                    "run" -> decodeRun element
                    name -> error $ "unexpected node " <> show name
            Xeno.CData txt ->
                pure $ Text txt

    decodePatchContent :: Monad m => Xeno.Node -> m PatchContent
    decodePatchContent node =
        case Xeno.name node of
            "path" ->
                case Xeno.contents node of
                    [] -> pure PPath
                    contents -> error $ "unexpected contents " <> show contents
            "action" ->
                case Xeno.contents node of
                    [] -> pure PAction
                    contents -> error $ "unexpected contents " <> show contents
            "code" ->
                PCode
                    <$> decodeContents
                        (error . ("unexpected node " <>) . show . Xeno.name)
                        node
            _ -> error $ "unexpected node " <> show (Xeno.name node)

    decodePatch :: Monad m => Xeno.Node -> m Block
    decodePatch patch = do
        action <- decodeAction patch
        Patch
            <$> case action of
                "append" ->
                    Append
                        <$> decodePath patch
                        <*> decodeContents decodePatchContent patch
                "create" ->
                    Create
                        <$> decodePath patch
                        <*> decodeContents decodePatchContent patch
                _ -> error $ "invalid action " <> show action

    decodeRunContent :: Monad m => Xeno.Node -> m RunContent
    decodeRunContent node =
        case Xeno.name node of
            "command" ->
                pure Command
            "output" ->
                pure Output
            name -> error $ "unexpected node " <> show name

    decodeCommand :: Applicative m => Xeno.Node -> m String
    decodeCommand patch =
        case List.find (("command" ==) . fst) (Xeno.attributes patch) of
            Nothing -> error $ "missing attribute " <> show ("command" :: String)
            Just (_, value) -> pure $ ByteString.Char8.unpack value

    decodeRun :: Monad m => Xeno.Node -> m Block
    decodeRun run =
        Run
            <$> fmap words (decodeCommand run)
            <*> decodeContents decodeRunContent run

    decodeAction :: Applicative m => Xeno.Node -> m ByteString
    decodeAction patch =
        case List.find (("action" ==) . fst) (Xeno.attributes patch) of
            Nothing -> error $ "missing attribute " <> show ("action" :: String)
            Just (_, value) -> pure value

    pathParser :: Parser Path
    pathParser =
        Path
            <$> some
                ( '\\' <$ Parser.string "\\\\"
                    <|> ':' <$ Parser.string "\\:"
                    <|> Parser.satisfy (`notElem` ['\\', ':'])
                )
            <*> many
                ( Parser.char ':'
                    *> some
                        ( '\\' <$ Parser.string "\\\\"
                            <|> ':' <$ Parser.string "\\:"
                            <|> Parser.satisfy (`notElem` ['\\', ':'])
                        )
                )

    decodePath :: Applicative m => Xeno.Node -> m Path
    decodePath node =
        case List.find (("path" ==) . fst) (Xeno.attributes node) of
            Nothing -> error $ "missing attribute " <> show ("path" :: String)
            Just (_, value) ->
                case Parser.parseOnly pathParser value of
                    Left err -> error $ "path parse error: " <> err
                    Right path -> pure path

    decodeContents :: Applicative m => (Xeno.Node -> m a) -> Xeno.Node -> m [Content a]
    decodeContents f contentNode =
        over
            (_head . _CText)
            (\txt -> Maybe.fromMaybe txt (ByteString.stripPrefix "\n" txt))
            . over
                (_last . _CText)
                (\txt -> Maybe.fromMaybe txt (ByteString.stripSuffix "\n" txt))
            <$> traverse (decodeContent f) (Xeno.contents contentNode)

    decodeContent :: Applicative m => (Xeno.Node -> m a) -> Xeno.Content -> m (Content a)
    decodeContent f content =
        case content of
            Xeno.Text txt -> pure . CText $ unescape txt
            Xeno.Element element ->
                case Xeno.name element of
                    "fragment" -> CFragment <$> decodeFragmentName element
                    _ -> CExtend <$> f element
            Xeno.CData txt ->
                pure $ CText txt

    decodeFragmentName :: Applicative m => Xeno.Node -> m String
    decodeFragmentName node =
        case List.find (("name" ==) . fst) (Xeno.attributes node) of
            Nothing -> error $ "missing attrbute " <> show ("name" :: String)
            Just (_, value) -> pure $ ByteString.Char8.unpack value

renderRunContent :: String -> String -> RunContent -> String
renderRunContent cmd output content =
    case content of
        Command ->
            cmd
        Output ->
            output

renderPatchContent :: String -> String -> PatchContent -> String
renderPatchContent path action content =
    case content of
        PPath -> path
        PAction -> action
        PCode cs ->
            foldMap (renderContent absurd) cs

renderBlock :: MonadIO m => Block -> m String
renderBlock block =
    case block of
        Text txt ->
            pure $ ByteString.Char8.unpack txt
        Patch patch ->
            case patch of
                Append path content ->
                    pure $
                        foldMap
                            (renderContent $ renderPatchContent (renderPath path) "+=")
                            content
                Create path content ->
                    pure $
                        foldMap
                            (renderContent $ renderPatchContent (renderPath path) "=")
                            content
        Run cmd content -> do
            output <- liftIO $ Process.readProcess (head cmd) (tail cmd) ""
            pure $ foldMap (renderContent $ renderRunContent (unwords cmd) output) content

toDoc :: MonadIO m => Document -> m String
toDoc (Document blocks) =
    fold <$> traverse renderBlock blocks

runDoc :: FilePath -> Maybe FilePath -> IO ()
runDoc file mOutFile = do
    content <-
        stripShebang
            <$> case file of
                "-" -> ByteString.hGetContents System.IO.stdin
                _ -> ByteString.readFile file
    dom <- either throwIO pure (Xeno.parse content)
    document <- decodeDocument dom
    let docString = toDoc document
    case mOutFile of
        Nothing -> putStrLn =<< docString
        Just outFile -> (writeFile outFile =<< docString) *> putStrLn ("Wrote " <> outFile)

newtype CodeState = CodeState
    { csFiles :: HashMap FilePath CodeContent
    }

data CodeContent = CodeContent
    { ccContents :: [Content Void]
    , ccFragments :: HashMap String CodeContent
    }

codeContentToCode :: CodeContent -> String
codeContentToCode (CodeContent contents fragments) =
    go absurd contents
  where
    go :: (a -> String) -> [Content a] -> String
    go f =
        foldMap
            ( \case
                CText txt -> ByteString.Char8.unpack txt
                CFragment name ->
                    case HashMap.lookup name fragments of
                        Nothing -> error $ "fragment " <> show name <> " does not exist"
                        Just contents' -> codeContentToCode contents'
                CExtend extend ->
                    f extend
            )

create :: MonadState CodeState m => Path -> [Content Void] -> m ()
create path content =
    modify $ createCodeState path content
  where
    createCodeState :: Path -> [Content Void] -> CodeState -> CodeState
    createCodeState (Path file fragments) cs (CodeState files) =
        let codeContent = Maybe.fromMaybe (CodeContent mempty mempty) (HashMap.lookup file files)
         in CodeState $ HashMap.insert file (createCodeContent fragments cs codeContent) files

    createCodeContent :: [String] -> [Content Void] -> CodeContent -> CodeContent
    createCodeContent path' cs (CodeContent content' fragments) =
        case path' of
            [] ->
                error $ "path already has content"
            fragmentName : path'' ->
                case HashMap.lookup fragmentName fragments of
                    Nothing ->
                        case path'' of
                            [] ->
                                CodeContent
                                    content'
                                    (HashMap.insert fragmentName (CodeContent cs mempty) fragments)
                            fragmentName' : _ ->
                                error $ "fragment " <> show fragmentName' <> " does not exist"
                    Just fragment ->
                        CodeContent
                            content'
                            (HashMap.insert fragmentName (createCodeContent path'' cs fragment) fragments)

append :: MonadState CodeState m => Path -> [Content Void] -> m ()
append path content =
    modify $ appendCodeState path content
  where
    appendCodeState :: Path -> [Content Void] -> CodeState -> CodeState
    appendCodeState (Path file fragments) cs (CodeState files) =
        case HashMap.lookup file files of
            Nothing ->
                error $ "file " <> show file <> " does not exist"
            Just content' ->
                CodeState $
                    HashMap.insert file (appendCodeContent fragments cs content') files

    appendCodeContent :: [String] -> [Content Void] -> CodeContent -> CodeContent
    appendCodeContent path' cs (CodeContent content' fragments) =
        case path' of
            [] -> CodeContent (content' <> if null content' then cs else CText "\n" : cs) fragments
            fragmentName : path'' ->
                case HashMap.lookup fragmentName fragments of
                    Nothing ->
                        error $ "fragment " <> show fragmentName <> " does not exist"
                    Just fragment ->
                        CodeContent
                            content'
                            (HashMap.insert fragmentName (appendCodeContent path'' cs fragment) fragments)

toCode :: Document -> HashMap FilePath String
toCode (Document blocks) =
    let ((), CodeState files) = flip runState (CodeState mempty) $ traverse_ blockToCode blocks
     in codeContentToCode <$> files
  where
    getCode :: Applicative m => [Content PatchContent] -> m [Content Void]
    getCode cs =
        case cs of
            [] -> undefined
            c : cs' ->
                case c of
                    CExtend extend ->
                        case extend of
                            PCode content -> pure content
                            _ -> getCode cs'
                    CText _ -> getCode cs'
                    CFragment _ -> getCode cs'

    blockToCode :: MonadState CodeState m => Block -> m ()
    blockToCode block =
        case block of
            Text _ -> pure ()
            Patch patch ->
                case patch of
                    Create path content ->
                        create path =<< getCode content
                    Append path content ->
                        append path =<< getCode content
            Run _ _ -> pure ()

stripShebang :: ByteString -> ByteString
stripShebang input =
    case ByteString.Char8.stripPrefix "#!" input of
        Nothing -> input
        Just input' ->
            ByteString.Char8.drop 1 $
                ByteString.Char8.dropWhile (\c -> c /= '\n') input'

runCode :: FilePath -> Maybe FilePath -> Maybe String -> IO ()
runCode file mOutDir mExec = do
    content <-
        stripShebang
            <$> case file of
                "-" -> ByteString.hGetContents System.IO.stdin
                _ -> ByteString.readFile file
    dom <- either throwIO pure (Xeno.parse content)
    document <- decodeDocument dom
    let files = toCode document
    case mOutDir of
        Nothing ->
            case mExec of
                Nothing ->
                    HashMap.foldlWithKey
                        (\acc filename fileContent -> acc *> putStrLn ("-- " <> filename) *> putStrLn fileContent)
                        (pure ())
                        files
                Just exec ->
                    Temporary.withSystemTempDirectory "little-code" $ \tmpDir ->
                        HashMap.foldlWithKey
                            ( \acc filename fileContent -> do
                                acc
                                let path = tmpDir <> "/" <> filename
                                writeFile path fileContent
                                Process.callProcess exec [path]
                            )
                            (pure ())
                            files
        Just outDir -> do
            Directory.removeDirectoryRecursive outDir
            Directory.createDirectoryIfMissing True outDir
            HashMap.foldlWithKey
                ( \acc filename fileContent -> do
                    acc
                    let path = outDir <> "/" <> filename
                    writeFile path fileContent
                    putStrLn $ "Wrote " <> path
                    traverse_ (\exec -> Process.callProcess exec [path]) mExec
                )
                (pure ())
                files

main :: IO ()
main = do
    cli <- Options.execParser $ Options.info (cliParser <**> Options.helper) Options.fullDesc
    case cli of
        Doc file mOut -> runDoc file mOut
        Code file mOut mExec -> runCode file mOut mExec