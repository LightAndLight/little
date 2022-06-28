{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (many, optional, some, (<**>), (<|>))
import Control.Exception (throwIO)
import Control.Lens.Cons (_head, _last)
import Control.Lens.Setter (ASetter, mapped, over)
import Control.Lens.TH (makePrisms)
import Control.Monad (when)
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
import Data.Void (Void)
import qualified Options.Applicative as Options
import qualified System.Directory as Directory
import qualified Xeno.DOM as Xeno

data Cli
    = Doc {docInput :: FilePath, docOutput :: Maybe FilePath}
    | Code {codeInput :: FilePath, codeOutput :: Maybe FilePath}

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

data Patch
    = Append Path [Content Void]
    | Create Path [Content Void]
    deriving (Eq, Show)

data Block
    = Text ByteString
    | Patch Patch
    deriving (Eq, Show)

$(makePrisms ''Block)

newtype Document
    = Document [Block]
    deriving (Eq, Show)

renderContent :: Content Void -> String
renderContent content =
    case content of
        CText txt -> ByteString.Char8.unpack txt
        CFragment name -> "<<" <> name <> ">>"

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
            error $ "expecting node " <> show "document" <> ", got node " <> show name
  where
    decodeBlocks :: Monad m => Xeno.Node -> m [Block]
    decodeBlocks document =
        over
            (_last . _Text)
            (\txt -> Maybe.fromMaybe txt (ByteString.stripSuffix "\n" txt))
            . over
                (mapped . _Text)
                (\txt -> Maybe.fromMaybe txt (ByteString.stripPrefix "\n" txt))
            <$> traverse decodeBlock (Xeno.contents document)

    decodeBlock :: Monad m => Xeno.Content -> m Block
    decodeBlock content =
        case content of
            Xeno.Text txt ->
                pure . Text $ unescape txt
            Xeno.Element element ->
                decodePatch element
            Xeno.CData txt ->
                pure $ Text txt

    decodePatch :: Monad m => Xeno.Node -> m Block
    decodePatch patch =
        case Xeno.name patch of
            "patch" -> do
                action <- decodeAction patch
                Patch
                    <$> case action of
                        "append" ->
                            Append <$> decodePath patch <*> decodeContents patch
                        "create" ->
                            Create <$> decodePath patch <*> decodeContents patch
                        _ -> error $ "invalid action " <> show action
            name -> error $ "expected node " <> show "patch" <> ", got node " <> show name

    decodeAction :: Applicative m => Xeno.Node -> m ByteString
    decodeAction patch =
        case List.find (("action" ==) . fst) (Xeno.attributes patch) of
            Nothing -> error $ "missing attribute " <> show "action"
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
            Nothing -> error $ "missing attribute " <> show "path"
            Just (_, value) ->
                case Parser.parseOnly pathParser value of
                    Left err -> error $ "path parse error: " <> err
                    Right path -> pure path

    decodeContents :: Applicative m => Xeno.Node -> m [Content Void]
    decodeContents contentNode =
        over
            (_head . _CText)
            (\txt -> Maybe.fromMaybe txt (ByteString.stripPrefix "\n" txt))
            . over
                (_last . _CText)
                (\txt -> Maybe.fromMaybe txt (ByteString.stripSuffix "\n" txt))
            <$> traverse decodeContent (Xeno.contents contentNode)

    decodeContent :: Applicative m => Xeno.Content -> m (Content Void)
    decodeContent content =
        case content of
            Xeno.Text txt -> pure . CText $ unescape txt
            Xeno.Element element ->
                case Xeno.name element of
                    "fragment" -> CFragment <$> decodeFragmentName element
                    name -> error $ "expected node " <> show "fragment" <> ", got " <> show name
            Xeno.CData txt ->
                pure $ CText txt

    decodeFragmentName :: Applicative m => Xeno.Node -> m String
    decodeFragmentName node =
        case List.find (("name" ==) . fst) (Xeno.attributes node) of
            Nothing -> error $ "missing attrbute " <> show "name"
            Just (_, value) -> pure $ ByteString.Char8.unpack value

decodeDocumentFile :: FilePath -> IO Document
decodeDocumentFile file = do
    content <- ByteString.readFile file
    dom <- either throwIO pure (Xeno.parse content)
    decodeDocument dom

toDoc :: Document -> String
toDoc (Document blocks) =
    foldMap
        ( \case
            Text txt -> ByteString.Char8.unpack txt
            Patch patch ->
                case patch of
                    Append path content ->
                        unlines
                            [ "```haskell"
                            , "-- <<" <> renderPath path <> ">> +="
                            , foldMap renderContent content
                            , "```"
                            ]
                    Create path content ->
                        unlines
                            [ "```haskell"
                            , "-- <<" <> renderPath path <> ">> ="
                            , foldMap renderContent content
                            , "```"
                            ]
        )
        blocks

runDoc :: FilePath -> Maybe FilePath -> IO ()
runDoc file mOutFile = do
    document <- decodeDocumentFile file
    let docString = toDoc document
    case mOutFile of
        Nothing -> putStrLn docString
        Just outFile -> writeFile outFile docString *> putStrLn ("Wrote " <> outFile)

newtype CodeState = CodeState
    { csFiles :: HashMap FilePath CodeContent
    }

data CodeContent = CodeContent
    { ccContents :: [Content Void]
    , ccFragments :: HashMap String CodeContent
    }

renderCodeContent :: CodeContent -> String
renderCodeContent (CodeContent contents fragments) =
    foldMap
        ( \case
            CText txt -> ByteString.Char8.unpack txt
            CFragment name ->
                case HashMap.lookup name fragments of
                    Nothing -> error $ "fragment " <> show name <> " does not exist"
                    Just contents' -> renderCodeContent contents'
        )
        contents

create :: MonadState CodeState m => Path -> [Content Void] -> m ()
create path content =
    modify $ createCodeState path content
  where
    createCodeState :: Path -> [Content Void] -> CodeState -> CodeState
    createCodeState (Path file fragments) cs (CodeState files) =
        case HashMap.lookup file files of
            Nothing ->
                case fragments of
                    [] ->
                        CodeState $ HashMap.insert file (CodeContent content mempty) files
                    fragmentName : _ ->
                        error $ "fragment " <> show fragmentName <> " does not exist"
            Just content ->
                CodeState $
                    HashMap.insert file (createCodeContent fragments cs content) files

    createCodeContent :: [String] -> [Content Void] -> CodeContent -> CodeContent
    createCodeContent path cs (CodeContent content fragments) =
        case path of
            [] ->
                error $ "path already has content"
            fragmentName : path' ->
                case HashMap.lookup fragmentName fragments of
                    Nothing ->
                        case path' of
                            [] ->
                                CodeContent
                                    content
                                    (HashMap.insert fragmentName (CodeContent cs mempty) fragments)
                            fragmentName' : _ ->
                                error $ "fragment " <> show fragmentName' <> " does not exist"
                    Just fragment ->
                        CodeContent
                            content
                            (HashMap.insert fragmentName (createCodeContent path' cs fragment) fragments)

append :: MonadState CodeState m => Path -> [Content Void] -> m ()
append path content =
    modify $ appendCodeState path content
  where
    appendCodeState :: Path -> [Content Void] -> CodeState -> CodeState
    appendCodeState (Path file fragments) cs (CodeState files) =
        case HashMap.lookup file files of
            Nothing ->
                error $ "file " <> show file <> " does not exist"
            Just content ->
                CodeState $
                    HashMap.insert file (appendCodeContent fragments cs content) files

    appendCodeContent :: [String] -> [Content Void] -> CodeContent -> CodeContent
    appendCodeContent path cs (CodeContent content fragments) =
        case path of
            [] -> CodeContent (content <> if null content then cs else CText "\n" : cs) fragments
            fragmentName : path' ->
                case HashMap.lookup fragmentName fragments of
                    Nothing ->
                        error $ "fragment " <> show fragmentName <> " does not exist"
                    Just fragment ->
                        CodeContent
                            content
                            (HashMap.insert fragmentName (appendCodeContent path' cs fragment) fragments)

toCode :: Document -> HashMap FilePath String
toCode (Document blocks) =
    let ((), CodeState files) = flip runState (CodeState mempty) $ traverse_ blockToCode blocks
     in renderCodeContent <$> files
  where
    blockToCode :: MonadState CodeState m => Block -> m ()
    blockToCode block =
        case block of
            Text _ -> pure ()
            Patch patch ->
                case patch of
                    Create path content ->
                        create path content
                    Append path content ->
                        append path content

runCode :: FilePath -> Maybe FilePath -> IO ()
runCode file mOutDir = do
    document <- decodeDocumentFile file
    let files = toCode document
    case mOutDir of
        Nothing ->
            HashMap.foldlWithKey
                (\acc filename content -> acc *> putStrLn ("-- " <> filename) *> putStrLn content)
                (pure ())
                files
        Just outDir -> do
            Directory.removeDirectoryRecursive outDir
            Directory.createDirectoryIfMissing True outDir
            HashMap.foldlWithKey
                ( \acc filename content -> do
                    acc
                    let path = outDir <> "/" <> filename
                    writeFile path content
                    putStrLn $ "Wrote " <> path
                )
                (pure ())
                files

main :: IO ()
main = do
    cli <- Options.execParser $ Options.info (cliParser <**> Options.helper) Options.fullDesc
    case cli of
        Doc file mOut -> runDoc file mOut
        Code file mOut -> runCode file mOut