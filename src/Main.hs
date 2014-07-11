-- | Main entry point to the application.
module Main where
import Control.Monad.State
import Data.Map
import Data.ByteString
import Prelude hiding (lookup, writeFile)
import Data.ByteString.UTF8 (fromString)
import Test.Hspec

-- | The main entry point.
main :: IO ()
main = hspec  $ do
        describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
            test
--    putStrLn "Welcome to FP Haskell Center!"
--    putStrLn "Have a good day!"

data FileSystem m = FileSystem {
  readFileFromFS :: FilePath -> m ByteString
 , writeFileToFS :: FilePath -> ByteString -> m ()
}

normalFileSystem :: FileSystem IO
normalFileSystem = FileSystem
    { readFileFromFS = Data.ByteString.readFile
    , writeFileToFS = Data.ByteString.writeFile
    }

usesFileSystem :: Monad m => FileSystem m -> m ByteString
usesFileSystem fs = do
    contents1 <- readFileFromFS fs "file1.txt"
    contents2 <- readFileFromFS fs "file2.txt"
    return $ Data.ByteString.append contents1 contents2

throwExceptionIfNone::Maybe a -> a
throwExceptionIfNone (Just x) = x
throwExceptionIfNone Nothing =  error "not found"
    
pureFileSystem :: FileSystem (State (Map FilePath ByteString))
pureFileSystem = FileSystem
    { readFileFromFS = \fp -> do
        m <- get
        return $ throwExceptionIfNone $ lookup fp m
    , writeFileToFS = \fp bs -> do
        m <- get
        put $ insert fp bs m
    }
    

test = do
     let m = fromList [ ("file1.txt", fromString "hello\n") , ("file2.txt",  fromString "world\n")]
         result = fst $ runState (usesFileSystem pureFileSystem) m
         expected = fromString "hello\nworld\n"
     result `shouldBe` expected
