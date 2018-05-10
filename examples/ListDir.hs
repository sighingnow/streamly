import Control.Monad.IO.Class (liftIO)
import Path.IO (listDir, getCurrentDir)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import Streamly (runStream, asParAhead)

-- | List the current directory recursively using concurrent processing
--
-- This example demonstrates that there is little difference between regular
-- IO code and concurrent streamly code. You can just remove
-- 'runStream . asParAhead' and this becomes your regular IO code.
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    runStream . asParAhead $ getCurrentDir >>= readdir
    where readdir d = do
            (ds, fs) <- liftIO $ listDir d
            liftIO $ mapM_ putStrLn $ map show fs ++ map show ds
            foldMap readdir ds