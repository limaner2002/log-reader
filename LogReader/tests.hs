{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude ()
import ClassyPrelude hiding (assert)
import LogReader.Watcher
import Test.QuickCheck.Monadic
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

prop1 :: LogKey -> DirChannel -> LogChannel -> LogDirMap -> LogDirMap
prop1 path dChan lChan logDirMap = close $ open $ logDirMap
    where
      open = openFile path dChan lChan
      close = closeFile path

propm :: FilePath -> Property
propm fPath = monadicIO $ do
  dChan <- run $ DirChannel <$> newTChanIO <*> newBroadcastTChanIO
  lChan <- run $ LogChannel <$> newBroadcastTChanIO
  ePath <- run $ tryAny $ toLogKey $ "/" <> fPath

  let eResult = fmap (\path -> prop1 path dChan lChan mempty) ePath

  case eResult of
    Left _ -> assert True
    Right result -> do
                 assert (mempty == result)

-- -- This instance does not work. The rest does
-- instance Arbitrary LogDirMap where
--     arbitrary = do
--       dChan <- liftIO $ DirChannel <$> newTChanIO <*> newBroadcastTChanIO
--       lChan <- liftIO $ LogChannel <$> newBroadcastTChanIO
--       fPath <- arbitrary
--       ePath <- tryAny $ toLogKey fPath
--       let eResult = fmap (\path -> openFile path dChan lChan mempty) ePath

--       case eResult of
--         Left _ -> return mempty
--         Right result -> return result

main = quickCheckWith stdArgs {maxSuccess = 100000} propm

-- prop_main :: LogKey -> LogDirMap -> Property
-- prop_main path logDirMap = monadicIO $ do
--   result <- run $ propm path logDirMap
--   _

-- logKey1 = (DirPath "/dir1/", FileName "test1.log")
-- logKey2 = (DirPath "/dir1/", FileName "test2.log")
-- logKey3 = (DirPath "/dir2/", FileName "test1.log")
-- logKey4 = (DirPath "/dir2/", FileName "test2.log")

-- createFile :: LogReader ()
-- createFile = createLog logKey1

-- createDeleteFile :: LogReader ()
-- createDeleteFile = do
--   createFile
--   deleteLog logKey1

-- multiCreate :: LogReader ()
-- multiCreate = do
--   createFile
--   createLog logKey2

-- multiCreateOneDelete :: LogReader ()
-- multiCreateOneDelete = do
--   multiCreate
--   deleteLog logKey1

-- multiCreateDeleteAll :: LogReader ()
-- multiCreateDeleteAll = do
--   multiCreateOneDelete
--   deleteLog logKey2

-- createDifferentDir :: LogReader ()
-- createDifferentDir = do
--   createFile
--   createLog logKey3

-- multiCreateDifferentDir :: LogReader ()
-- multiCreateDifferentDir = do
--   createDifferentDir
--   createLog logKey2
--   createLog logKey3

-- multiCreateDeleteOneDifferentDir :: LogReader ()
-- multiCreateDeleteOneDifferentDir = do
--   multiCreateDifferentDir
--   deleteLog logKey3

-- multiCreateDeleteAllOneDir :: LogReader ()
-- multiCreateDeleteAllOneDir = do
--   multiCreateDeleteOneDifferentDir
--   deleteLog logKey4

-- multiCreateDeleteAllDifferentDir :: LogReader ()
-- multiCreateDeleteAllDifferentDir = do
--   multiCreateDeleteAllOneDir
--   deleteLog logKey1
--   deleteLog logKey2

-- sendTest :: LogReader ()
-- sendTest = do
--   createFile
--   sendUpdates logKey1

-- multiCreateSameFile :: LogReader ()
-- multiCreateSameFile = do
--   createLog logKey1
--   createLog logKey1

-- runTest (name, test) = do
--   result <- execStateT (runLogReader test) mempty
--   return (name, result)

-- tests = [ ("createFile", createFile)
--         , ("createDeleteFile", createDeleteFile)
--         , ("multiCreate", multiCreate)
--         , ("multiCreateOneDelete", multiCreateOneDelete)
--         , ("multiCreateDeleteAll", multiCreateDeleteAll)
--         , ("createDifferentDir", createDifferentDir)
--         , ("multiCreateDifferentDir", multiCreateDifferentDir)
--         , ("multiCreateDeleteOneDifferentDir", multiCreateDeleteOneDifferentDir)
--         , ("multiCreateDeleteAllOneDir", multiCreateDeleteAllOneDir)
--         , ("multiCreateDeleteAllDifferentDir", multiCreateDeleteAllDifferentDir)
--         , ("sendTest", sendTest)
--         , ("multiCreateSameFile", multiCreateSameFile)
--         ]



-- main :: IO ()
-- main = do
--   results <- mapM runTest tests
--   mapM_ (\(name,result) -> do
--            putStrLn name
--            print result
--            putStrLn mempty
--         ) results