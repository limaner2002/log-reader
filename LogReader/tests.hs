{-# LANGUAGE OverloadedStrings #-}

import FreeLog

logKey1 = (DirPath "/dir1/", FileName "test1.log")
logKey2 = (DirPath "/dir1/", FileName "test2.log")
logKey3 = (DirPath "/dir2/", FileName "test1.log")
logKey4 = (DirPath "/dir2/", FileName "test2.log")

createFile :: LogReader ()
createFile = createLog logKey1

createDeleteFile :: LogReader ()
createDeleteFile = do
  createFile
  deleteLog logKey1

multiCreate :: LogReader ()
multiCreate = do
  createFile
  createLog logKey2

multiCreateOneDelete :: LogReader ()
multiCreateOneDelete = do
  multiCreate
  deleteLog logKey1

multiCreateDeleteAll :: LogReader ()
multiCreateDeleteAll = do
  multiCreateOneDelete
  deleteLog logKey2

createDifferentDir :: LogReader ()
createDifferentDir = do
  createFile
  createLog logKey3

multiCreateDifferentDir :: LogReader ()
multiCreateDifferentDir = do
  createDifferentDir
  createLog logKey2
  createLog logKey3

multiCreateDeleteOneDifferentDir :: LogReader ()
multiCreateDeleteOneDifferentDir = do
  multiCreateDifferentDir
  deleteLog logKey3

multiCreateDeleteAllOneDir :: LogReader ()
multiCreateDeleteAllOneDir = do
  multiCreateDeleteOneDifferentDir
  deleteLog logKey4

multiCreateDeleteAllDifferentDir :: LogReader ()
multiCreateDeleteAllDifferentDir = do
  multiCreateDeleteAllOneDir
  deleteLog logKey1
  deleteLog logKey2

runTest test = execStateT (runLogReader test) mempty

tests = [ createFile
        , createDeleteFile
        , multiCreate
        , multiCreateOneDelete
        , multiCreateDeleteAll
        , createDifferentDir
        , multiCreateDifferentDir
        , multiCreateDeleteOneDifferentDir
        , multiCreateDeleteAllOneDir
        , multiCreateDeleteAllDifferentDir
        ]

main :: IO ()
main = do
  results <- mapM runTest tests
  mapM_ print results