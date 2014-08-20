{-# LANGUAGE OverloadedStrings #-}

module Junction.Git where

import Preface
import Bindings.Libgit2

import Control.Monad (ap)

type GitError = String
type TreeFilePath = ByteString

data GitRepository = GitRepository (ForeignPtr C'git_repository) deriving(Show)

mkRepo :: (Show a, Integral a) => Ptr (Ptr C'git_repository) -> a -> IO (Either GitError GitRepository)
mkRepo ptr r =
  if r < 0 then lasterr "Could not open repository"
  else return . Right . GitRepository =<< ap newForeignPtr c'git_repository_free =<< peek ptr

createGitRepository :: FilePath -> Bool -> IO (Either GitError GitRepository)
createGitRepository rpath isBare = c'git_threads_init >>
  alloca (\ptr -> withCString rpath $ \str -> mkRepo ptr =<< c'git_repository_init ptr str (fromBool isBare))

-- | openGitRepository initializes the C library for accessing git.
-- It expects the pathname of the repository
openGitRepository :: FilePath -> IO (Either GitError GitRepository)
openGitRepository rpath = c'git_threads_init >>
  alloca (\ptr -> withCString rpath $ \str -> mkRepo ptr =<< c'git_repository_open ptr str)

lookupCommit :: GitRepository -> OidPtr -> IO (Either GitError Commit)
lookupCommit repo oid = lookupObject' repo oid c'git_commit_lookup lgObjToCommit

lookupObject':: GitRepository -> OidPtr
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> IO CInt)
  -> ( ForeignPtr a -> IO b)
  -> IO (Either GitError b)
lookupObject' (GitRepository repo) (OidPtr oid) lookupFn createFn =
    alloca $ \ptr -> do
        r <- withForeignPtr repo $ \repoPtr -> withForeignPtr oid $ \oidPtr -> lookupFn ptr repoPtr oidPtr
        if r < 0 then do
              oidStr <- withForeignPtr oid (flip oidToStr 40)
              lasterr ("Could not look up "++oidStr)
        else do
              ptr'     <- peek ptr
              let p = castPtr ptr'
              fptr <- newForeignPtr p (c'git_object_free p)
              Right <$> createFn (castForeignPtr fptr)

lgObjToCommit :: ForeignPtr C'git_commit -> IO Commit
lgObjToCommit = flip withForeignPtr (fmap Commit . ( coidPtrToOid =<< ) . c'git_commit_tree_id)

data Commit = Commit OidPtr deriving Show

{-
peekGitTime :: Ptr C'git_time -> IO ZonedTime
peekGitTime tptr = do
    moment <- peek tptr
    return (utcToZonedTime
            (minutesToTimeZone (fromIntegral (c'git_time'offset moment)))
            (posixSecondsToUTCTime (fromIntegral (c'git_time'time moment))))
-}

commitTreeEntry :: GitRepository -> Commit -> TreeFilePath -> IO (Either GitError TreeEntry)
commitTreeEntry repo (Commit c) path = do
  a <- lookupTree repo c
  case a of
     Left e -> return $ Left e
     Right t -> treeEntry t path

treeEntry :: Tree -> TreeFilePath -> IO (Either GitError TreeEntry)
treeEntry (LgTree tree) fp = alloca $ \entryPtr ->
    useAsCString fp $ \pathStr -> withForeignPtr tree $ \treePtr -> do
        r <- c'git_tree_entry_bypath entryPtr treePtr pathStr
        if r < 0 then lasterr "getting tree entry" else Right <$> (entryToTreeEntry =<< peek entryPtr)

lookupTree :: GitRepository -> OidPtr -> IO (Either GitError Tree)
lookupTree repo oid
    | show oid == emptyTreeId = (return . Left) "empty tree"
    | otherwise = lookupObject' repo oid c'git_tree_lookup (return . LgTree)

lgObjToBlob :: ForeignPtr C'git_blob -> IO ByteString
lgObjToBlob fptr = do
    bs <- withForeignPtr fptr $ \ptr -> do
        size <- c'git_blob_rawsize ptr
        buf  <- c'git_blob_rawcontent ptr
        packCStringLen (castPtr buf, fromIntegral size)
    return bs

entryToTreeEntry :: Ptr C'git_tree_entry -> IO TreeEntry
entryToTreeEntry entry = do
    coid <- c'git_tree_entry_id entry
    oid  <- coidPtrToOid coid
    typ  <- c'git_tree_entry_type entry
    case () of
        () | typ == c'GIT_OBJ_BLOB ->
             do mode <- c'git_tree_entry_filemode entry
                BlobEntry oid <$>
                    case mode of
                        0o100644 -> return PlainBlob
                        0o100755 -> return ExecutableBlob
                        0o120000 -> return SymlinkBlob
                        _        -> error $ "Unknown blob mode: " ++ show mode
           | typ == c'GIT_OBJ_TREE -> return $ TreeEntry oid
           | typ == c'GIT_OBJ_COMMIT -> return $ CommitEntry oid
           | otherwise -> error "Unexpected"

emptyTreeId :: String
emptyTreeId = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

data Tree = LgTree (ForeignPtr C'git_tree)
data BlobKind = PlainBlob | ExecutableBlob | SymlinkBlob deriving (Show, Eq, Enum)

lookupBlob :: GitRepository -> OidPtr -> IO (Either GitError ByteString)
lookupBlob repo oid = lookupObject' repo oid c'git_blob_lookup lgObjToBlob

revparse :: GitRepository -> String -> IO (Either GitError OidPtr)
revparse (GitRepository repo) string =
  alloca $ \pptr -> do
    ret <- withForeignPtr repo $ \repoPtr -> withCString string $ \namePtr ->
             c'git_revparse_single pptr repoPtr namePtr
    if ret < 0 then lasterr "Could not parse commit" -- (return . Left . errmsg) ret
    else do
      ref <- peek pptr
      oo <- coidPtrToOid =<< c'git_object_id ref
      c'git_object_free ref
      (return . Right) oo

coidPtrToOid :: Ptr C'git_oid -> IO OidPtr
coidPtrToOid coidptr = do
    fptr <- mallocForeignPtr
    withForeignPtr fptr (flip c'git_oid_cpy coidptr)
    return $ OidPtr fptr

data OidPtr = OidPtr (ForeignPtr C'git_oid) deriving(Show)

oidToStr :: Ptr C'git_oid -> Int -> IO String
oidToStr oid len = c'git_oid_allocfmt oid >>= fmap (take len) . peekCString

lasterr :: String -> IO (Either GitError a)
lasterr str = do
    err <- c'giterr_last
    if err == nullPtr then return $ Left $ "No error: "++str -- concat args
    else do errm <- peekCString . c'git_error'message =<< peek err
            return $ Left $ concat [str, ": ", errm]

data TreeEntry = BlobEntry   { blobEntryOid   :: OidPtr, blobEntryKind  :: BlobKind }
                 | TreeEntry   { treeEntryOid   :: OidPtr }
                 | CommitEntry { commitEntryOid :: OidPtr }
