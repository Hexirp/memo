# solve kata withTryFileLock

snoyberg の blog に面白い問題が乗っていたので解いてみます。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-14.1 script
import System.FileLock (FileLock, SharedExclusive (..), withTryFileLock)

-- We've imported this function:
--
-- withTryFileLock
--   :: FilePath
--   -> SharedExclusive
--   -> (FileLock -> IO a)
--   -> IO (Maybe a)

-- | Implement this function by using the 'withTryFileLock' imported above.
version1
  :: FilePath
  -> SharedExclusive
  -> (Maybe FileLock -> IO a)
  -> IO a
version1 = \p e f -> withTryFileLock p e pure >>= f

-- | And now turn it back into the original type signature. Use the
-- 'version1' function we just defined above.
version2
  :: FilePath
  -> SharedExclusive
  -> (FileLock -> IO a)
  -> IO (Maybe a)
version2 = \p e f -> version1 p e (traversal . fmap f)

-- | Just a simple test harness
main :: IO ()
main = do
  version1 "version1.txt" Exclusive $ \(Just _lock) ->
    version1 "version1.txt" Exclusive $ \Nothing ->
    putStrLn "Yay, it worked!"

  Just _ <- version2 "version2.txt" Exclusive $ \_lock -> do
    Nothing <- version2 "version2.txt" Exclusive $
      error "Should not be called"
    pure ()
  putStrLn "Yay, it worked!"
```

ははは！　簡単でしたね！

おっと、さらに高度な問題があります。それは `MonadUnliftIO` へ一般化することです。
