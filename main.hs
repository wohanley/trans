import Trans

main = do
    stream <- getContents
    putStrLn . unlines . map transmit . lines $ stream
