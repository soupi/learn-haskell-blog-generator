main = putStrLn (myhtml "Hello, function!")

myhtml content = "<html><body>" <> content <> "</body></html>"
