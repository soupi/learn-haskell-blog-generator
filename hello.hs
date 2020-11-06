main = putStrLn (myhtml "My title" "Hello, function!")

myhtml title content =
  "<html><head><title>" <> title <> "</title></head><body>" <> content <> "</body></html>"
