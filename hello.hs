main :: IO ()
main = putStrLn myhtml

myhtml :: String
myhtml =
  html_ "My title" (h1_ "Header" <> p_ "Paragraph #1" <> p_ "Paragraph #2")

html_ :: String -> String -> String
html_ title content =
  "<html><head><title>" <> title <> "</title></head><body>" <> content <> "</body></html>"

p_ :: String -> String
p_ txt =
  "<p>" <> txt <> "</p>"

h1_ :: String -> String
h1_ txt =
  "<h1>" <> txt <> "</h1>"
