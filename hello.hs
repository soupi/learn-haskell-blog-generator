main = putStrLn myhtml

myhtml =
  html_ "My title" (h1_ "Header" <> p_ "Paragraph #1" <> p_ "Paragraph #2")

html_ title content =
  "<html><head><title>" <> title <> "</title></head><body>" <> content <> "</body></html>"

p_ txt =
  "<p>" <> txt <> "</p>"

h1_ txt =
  "<h1>" <> txt <> "</h1>"
