# Gluing things together

In this chapter we are going to glue the pieces that we built together
and build an actual blog generator. We will:

1. Read markup text from a file
2. Parse the text to a `Document`
3. Convert the result to our `Html` EDSL
4. Generate HTML code
5. Write it to file

While doing so, we will learn:

- How to work with IO
- How to import external libraries to process whole directories and create a simple command-line interface
