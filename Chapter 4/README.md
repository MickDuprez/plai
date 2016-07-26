# Chapter 4 solution

http://cs.brown.edu/courses/cs173/2012/book/first-desugar.html

This one was tricky for me, the take away from this was to understand the order in which to apply
the functions. I struggled a bit with the parser and interpreter until I worked out the 'rules' and steps involved.

The sequence of operations is key:
parse->desugar->interpret

I was trying to use the desugar function in the parser which really had me stumped for a while and then it clicked :)


While you will most likely develop all of this at the same time incrementally, the main steps are:

1. create the 'core' methods - create some basic methods and objects in the host language to be used
by the interpreter we are about to build. These should be the lowest level/primative objects and methods
we require to implement our language. From here we can build our language up from these basic building blocks.
You could think of this as the 'bootstrapping' methods perhaps(?).

2. create an interpreter - the interpreter carries out the actual processing of the core language constructs.
The interpreter is where we convert our 'core' language into the host language and execute it.

3. create the core 'surface' objects and methods - these will be the core language constructs and objects
our new language will use. These typically build upon the 'core' library we built in step 2.

4. create a parser - this parses our new language's source code into an AST (abstract syntax tree).
It essentially turns our source text structure into our 'surface' functions and objects structure in the AST.

5. create a 'desugar' function - since our parser's output is in our 'surface' language and our interpreter
uses our 'core' language only we need to 'desugar' or transform it down to our core language for the
interpreter to execute it.
