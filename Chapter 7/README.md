# Chapter 7 - Functions Anywhere

I've added 2 files here, one is for section 7.1 and the other for the rest of section 7.

## Section 7.1 - Functions as Expressions and Values

Still having trouble with the mt-env in the appC case of the interpreter.
Stepping through the code it's obvious that variables are getting wiped out when we have nested functions and arguments.
I wish the author/s made it more clear that this was indeed the case.
This was expected really as everytime we get to the end of this interp case we pass the mt-env into the next recursion.

The only way I could get the two tests in the book at this section to pass was to revert the mt-env back to just env which allowed the interpreter to 'add' and keep the variables needed for both functions in the second test code.
The test/exn version was more targeted at proving the function was missing with some extra checking code but as we are no longer using substitution, our functions are being evaluated in place they they always exist.
It's only the variables that disappear.

Implementing closures is coming later in the chapter, maybe that will help but I can't help think a 'stack' like env would and a way to use it with 'scope' could be a solution.

## Section 7.2 - Nested What?

coming soon...