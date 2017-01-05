# HaskellRegex
Regular expression parser and matcher implemented from scratch in Haskell

All characters can be used as alphabet, with the exception of the following 5: $()+*

Where R and r are valid RegEx, the following are valid:
 Rr, the concatenation of R and r.
 R+r, a union of R and r.
 R*, the Kleene Star (repetition) of R.
 As an example,
 ((Rr*)+r)(((r))): parentheses can be used to group, and can be used extraneously as well.

'$' is a special character which corresponds to the empty string ("epsilon").

The empty RegEx can be conveyed by the empty string. For clarity, users will want to nest it in parentheses. "()" is the most appropriate expression of the empty RegEx.

I have tried to implement RegEx in a manner that minimizes use of RegEx by imported functions and other shortcuts under the surface. Obviously, all interpreters and compilers use a lot RegEx themselves, so this goal is relatively futile. Moreover, pattern matching in function definitions is an essential advantage of Haskell, and I didn't avoid that. Still, I consider the implementation to be reasonably "from scratch" due to its barebones implementation of Brzozowski derivatives from my own defined types.

TODO:
- eliminate need for "$" as special epsilon character
- utilize escape character mechanism to allow for use of ()+* in RegEx
- implement common RegEx shortcuts to extend model (e.g. A-z, brackets, etc.)