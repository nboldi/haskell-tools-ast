# Known limitation

 - Only cabal, cabal-sandbox and stack projects are supported. The project type is automatically detected.

## Limitation on source code

- Currently all modules are loaded into a shared GHC session. This may be changed in the future if different GHC sessions can work in the same process without interfering with each other.
  - Because of that, the tool does not support modules with the same qualified name.
  - Also, the tool applies some flags to more package components than it would be needed.
- The tool does not support packages where some of the haskell code is generated. (By parser generators, hsc files, ...)
- In general the tool does not support preprocessor usage in general. Preprocessor pragmas for conditional compilation are handled. (They are commonly used for backward compatibility.)
- Literate Haskell (.lhs files) is not supported.
- We don't load ghc flags from stack config files.
- A few of the GHC extensions are unsupported:
  - `ApplicativeDo` changes the GHC representation in an unexpected way. We suggest manually rewriting your monadic logic to applicative.
  - `OverloadedLabels` creates implicit bindings between names that may confuse a lot of refactorings based on names, like Rename Definition.
  - `ImplicitParams` enables implicit bindings that sometimes cannot be detected.

## Limitation on projects we handle

- If using on a stack project the packages must be under the project root.
- The `build-type: custom` attribute of cabal packages is not supported.
- Cabal flags with `manual` clauses are not recognized.

## Limitations for tool developers

- Some semantic information is missing, for example, kinds of type variables, types of variables local to TH splices, fixity of locally defined operators.
  - We don't have the type of each expression. To acquire it, we would need to re-type check the expressions (with the knowledge of the type of names inside).

## Limitations on testing

 - There is not enough memory in a container-based travis machine to run the reloading and refactoring self-tests. Virtual machines are too slow to install the required packages.
 - OSX VM is too slow to install the required packages to build.
