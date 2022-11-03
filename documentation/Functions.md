# Syntaxe de Sigma Code

- expr => deja defini

## Interpréteur

- repl syntax :
  - `fct_i [nom_params]* = expr`
  - `expr`
  - cmd :
    - `:l <file>` -> load
    - `:d <expr>` -> describe expression
    - `:d <fct>` -> describe fonction
    - `:q` -> quit
- command line param :
  - `repl <file w main> [exe]`  : exe w/ repl
  - `repl <file w/ main> [lib]` : repl + lib
  - `repl <file w main> lib`    : repl + lib
  - `repl <file w/ main> exe`   : error
  - `repl`                      : repl w/ lib
- repl file load
  - `:l <file w main>`  : exe main + import lib to repl
  - `:l -safe <file>`   : import lib to repl
  - `:l <file \w main>` : import lib to repl
- file syntax
  - functions syntax : `[fct_i [nom_params]* = expr.]*`

## Compilateur (transforme le code source en language machine)

- command line param :
  - `acc <fichier source> [<fichier dest>.acc]`

- file
  - functions syntax :

    ``` acc
    [fct_i [nom_params]* = expr.]*
    main_name [nom_params]* = expr.
    ```

## Runtime

TODO... later.
