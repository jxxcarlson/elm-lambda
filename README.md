# Elm-Lambda

The Lambda modules provide tools for computations with the 
lambda calculus.  See the app folder for a command-line program
for carrying them out in a convenient way.  Here is a sample 
session carried out in the folder `./app`:

```
$ sh cli

Type ':help' for help

> (\x.x) (\y.y)
λy.y

> and true false
λx.λy.y

```

The `and true false` line works because on startup the app loaded
a file `defs.txt` that contained the lines

```
true      \\x.\\y.x
false     \\x.\\y.y
and       \\p.\\q.p q p
```

Thus `true` is re-written as ` \\x.\\y.x`, etc.