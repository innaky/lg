# lg
List greater files

Return a filename and the size: 

* If the __input__ is a __file__ return the size. 
* if the __input__ is a file __type directory__ return the sum (recursively) of all of files.

## Usage

```bash
> lg /home/live/src/haskell-misc/
"76187 /home/live/src/haskell-misc/.git"
"20779 /home/live/src/haskell-misc/learn-you-a-haskell"
"1063 /home/live/src/haskell-misc/LICENSE"
"617 /home/live/src/haskell-misc/README.md"

> lg /home/live/src/haskell-misc/README.md
"617 /home/live/src/haskell-misc/README.md"

lg /home/live/src/haskell-misc/ | head -n 2
"76187 /home/live/src/haskell-misc/.git"
"20779 /home/live/src/haskell-misc/learn-you-a-haskell"
```
