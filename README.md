# lg
list greater files

Return a filename and the size. If the input is a file return the size, but if the input is a file type directory return the sum (recursively) of all of files.

## Usage

```bash
lg /home/live/src/haskell-misc/
(1837292,"/home/live/src/haskell-misc/learn-you-a-haskell")
(73701,"/home/live/src/haskell-misc/.git")
(1063,"/home/live/src/haskell-misc/LICENSE")
(617,"/home/live/src/haskell-misc/README.md")

lg /home/live/src/haskell-misc/README.md
(617,"/home/live/src/haskell-misc/README.md")
```