# lg
list greater files

Return a filename and the size. If the input is a file return the size, but if the input is a file type directory return the sum (recursively) of all of files.

## Usage

```bash
./lg "/home/live/src/haskell-misc/"
[("/home/live/src/haskell-misc/learn-you-a-haskell",1837292),("/home/live/src/haskell-misc/.git",73701),("/home/live/src/haskell-misc/LICENSE",1063),("/home/live/src/haskell-misc/README.md",617)]

./lg "/home/live/src/haskell-misc/README.md"
[("/home/live/src/haskell-misc/README.md",617)]
```