# lg (Free Unix-like binary command)
List greater files

Return a filename and the size: 

* If the __input__ is a __file__ return the size. 
* if the __input__ is a file __type directory__ return the sum (recursively) of all of files.

## Usage

### __lg__ of a __directory__

```bash
> lg /home/live/src/haskell-misc/
"76187 /home/live/src/haskell-misc/.git"
"20779 /home/live/src/haskell-misc/learn-you-a-haskell"
"1063 /home/live/src/haskell-misc/LICENSE"
"617 /home/live/src/haskell-misc/README.md"
```

### __lg__ of a __file__

```bash
> lg /home/live/src/haskell-misc/README.md
"617 /home/live/src/haskell-misc/README.md"
```

### Shell pipes
```bash
lg /home/live/src/haskell-misc/ | head -n 2
"76187 /home/live/src/haskell-misc/.git"
"20779 /home/live/src/haskell-misc/learn-you-a-haskell"
```

### Symbolic link management (Example)
```bash

# Input in the directory

> cd /home/live/src/haskell-misc

# List the files
> ls -l 
total 12
drwxr-xr-x 7 live live 4096 feb 17 21:40 learn-you-a-haskell
-rw-r--r-- 1 live live 1063 feb 17 21:40 LICENSE
-rw-r--r-- 1 live live  617 feb 24 18:04 README.md

# Creating directory symbolic link (DirHomeLink) and file symbolic link (MyFileLink)
> ln -s /home/live/ DirHomeLink
> ln -s /home/live/src/haskell-misc/README.md MyFileLink

# List Everything
> ls -la
total 24
drwxr-xr-x 4 live live 4096 feb 29 21:48 .
drwxr-xr-x 6 live live 4096 feb 25 04:36 ..
lrwxrwxrwx 1 live live   11 feb 29 21:47 DirHomeLink -> /home/live/
drwxr-xr-x 8 live live 4096 feb 28 21:01 .git
drwxr-xr-x 7 live live 4096 feb 17 21:40 learn-you-a-haskell
-rw-r--r-- 1 live live 1063 feb 17 21:40 LICENSE
lrwxrwxrwx 1 live live   37 feb 29 21:48 MyFileLink -> /home/live/src/haskell-misc/README.md
-rw-r--r-- 1 live live  617 feb 24 18:04 README.md

# Testing "List greater" binary (lg)
> lg .
"76187 ./.git"
"20779 ./learn-you-a-haskell"
"1063 ./LICENSE"
"617 ./README.md"
"37 ./MyFileLink"
"0 ./DirHomeLink"
```

for :
* __file symbolic link__ return the size.
* __directory symbolic  link__ return __zero__.


