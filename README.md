# idiomas/idioms

* [English](https://github.com/innaky/lg#lg-free-unix-like-binary-command)
* [Español](https://github.com/innaky/lg#lg-comando-linuxunix-like)

# lg (Comando Linux/Unix-like)
Lista los archivos por tamaños de mayor a menor. 

El tamaño de los directorios es equivalente a la cantidad de información que contengan recursivamente.

El tamaño de los directorios vacíos es cero.

## Motivación
En trabajos con servidores (GNU/Linux, BSD), ocasionalmente tenía que buscar los directorios o archivos de mayor tamaño, para respaldo u otra actividad.
Usualmente con unos pocos comandos es fácil resolverle.

Pero ¿Si tienes muchísimos directorios con un gran árbol de subdirectorios, con muchos archivos? (ejemplo colapso de disco por mal empleo de logs)

Pues con algunas tuberías asunto resuelto...

Pero quería programar un binario en Haskell para mis amistades, así que tomé este problema,
el binario lo ejecutas llamando a un directorio y obtienes los archivos o directorios de mayor tamaño a un nivel de profundidad, sin embargo, 
la sumatoria es recursiva, es decir, revisa todo el árbol de subdirectorios.

## Construcción e instalación

* [Instalar stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
* Descargar el repositorio:
```
git clone https://github.com/innaky/lg
```
* Construcción:
```
cd lg
stack build
```

Tomas el binario que se encuentra bajo el directorio *.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/build/lg* 
```
chmod 755 lg
mv lg /usr/local/bin
```

Listo, con estos pasos lo tendrás disponible en el sistema.

## Uso

* Ayuda:

**lg [-h | --help]**

* Ejecución:

**lg [-b] archivo | directorio**

Por defecto está en formato de fácil lectura (Kilobytes, Megabytes etc), con el parámetro *-b* al inicio, la salida es en bytes.

## Ejemplos

* Salida con lectura agradable (lg directorio)
```bash
> lg /home/live/src/haskell-misc/
106K     /home/live/src/haskell-misc/.git
23K      /home/live/src/haskell-misc/learn-you-a-haskell
1K       /home/live/src/haskell-misc/LICENSE
617      /home/live/src/haskell-misc/README.md
```

* Salida en bits (lg -b directorio)
```bash
> lg -b /home/live/src/haskell-misc/
108837   /home/live/src/haskell-misc/.git
24372    /home/live/src/haskell-misc/learn-you-a-haskell
1063     /home/live/src/haskell-misc/LICENSE
617      /home/live/src/haskell-misc/README.md`
```

* Salida agradable de archivo (lg /ruta/archivo):
```bash
> lg /home/live/ISOS/test.iso
516M     /home/live/ISOS/test.iso
```

* Salida en bytes para archivos (lg -b /ruta/archivo):
```bash
> lg -b /home/live/ISOS/test.iso
541065216        /home/live/ISOS/test.iso
```

* ayuda (lg -h):
```
SYNOPSIS
    lg [-h | --help ]
    lg [-b] Filename | Directory

    For more information lg --help
```

* Para ayuda detallada usar (lg --help)

## Escalabilidad
Ya desde esta versión (gracias a @echarte) es fácil escalar el binario, agregando una módulo en */src* e importandole en */app/Main.hs*, 
sólo debes agregar el parámetro en la función *main :: IO ()*

# lg (Free Unix-like binary command)
List greater files

Return a filename and the size: 

* If the __input__ is a __file__ return the size. 
* if the __input__ is a file __type directory__ return the sum (recursively) of all of files.

## Usage

### __lg__ of a __directory__

```bash
> lg /home/live/src/haskell-misc/
106K     /home/live/src/haskell-misc/.git
23K      /home/live/src/haskell-misc/learn-you-a-haskell
1K       /home/live/src/haskell-misc/LICENSE
617      /home/live/src/haskell-misc/README.md
```

### __lg__ of a __file__

```bash
> lg /home/live/src/haskell-misc/README.md
617      /home/live/src/haskell-misc/README.md
```

### Shell pipes
```bash
lg /home/live/src/haskell-misc/ | head -n 2
106K     /home/live/src/haskell-misc/.git
23K      /home/live/src/haskell-misc/learn-you-a-haskell
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
106K     /home/live/src/haskell-misc/.git
23K      /home/live/src/haskell-misc/learn-you-a-haskell
1K       /home/live/src/haskell-misc/LICENSE
617      /home/live/src/haskell-misc/README.md
8        /home/live/src/haskell-misc/MyFileLink
0        /home/live/src/haskell-misc/DirHomeLink
```

for :
* __file symbolic link__ return the size.
* __directory symbolic  link__ return __zero__.


