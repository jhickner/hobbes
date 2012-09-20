Hobbes
======

Hobbes is a small UNIX-style file watcher for OSX. The filenames of modified files are simply echoed to stdout, one file per line. You take it from there.

Complex tasks can be accomplished by combining ```hobbes``` with other utilities such as ```xargs```, for example:

```bash
# automatic GHC recompile when your source files change
hobbes "*.hs" | xargs -n1 ghc --make
```

### Todo

- cabal install hobbes
