Hobbes
======

Hobbes is a small UNIX-style file watcher for OSX. The filenames of modified files are simply echoed to stdout, one file per line. You take it from there.

Complex tasks can be accomplished by combining ```hobbes``` with other utilities such as ```xargs```, for example:

```bash
# automatic GHC recompile when your source files change
hobbes "*.hs" | xargs -n1 ghc --make
```

Another example: I have a script called ```kick``` that reloads the current tab in Chrome and then refocuses iTerm. With this command to ```hobbes``` I get automatic browser reloading on every save.  

```bash
hobbes "*.html" | xargs -n1 kick
```


### Todo

- cabal install hobbes


### Thanks

Hobbes is a very small wrapper around the excellent [hfsevents](https://github.com/luite/hfsevents) haskell library by Luite Stegeman.
