Hobbes
======

[![Circle CI](https://circleci.com/gh/jhickner/hobbes.svg?style=svg)](https://circleci.com/gh/jhickner/hobbes)

Hobbes is a small UNIX-style file watcher for ~~OSX~~ windows, linux and OSX (thanks @cgag), written after experiencing some OSX bugs with my usual standby [guard](https://github.com/guard/guard). The filenames of modified files are simply echoed to stdout, one file per line. You take it from there.

Complex tasks can be accomplished by combining ```hobbes``` with other commands such as ```xargs```, for example:

```bash
# automatic GHC recompile when your source files change
hobbes "*.hs" | xargs -n1 ghc --make
```

```xargs -n1 <command>``` means essentially "run the command on each word of input". So ```ghc --make``` is run on each modified file.


Another example: I have a script called ```kick``` that reloads the current tab in Chrome and then refocuses iTerm. With this command to ```hobbes``` I get automatic browser reloading on every save. Script [here](https://gist.github.com/4081943) if you're interested.

```bash
hobbes "*.html" | xargs -n1 kick
```

### Installation

First install haskell via your system's package manager. On OSX it's as simple as:

```brew install haskell-platform```

then install hobbes with:

```cabal install hobbes```


### Thanks

Hobbes is a very small wrapper around fsnotify.
