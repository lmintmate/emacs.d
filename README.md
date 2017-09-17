# README

Here are my emacs config files.
It is worth noting that the emacs version I'm using is 24.5.1[^1], and thus a couple settings are different in emacs 25+, and will need some adjustment to make certain the init files play well with that version.

## Changes in emacs 25+ that affect the init files
Source: [Mickey Petersen's emacs 25 changelog](https://www.masteringemacs.org/article/whats-new-in-emacs-25-1).

``` 
	 The default 'diary-file' is now located in "~/.emacs.d".
```

This means that one will just need to remove the related config line, since the file is already there.
  
``` 
 'package-initialize' now sets 'package-enable-at-startup' to nil if
called during startup.  Users who call this function in their init
file and still expect it to be run after startup should set
'package-enable-at-startup' to t after the call to
'package-initialize'.
```

One will probably need to add the above-mentioned 'package-enable-at-startup' line in the config.

## Peculiar bits of code

```
;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")
[...]
(load "malyon")
```

I load a locally stored malyon.el file instead of the MELPA package version because I use [the version I forked from the repo used by MELPA](https://github.com/lmintmate/malyon), where I made a couple minor changes, such as [amending the error messages to account for the zblorb compatibility added by a previous maintainer](https://github.com/lmintmate/malyon/commit/e95759f5779553f64280ae0101610b03bf4eb9cd). If you don't care about these minor changes, you can use the MELPA version instead.

```
;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")
[...]
(load "web-search")
```

I load a locally stored web-search.el file instead of the MELPA package version because I use [the version I forked from the repo used by MELPA](https://github.com/lmintmate/web-search.el), where I made a couple changes, such as [adding the search provider DuckDuckGo](https://github.com/lmintmate/web-search.el/commit/88641a2f90ed599b3e400cadd2c470662b2c9a6f), as well as [the search engines of Bandcamp and Soundcloud](https://github.com/lmintmate/web-search.el/commit/8bba746feda09970adbf9d76dbef1291d4833af9). I use this manually instead of the MELPA version (with which it is at this point identical) just in case I wish to change anything further. 

- On another note, I also include the old init file in case you don't like literate configs, and will (probably) update it along with the literate version.

[^1]: on my main Linux pc, and 24.3.1 on my 32-bit Windows pc (as this was the latest version that worked there).
