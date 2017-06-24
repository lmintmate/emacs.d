# README

Here are my emacs config files.
It is worth noting that the emacs version I'm using is 24.5.1, and thus a couple settings are different in emacs 25+, and will need some adjustment to make certain the init files play well with that version.

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
;; personal directory parameters - not included in the version control, for obvious reasons. The list of relevant parameters is however included in a text file.
(load-file "~/.emacs.d/personal-parameters.el")
```

This loads an .el file not included in the version control, which sets the directories for a number of emacs package settings. The list of all of them is at personal-parameters.txt.
