# README

Here are my emacs config files.
It is worth noting that the emacs version I'm using is 24.5.1, and thus a couple settings are different in emacs 25+, and will need some adjustment to make certain the init files play well with that version.

## Changes in emacs 25+ that affect the init files

> The default 'diary-file' is now located in "~/.emacs.d".

This means that one will just need to remove the related config line, since the file is already there.
  
> 'package-initialize' now sets 'package-enable-at-startup' to nil if
called during startup.  Users who call this function in their init
file and still expect it to be run after startup should set
'package-enable-at-startup' to t after the call to
'package-initialize'.

One will probably need to add the above-mentioned 'package-enable-at-startup' line in the config.
