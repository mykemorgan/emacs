emacs
=====

Custom emacs elisp files for development. Contains custom written
tools and modes, as well as possible links to open source stuff that
is useful.

Note: the file "dot-emacs" is the .emacs file that needs to be in
one's home directory. I just link my .emacs to this file in whatever
directory this repo is put.

Yes, for larger functions it would be better to have a single file per
functional block of code. mm-find-header-at-point is a good
example. But for a lot of the super-small helper type things, it was
too much work to have 5-line functions all in their own files...

### ToDo:

Note, try to not check in elisp files that were not written by me but
were instead manually downloaded for use.

Really need to figure out how to use a real emacs package manager...

* [Packages to Download](packages-to-download.md): Contains helpful list
of possible elisp files, and where they come from.

* [Packages to Ignore](.gitignore) Add them to git ignore file.

### Some file explanations:

* **Samples.txt** - Text file to contain notes about "how to do stuff" that
isn't necessarily ready-to-compile elisp.

* **mm-examples.el** - Random test code and examples of how some things in
elisp work.

* **mm-test.el** - Strikingly similar to the examples file, mostly because I
forgot I had this also. But more a place to put test shells to call
"functions in development" before or after putting them as actual test
functions in the same file as useful stuff.

