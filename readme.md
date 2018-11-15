# exec-region

Emacs provides many ways to run shell commands. You could, for example, use `M-|`, or `M-!`, or `eshell`. This package provides another option.


## Installation

0. Download the `exec-region.el` file, clone the repo, whatever. Make sure `exec-region.el` is somewhere in the `load-path`.
1. Add `(require 'exec-region)` to your `.emacs`.
2. There is no step 2.


## Usage

To use, select a region of text containing the command(s) you wish to run, then run one of the callable commands. The output will be written either to the current buffer or an output buffer, depending on the callable you call:

- `exec-region-here`: write the output to the current buffer after the region
- `exec-region-to-new-buffer`: write the output to a buffer called `*Exec Region Output*`
- `exec-region-and-replace`: replace the region (in the current buffer) with the output

Each callable has a counterpart that will act in the same way but will prepend each command's output with the command that generated it. The function names are the same but with `-with-commands` appended.

The default key mappings are:

- `C-| h`: exec-region-here
- `C-| H`: exec-region-here-with-commands
- `C-| b`: exec-region-to-new-buffer
- `C-| B`: exec-region-to-new-buffer-with-commands
- `C-| r`: exec-region-and-replace
- `C-| R`: exec-region-and-replace-with-commands

If the region contains multiple lines, then each line will be treated as a separate command.

So if the region is
```
date
cowsay "Howdy, pardner"
```
then running `exec-region-here` will result in
```
date
cowsay "Howdy, pardner"
Tue Nov 13 21:00:59 PST 2018
 ________________
< Howdy, pardner >
 ----------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```
and running `exec-region-here-with-commands` will result in
```
date
cowsay "Howdy, pardner"
$ date
Tue Nov 13 21:04:19 PST 2018
$ cowsay "Howdy, pardner"
 ________________
< Howdy, pardner >
 ----------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```
and running `exec-region-and-replace` will result in
```
Tue Nov 13 21:13:02 PST 2018
 ________________
< Howdy, pardner >
 ----------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```
