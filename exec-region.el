;;; exec-region.el --- Functions for executing a region of shell commands

;; Copyright (C) 2018 Richard Mavis

;; Author: Richard Mavis <rmavis@gmail.com> - http://richardmavis.info
;; URL: https://github.com/rmavis/exec-region
;; Created: Nov 13, 2018
;; Version: 0.0.1
;; Keywords: shell, command, region

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:
;;
;; Emacs provides many ways to run shell commands. You could, for
;; example, use M-|, or M-!, or eshell. This package provides another
;; option.
;;
;; To use, select a region of text containing the command(s) you wish
;; to run, then run one of the callable commands. The output will be
;; written either to the current buffer or an output buffer, depending
;; on the callable you call:
;;   exec-region-here: write the output to the current buffer after the
;;     region
;;   exec-region-to-new-buffer: write the output to a buffer named by
;;     the value of the `exec-region--output-buffer-name` variable
;;   exec-region-and-replace: replace the region (in the current buffer)
;;     with the output
;;
;; Each callable has a counterpart that will act in the same way but will
;; prepend each command's output with the command that generated it.
;; The function names are the same but with `-with-commands` appended.
;;
;;; Code:

;;
;; Key bindings.
;;

(global-set-key (kbd "C-| h") 'exec-region-here)
(global-set-key (kbd "C-| H") 'exec-region-here-with-commands)
(global-set-key (kbd "C-| b") 'exec-region-to-new-buffer)
(global-set-key (kbd "C-| B") 'exec-region-to-new-buffer-with-commands)
(global-set-key (kbd "C-| r") 'exec-region-and-replace)
(global-set-key (kbd "C-| R") 'exec-region-and-replace-with-commands)


;;
;; Settings.
;;

(defvar exec-region--output-buffer-name "*Exec Region Output*")
(defvar exec-region--prompt-user "$")
(defvar exec-region--prompt-root "#")


;;
;; Callable functions.
;;

;; exec-region-to-new-buffer :: void -> nil
(defun exec-region-to-new-buffer ()
  "Executes each line of the region in a shell, printing the output to a buffer."
  (interactive)
  (exec-region--check-and-run 'exec-region--in-output-buffer nil))

;; exec-region-to-new-buffer-with-commands :: void -> nil
(defun exec-region-to-new-buffer-with-commands ()
  "Executes each line of the region in a shell, printing the output to a buffer.
Each chunk of output will be preceded by the command that generated it."
  (interactive)
  (exec-region--check-and-run 'exec-region--in-output-buffer t))

;; exec-region-here :: void -> nil
(defun exec-region-here ()
  "Executes each line of the region in a shell, printing the output below the region."
  (interactive)
  (exec-region--check-and-run 'exec-region--in-current-buffer nil))

;; exec-region-here-with-commands :: void -> nil
(defun exec-region-here-with-commands ()
  "Executes each line of the region in a shell, printing the output below the region.
Each chunk of output will be preceded by the command that generated it."
  (interactive)
  (exec-region--check-and-run 'exec-region--in-current-buffer t))

;; exec-region-and-replace :: void -> nil
(defun exec-region-and-replace ()
  "Executes each line of the region in a shell, replacing the text in the region with the output."
  (interactive)
  (exec-region--check-and-run 'exec-region--exec-and-replace nil))

;; exec-region-and-replace-with-commands :: void -> nil
(defun exec-region-and-replace-with-commands ()
  "Executes each line of the region in a shell, replacing the text in the region with the output.
Each chunk of output will be preceded by the command that generated it."
  (interactive)
  (exec-region--check-and-run 'exec-region--exec-and-replace t))


;;
;; Non-interactive functions.
;;

;; exec-region--check-and-run :: (symbol bool) -> nil
;; The `symbol` parameter must name a function with the signature
;; ([string] string?) -> nil
;; that will perform the work on list of strings it's given.
;; The `bool` parameter indicates whether to include the command
;; header over the output.
(defun exec-region--check-and-run (func print-header)
  "If the region is active, runs the given function with the region's command(s)."
  (if (use-region-p)
      (funcall func
               (exec-region--get-region-lines)
               (if print-header (exec-region--get-prompt) nil))
    (message "Can't run region: region isn't active.")))

;; exec-region--in-output-buffer :: ([string] string?) -> nil
(defun exec-region--in-output-buffer (lines prompt)
  "Writes output from the command(s) to an output buffer."
  (let ((output-buffer (get-buffer-create exec-region--output-buffer-name)))
    (with-current-buffer output-buffer
      (exec-region--print-lines output-buffer lines prompt))
    (unless (get-buffer-window output-buffer 0)
      (display-buffer-below-selected output-buffer '(nil)))))

;; exec-region--in-current-buffer :: ([string] string?) -> nil
(defun exec-region--in-current-buffer (lines prompt)
  "Writes output from the command(s) to the current buffer."
  (save-mark-and-excursion
   (goto-char (region-end))
   (end-of-line)
   (newline)
   (exec-region--print-lines (current-buffer) lines prompt))
  (setq deactivate-mark nil))

;; exec-region--exec-and-replace :: ([string] string?) -> nil
(defun exec-region--exec-and-replace (lines prompt)
  "Writes output from the command(s) to the current buffer in place of the region."
  (save-mark-and-excursion
   (delete-region (region-beginning) (region-end))
   (exec-region--print-lines output-buffer lines prompt)
   (search-backward "\n" nil t) (replace-match "" nil t)))

;; exec-region--get-region-lines :: void -> [string]
(defun exec-region--get-region-lines ()
  "Returns the region's command(s) as a list of strings."
  (split-string (buffer-substring (region-beginning) (region-end)) "[\n]+" t "[\n]+"))

;; exec-region--get-prompt :: void -> string
(defun exec-region--get-prompt ()
  "Returns the prompt according to the user type."
  (if (eq (user-uid) 0) 
      exec-region--prompt-root
    exec-region--prompt-user))

;; exec-region--print-lines :: (buffer [string] string?) -> nil
(defun exec-region--print-lines (output-buffer lines prompt)
  "Prints the output from the list of commands to the buffer."
  (dolist (line lines)
    (exec-region--print-line output-buffer line prompt)))

;; exec-region--print-line :: (buffer string string?) -> nil
(defun exec-region--print-line (output-buffer command prompt)
  "Prints the command's output (and maybe the command itself) to the buffer."
  (with-current-buffer output-buffer
    (if prompt
        (insert (format "%s %s\n" prompt command)))
    (insert (shell-command-to-string command))))



(provide 'exec-region)
;;; exec-region.el ends here
