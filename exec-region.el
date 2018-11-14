;;; exec-region.el --- Functions for executing a region of shell commands

;; Copyright (C) 2018 Richard Mavis <rmavis@gmail.com>

;; Author: Richard Mavis <rmavis@gmail.com> - http://richardmavis.info
;; URL: https://github.com/antonj/scss-mode
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
;;   exec-region-to-new-buffer: write the output to a buffer called
;;     *Exec Region Output*
;;   exec-region-and-replace: replace the region (in the current buffer)
;;     with the output
;;
;; Each callable has a counterpart that will act in the same way but will
;; prepend each command's output with the command that generated it.
;; The function names are the same but with `-with-commands` appended.
;;
;;; Code:

;;
;; Key mappings.
;;

(global-set-key (kbd "C-| h") 'exec-region-here)
(global-set-key (kbd "C-| H") 'exec-region-here-with-commands)
(global-set-key (kbd "C-| b") 'exec-region-to-new-buffer)
(global-set-key (kbd "C-| B") 'exec-region-to-new-buffer-with-commands)
(global-set-key (kbd "C-| r") 'exec-region-and-replace)
(global-set-key (kbd "C-| R") 'exec-region-and-replace-with-commands)


;;
;; Callable functions.
;;

(defun exec-region-here ()
  "Executes each line of the region in a shell, printing the output below the region."
  (interactive)
  (exec-region--in-current-buffer nil))

(defun exec-region-here-with-commands ()
  "Executes each line of the region in a shell, printing the output below the region.
Each chunk of output will be preceded by the command that generated it."
  (interactive)
  (exec-region--in-current-buffer t))

(defun exec-region-to-new-buffer ()
  "Executes each line of the region in a shell, printing the output to a buffer."
  (interactive)
  (exec-region--to-buffer (get-buffer-create "*Exec Region Output*") nil))

(defun exec-region-to-new-buffer-with-commands ()
  "Executes each line of the region in a shell, printing the output to a buffer.
Each chunk of output will be preceded by the command that generated it."
  (interactive)
  (exec-region--to-buffer (get-buffer-create "*Exec Region Output*") t))

(defun exec-region-and-replace ()
  "Executes each line of the region in a shell, replacing the text in the region with the output."
  (interactive)
  (exec-region--exec-and-replace nil))

(defun exec-region-and-replace-with-commands ()
  "Executes each line of the region in a shell, replacing the text in the region with the output.
Each chunk of output will be preceded by the command that generated it."
  (interactive)
  (exec-region--exec-and-replace t))


;;
;; Non-interactive functions.
;;

(defvar exec-region--output-buffer-name "*Exec Region Output*")

(defun exec-region--in-current-buffer (include-commands)
  (exec-region--to-buffer (current-buffer)
                          include-commands
                          (lambda ()
                            (goto-char (region-end))
                            (end-of-line)
                            (newline))))

(defun exec-region--in-output-buffer (include-commands)
  (exec-region--to-buffer (get-buffer-create "*Exec Region Output*") include-commands))

(defun exec-region--to-buffer (output-buffer &optional include-commands before printer after)
  "Executes each line of the region in a shell, printing the output to the given buffer."
  (if (use-region-p)
      (let ((lines (split-string (buffer-substring (region-beginning) (region-end)) "[\n]+" t "[\n]+"))
            (prompt (if (eq (user-uid) 0) "#" "$")))
        (save-mark-and-excursion
         (unless (null before) (funcall before))
         (dolist (line lines)
           (if (not (null printer))
               (funcall printer output-buffer line include-commands prompt)
             (exec-region--print-line output-buffer line include-commands prompt)))
         (unless (null after) (funcall after)))
        (unless (get-buffer-window output-buffer 0)
          (display-buffer-below-selected output-buffer '(nil)))
        (setq deactivate-mark nil))
    (message "Can't run region: region isn't active.")))

(defun exec-region--exec-and-replace (include-commands)
  (exec-region--to-buffer (current-buffer)
                          include-commands
                          (lambda ()
                            (delete-region (region-beginning) (region-end)))
                          (lambda (output-buffer command include-command prompt)
                            (with-current-buffer output-buffer
                              (exec-region--print-line output-buffer command include-command prompt)
                              (search-forward "\n" nil t) (replace-match "" nil t)))))

(defun exec-region--print-line (output-buffer command include-command prompt)
  (with-current-buffer output-buffer
    (if include-command
        (insert (format "%s %s\n" prompt command)))
    (insert (shell-command-to-string command))
    (newline)))


(provide 'exec-region)
;;; exec-region.el ends here
