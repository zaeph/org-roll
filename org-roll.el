;;; org-roll.el --- Minor-mode for parsing dice-roll instructions  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2022 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
;; URL: https://github.com/zaeph/org-roll
;; Keywords: org, convenience, gamemaster, dice-rolls
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Minor-mode for parsing dice-roll instructions.

;;; Code:
;;;; Library Requires
(require 'org)
(require 'map)

(defun zp/org-roll--parse (str)
  "Parse dice-roll instructions in STR."
  (let* ((instructions-str (split-string str "[, \n]+ *" t))
         (max-name-length 1)
         (rolls-total 0)
         (instructions
          (mapcar (lambda (str)
                    (save-match-data
                      (string-match "\\([0-9]+\\)d\\([0-9]+\\)\\([><+-]\\)?" str)
                      (let* ((name (match-string 0 str))
                             (number (string-to-number (match-string 1 str)))
                             (range (string-to-number (match-string 2 str)))
                             (func (match-string 3 str))
                             (rolls (cl-loop for i from 1 to number
                                             collect (+ (random range) 1))))
                        ;; (debug name number func)
                        (setq max-name-length (max max-name-length
                                                   (length name)))
                        (unless (> rolls-total 1)
                          (setq rolls-total (+ rolls-total number)))
                        (list name rolls func))))
                  instructions-str)))
    (list instructions
          ;; Metadata
          (list
           :max-name-length max-name-length
           :rolls-total rolls-total))))

(defun zp/org-roll--format (instructions)
  "Format dice-roll INSTRUCTIONS."
  (pcase-let* ((`(,instructions ,metadata) instructions)
               ((map :max-name-length
                     :rolls-total)
                metadata))
    (format
     "Roll%s:\n%s%s"
     ;; Handle plural
     (if (> rolls-total 1) "s" "")
     (mapconcat #'identity
                (mapcar
                 (lambda (instruction)
                   (pcase-let ((`(,name ,rolls) instruction))
                     (format (format "- %%-%ss :: [ %%s ]"
                                     (number-to-string max-name-length))
                             name
                             (mapconcat #'identity
                                        (mapcar #'number-to-string rolls)
                                        ", "))))
                 instructions)
                "\n")
     "\n")))

(defun zp/org-roll--process-instructions (str)
  "Process dice-roll instructions from STR."
  (thread-first str
      (zp/org-roll--parse)
      (zp/org-roll--format)))

(defun zp/org-roll--extract-instructions (region)
  "Extract dice-roll instructions in REGION.
If the line contains dice-rolls instructions, return them.
Otherwise, return nil."
  (let (match?)
    (save-excursion
      (let ((beg (car region))
            (end (cdr region)))
        (goto-char beg)
        (condition-case nil
            (progn (re-search-forward
                    (concat "^ *\\([0-9]+d[0-9]+[<>+]?"
                            "\\([, ]+ *\\|$\\)+\\)+$")
                    end)
                   (setq match? t))
          (search-failed nil))
        (when match?
          (match-string-no-properties 0))))))

(defun zp/org-roll-replace-line (&optional arg)
  "Replace the dice-roll instructions in the current line.
Return t if the line has been replaced.
When called interactively, return an error if the line is
malformed."
  (interactive "p")
  (let ((interactive? arg)
        (beg (line-beginning-position))
        (end (line-end-position)))
    (if-let ((instructions
              (zp/org-roll--extract-instructions (cons beg end))))
        (let ((string (zp/org-roll--process-instructions instructions)))
          (kill-region beg end)
          (insert string)
          t)
      (when interactive?
        (error "Malformed line")))))

(defun zp/org-roll-replace-line-maybe ()
  "Maybe replace the dice-roll instructions in the current line.
Only does the replacement if point is at the end of the line.
This is intended to work as a :before hook to *-return commands."
  (when (eolp)
    (zp/org-roll-replace-line)))

(defun zp/org-roll-org-return (&rest args)
  (interactive "i\nP\np")
  (if (zp/org-roll-replace-line-maybe)
      (org-return-and-maybe-indent)
    (apply #'org-return args)))

(define-minor-mode zp/org-roll-mode
  "Allow inline processing of dice-rolls instructions."
  :lighter " roll"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap org-return] 'zp/org-roll-org-return)
            map))

(provide 'org-roll)
;;; org-roll.el ends here
