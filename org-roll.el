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

(defgroup org-roll ()
  "Roll dice in org mode."
  :group 'org
  :prefix "org-roll-"
  :link '(url-link :tag "Homepage" "https://github.com/zaeph/org-roll"))

(defcustom org-roll-processors
  '(("<" . (lambda (rolls) (list (seq-sort #'< rolls))))
    (">" . (lambda (rolls) (list (seq-sort #'> rolls))))
    ("+" . (lambda (rolls) (list rolls (apply #'+ rolls)))))
  "Alist of roll processors.

This variable is a list of pairs, its car being the string
representing the processor, and its cdr being the function that
processes the user’s rolls.

The lambda must accept only one argument, ROLLS, which is a list
of dice rolls."
  :group 'org-roll
  :type '(alist :key-type string :value-type function))

(defun zp/org-roll--process-rolls (rolls processor)
  "Process ROLLS with PROCESSOR.
ROLLS is a list; PROCESSOR is a string.
Return either a list of rolls, or a list of the format ( rolls
extra-info ) where extra-info contains extra information for the
formatter.
For the list of available processors, see `org-roll-processors'."
  (let ((fn-proc (cdr (assoc-string processor org-roll-processors))))
    (if fn-proc
        (funcall fn-proc rolls)
      (error "Unrecognized processor"))))

(defun zp/org-roll--parse (str)
  "Parse dice-roll instructions in STR."
  (let* ((instructions-str (split-string str "[, \n]+ *" t))
         (max-name-length 3)
         (rolls-total 0)
         (instructions
          (mapcar
           (lambda (str)
             (save-match-data
               ;; TODO; Rewrite with rx for readability
               (string-match "\\([0-9]+\\)?d\\([0-9]+\\)\\([><+]\\)?" str)
               (let* ((name (match-string 0 str))
                      (number (if-let ((match (match-string 1 str)))
                                  (string-to-number match)
                                (prog1 1
                                  (setq name (concat "1" name)))))
                      (range (string-to-number (match-string 2 str)))
                      (processor (match-string 3 str))
                      (rolls (cl-loop for i from 1 to number
                                      collect (+ (random range) 1)))
                      (rolls-processed
                       (if processor
                           (zp/org-roll--process-rolls rolls processor)
                         (list rolls))))
                 ;; (debug name number processor)
                 (setq max-name-length (max max-name-length
                                            (length name)))
                 (unless (> rolls-total 1)
                   (setq rolls-total (+ rolls-total number)))
                 (list name rolls-processed processor))))
           instructions-str)))
    (list instructions
          ;; Metadata
          (list
           :max-name-length max-name-length
           :rolls-total rolls-total))))

(defun zp/org-roll--format (instructions)
  "Format dice-roll INSTRUCTIONS."
  (pcase-let*
      ((`(,instructions ,metadata) instructions)
       ((map :max-name-length
             :rolls-total)
        metadata)
       (instructions-formatted
        (mapcar
         (lambda (instruction)
           (pcase-let* ((`(,name (,rolls . ,extra-info) ,processor)
                         instruction))
             (concat
              ;; Main instruction
              (format (format "- %%-%ss :: [ %%s ]"
                              (number-to-string max-name-length))
                      name
                      (mapconcat #'number-to-string rolls ", "))
              ;; Extra info
              (pcase processor
                ("+"
                 (format (format "\n  %%%ss :: %%s"
                                 (number-to-string max-name-length))
                         "Σ"
                         (number-to-string (car extra-info))))))))
         instructions)))
    (format
     "Roll%s:\n%s\n"
     ;; Handle plural
     (if (> rolls-total 1) "s" "")
     (string-join instructions-formatted
                  "\n"))))

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
            ;; TODO; Rewrite with rx for readability
            (progn (re-search-forward
                    (concat "^ *\\(\\([0-9]+\\)?d[0-9]+[<>+]?"
                            "\\([, ]+ *\\|$\\)+\\)+$")
                    end)
                   (setq match? t))
          (search-failed nil))
        (when match?
          (match-string-no-properties 0))))))

(defun zp/org-roll-replace-line (&optional arg)
  "Replace the dice-roll instructions in the current line.
Return t if the line has been replaced.
When called interactively with ARG, return an error if the line
is malformed."
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
  "Wrapper for `org-return'.
Prepend `zp/org-roll-replace-line-maybe' to `org-return' and
forwards the same ARGS."
  (interactive "i\nP\np")
  (if (zp/org-roll-replace-line-maybe)
      (org-return-and-maybe-indent)
    (apply #'org-return args)))

;;;###autoload
(define-minor-mode zp/org-roll-mode
  "Allow inline processing of dice-roll instructions."
  :lighter " roll"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap org-return] 'zp/org-roll-org-return)
            map))

(provide 'org-roll)
;;; org-roll.el ends here
