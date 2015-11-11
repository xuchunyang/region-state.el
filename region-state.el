;;; region-state.el --- Displays the Region state somewhere  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; Keywords: convenience
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a global minor-mode. Turn it on everywhere with:
;; ┌────
;; │ (region-state-mode 1)
;; └────
;;
;; What's in your region? This package is for the question.
;;
;; That’s it.
;;
;; See the accompanying README.org for configuration details.

;;; Code:

(eval-when-compile (require 'rect))
(declare-function rectangle--pos-cols 'rect)


;;; Compatibility
(eval-and-compile
  (unless (macrop 'defvar-local)
    ;; `defvar-local' for Emacs 24.2 and below
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      ;; Can't use backquote here, it's too early in the bootstrap.
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var))))))


;;; Customization
(defgroup region-state nil
  "Displays the Region state somewhere"
  :prefix "region-state-"
  ;; Parent group, any better choice?
  :group 'emacs)

(defcustom region-state-format-function #'region-state-format-default
  "Function to use for constructing `region-state-string'.
Called with two arguments: (BEG END)
BEG is the beginning of the region.
END is the end of the region."
  :type 'function
  :group 'region-state)

(defcustom region-state-display-place 'echo-area
  "Where to display `region-state-string'."
  :type '(radio (const :tag "Display in the Echo Area" echo-area)
                (const :tag "Display in the Mode Line" mode-line)
                (const :tag "Display in the Header line" header-line)
                (const :tag "Don't use any of above built-in solutions" nil))
  :set (lambda (var-name value)
         (let ((on (bound-and-true-p region-state-mode)))
           (when on (region-state-mode -1))
           (set var-name value)
           (when on (region-state-mode 1))))
  :group 'region-state)


;;; Variables
(defvar-local region-state-string nil
  "Description of the region.")
(put 'region-state-string 'risky-local-variable t)

(defvar region-state-after-update-hook nil
  "Run by `region-state--update', after `region-state-string' is updated.")

(defvar-local region-state-last-beginning 0
  "Beginning position of the last region.")

(defvar-local region-state-last-ending 0
  "Ending position of the last region.")


;;; Function
(defun region-state-format-default (beg end)
  (if (not rectangle-mark-mode)
      (let ((chars (- end beg))
            (lines (count-lines beg end)))
        (setq region-state-string
              (concat
               (and (> lines 1) (format "%d lines, " lines))
               (and (> chars 0) (format "%d characters selected" chars)))))
    ;; FIXME: Handle beg > end
    (let* ((col (save-excursion (rectangle--pos-cols beg end)))
           (startcol (car col))
           (endcol (cdr col))
           (cols (- endcol startcol))
           ;; FIXME: Can't handle empty line somethmes, count myself
           ;; maybe by using `apply-on-rectangle'
           (rows (count-lines beg end)))
      (setq region-state-string
            (format "[%d, %d] rectangle selected"  cols rows)))))

(defun region-state--update ()
  (let ((beg (region-beginning))
        (end (region-end)))
    ;; Debug
    ;; (message "[region-state]: maybe update on region (%d, %d) -> (%d, %d)"
    ;;          region-state-last-beginning region-state-last-ending
    ;;          beg end)
    ;; Recompute only if the region actually is changed
    (unless (or (and (= beg region-state-last-beginning)
                     (= end region-state-last-ending))
                (and (= beg region-state-last-ending)
                     (= end region-state-last-beginning))
                ;; TODO: Also update after C-x SPC
                ;; (eq this-command 'rectangle-mark-mode)
                )
      ;; Debug
      ;; (message "[region-state]: updating...")
      (funcall region-state-format-function beg end)
      (run-hooks 'region-state-after-update-hook)
      (setq region-state-last-beginning beg
            region-state-last-ending end))))

(defvar-local region-state--default-header-line nil)
(defvar-local region-state--header-line-changed nil)

(defun region-state--activate ()
  (add-hook 'post-command-hook #'region-state--update t t)
  (when (eq region-state-display-place 'header-line)
    (setq region-state--default-header-line header-line-format
          header-line-format '(region-state-mode ("" region-state-string " "))
          region-state--header-line-changed t)))

(defun region-state--deactivate ()
  (remove-hook 'post-command-hook #'region-state--update t)
  (setq region-state-string nil)
  (setq region-state-last-beginning 0
        region-state-last-ending 0)
  (when region-state--header-line-changed
    (setq header-line-format region-state--default-header-line
          region-state--header-line-changed nil)))

(defun region-state--display-in-echo-area ()
  (let (message-log-max)
    (message "%s" region-state-string)))

(defun region-state-mode--reset ()
  "Initialize or clean up `region-state-mode'.
Run at the start of `region-state-mode'."
  (cond ((eq region-state-display-place 'mode-line)
         (or global-mode-string (setq global-mode-string '("")))
         (if region-state-mode
             (add-to-list 'global-mode-string 'region-state-string t)
           (setq global-mode-string
                 (delq 'region-state-string global-mode-string))))
        ((eq region-state-display-place 'echo-area)
         (if region-state-mode
             (add-hook 'region-state-after-update-hook #'region-state--display-in-echo-area)
           (remove-hook 'region-state-after-update-hook #'region-state--display-in-echo-area)))))


;;; Minor mode
;;;###autoload
(define-minor-mode region-state-mode
  "Toggle the display of the region.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :global t
  (region-state-mode--reset)
  (if region-state-mode
      (progn
        (add-hook 'activate-mark-hook #'region-state--activate)
        (add-hook 'deactivate-mark-hook #'region-state--deactivate))
    (add-hook 'activate-mark-hook #'region-state--activate)
    (add-hook 'deactivate-mark-hook #'region-state--deactivate)))

(provide 'region-state)
;;; region-state.el ends here
