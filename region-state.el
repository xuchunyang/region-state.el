;;; region-state.el --- Displays the Region state somewhere  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/region-state.el
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
(declare-function apply-on-rectangle 'rect)


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


;;; Utility
(defun region-state--minibuffer-window-selected-p ()
  "Return t if minibuffer window is selected."
  (minibuffer-window-active-p (selected-window)))


;;; Customization
(defgroup region-state nil
  "Displays the Region state somewhere"
  :prefix "region-state-"
  :group 'convenience)


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
(defun region-state-format (beg end)
  (if (not rectangle-mark-mode)
      (let ((chars (- end beg))
            (lines (count-lines beg end)))
        (setq region-state-string
              (concat
               ;; Maybe need some special care for the first selected line
               (and (> lines 1) (format "%d lines, " lines))
               (and (> chars 0) (format "%d characters selected" chars)))))
    (let ((rows 0)
          (columns 0))
      (apply-on-rectangle (lambda (startcol endcol)
                            (setq rows (1+ rows))
                            (setq columns (- endcol startcol)))
                          beg end)
      (setq region-state-string
            (format "%d rows, %d columns rectangle selected" rows columns)))))

(defun region-state--update ()
  (let ((beg (region-beginning))
        (end (region-end)))
    (when (or (eq this-command 'rectangle-mark-mode)
              ;; For side effect only
              (eq this-command 'exchange-point-and-mark)
              (not (and (= beg region-state-last-beginning)
                        (= end region-state-last-ending))))
      (region-state-format beg end)
      (run-hooks 'region-state-after-update-hook)
      (setq region-state-last-beginning beg
            region-state-last-ending end))))

(defun region-state--activate ()
  (add-hook 'post-command-hook #'region-state--update t t))

(defun region-state--deactivate ()
  (remove-hook 'post-command-hook #'region-state--update t)
  (setq region-state-string nil)
  (setq region-state-last-beginning 0
        region-state-last-ending 0))

(defun region-state--display-in-echo-area ()
  ;; TODO: (low priority) Use mode-line to display if in minibuffer like el-doc
  (unless (region-state--minibuffer-window-selected-p)
    (let (message-log-max)
      (message "%s" region-state-string))))


;;; Minor mode
;;;###autoload
(define-minor-mode region-state-mode
  "Toggle the display of the region.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :global t
  (if region-state-mode
      (progn
        (add-hook 'activate-mark-hook #'region-state--activate)
        (add-hook 'deactivate-mark-hook #'region-state--deactivate)
        (add-hook 'region-state-after-update-hook #'region-state--display-in-echo-area))
    (remove-hook 'activate-mark-hook #'region-state--activate)
    (remove-hook 'deactivate-mark-hook #'region-state--deactivate)
    (remove-hook 'region-state-after-update-hook #'region-state--display-in-echo-area)))

(provide 'region-state)
;;; region-state.el ends here
