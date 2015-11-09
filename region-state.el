;;; region-state.el --- Displays the region state in mode-line  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; Keywords: convenience, mode-line

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
;;
;;   (region-state-mode 1)

;;; Code:

(defvar region-state-string nil
  "String to display in the mode line.")
(make-variable-buffer-local 'region-state-string)
(put 'region-state-string 'risky-local-variable t)

(defun region-state--update ()
  (setq region-state-string
        ;; TODO: Make more meaningful summery on what's selected
        (format "%-3d Marked" (- (region-end) (region-beginning)))))

(defun region-state--activate ()
  (add-hook 'post-command-hook #'region-state--update t t))

(defun region-state--deactivate ()
  (remove-hook 'post-command-hook #'region-state--update t)
  (setq region-state-string nil))

;;;###autoload
(define-minor-mode region-state-mode
  "Toggle the region state display in mode line (Region State mode).
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :global t
  (or global-mode-string (setq global-mode-string '("")))
  (if region-state-mode
      (progn
        (add-to-list 'global-mode-string 'region-state-string t)
        (add-hook 'activate-mark-hook #'region-state--activate)
        (add-hook 'deactivate-mark-hook #'region-state--deactivate))
    (setq global-mode-string
          (delq 'region-state-string global-mode-string))
    (add-hook 'activate-mark-hook #'region-state--activate)
    (add-hook 'deactivate-mark-hook #'region-state--deactivate)))

(provide 'region-state)
;;; region-state.el ends here
