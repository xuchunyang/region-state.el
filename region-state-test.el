(require 'ert)
(require 'region-state)

;; For some reason, function added to `post-command-hook' seems doesn't get
;; called, so in below, I have to call `region-state--update' manually.
;; (region-state-mode 1)

(ert-deftest region-state-test ()
  (with-temp-buffer
    (insert "aaa\nbbb")
    (mark-whole-buffer)
    (region-state--update)
    (should (and (= region-state-lines 2)
                 (= region-state-chars 7)))))

(ert-deftest region-state-rect-test ()
  (with-temp-buffer
    (insert "aaa\nbbb")
    (if (fboundp 'rectangle-mark-mode)
        (progn
          (rectangle-mark-mode)
          (goto-char (point-min))
          (region-state--update)
          (should (and (= region-state-rows 2)
                       (= region-state-cols 3))))
      (message "This version of Emacs doesn't have rectangle-mark-mode"))))
