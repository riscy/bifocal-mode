;;; bifocal-test.el --- Tests for bifocal-mode

;; Authors: Chris Rayner (dchrisrayner @ gmail)
;; URL: https://github.com/riscy/bifocal-mode
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.0.0

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for bifocal-mode.  Uses the shx-test module.
;; To run on a comint session with shx active type :test

;;; Code:

(require 'shx-test)

(defun shx-test-integration-bifocal-splitting ()
  "Test window splitting functions."
  (goto-char (point-max))
  (if (< (window-height) bifocal-minimum-rows-before-splitting)
      (shx-test-warn "Warning: window too short to test bifocal-splitting")
    (forward-line -1)
    (shx-test-assert "bifocal--splittable-p is false when not on last line"
                     (not (bifocal--splittable-p)))
    (goto-char (point-max))
    (shx-test-assert "bifocal--create-split split doesn't move cursor"
                     (let ((currpt (point)))
                       ;; Manually create window (instead of `bifocal-up')
                       ;; to force the creation of a split.
                       (bifocal--create-split)
                       (select-window bifocal--tail)
                       (eq currpt (point))))
    ;; NOTE: the next tests assume window has been split from test above:
    (shx-test-assert "bifocal--oriented-p recognizes window orientation"
        (bifocal--oriented-p bifocal--tail 'up bifocal--head))
    (shx-test-assert "bifocal-end doesn't move the cursor"
                     (let ((currpt (point)))
                       (bifocal-end)
                       (eq currpt (point))))
    (shx-test-assert "bifocal-home/bifocal-end don't move the cursor"
                     (let ((currpt (point)))
                       (bifocal-home)
                       (bifocal-end)
                       (eq currpt (point))))
    (shx-test-assert "bifocal-up/bifocal-down don't move the cursor"
                     (let ((currpt (point)))
                       (bifocal-up) (bifocal-up)
                       (bifocal-down) (bifocal-down) (bifocal-down)
                       (eq currpt (point))))))

(defun shx-test-integration-bifocal-find-head ()
  (shx-test-assert "bifocal--find-head with no split returns nil"
                   (null (bifocal--find-head))))

(provide 'bifocal-test)
;;; bifocal-test.el ends here
