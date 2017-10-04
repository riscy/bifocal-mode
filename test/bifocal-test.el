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

(defun shx-test-case-bifocal-mode ()
  "Test window splitting functions."
  (goto-char (point-max))
  (if (< (window-height) bifocal-minimum-rows-before-splitting)
      (shx-test-warn "Warning: window too short to test bifocal")
    (shx-test-assert "Create split, don't move cursor."
                     (let ((currpt (point)))
                       (bifocal-up)
                       (eq currpt (point))))
    (shx-test-assert "Maintain split."
                     (let ((currpt (point)))
                       (bifocal-home)
                       (eq currpt (point))))
    ;;(shx-test-assert "Stay in head when paging up on the head."
    ;;(shx-test-assert "Stay in head when paging down on the head."
    (shx-test-assert "Destroy split, don't move cursor."
                     (let ((currpt (point)))
                       (bifocal-end)
                       (eq currpt (point))))
    (shx-test-assert "Create and destroy with home/end."
                     (let ((currpt (point)))
                       (bifocal-home)
                       (bifocal-end)
                       (eq currpt (point))))
    (shx-test-assert "Create destroy with pgup/pgdn."
                     (let ((currpt (point)))
                       (bifocal-up) (bifocal-up)
                       (bifocal-down) (bifocal-down) (bifocal-down)
                       (eq currpt (point))))
    (shx-test-assert "Try to find nonexistent split."
                     (null (bifocal--find-head)))))

(provide 'bifocal-test)
;;; bifocal-test.el ends here
