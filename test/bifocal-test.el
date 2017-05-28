;;; bifocal-test.el --- Tests for bifocal-mode

;; Authors: Chris Rayner (dchrisrayner @ gmail)
;; Homepage: https://github.com/riscy/bifocal-mode

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

(defun shx-test-bifocal-mode ()
  "Test window splitting functions."
  (goto-char (point-max))
  (if (< (window-height) bifocal-min-rows)
      (shx-test-warn "Warning: window too short to test bifocal")
    (shx-test-assert "Create split."
                     (let ((currpt (point)))
                       (bifocal-up)
                       (and bifocal-active
                            (eq currpt (point)))))
    (shx-test-assert "Maintain split."
                     (let ((currpt (point)))
                       (bifocal-home)
                       (eq currpt (point))))
    (shx-test-assert "Destroy split."
                     (let ((currpt (point)))
                       (bifocal-end)
                       (and (not bifocal-active)
                            (eq currpt (point)))))
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
                     (null (bifocal-find-tail)))))

(provide 'bifocal-test)
