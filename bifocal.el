;;; bifocal.el --- split-screen scrolling for comint-mode buffers

;; Authors: Chris Rayner (dchrisrayner @ gmail)
;; Created: May 23 2011
;; Keywords: frames, processes, tools
;; Homepage: https://github.com/riscy/bifocal-mode
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

;; In bifocal-mode, paging up causes a comint-mode window to be split in two
;; with a larger window on top (the head) and a smaller input window preserved
;; on the bottom (the tail):
;;
;; +--------------+
;; | -------      |
;; | -------      |
;; | -------      |
;; |    [head]    |
;; |(show history)|
;; +--------------+
;; |    [tail]    |
;; |(show context)|
;; +--------------+
;;
;; Paging down all the way causes the split to disappear.  This lets you enter
;; text at the prompt (in the tail window) and monitor new input, while
;; reviewing previous output (in the head window).
;;
;; This version tested with Emacs 25.1.1
;;
;; See README.org for more details.

;;; Installation:

;; 1. Move this file to a directory in your load-path or add
;;    this to your .emacs:
;;    (add-to-list 'load-path "~/path/to/this-file/")
;; 2. Next add this line to your .emacs:
;;    (require 'bifocal)

;;; Code:

(require 'comint)
(require 'windmove)

(defgroup bifocal nil
  "For split-screen scrolling inside a comint-mode buffer."
  :prefix "bifocal-"
  :group 'comint
  :link '(url-link
          :tag "Github"
          "https://github.com/riscy/bifocal"))

(defcustom bifocal-min-rows 30
  "The minimum window height before splitting the window is allowed."
  :link '(function-link bifocal--splittable-p)
  :type 'integer)

(defcustom bifocal-rows 15
  "How large the tail will be."
  :type 'integer)

(defvar bifocal-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<prior>") #'bifocal-up)
    (define-key keymap (kbd "<next>") #'bifocal-down)
    (define-key keymap (kbd "<home>") #'bifocal-home)
    (define-key keymap (kbd "M-<") #'bifocal-home)
    (define-key keymap (kbd "<end>") #'bifocal-end)
    (define-key keymap (kbd "M->") #'bifocal-end)
    keymap)
  "Bifocal keymap.")

(defvar-local bifocal--old-scroll-on-input nil
  "For restoring previous scroll options.")

(defvar-local bifocal--old-scroll-on-output nil
  "For restoring previous scroll options.")

(defvar-local bifocal--head-window nil
  "For remembering which window was the head.")

(defvar-local bifocal--tail-window nil
  "For remembering which window was the tail.")

(defun bifocal-down ()
  "Scroll down.
If the window is split, scroll the head window only.  If this
scrolls all the way down to the prompt, remove the split."
  (interactive)
  (if (not (bifocal--find-tail))
      (bifocal--move-point-down)
    (windmove-up)
    (move-to-window-line -1)
    (bifocal--move-point-down)
    ;; go to end of line so that on-last-line works:
    (if (bifocal--on-input-p (point-at-eol))
        (bifocal-end)
      (windmove-down))
    (goto-char (point-max)))
  (recenter -1))

(defun bifocal-end ()
  "Remove the head/tail split, if it exists."
  (interactive)
  (bifocal--destroy-split)
  (bifocal--recenter-at-point-max))

(defun bifocal-home ()
  "Scroll to the top of the buffer.
Create the head/tail split if none exists."
  (interactive)
  (bifocal-up 'home))

(defun bifocal-up (&optional home)
  "Scroll up.
If the window is not split, try to split it.  Then scroll up in
the head window.  If HOME is non-nil, scroll to the top."
  (interactive)
  (cond
   ((bifocal--splittable-p)
    (if (bifocal--find-tail) (windmove-up)
      (bifocal--create-split))
    (if home
        (goto-char (point-min))
      (bifocal--move-point-up))
    (windmove-down)
    (bifocal--recenter-at-point-max))
   (t ; window is unsplittable (too small)
    (if home
        (goto-char (point-min))
      (bifocal--move-point-up)))))

(defun bifocal--create-split ()
  "Create the head/tail window pair; leave the point on the head.
Tweak comint scrolling settings for split-screen scrolling.
Remember old comint-scroll settings to restore later."
  (bifocal--recenter-at-point-max)
  (split-window-vertically (- (window-height) bifocal-rows))
  (setq bifocal--head-window (selected-window)
        bifocal--tail-window (next-window)
        bifocal--old-scroll-on-output comint-scroll-to-bottom-on-output
        bifocal--old-scroll-on-input comint-scroll-to-bottom-on-input
        comint-scroll-to-bottom-on-output "this"
        comint-scroll-to-bottom-on-input nil))

(defun bifocal--destroy-split ()
  "Destroy the head/tail window pair.
Restore old comint settings for scrolling."
  (when (bifocal--find-tail)
    (delete-window (windmove-find-other-window 'up)))
  (setq bifocal--head-window nil
        bifocal--tail-window nil
        comint-scroll-to-bottom-on-output bifocal--old-scroll-on-output
        comint-scroll-to-bottom-on-input bifocal--old-scroll-on-input))

(defun bifocal--find-tail ()
  "Find the tail window.
Put the point on the tail at the end of buffer, or return nil if
the tail is not visible and/or the matching buffer is not above."
  (cond
   ((bifocal--point-on-head-p) (windmove-down))
   ((bifocal--point-on-tail-p) t)
   (t nil)))

(defun bifocal--last-line-p (point)
  "Whether POINT is on the same line as the process-mark."
  (save-excursion
    (forward-line)
    (>= point (process-mark (get-buffer-process (current-buffer))))))

(defun bifocal--move-point-down ()
  "Move the point down `bifocal-rows' and recenter the buffer."
  (let ((line-move-visual t))
    (ignore-errors (line-move bifocal-rows)))
  (recenter -1))

(defun bifocal--move-point-up ()
  "Move the point up `bifocal-rows' and recenter the buffer."
  (let ((line-move-visual t))
    (ignore-errors (line-move (- bifocal-rows))))
  (recenter -1))

(defun bifocal--point-on-head-p ()
  "Whether the point is on the head window."
  (let ((tail-window (windmove-find-other-window 'down)))
    (and (eq (current-buffer) (window-buffer tail-window))
         (eq (selected-window) bifocal--head-window)
         (eq tail-window bifocal--tail-window))))

(defun bifocal--point-on-tail-p ()
  "Whether the point is on the tail window."
  (let ((head-window (windmove-find-other-window 'up)))
    (and (eq (current-buffer) (window-buffer head-window))
         (eq (selected-window) bifocal--tail-window)
         (eq head-window bifocal--head-window))))

(defun bifocal--recenter-at-point-max ()
  "Move to point-max; align content with bottom of window."
  (goto-char (point-max))
  (recenter -1))

(defun bifocal--splittable-p ()
  "Whether the current window is able to be split."
  (and (bifocal--on-last-line-p (point-marker))
       (or
        (bifocal--find-tail)
        (>= (window-height) bifocal-min-rows))))

;;;###autoload
(define-minor-mode bifocal-mode
  "Toggle bifocal-mode on or off.
\nThis minor mode will automatically split the buffer into a head
and a tail when paging up and down in a comint-mode derived
buffer (such as shell-mode, inferior-python-mode, etc).\n
Use `bifocal-global-mode' to enable `bifocal-mode'
in all buffers that support it.\n
Provides the following bindings: \n\\{bifocal-mode-map}"
  :lighter bifocal-mode-lighter
  :keymap bifocal-mode-map
  (if bifocal-mode
      nil
    (bifocal-end)))

;;;###autoload
(define-globalized-minor-mode bifocal-global-mode bifocal-mode
  (lambda ()
    (interactive)
    (when (derived-mode-p 'comint-mode) (bifocal-mode +1))))

(provide 'bifocal)
;;; bifocal.el ends here
