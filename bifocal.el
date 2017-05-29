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

(defvar-local bifocal--old-scroll-on-output
  "Internal variable for remembering user scroll options.")

(defvar-local bifocal--old-scroll-on-input
  "Internal variable for remembering user scroll options.")


;;; core

(defun bifocal-begin ()
  "Create the head/tail window pair."
  (interactive)
  (bifocal--recenter-at-point-max)
  (bifocal--tweak-scroll-settings)
  (split-window-vertically (- (window-height) bifocal-rows)))

(defun bifocal-down ()
  "Scroll down.
If the window is split, scroll the head window only.  If this
scrolls all the way down to the prompt, remove the split."
  (interactive)
  (if (not (bifocal--find-tail))
      (bifocal--scroll-down)
    (windmove-up)
    (move-to-window-line -1)
    (bifocal--scroll-down)
    ;; go to end of line so that on-last-line works:
    (if (bifocal--on-input-p (point-at-eol))
        (bifocal-end)
      (windmove-down))
    (goto-char (point-max)))
  (recenter -1))

(defun bifocal-end ()
  "Remove the head/tail split, if it exists."
  (interactive)
  (when (bifocal--find-tail)
    (bifocal--untweak-scroll-settings)
    (windmove-up)
    (delete-window))
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
    (if (bifocal--find-tail)
        (windmove-up)
      (bifocal-begin))
    (if home
        (goto-char (point-min))
      (bifocal--scroll-up))
    (windmove-down)
    (bifocal--recenter-at-point-max))
   (t ; window is unsplittable (too small)
    (if home
        (goto-char (point-min))
      (bifocal--scroll-up)))))

(defun bifocal--scroll-down ()
  "Move the point down `bifocal-rows' and recenter the buffer."
  (let ((line-move-visual t))
    (ignore-errors (line-move bifocal-rows)))
  (recenter -1))

(defun bifocal--scroll-up ()
  "Move the point up `bifocal-rows' and recenter the buffer."
  (let ((line-move-visual t))
    (ignore-errors (line-move (- bifocal-rows))))
  (recenter -1))


;;; util

(defun bifocal--find-tail ()
  "Find the tail window.
Put the cursor on the tail at the end of buffer, or return nil if
the tail is not visible and/or the matching buffer is not above."
  (cond
   ((bifocal--on-head-p) (windmove-down))
   ((bifocal--on-tail-p) t)
   (t nil)))

(defun bifocal--on-head-p ()
  "Whether the cursor is on the head window."
  ;; TODO: use (windmove-find-other-window 'down)
  (and (eq (current-buffer) (window-buffer (next-window)))
       ;; check if the bottom window is approximately the right size
       (< (abs (- (window-height (next-window)) bifocal-rows)) 5)))

(defun bifocal--on-input-p (point)
  "Whether POINT is in the input region."
  (>= point (process-mark (get-buffer-process (current-buffer)))))

(defun bifocal--on-tail-p ()
  "Whether the cursor is on the tail window."
  (and (eq (current-buffer) (window-buffer (previous-window)))
       ;; check if the bottom window is approximately the right size
       ;; TODO: instead use (window-normalize-window nil) to verify?
       (< (abs (- (window-height) bifocal-rows)) 5)))

(defun bifocal--recenter-at-point-max ()
  "Move to point-max; align content with bottom of window."
  (goto-char (point-max))
  (recenter -1))

(defun bifocal--splittable-p ()
  "Whether the current window is able to be split."
  (and (bifocal--on-input-p (point-marker))
       (or
        (bifocal--find-tail)
        (>= (window-height) bifocal-min-rows))))

(defun bifocal--tweak-scroll-settings ()
  "Tweak comint scrolling settings for split-screen scrolling.
Remember old comint-scroll settings to restore later."
  (setq bifocal--old-scroll-on-output comint-scroll-to-bottom-on-output
        bifocal--old-scroll-on-input comint-scroll-to-bottom-on-input)
  ;; only auto-scroll the window the user's cursor is in
  (setq comint-scroll-to-bottom-on-output "this"
        comint-scroll-to-bottom-on-input nil))

(defun bifocal--untweak-scroll-settings ()
  "Restore old comint settings regarding scrolling."
  (setq comint-scroll-to-bottom-on-output bifocal--old-scroll-on-output
        comint-scroll-to-bottom-on-input bifocal--old-scroll-on-input))


;;; loading

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

(define-globalized-minor-mode bifocal-global-mode bifocal-mode
  (lambda ()
    (interactive)
    (when (derived-mode-p 'comint-mode) (bifocal-mode +1))))

(provide 'bifocal)
;;; bifocal.el ends here
