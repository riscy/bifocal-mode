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

;; In bifocal-mode, paging up causes a comint-mode window to be split in two,
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

;; Compiler pacifier
(defvar evil-insert-state-local-map)
(defvar evil-normal-state-local-map)

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
  "Move to the end of the buffer and create the head/tail window pair."
  (interactive)
  (goto-char (point-max))
  (split-window-vertically (- (window-height) bifocal-rows))
  ;; remember old comint-scroll settings
  (setq bifocal--old-scroll-on-output comint-scroll-to-bottom-on-output)
  (setq bifocal--old-scroll-on-input comint-scroll-to-bottom-on-input)
  ;; only auto-scroll the window the user's cursor is in
  (setq comint-scroll-to-bottom-on-output "this")
  (setq comint-scroll-to-bottom-on-input nil))

(defun bifocal-end ()
  "If the window is split, remove the split."
  (interactive)
  (when (not (bifocal--find-tail))
    (goto-char (point-max))
    (recenter -1))
  (when (bifocal--find-tail)
    (goto-char (point-max))
    ;; restore old comint settings regarding scrolling
    (setq-local comint-scroll-to-bottom-on-output bifocal--old-scroll-on-output)
    (setq-local comint-scroll-to-bottom-on-input bifocal--old-scroll-on-input)
    (windmove-up)
    (delete-window))
  ;; realign
  (goto-char (point-max))
  (recenter -1))

(defun bifocal-home ()
  "Scroll all the way to the top."
  (interactive)
  (bifocal-up 'home))

(defun bifocal-up (&optional home)
  "Scroll up in the buffer.
If the window is not split, try to split it.  Then scroll the head
window up.  If HOME is non-nil, scroll all the way to the top."
  (interactive)
  (cond
   ((bifocal--splittable-p)
    (if (bifocal--find-tail)
        (select-window (previous-window))
      (bifocal-begin))
    (if home
        (goto-char (point-min))
      (bifocal--scroll-up)
      (recenter -1))
    (select-window (next-window))
    (goto-char (point-max))
    (recenter -1))
   (t
    (if home
        (goto-char (point-min))
      (bifocal--scroll-up)
      (recenter -1)))))

(defun bifocal--scroll-down ()
  "Scroll down."
  (let ((line-move-visual t))
    (ignore-errors (line-move bifocal-rows))))

(defun bifocal--scroll-up ()
  "Scroll up."
  (let ((line-move-visual t))
    (ignore-errors (line-move (- bifocal-rows)))))

(defun bifocal-down ()
  "Scroll down in the buffer.
If the window is split, scroll the head window only.  If scrolling
down scrolls all the way down to the prompt, remove the split."
  (interactive)
  (if (not (bifocal--find-tail))
      (bifocal--scroll-down)
    (select-window (previous-window))
    (move-to-window-line -1)
    (bifocal--scroll-down)
    (recenter -1)
    ;; go to end of line so that on-last-line works:
    (if (save-excursion (end-of-line) (bifocal--point-on-input-p))
        (bifocal-end)
      (select-window (next-window)))
    (goto-char (point-max)))
  (recenter -1))


;;; util

(defun bifocal--find-tail ()
  "Find the tail window.
Put the cursor on the tail at the end of buffer, or return nil if
the tail is not visible and/or the matching buffer is not above."
  (cond
   ((bifocal--on-head-p) (select-window (next-window)))
   ((bifocal--on-tail-p) t)
   (t nil)))

(defun bifocal--on-tail-p ()
  "True if the cursor is on the tail window."
  (and (eq (current-buffer) (window-buffer (previous-window)))
       ;; check if the bottom window is approximately the right size
       (< (abs (- (window-height) bifocal-rows)) 5)))

(defun bifocal--on-head-p ()
  "True if the cursor is on the head window."
  (and (eq (current-buffer) (window-buffer (next-window)))
       ;; check if the bottom window is approximately the right size
       (< (abs (- (window-height (next-window)) bifocal-rows)) 5)))

(defun bifocal--point-on-input-p ()
  "Check if point is on the input region."
  (>= (point-marker)
      (process-mark (get-buffer-process (current-buffer)))))

(defun bifocal--splittable-p ()
  "Whether the current window is able to be split."
  (and (bifocal--point-on-input-p)
       (or
        (bifocal--find-tail)
        (>= (window-height) bifocal-min-rows))))


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
  :keymap bifocal-mode-map)

(define-globalized-minor-mode bifocal-global-mode bifocal-mode
  (lambda ()
    (interactive)
    (when (derived-mode-p 'comint-mode) (bifocal-mode +1))))

(provide 'bifocal)
;;; bifocal.el ends here
