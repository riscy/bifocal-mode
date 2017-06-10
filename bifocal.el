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

;; In bifocal-mode, paging up causes a comint-mode buffer to be split into two
;; windows with a larger window on top (the head) and a smaller input window
;; preserved on the bottom (the tail):
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
;; This helps with monitoring new output and entering text at the prompt (in the
;; tail window), while reviewing previous output (in the head window).  Paging
;; down all the way causes the split to disappear.
;;
;; Note if you're not on the last line of a buffer, no split will appear.
;;
;; This version tested with Emacs 25.2.1
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
          :tag "the Github repository"
          "https://github.com/riscy/bifocal-mode"))

(defcustom bifocal-minimum-rows-before-splitting 30
  "The minimum window height before splitting the window is allowed."
  :link '(function-link bifocal--splittable-p)
  :type 'integer)

(defcustom bifocal-mode-lighter "B"
  "Lighter for the bifocal minor mode."
  :type 'string)

(defcustom bifocal-tail-size 15
  "The number of rows in the tail when splitting."
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
  "Keymap for the bifocal minor mode.")

(defvar-local bifocal--old-comint-scroll-on-input 'unset
  "Stores previously set value, so it can be restored.")

(defvar-local bifocal--old-comint-move-point-for-output 'unset
  "Stores previously set value, so it can be restored.")

(defvar-local bifocal--head nil
  "For remembering which window is the head.")

(defvar-local bifocal--tail nil
  "For remembering which window is the tail.")

(defun bifocal-down ()
  "Scroll down.
If the window is split, scroll down in the head window only.
If this scrolls to the last line, remove the split."
  (interactive)
  (if (not (bifocal--find-head))
      (bifocal--move-point-down)
    (move-to-window-line -1)
    (bifocal--move-point-down)
    (if (bifocal--last-line-p)
        (bifocal-end)
      (select-window bifocal--tail))
    (bifocal--recenter-at-point-max)))

(defun bifocal-end ()
  "Scroll to the end of the buffer.
Remove the head/tail split if it exists."
  (interactive)
  (bifocal--destroy-split)
  (bifocal--recenter-at-point-max))

(defun bifocal-home ()
  "Scroll to the top of the buffer.
Create the head/tail split unless it exists."
  (interactive)
  (bifocal-up 'home))

(defun bifocal-up (&optional home)
  "Scroll up.
If the window is not split, try to split it.  Then scroll up in
the head window.  If HOME is non-nil, scroll to the top."
  (interactive)
  (cond ((bifocal--splittable-p)
         (unless (bifocal--find-head)
           (bifocal--create-split))
         (bifocal--move-point-up home)
         (select-window bifocal--tail)
         (bifocal--recenter-at-point-max))
        (t (bifocal--move-point-up home))))

(defun bifocal--create-split ()
  "Create the head/tail split, leaving `point' on the head.
Adjust comint-scroll variables for split-screen scrolling."
  (bifocal--recenter-at-point-max)
  (split-window-vertically (- (window-height) bifocal-tail-size))
  (when (eq bifocal--old-comint-scroll-on-input 'unset)
    ;; adjust comint-scroll options
    (setq bifocal--old-comint-move-point-for-output comint-move-point-for-output
          bifocal--old-comint-scroll-on-input comint-scroll-to-bottom-on-input))
  (setq comint-move-point-for-output 'this
        comint-scroll-to-bottom-on-input nil)
  (setq bifocal--head (selected-window)
        bifocal--tail (next-window)))

(defun bifocal--destroy-split ()
  "Destroy the head/tail window pair.
Restore comint-scroll variables to their original values."
  (when (bifocal--find-head)
    (delete-window))
  (unless (eq bifocal--old-comint-scroll-on-input 'unset)
    ;; unadjust comint-scroll options
    (setq comint-move-point-for-output bifocal--old-comint-move-point-for-output
          comint-scroll-to-bottom-on-input bifocal--old-comint-scroll-on-input))
  (setq bifocal--old-comint-move-point-for-output 'unset
        bifocal--old-comint-scroll-on-input 'unset
        bifocal--head nil
        bifocal--tail nil))

(defun bifocal--find-head ()
  "Put the point on the head window.
Return nil if the head window is not identifiable."
  (and bifocal--head
       bifocal--tail
       (cond ((bifocal--point-on-tail-p) (select-window bifocal--head) t)
             ((bifocal--point-on-head-p) t)
             (t nil))))

(defun bifocal--last-line-p ()
  "Whether POINT is on the last line of the buffer."
    (eq (point-max) (point-at-eol)))

(defun bifocal--move-point-down ()
  "Move the point down `bifocal-tail-size' rows, and recenter."
  (let ((line-move-visual t))
    (ignore-errors (line-move bifocal-tail-size)))
  (recenter -1))

(defun bifocal--move-point-up (&optional home)
  "Move the point up `bifocal-tail-size' rows, and recenter.
If HOME is non-nil, go to `point-min' instead."
  (if home
      (goto-char (point-min))
    (let ((line-move-visual t))
      (ignore-errors (line-move (- bifocal-tail-size)))))
  (recenter -1))

(defun bifocal--oriented-p (start-window dir end-window)
  "Confirm the relative position of two windows viewing one buffer.
That is, START-WINDOW is selected, moving in direction DIR (via
'windmove') selects END-WINDOW, and both view the same buffer."
  (and (eq (selected-window) start-window)
       (let ((dir-window (windmove-find-other-window dir)))
         (and (eq dir-window end-window)
              (eq (current-buffer) (window-buffer dir-window))))))

(defun bifocal--point-on-head-p ()
  "Whether the point is on the head window."
  (bifocal--oriented-p bifocal--head 'down bifocal--tail))

(defun bifocal--point-on-tail-p ()
  "Whether the point is on the tail window."
  (bifocal--oriented-p bifocal--tail 'up bifocal--head))

(defun bifocal--recenter-at-point-max ()
  "Move the point to `point-max', and recenter."
  (goto-char (point-max))
  (recenter -1))

(defun bifocal--splittable-p ()
  "Whether the current window is able to be split."
  (and (bifocal--last-line-p)
       (or (bifocal--find-head)
           (>= (window-height) bifocal-minimum-rows-before-splitting))))

;;;###autoload
(define-minor-mode bifocal-mode
  "Toggle bifocal-mode on or off.\n
  bifocal-mode splits the buffer into a head and a tail when
paging up and down in a comint-mode derived buffer (such as
shell-mode, inferior-python-mode, etc).\n
  Use `bifocal-global-mode' to enable `bifocal-mode' in all
buffers that support it.\n
  Provides the following bindings:\n
\\{bifocal-mode-map}"
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
