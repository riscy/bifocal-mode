;;; split-scroll.el --- Split a comint-mode buffer while scrolling

;; Authors: Chris Rayner (dchrisrayner @ gmail)
;; Created: May 23 2011
;; Keywords: frames, tools
;; Homepage: https://github.com/riscy/split-scroll
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

;; Automatically split the buffer into a head and a tail when you page up and
;; down in a scrolling buffer (such as shell-mode, comint-mode, ...) to help you
;; keep context when referring to earlier output.
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
;; This version tested with Emacs 25.1.1
;;
;; See README.org for more details.

;;; Installation:

;; 1. Move this file to a directory in your load-path or add
;;    this to your .emacs:
;;    (add-to-list 'load-path "~/path/to/this-file/")
;; 2. Next add this line to your .emacs:
;;    (require 'split-scroll)
;;
;; By default, split-scroll runs automatically in comint-mode buffers.

;;; Code:

(require 'shx)

;; Compiler pacifier
(defvar evil-insert-state-local-map)
(defvar evil-normal-state-local-map)

(defgroup split-scroll nil
  "For split-scrolling inside a comint-mode buffer."
  :prefix "split-scroll-"
  :group 'comint
  :link '(url-link
          :tag "Github"
          "https://github.com/riscy/split-scrol"))

(defcustom split-scroll-rows 12
  "How large the tail will be."
  :type 'integer)

(defcustom split-scroll-min-rows 30
  "The minimum window height before splitting is allowed."
  :link '(function-link split-scroll-unsplittable-p)
  :type 'integer)

(defvar-local split-scroll-active nil
  "Whether the split is active.")

(defvar-local split-scroll--default-scroll-on-output
  "Internal variable for remembering user scroll options.")

(defvar-local split-scroll--default-scroll-on-input
  "Internal variable for remembering user scroll options.")

(defun split-scroll-on-tail-p ()
  "True if the cursor is on the tail window."
  (and (eq (current-buffer) (window-buffer (previous-window)))
       ;; check if the bottom window is approximately the right size
       (< (abs (- (window-height) split-scroll-rows)) 5)))

(defun split-scroll-on-head-p ()
  "True if the cursor is on the head window."
  (and (eq (current-buffer) (window-buffer (next-window)))
       ;; check if the bottom window is approximately the right size
       (< (abs (- (window-height (next-window)) split-scroll-rows)) 5)))

(defun split-scroll-unsplittable-p ()
  "True if the window is too small to be split."
  (or (not (shx-point-on-input-p))
      (and (not (split-scroll-find-tail))
           (< (window-height) split-scroll-min-rows))))

(defun split-scroll-begin ()
  "Create the head/tail window pair."
  (interactive)
  (goto-char (point-max))
  (save-excursion
    ;; open a small window below
    (split-window-vertically (- (window-height) split-scroll-rows)))
  (setq-local split-scroll-active t)
  ;; remember previous comint settings regarding the scrolling
  (setq-local split-scroll--default-scroll-on-output comint-scroll-to-bottom-on-output)
  (setq-local split-scroll--default-scroll-on-input comint-scroll-to-bottom-on-input)
  ;; only auto-scroll the window the user's cursor is in
  (setq-local comint-scroll-to-bottom-on-output "this")
  (setq-local comint-scroll-to-bottom-on-input nil))

(defun split-scroll-end ()
  "If the window is split, remove the split.
See `split-scroll-up' and `split-scroll-down'."
  (interactive)
  (when (not (split-scroll-find-tail))
    (goto-char (point-max))
    (recenter -1))
  (when (split-scroll-find-tail)
    (goto-char (point-max))
    (setq-local comint-scroll-to-bottom-on-output split-scroll--default-scroll-on-output)
    (setq-local comint-scroll-to-bottom-on-input split-scroll--default-scroll-on-input)
    (windmove-up) ;; go to the head?
    (delete-window))
  (setq-local split-scroll-active nil)
  ;; realign
  (goto-char (point-max))
  (recenter -1))

(defun split-scroll-find-tail ()
  "Find the tail window.
Put the cursor on the tail at the end of buffer, or return nil if
the tail is not visible and/or the matching buffer is not above."
  (cond ((split-scroll-on-tail-p) t)
        ((split-scroll-on-head-p) (select-window (next-window)))
        (t nil)))

(defun split-scroll-up (&optional home)
  "Scroll up in the buffer.
If the window is not split, try to split it.  Then scroll the top
window up.  If HOME is non-nil, scroll all the way to the top."
  (interactive)
  (cond
   ((split-scroll-unsplittable-p)
    (if home
        (goto-char (point-min))
      (let ((line-move-visual t))
        (ignore-errors (line-move (- split-scroll-rows))))
      (recenter -1)))
   (t
    (if (split-scroll-find-tail)
        (select-window (previous-window))
      (split-scroll-begin))
    (if home
        (goto-char (point-min))
      (let ((line-move-visual t))
        (ignore-errors (line-move (- split-scroll-rows))))
      (recenter -1))
    (select-window (next-window))
    (goto-char (point-max))
    (recenter -1))))                    ; realign tail

(defun split-scroll-down ()
  "Scroll down in the buffer.
If the window is split, scroll the top window only.  If scrolling
down scrolls all the way down to the prompt, remove the split."
  (interactive)
  (if (not (split-scroll-find-tail))
      (let ((line-move-visual t))
        (ignore-errors (line-move split-scroll-rows)))
    (select-window (previous-window))
    (move-to-window-line -1)
    (let ((line-move-visual t))
      (ignore-errors (line-move split-scroll-rows)))
    (recenter -1)
    ;; go to end of line so that on-last-line works:
    (if (save-excursion (end-of-line) (shx-point-on-input-p))
        (split-scroll-end)
      (select-window (next-window)))
    (goto-char (point-max)))
  (recenter -1))

(defun split-scroll-home ()
  "Scroll all the way to the top."
  (interactive)
  (split-scroll-up 'home))

(defun split-scroll-mode-map (&optional parent)
  "Keymap used for `split-scroll'.
PARENT keymap is optional."
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap parent)
    (when (featurep 'evil-states)
      (define-key evil-insert-state-local-map (kbd "C-u") nil)
      (define-key evil-normal-state-local-map (kbd "C-u") nil)
      (define-key evil-insert-state-local-map (kbd "C-d") nil)
      (define-key evil-normal-state-local-map (kbd "C-d") nil)
      (define-key keymap (kbd "C-u") #'split-scroll-up)
      (define-key keymap (kbd "C-d") #'split-scroll-down))
    (define-key keymap (kbd "<prior>") #'split-scroll-up)
    (define-key keymap (kbd "<next>") #'split-scroll-down)
    (define-key keymap (kbd "<home>") #'split-scroll-home)
    (define-key keymap (kbd "M-<") #'split-scroll-home)
    (define-key keymap (kbd "<end>") #'split-scroll-end)
    (define-key keymap (kbd "M->") #'split-scroll-end)
    keymap))

(defun split-scroll-activate ()
  "Activate split-scroll in the current buffer."
  (interactive)
  (use-local-map (split-scroll-mode-map (current-local-map))))

;; Run whenever comint runs
(add-hook 'comint-mode-hook 'split-scroll-activate)

(provide 'split-scroll)
;;; split-scroll.el ends here
