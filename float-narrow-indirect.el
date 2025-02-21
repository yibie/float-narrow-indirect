;;; float-narrow-indirect.el --- Floating window support for narrow-indirect -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Yibie
;; Keywords: convenience, frames
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/yibie/float-narrow-indirect

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides floating window support for narrow-indirect operations.
;; It allows users to create floating windows containing narrowed regions of
;; buffers, making it easier to view and compare different parts of a file.

;; Key features:
;; - Create floating windows for narrowed regions
;; - Aggregate multiple regions in a single floating frame
;; - Easy navigation between main and floating windows
;; - Customizable window appearance and behavior

;; Usage:
;; 1. Select a region
;; 2. M-x ni-narrow-to-region-floating
;; 3. Use ni-toggle-focus to switch between windows
;; 4. Use ni-clear-aggregation to close all floating windows

;;; Acknowledgements:
;; - This package is inspired by the narrow-indirect package, see: https://www.emacswiki.org/emacs/NarrowIndirect.


;;; Code:

(defcustom fni-floating-window-size '(0.3 . 0.5)
  "Size of floating windows as ratio of parent frame.
Width and height should be float numbers between 0 and 1."
  :type '(cons float float)
  :group 'Narrow-Indirect)

(defcustom fni-floating-frame-border-color "gray50"
  "Border color for floating frames."
  :type 'string
  :group 'Narrow-Indirect)

(defcustom fni-floating-frame-transparency '(95 . 90)
  "Transparency for floating frames. (active . inactive)"
  :type '(cons integer integer)
  :group 'Narrow-Indirect)

(defcustom fni-buf-name-prefix "FNI-"
  "Prefix for names of indirect buffers."
  :type 'string
  :group 'Narrow-Indirect)

(defcustom fni-buf-name-separator "::"
  "Separator between original buffer name and region content."
  :type 'string
  :group 'Narrow-Indirect)

(defcustom fni-narrowed-buf-name-max 50
  "Maximum length for generated buffer names."
  :type 'integer
  :group 'Narrow-Indirect)

(defvar-local fni-original-buffer nil
  "Reference to the original buffer for indirect buffers.")

(defvar-local fni-floating-window nil
  "Reference to the floating window if this buffer is displayed in one.")

(defvar fni-shared-frame nil
  "Shared floating frame for displaying aggregated regions.")

(defvar fni-aggregated-regions nil
  "List of all aggregated regions.
Each element is (buffer marker indirect-buffer start . end).")

(defcustom fni-region-separator "\n\n;; ========== %s ==========\n\n"
  "Template for separator between regions.
%s will be replaced with the region description."
  :type 'string
  :group 'Narrow-Indirect)

(defvar fni-indirect-buffer-hook nil
  "Hook run after creating an indirect buffer in narrow-indirect.")

(defun fni-buffer-substring-collapsed-visible (start end)
  "Get a collapsed visible substring between START and END.
Collapses multiple spaces into one, removes newlines, and truncates if too long.
Returns a string suitable for use in buffer names."
  (let ((str (buffer-substring-no-properties start end)))
    ;; Remove all newlines and extra spaces
    (setq str (replace-regexp-in-string "[ \t\n\r]+" " " str))
    ;; Remove leading and trailing spaces
    (setq str (string-trim str))
    ;; If the string is too long, truncate it
    (if (> (length str) 30)
        (concat (substring str 0 27) "...")
      str)))

;; Add a function to manage floating frames
(defun fni--create-floating-frame ()
  "Create a new child frame for displaying indirect buffers."
  (let* ((parent-width (frame-pixel-width))
         (parent-height (frame-pixel-height))
         ;; Calculate the actual width and height (using the ratio)
         (width-ratio (car fni-floating-window-size))
         (height-ratio (cdr fni-floating-window-size))
         (char-width (frame-char-width))
         (char-height (frame-char-height))
         ;; Calculate the number of characters instead of pixels
         (width-chars (max 40 (floor (* (/ parent-width char-width) width-ratio))))
         (height-chars (max 10 (floor (* (/ parent-height char-height) height-ratio))))
         ;; The top-right position
         (pos-x (- parent-width (* width-chars char-width) 30))
         (pos-y 20)
         (parent-frame (selected-frame))
         (frame (make-frame
                `((parent-frame . ,parent-frame)
                  (minibuffer . nil)
                  (width . ,width-chars)
                  (height . ,height-chars)
                  (left . ,pos-x)
                  (top . ,pos-y)
                  (undecorated . t)
                  (no-accept-focus . nil)
                  (no-special-glyphs . t)
                  (desktop-dont-save . t)
                  (internal-border-width . 2)
                  (background-color . ,(face-attribute 'default :background))
                  (alpha . ,(car fni-floating-frame-transparency))
                  (tool-bar-lines . 0)
                  (menu-bar-lines . 0)
                  (mode-line-format . nil)  
                  (vertical-scroll-bars . nil)))))
    
    (with-selected-frame frame
      (delete-other-windows)
      (set-face-background 'internal-border fni-floating-frame-border-color))
    frame))

(defun fni--ensure-aggregation-environment ()
  "Ensure floating frame exists for displaying indirect buffers."
  (unless (and fni-shared-frame (frame-live-p fni-shared-frame))
    (setq fni-shared-frame (fni--create-floating-frame)))
  fni-shared-frame)

(defun fni--add-region-to-aggregation (start end)
  "Add region between START and END to aggregation buffer"
  (let* ((source-buf (current-buffer))
         (source-mode major-mode)
         (region-name (fni-buffer-substring-collapsed-visible start end))
         (buf-name (concat fni-buf-name-prefix 
                          (buffer-name) 
                          fni-buf-name-separator 
                          region-name))
         (indirect-buf (clone-indirect-buffer buf nil)))    
    ;; Setting the indirect buffer
    (with-current-buffer indirect-buf
      (narrow-to-region start end)
      (setq fni-original-buffer source-buf)
      ;; Move to the beginning of the buffer
      (goto-char (point-min))
      ;; Run the hook
      (run-hooks 'fni-indirect-buffer-hook))
    ;; Handling the window in the aggregation frame
    (with-selected-frame fni-shared-frame
      ;; If it is the first region, use the existing window
      (if (not fni-aggregated-regions)
          (set-window-buffer (frame-selected-window fni-shared-frame) indirect-buf)
        ;; Otherwise, create a new window
        (let ((window (split-window-below)))
          (set-window-buffer window indirect-buf)))
      ;; Balance all windows
      (balance-windows)
      ;; Add the window separator
      (with-current-buffer indirect-buf
        (setq header-line-format 
              (format "=== %s: %s ===" 
                      (buffer-name source-buf) 
                      region-name))))
    ;; Adding to the regions list
    (push (list source-buf indirect-buf start end) 
          fni-aggregated-regions)
    
    indirect-buf))

;;;###autoload
(defun fni-narrow-to-region-floating (start end)
  "Add region between START and END to floating aggregation buffer.
After adding the region, return focus to the parent frame and
deactivate the mark."
  (interactive "r")
  ;; Save the region information and current buffer
  (let ((region-start start)
        (region-end end)
        (source-buffer (current-buffer)))  
    ;; Immediately deactivate the mark
    (deactivate-mark)
    ;; Ensure the environment exists
    (fni--ensure-aggregation-environment)
    ;; Use the saved region information to add the region
    (fni--add-region-to-aggregation region-start region-end)
    ;; Use ni-toggle-focus to return to the parent window
    (select-frame-set-input-focus (frame-parent fni-shared-frame))
    (switch-to-buffer source-buffer)))

;;;###autoload
(defun fni-narrow-to-region-replace (start end)
  "Replace current window content with narrowed region between START and END."
  (interactive "r")
  (let* ((here (point))
         (buf (fni-buffer-substring-collapsed-visible start end))
         (buf (concat fni-buf-name-prefix (buffer-name) fni-buf-name-separator buf))
         (buf (substring buf 0 (min (length buf) fni-narrowed-buf-name-max)))
         (indirect-buf (clone-indirect-buffer buf nil)))
    (with-current-buffer indirect-buf
      (narrow-to-region start end)
      (goto-char here)
      (setq fni-original-buffer (current-buffer)))
    (switch-to-buffer indirect-buf)))

;;;###autoload
(defun fni-toggle-focus ()
  "Toggle focus between main window and floating window.
If currently in floating window, switch to parent frame.
If currently in parent frame, switch to floating window."
  (interactive)
  (when fni-shared-frame
    (let ((current-frame (selected-frame))
          (parent-frame (frame-parent fni-shared-frame)))
      (cond
       ;; If in the floating window, switch to the parent window
       ((eq current-frame fni-shared-frame)
        (when parent-frame
          (select-frame-set-input-focus parent-frame)
          ;; Ensure focus returns to the original buffer
          (when (and fni-original-buffer 
                     (buffer-live-p fni-original-buffer))
            ;; First switch to the parent frame, then find the window
            (select-frame-set-input-focus parent-frame)
            (when-let* ((win (get-buffer-window fni-original-buffer t)))
              (select-window win)))))
       ;; If in other windows, switch to the floating window
       ((frame-live-p fni-shared-frame)
        (select-frame-set-input-focus fni-shared-frame))))))

(defun fni-clear-aggregation ()
  "Clear all aggregated regions and reset the environment."
  (interactive)
  (when fni-aggregated-regions
    (dolist (region fni-aggregated-regions)
      (let ((indirect-buf (nth 1 region)))
        (when (buffer-live-p indirect-buf)
          (kill-buffer indirect-buf))))
    (setq fni-aggregated-regions nil))
  
  (when (frame-live-p fni-shared-frame)
    (delete-frame fni-shared-frame)
    (setq fni-shared-frame nil)))

(define-minor-mode float-narrow-indirect-mode
  "Minor mode for float-narrow-indirect functionality with floating windows support."
  :lighter " FNI"
  :global t)

;; Add a default hook to close the mode-line
(add-hook 'fni-indirect-buffer-hook
          (lambda () 
            (setq mode-line-format nil)))

(provide 'float-narrow-indirect)
;;; float-narrow-indirect.el ends here