;;; clue.el --- Connecting clues while reading code -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao WANG

;; Author: Hao WANG <amaikinono@gmail.com>
;; Maintainer: Hao WANG <amaikinono@gmail.com>
;; Created: 08 Nov 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/AmaiKinono/clue
;; Version: 0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Clue is a tool for helping you take notes while reading code.

;; See README.md to know about the usage.  If you haven't received a copy of
;; README.md, please visit https://github.com/AmaiKinono/clue.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'rx)

;;;; User options

(defgroup clue nil
  "Connecting clues while reading code."
  :group 'convenience
  :group 'tools
  :prefix "clue-"
  :link '(url-link "https://github.com/AmaiKinono/clue"))

(defcustom clue-project-root-function #'clue-project-root
  "A function that returns project root in current buffer.
It takes no arguments.  It's used for automatically identify the
project root when copy a location."
  :type 'function)

(defcustom clue-after-jump-hook '(clue-recenter-and-blink)
  "Hook to run after jumping to a location."
  :type 'hook)

(defcustom clue-auto-enable-modes '(text-mode)
  "Let `clue-auto-enable-clue-mode' work only for these major modes.
If this list is nil, it works for all files, no matter what the
major mode is."
  :type 'list)

(defcustom clue-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'clue-follow-link)
    (define-key map (kbd "<mouse-1>") 'clue-follow-link)
    map)
  "Keymap that's enabled on links."
  :type 'keymap)

;;;; Internals

(defvar clue--link-regexp
  (rx "#[" (* (not (or "[" "]"))) "]")
  "Regexp to match links.")

;; Suppress the byte-compile warning.
(defvar clue-mode nil
  "Non-nil if Clue mode is enabled.
Use the command `clue-mode' to change this variable.")

(defvar clue--copied-location nil
  "Recent copied location.
It's a plist, with props/vals being:

- `:file': Full path of current file.
- `:line': The line number of current line.
- `:root': The project root of current file.")

(defun clue-project-root ()
  "Return the path of project root of current buffer.
This uses `project-current' internally."
  (when-let ((project (project-current nil)))
    (expand-file-name (cdr project))))

(defun clue-recenter-and-blink ()
  "Recenter point and blink after point.
This is suitable to run after jumping to a location."
  (recenter)
  (pulse-momentary-highlight-one-line (point)))

;;;;; Font lock.

(defun clue--unfontify (beg end)
  "Remove fontification of links between BEG and END."
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'clue-link-p)
      (delete-overlay ov))))

(defun clue--fontify (beg end)
  "Fontify the links between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward clue--link-regexp end t)
      (let* ((b (match-beginning 0))
             (e (match-end 0))
             ov)
        (setq ov (make-overlay b e))
        (overlay-put ov 'clue-link-p t)
        (overlay-put ov 'face 'button)
        (overlay-put ov 'mouse-face 'link)
        (overlay-put ov 'evaporate t)
        ;; Fixing the behavior of pressing RET just before the link.
        (overlay-put (make-overlay (1+ b) e) 'keymap clue-keymap)))))

(defun clue--refontify (beg end)
  "Refontify the links between BEG and END.
This is for use with jit-lock fontification, so the region to
refontify includes the two logical lines including BEG and END,
to prevent miss caused by line truncation inside the clues."
  (let ((beg (save-excursion (goto-char beg)
                             (line-beginning-position)))
        (end (save-excursion (goto-char end)
                             (line-end-position))))
    (clue--unfontify beg end)
    (clue--fontify beg end)))

;;;;; Parse meta links

(defun clue--metalink-project-root ()
  "Return the project root recorded in the metalink."
  (save-restriction
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (rx "#[:meta:root:"
                                   (group (+ (not (or "[" "]"))))
                                   "]")
                               nil t)
        (match-string 1)))))

;;;;; Follow links

(defun clue--link-location-at-point ()
  "Get the link location at point.
See `clue--copied-location' to know the returned data."
  (save-excursion
    (let ((pt (point)))
      (goto-char (line-beginning-position))
      (when (and (re-search-forward (rx "#["
                                        (group (+ (not (or "[" "]"))))
                                        ":L"
                                        (group (+ digit))
                                        "]")
                                    (line-end-position) t)
                 (< (match-beginning 0) pt (match-end 0)))
        `(:file
          ,(match-string 1)
          :line
          ,(string-to-number (match-string 2))
          :root
          ,(clue--metalink-project-root))))))

(defun clue--goto-location (location)
  "Goto LOCATION.
See `clue--copied-location' to know the form of LOCATION."
  (let* ((file (plist-get location :file))
         (line (plist-get location :line))
         (root (plist-get location :root)))
    (when (and root (not (file-name-absolute-p file)))
      (setq file (expand-file-name file root)))
    (unless (file-exists-p file)
      (user-error "File %s doesn't exist" file))
    (pop-to-buffer (find-file-noselect file))
    (goto-char
     (save-restriction
       (save-excursion
         (widen)
         (goto-char (point-min))
         (forward-line (1- line))
         (point))))))

;;;; Commands

;;;###autoload
(defun clue-copy ()
  "Copy the location of current line."
  (interactive)
  (let ((file (or (buffer-file-name)
                  (user-error "Buffer is not visiting a file")))
        (project (funcall clue-project-root-function)))
    (setq clue--copied-location
          `(:file ,file :line ,(line-number-at-pos) :root ,project))))

(defun clue-paste ()
  "Paste the copied link."
  (interactive)
  (unless clue--copied-location
    (user-error "The clipboard is empty"))
  (unless clue-mode
    (clue-mode))
  (let* ((file (plist-get clue--copied-location :file))
         (line (plist-get clue--copied-location :line))
         (root-in-link (plist-get clue--copied-location :root))
         (root-in-metalink (clue--metalink-project-root)))
    (when (and root-in-link (not root-in-metalink)
               (y-or-n-p
                (format "Set %s as project root? " root-in-link)))
      (save-restriction
        (save-excursion
          (goto-char (point-max))
          (unless (bolp)
            (insert "\n"))
          (insert (format "#[:meta:root:%s]\n" root-in-link))
          (setq root-in-metalink root-in-link))))
    (when (and root-in-metalink
               (file-in-directory-p file root-in-metalink))
      (setq file (file-relative-name file root-in-metalink)))
    (insert (format "#[%s:L%s]\n" file line))))

(defun clue-follow-link ()
  "Follow the link under point."
  (interactive)
  (when-let ((loc (clue--link-location-at-point)))
    (clue--goto-location loc)
    (run-hooks 'clue-after-jump-hook)))

;;;###autoload
(define-minor-mode clue-mode
  "Connect your clues in code reading."
  :lighter " Clue"
  (cond
   (clue-mode
    (jit-lock-register #'clue--refontify))
   (t
    (jit-lock-unregister #'clue--refontify)
    (save-restriction
      (widen)
      (clue--unfontify (point-min) (point-max))))))

;;;; Helpers for users

;;;###autoload
(defun clue-auto-enable-clue-mode ()
  "Enable `clue-mode' if a link is found in current buffer.
Add this to `find-file-hook' to auto enable `clue-mode' for code
reading notes.

See also `clue-auto-enable-modes'."
  (when (or (null clue-auto-enable-modes)
            (apply #'derived-mode-p clue-auto-enable-modes))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward clue--link-regexp nil t)
        (clue-mode)))))

(provide 'clue)

;; Local Variables:
;; outline-regexp: ";;;;* "
;; indent-tabs-mode: nil
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; clue.el ends here
