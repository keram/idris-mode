;;; idris-warnings.el --- Mark warnings reported by Idris in buffers -*- lexical-binding: t -*-

;; Copyright (C) 2013 Hannes Mehnert

;; Author: Hannes Mehnert <hannes@mehnert.org>

;; License:
;; Inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
;; Therefore license is GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'idris-core)
(require 'idris-common-utils)
(require 'cl-lib)

(defface idris-warning-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "red"))
    (t
     :inherit warning))
  "Face for warnings from the compiler."
  :group 'idris-faces)

(defvar idris-warnings-buffers '() "All buffers which have warnings.")
(defvar-local idris-warnings '() "All warnings in the current buffer.")
(defvar idris-raw-warnings '() "All warnings from Idris.")

(cl-defstruct idris-warning
  filename startline startcol endline endcol message spans)

(defun idris-warning-event-hook-function (event)
  "Process normalised warning EVENT.
First Ensure the coordinates are in expected format.
Then persist the warning in `idris-raw-warnings' and
the warning as overlay to the buffer."
  (pcase event
    (`(:warning (,filename ,sl1 ,sl2 ,message ,spans) ,_target)
     (let ((startline (if (>=-protocol-version 2 1)
                          (1+ (nth 0 sl1))
                        (nth 0 sl1)))
           (startcol  (if (>=-protocol-version 2 1)
                          (nth 1 sl1)
                        (1- (nth 1 sl1))))
           (endline   (if (>=-protocol-version 2 1)
                          (1+ (nth 0 sl2))
                        (nth 0 sl2)))
           (endcol    (if (>=-protocol-version 2 1)
                          (nth 1 sl2)
                        (1- (nth 1 sl2)))))
       (push (list filename startline startcol message spans) idris-raw-warnings)
       (idris-warning-overlay (make-idris-warning :filename filename
                                                  :startline startline
                                                  :startcol startcol
                                                  :endline endline
                                                  :endcol endcol
                                                  :message message
                                                  :spans spans)))
     t)
    (_ nil)))

(defun idris-warning-reset-all ()
  "Remove warning overlays."
  (mapc #'idris-warning-reset-buffer idris-warnings-buffers)
  (setq idris-raw-warnings '())
  (setq idris-warnings-buffers '()))

(defun idris-warning-reset-buffer (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (idris-warning-reset))))

(defun idris-warning-reset ()
  (mapc #'delete-overlay idris-warnings)
  (setq idris-warnings '())
  (delq (current-buffer) idris-warnings-buffers))

(defun idris-warning-overlay-p (overlay)
  (overlay-get overlay 'idris-warning))

(defun idris-warning-overlay-at-point (point)
  "Return the overlay for a note starting at POINT, otherwise nil."
  (cl-find point (cl-remove-if-not 'idris-warning-overlay-p (overlays-at point))
        :key 'overlay-start))

(defun idris-warning-overlay (warning)
  "Add a compiler WARNING to the buffer as an overlay.
May merge overlays, if there's already one in the same location.
WARNING is of form (filename (startline startcolumn) (endline endcolumn)
message &optional highlighting-spans).
As of 20140807 (Idris 0.9.14.1-git:abee538) (endline endcolumn)
is mostly the same as (startline startcolumn)"
  (let* ((fullpath (concat (file-name-as-directory idris-process-current-working-directory)
                           (idris-warning-filename warning)))
         (buffer (get-file-buffer fullpath)))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen) ;; Show errors at the proper location in narrowed buffers
            (goto-char (point-min))
            (let* ((startline (idris-warning-startline warning))
                   (startcol (idris-warning-startcol warning))
                   (endline (idris-warning-endline warning))
                   (endcol (idris-warning-endcol warning))
                   (message (idris-warning-message warning))
                   (startp (line-beginning-position startline))
                   (endp (line-end-position startline))
                   (start (+ startp startcol))
                   (end (if (and (= startline endline) (= startcol endcol))
                            ;; a hack to have warnings, which point to empty lines, reported
                            (if (= startp endp)
                                (progn (goto-char startp)
                                       (insert " ")
                                       (1+ endp))
                              endp)
                          (+ (line-beginning-position endline) endcol)))
                   (overlay (idris-warning-overlay-at-point startp)))
              (if overlay
                  (idris-warning-merge-overlays overlay message)
                (idris-warning-create-overlay start end message)))))))))

(defun idris-warning-merge-overlays (overlay message)
  (overlay-put overlay 'help-echo
               (concat (overlay-get overlay 'help-echo) "\n" message)))

(defun idris-warning-create-overlay (start end message)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'idris-warning message)
    (overlay-put overlay 'help-echo message)
    (overlay-put overlay 'face 'idris-warning-face)
    (overlay-put overlay 'mouse-face 'highlight)
    (push overlay idris-warnings)
    (unless (memq (current-buffer) idris-warnings-buffers)
      (push (current-buffer) idris-warnings-buffers))
    overlay))

(provide 'idris-warnings)
