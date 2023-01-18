;;; idris-benchmarks.el --- Benchmarks for idris-mode  -*- lexical-binding: nil -*-

;; Copyright (C) 2022  Marek L.

;; Author: Marek L <nospam.keram@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a collection of benchmarks for profiling idris-mode.

;;; Code:

(require 'idris-mode)
(require 'benchmark)
(require 'elp)
(require 'profiler)

(defun idris-delete-source-highlight-overlays (buffer)
  (with-current-buffer buffer
    (mapc 'delete-overlay (seq-filter
                         (lambda (overlay)
                           (overlay-get overlay 'idris-source-highlight))
                         (overlays-in (point-min) (point-max))))))

(defun idris-run-overlay-benchmark (n relative-filepath hss bench-fn)
  (let ((gc-cons-threshold #x40000000)) ;; most-positive-fixnum
    (idris-delete-source-highlight-overlays (find-file-noselect relative-filepath))
    (garbage-collect)
    (benchmark-run-compiled n
      (idris-apply-source-highlight relative-filepath hss bench-fn))))

(defun idris-simplified-dispatch-event (event)
  (or (run-hook-with-args-until-success 'idris-event-hooks event)
      (destructure-case event
        ((:output value _id)
         (pcase (cadr value)
           (`(:highlight-source ,hs) (idris-highlight-source-file hs))
           (_ (progn (message "-t- unknown output value: %s" value) t))))
        ((:return value _id)
         (message "return value: %s" value)))))

(defun idris-mapf (fn fname)
  "Apply function FN to every expresion read from buffer visiting FNAME file.
Produces side-effects only."
  (let ((cur-buf (current-buffer))
        (buf (find-file-noselect fname))
        sexp)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (ignore-errors (setq sexp (read buf)))
        (with-current-buffer cur-buf
          (funcall fn sexp))))))

(defun idris-mapb (fn buffer)
  "Apply function FN to every expresion read from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min)))
  (while (ignore-errors (setq sexp (read buffer)))
    (funcall fn sexp)))

(defun idris-apply-events-to-buffer (buffer-file events-file)
  (let ((buf (find-file-noselect buffer-file))
        (ev-buf (find-file-noselect events-file)))
    (with-current-buffer buf
      (idris-mapb 'idris-simplified-dispatch-event ev-buf))))

(defun idris-run-highlight-events-benchmark (buffer-file events-file n)
  (let ((gc-cons-threshold #x40000000)
        (progress-reporter (make-progress-reporter "Collecting benchmark data..."))
        (idris-protocol-version 3)) ;; most-positive-fixnum
    (message "Cleaning existing overlays")
    (idris-delete-source-highlight-overlays (find-file-noselect buffer-file))
    (and (get-buffer events-file) (kill-buffer events-file))
    (garbage-collect)
    (prin1 (benchmark-run-compiled n
       (idris-apply-events-to-buffer buffer-file events-file)))
    (progress-reporter-done progress-reporter)))

(idris-run-highlight-events-benchmark
 "./test/test-data/perf/LambdaLift.idr"
 "./test/test-data/perf/idris2-load-file-src-Compiler-LambdaLift.el" 1)

;; (let ((f "./test/test-data/perf/idris2-load-file-src-Compiler-LambdaLift.el"))
;;   (and (get-buffer f) (kill-buffer f)))
;; (let ((f "./test/test-data/perf/test.el"))
;;   (and (get-buffer f) (kill-buffer f)))

;; (idris-apply-events-to-buffer
;;  "./test/test-data/perf/LambdaLift.idr"
;;  "./test/test-data/perf/test.el"
;;  ;;"./test/test-data/perf/idris2-load-file-src-Compiler-LambdaLift.el"
;;  )

;; (and (get-buffer "test.el") (kill-buffer "test.el"))
;; (with-current-buffer "LambdaLift.idr"
;;    (idris-mapf 'idris-simplified-dispatch-event "idris2-load-file-src-Compiler-LambdaLift.el"))

(defun idris-profile (n relative-filepath hss bench-fn)
  (let ((gc-cons-threshold #x40000000)) ;; most-positive-fixnum
    (idris-delete-source-highlight-overlays (find-file-noselect relative-filepath))
    (garbage-collect)
    (message "profile fn: %s" bench-fn)
    (profiler-start 'cpu)
    (dotimes (_ n)
      (idris-delete-source-highlight-overlays (find-file-noselect relative-filepath))
      (idris-apply-source-highlight relative-filepath hss bench-fn))
    (profiler-stop)
    (profiler-report)
    ))

(provide 'idris-benchmarks)
;;; idris-benchmarks.el ends here
