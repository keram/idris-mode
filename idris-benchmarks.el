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

;; (setq hss '(((((:filename "AddMissing.idr") (:start 0 0) (:end 0 6)) ((:decor :keyword))))
;;             ((((:filename "AddMissing.idr") (:start 0 7) (:end 0 17)) ((:decor :module))))
;;             ((((:filename "AddMissing.idr") (:start 1 0) (:end 1 49)) ((:decor :comment))))
;;             ((((:filename "AddMissing.idr") (:start 2 0) (:end 2 4)) ((:decor :keyword))))
;;             ((((:filename "AddMissing.idr") (:start 2 5) (:end 2 9)) ((:name "Test") (:namespace "AddMissing") (:decor :type) (:implicit :False) (:key "") (:doc-overview "") (:type ""))))
;;             ((((:filename "AddMissing.idr") (:start 2 5) (:end 2 9)) ((:name "Test") (:namespace "AddMissing") (:decor :type) (:implicit :False) (:key "") (:doc-overview "") (:type ""))))
;;             ((((:filename "AddMissing.idr") (:start 2 5) (:end 2 9)) ((:decor :type))))
;;             ((((:filename "AddMissing.idr") (:start 2 10) (:end 2 11)) ((:decor :keyword))))
;;             ((((:filename "AddMissing.idr") (:start 2 12) (:end 2 13)) ((:decor :data))))
;;             ((((:filename "AddMissing.idr") (:start 2 14) (:end 2 15)) ((:decor :keyword))))
;;             ((((:filename "AddMissing.idr") (:start 2 16) (:end 2 17)) ((:decor :data))))
;;             ((((:filename "AddMissing.idr") (:start 4 0) (:end 4 4)) ((:decor :function))))
;;             ((((:filename "AddMissing.idr") (:start 4 5) (:end 4 6)) ((:decor :keyword))))
;;             ((((:filename "AddMissing.idr") (:start 4 7) (:end 4 11)) ((:name "Test") (:namespace "AddMissing") (:decor :type) (:implicit :False) (:key "") (:doc-overview "") (:type ""))))
;;             ((((:filename "AddMissing.idr") (:start 4 12) (:end 4 14)) ((:decor :keyword))))
;;             ((((:filename "AddMissing.idr") (:start 4 15) (:end 4 18)) ((:decor :type))))
;;             ((((:filename "AddMissing.idr") (:start 5 0) (:end 5 4)) ((:decor :comment))))))

;; (with-current-buffer "AddMissing.idr"
;;   ;; (idris-delete-source-highlight-overlays)
;;   ;; (mapc 'idris-highlight-source-file hss)
;;   (benchmark-run 1
;;     (idris-delete-source-highlight-overlays)
;;     (mapc 'idris-highlight-source-file hss)))

(defun idris-delete-source-highlight-overlays (buffer)
  (with-current-buffer buffer
    (mapc 'delete-overlay (seq-filter
                         (lambda (overlay)
                           (overlay-get overlay 'idris-source-highlight))
                         (overlays-in (point-min) (point-max))))))

(defun idris-apply-source-highlight (relative-filepath hss fn)
  (let* ((buffer (find-file-noselect relative-filepath))
        (idris-process-current-working-directory (file-name-directory (buffer-file-name buffer)))
        (idris-protocol-version 3))
    (idris-delete-source-highlight-overlays buffer)
    (with-current-buffer buffer
      (mapc fn hss))))

(defun idris-run-overlay-benchmark (n relative-filepath hss bench-fn)
  (let ((gc-cons-threshold #x40000000)) ;; most-positive-fixnum
    (idris-delete-source-highlight-overlays (find-file-noselect relative-filepath))
    (garbage-collect)
    (message "bench fn: %s" bench-fn)
    (message "result: %s"
             (benchmark-run-compiled n
               (idris-apply-source-highlight relative-filepath hss bench-fn)))))

;; (idris-delete-source-highlight-overlays (find-file-noselect "test-data/perf/LambdaLift.idr"))
;; (garbage-collect)
;; (idris-apply-source-highlight "test-data/perf/LambdaLift.idr" hss)
;; (setq gc-cons-threshold #x40000000)
;; (memory-report)

(defun idris-highlight-source-file-mapc (hs)
  "Highlight source file based on highlight source block (HS) from Idris."
  (mapc (lambda (h)
          (pcase h
            (`(((:filename ,fn)
                (:start ,start-line-raw ,start-col-raw)
                (:end ,end-line-raw ,end-col-raw))
               ,props)
             (let ((buffer (get-file-buffer (expand-file-name fn idris-process-current-working-directory))))
               (when buffer
                 (let ((start-line (if (>=-protocol-version 2 1)
                                       (1+ start-line-raw)
                                     start-line-raw))
                       (start-col  (if (>=-protocol-version 2 1)
                                       (1+ start-col-raw)
                                     start-col-raw))
                       (end-line   (if (>=-protocol-version 2 1)
                                       (1+ end-line-raw)
                                     end-line-raw))
                       (end-col    (if (>= idris-protocol-version 1)
                                       (1+ end-col-raw)
                                     end-col-raw)))
                   (idris-highlight-input-region buffer
                                                 start-line start-col
                                                 end-line end-col
                                                 props)))))))
        hs))

(defun idris-highlight-source-file-dolist (hs)
  "Highlight source file based on highlight source block (HS) from Idris."
  (dolist (h hs)
    (pcase h
      (`(((:filename ,fn)
          (:start ,start-line-raw ,start-col-raw)
          (:end ,end-line-raw ,end-col-raw))
         ,props)
       (let ((buffer (get-file-buffer (expand-file-name fn idris-process-current-working-directory))))
         (when buffer
           (let ((start-line (if (>=-protocol-version 2 1)
                                 (1+ start-line-raw)
                               start-line-raw))
                 (start-col  (if (>=-protocol-version 2 1)
                                 (1+ start-col-raw)
                               start-col-raw))
                 (end-line   (if (>=-protocol-version 2 1)
                                 (1+ end-line-raw)
                               end-line-raw))
                 (end-col    (if (>= idris-protocol-version 1)
                                 (1+ end-col-raw)
                               end-col-raw)))
             (idris-highlight-input-region buffer
                                           start-line start-col
                                           end-line end-col
                                           props))))))
    ))
;; (idris-delete-source-highlight-overlays (find-file-noselect "test-data/perf/LambdaLift.idr"))
;; (garbage-collect)
;; (benchmark-run-compiled 1 (progn
;;                             (idris-apply-source-highlight "test-data/perf/LambdaLift.idr" hss)))
;; original: (0.9286343739999999 2 0.21494645699999992)
;; mapc: (0.961673232 2 0.21034819199999788)
;; (benchmark-run-compiled 1
;;   (progn
;;     (idris-apply-source-highlight "test-data/perf/LambdaLift.idr" hss
;;                                   'idris-highlight-source-file-mapc)))

(load-file "test-data/perf/lambda-lift-source-hss.el")

(defalias 'idris-highlight-source-file-bench
  (apply-partially 'idris-run-overlay-benchmark 10 "test-data/perf/LambdaLift.idr" lambda-lift-source-hss))

(dotimes (_ 5)
  (idris-highlight-source-file-bench 'idris-highlight-source-file)
  ;; (idris-highlight-source-file-bench 'idris-highlight-source-file-mapc)
  (idris-highlight-source-file-bench 'idris-highlight-source-file-dolist)
  (idris-highlight-source-file-bench 'idris-highlight-source-file-v2))

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

(defalias 'idris-profile-overlay-10
  (apply-partially 'idris-profile 10 "test-data/perf/LambdaLift.idr" lambda-lift-source-hss))

(idris-profile-overlay-10 'idris-highlight-source-file)

(defun idris-elp (n relative-filepath hss bench-fn)
  (let ((gc-cons-threshold #x40000000)) ;; most-positive-fixnum
    (idris-delete-source-highlight-overlays (find-file-noselect relative-filepath))
    (elp-instrument-package "idris")
    (garbage-collect)
    (message "profile fn: %s" bench-fn)
    (dotimes (_ n)
      (idris-delete-source-highlight-overlays (find-file-noselect relative-filepath))
      (idris-apply-source-highlight relative-filepath hss bench-fn))
    (elp-results)
    ))

(defalias 'idris-elp-10
  (apply-partially 'idris-elp 10 "test-data/perf/LambdaLift.idr" lambda-lift-source-hss))

;; (idris-elp-10 'idris-highlight-source-file)
;; idris-apply-source-highlight                    10          7.7770215369  0.7777021537
;; idris-highlight-source-file                     38750       7.7220788639  0.0001992794
;; idris-highlight-input-region                    38750       6.8874254789  0.0001777400
;; idris-delete-source-highlight-overlays          20          1.8147408170  0.0907370408
;; idris-semantic-properties                       37650       0.7098705620  1.885...e-05
;; idris-semantic-properties-face                  37650       0.1882114359  4.998...e-06
;; idris-semantic-properties-help-echo             37650       0.1288993830  3.423...e-06
;; idris-semantic-properties-eldoc                 37650       0.126822631   3.368...e-06
;; idris-add-overlay-properties                    37650       0.0272550330  7.239...e-07
;; idris-highlight-column                          77500       0.0179031210  2.310...e-07

;; (elp-reset-list)
;; (elp-restore-all)

;; bench fn: idris-highlight-source-file-mapc
;; 5.893836093199999
;; bench fn: idris-highlight-source-file
;; 5.9665548369999994

;; (let ((gc-cons-threshold #x40000000)
;;       (gc-cons-percentage 0.5))
;;   (idris-delete-source-highlight-overlays (find-file-noselect "test-data/perf/LambdaLift.idr"))
;;   (garbage-collect)
;;   (profiler-start 'cpu)
;;   (idris-apply-source-highlight "test-data/perf/LambdaLift.idr" hss)
;;   (profiler-stop)
;;   (profiler-report))

;; (let ((gc-cons-threshold #x40000000)
;;       (gc-cons-percentage 0.5))
;;   (idris-delete-source-highlight-overlays (find-file-noselect "test-data/perf/LambdaLift.idr"))
;;   (garbage-collect)
;;   (let (;(elp-function-list '(idris-highlight-source-file idris-highlight-input-region idris-add-overlay-properties))
;;         (elp-instrument-package 'idris))
;;     (elp-instrument-list)
;;     (dotimes (i 2)
;;       (idris-delete-source-highlight-overlays (find-file-noselect "test-data/perf/LambdaLift.idr"))
;;       (idris-apply-source-highlight "test-data/perf/LambdaLift.idr" hss))
;;     (elp-results)
;;     (elp-reset-list)))

;; (let ((gc-cons-threshold #x40000000)
;;       (gc-cons-percentage 0.5))
;;   (idris-delete-source-highlight-overlays (find-file-noselect "test-data/perf/LambdaLift.idr"))
;;   (garbage-collect)
;;   (benchmark-run 100
;;     (idris-apply-source-highlight "test-data/perf/LambdaLift.idr" hss)))

;; (let ((gc-cons-threshold #x40000000)
;;       (gc-cons-percentage 0.5))
;;   (idris-delete-source-highlight-overlays (find-file-noselect "test-data/perf/LambdaLift.idr"))
;;   (garbage-collect)
;;     (benchmark-run-compiled 10
;;       (idris-apply-source-highlight "test-data/perf/LambdaLift.idr" hss)))

;; (elp-restore-all)
(provide 'idris-benchmarks)
;;; idris-benchmarks.el ends here
