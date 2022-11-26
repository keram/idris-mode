;;; idris-tests3.el --- Tests for idris-mode  -*- lexical-binding: t -*-

;; Copyright (C)  2021 Yasu Watanabe

;; Author: Yasu Watanabe <ywata@protonmail.com>
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

;; Idris2 command action test

;;; Code:

(require 'idris-mode)
(require 'idris-commands)
(require 'inferior-idris)
(require 'idris-ipkg-mode)
(require 'cl-lib)
(require 'idris-test-utils)


;; (idris-test-text-update-command "test-data/CaseSplit.idr" idris-case-split '(lambda (x y) 'eq))
(ert-deftest idris-test-idris-run ()
  (let ((buffer (find-file "test-data/Empty.idr")))
    (should buffer)
    (with-current-buffer buffer
      (idris-run)
      (dotimes (_ 5) (accept-process-output nil 0.1))
      (should idris-process)
      )
    (kill-buffer))
  (idris-quit))


(ert-deftest idris-test-idris-connect-after-idris-run ()
  (let ((buffer (find-file "test-data/Empty.idr")))
    (with-current-buffer buffer
      (idris-load-file)
      (dotimes (_ 5) (accept-process-output nil 0.1))
      (should idris-connection)
      (kill-buffer)))
  (idris-quit))

(idris-ert-command-action "test-data/CaseSplit.idr" idris-case-split idris-test-eq-buffer)
(idris-ert-command-action "test-data/MakeLemma.idr" idris-make-lemma idris-test-eq-buffer)
(idris-ert-command-action "test-data/GenerateDef.idr" idris-generate-def idris-test-eq-buffer)
(idris-ert-command-action2 "test-data/AddClause.idr"
                           idris-add-clause
                           idris-test-eq-buffer)

(provide 'idris-tests3)
;;; idris-tests3.el ends here
