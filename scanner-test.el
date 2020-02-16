;;; scanner-test.el --- Scan documents and images -*- lexical-binding: t -*-

;; Copyright (C) 2020 Raffael Stocker

;; Author: Raffael Stocker <r.stocker@mnet-mail.de>
;; Maintainer: Raffael Stocker <r.stocker@mnet-mail.de>
;; Created: 05. Feb 2020
;; Version: 0.0
;; Package-Requires: ((emacs "25.1") (dash "2.12.0"))
;; Keywords: hardware multimedia
;; URL: https://gitlab.com/rstocker/scanner.git

;; This file is NOT part of GNU Emacs

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test the scanner package.
;; Rules:
;; - clean up after tests (using ‘save-*exursion’, ‘unwind-protect’ etc.)
;; - don't alter Emacs's state
;; - tests should not depend on the current state of the environment
;; - ‘let’-bind required variables, don't setq them
;; - ‘skip-unless’ tests if they have dependencies that might not be met
;;   on the test machine
;; - use temp buffers if necessary, maybe bind hooks to nil etc.
;; - tests should restore the environment, if they have side-effects on it
;; - if writing a test is difficult, maybe refactor the code under test for
;;   testability
;; - use :tag to mark tests that need a connection to a scanner, e.g.
;;   :tag :needs-hardware
;; - group tests by choosing sensible names
;; - see the ert doc for a way to implement test fixtures


;;; Code:

(load-file "scanner.el")
(require 'scanner)
(require 'dash)
(require 'ert)

(ert-deftest scanner-test-determine-format ()
  "Test format determination from extension."
  (let ((scanner-image-format "default"))
    (should (string= "jpeg" (scanner--determine-format "jpg")))
    (should (string= "jpeg" (scanner--determine-format "jpeg")))
    (should (string= "tiff" (scanner--determine-format "tiff")))
    (should (string= "tiff" (scanner--determine-format "tif")))
    (should (string= "pnm" (scanner--determine-format "pnm")))
    (should (string= "png" (scanner--determine-format "png")))
    (should (string= "jpeg" (scanner--determine-format "JPG")))
    (should (string= "default" (scanner--determine-format nil)))
    (should (string= "default" (scanner--determine-format "")))
    (should (string= "default" (scanner--determine-format 42)))
    (should-error (scanner--determine-format '(42))
		  :type 'wrong-type-argument)))


(provide 'scanner-test)


;; Local variables:
;; eval: (flycheck-mode)
;; End:

;;; scanner-test.el ends here
