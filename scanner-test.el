;;; scanner-test.el --- Scan documents and images -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2021  Free Software Foundation, Inc

;; Author: Raffael Stocker <r.stocker@mnet-mail.de>
;; Maintainer: Raffael Stocker <r.stocker@mnet-mail.de>
;; Created: 05. Feb 2020
;; Version: 0.2
;; Package-Requires: ((emacs "25.1") (dash "2.12.0"))
;; Keywords: hardware, multimedia
;; URL: https://gitlab.com/rstocker/scanner.git

;; This file is NOT part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
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
(require 'scanner)
(require 'dash)
(require 'ert)


(ert-deftest scanner-test-make-scanimage-command ()
  "Test the scanimage command construction."
  (let ((scanner--scanimage-version-o-switch "0"))
    (should (-is-infix-p '("-o" "outfile")
			 (scanner--make-scanimage-command
			  (scanner--program-args
			   scanner--scanimage-argspec
			   :scan-type :doc :img-fmt "pnm"
			   :device-dependent nil)
			  "outfile")))
    (should-not (-is-infix-p (list shell-file-name shell-command-switch)
			 (scanner--make-scanimage-command
			  (scanner--program-args
			   scanner--scanimage-argspec
			   :scan-type :doc :img-fmt "pnm"
			   :device-dependent nil)
			  "outfile")))
    (should-not (string-match " > outfile\\'"
			  (car (last (scanner--make-scanimage-command
				      (scanner--program-args
				       scanner--scanimage-argspec
				       :scan-type :doc :img-fmt "pnm"
				       :device-dependent nil)
				      "outfile"))))))
  (let ((scanner--scanimage-version-o-switch "10"))
    (should-not (-is-infix-p '("-o" "outfile")
			     (scanner--make-scanimage-command
			      (scanner--program-args
			       scanner--scanimage-argspec
			       :scan-type :doc :img-fmt "pnm"
			       :device-dependent nil)
			      "outfile")))
    (should (-is-infix-p (list shell-file-name shell-command-switch)
			 (scanner--make-scanimage-command
			  (scanner--program-args
			   scanner--scanimage-argspec
			   :scan-type :doc :img-fmt "pnm"
			   :device-dependent nil)
			  "outfile")))
    (should (string-match " > outfile\\'"
			  (car (last (scanner--make-scanimage-command
				      (scanner--program-args
				       scanner--scanimage-argspec
				       :scan-type :doc :img-fmt "pnm"
				       :device-dependent nil)
				      "outfile")))))))


;; Note: interactive commands are only tested for their non-interactive
;; behavior
(ert-deftest scanner-test-set-image-resolution ()
  "Test the image resolution setter."
  (let ((scanner-resolution '(:image 600 :doc 600)))
    (should (eql 300 (progn (scanner-set-image-resolution 300)
			    (plist-get scanner-resolution :image))))
    (should (eql 600 (progn (scanner-set-image-resolution 300)
			    (plist-get scanner-resolution :doc))))))

(ert-deftest scanner-test-set-document-resolution ()
  "Test the document resolution setter."
  (let ((scanner-resolution '(:image 600 :doc 600)))
    (should (eql 600 (progn (scanner-set-document-resolution 300)
			    (plist-get scanner-resolution :image))))
    (should (eql 300 (progn (scanner-set-document-resolution 300)
			    (plist-get scanner-resolution :doc))))))

(ert-deftest scanner-test-select-papersize ()
  "Test the papersize selection command."
  (let ((scanner-paper-sizes '(:a4 (210 297) :a5 (148 210)))
	(scanner-doc-papersize :a5))
    (should (eq :a4 (scanner-select-papersize :a4)))))

(ert-deftest scanner-test-select-languages ()
  "Test the language selection command."
  (let ((scanner-tesseract-languages '("eng")))
    (should (equal '("deu") (scanner-select-languages '("deu"))))
    (should (equal '("deu" "eng") (scanner-select-languages '("deu" "eng"))))))

(ert-deftest scanner-test-select-outputs ()
  "Test the output selection command."
  (let ((scanner-tesseract-outputs '("pdf")))
    (should (equal '("txt") (scanner-select-outputs '("txt"))))
    (should (equal '("pdf" "txt") (scanner-select-outputs '("pdf" "txt"))))))

(ert-deftest scanner--cm-to-pixels ()
  "Test the cm to pixel conversion."
  (should (= 100 (scanner--cm-to-pixels 2.54 100)))
  (should (= 600 (scanner--cm-to-pixels 2.54 600)))
  (should (= 0 (scanner--cm-to-pixels 2.54 0)))
  (should (= 354 (scanner--cm-to-pixels 3 300)))
  (should (= 300 (scanner--cm-to-pixels 2.54508 300))))

(ert-deftest scanner--corner-pixels ()
  "Test the paper size to corner coordinates conversion."
  (should (equal '(0 0 35078 25393) (scanner--corner-pixels '(297 215) 300))))

(ert-deftest scanner--keyword-string ()
  "Test the keyword to string conversion."
  (should (string= "keyword" (scanner--keyword-string :keyword)))
  (should (string= "" (scanner--keyword-string :))))

(ert-deftest scanner--process-unpaper-size ()
  "Test paper size parsing."
  (should (eq nil (scanner--process-unpaper-size "none")))
  (should (eq :the-size (scanner--process-unpaper-size ":the-size")))
  (should (string= "297cm,210cm" (scanner--process-unpaper-size "297cm,210cm"))))

(provide 'scanner-test)


;; Local variables:
;; eval: (flycheck-mode)
;; End:

;;; scanner-test.el ends here
