;;; scanner-test.el --- Scan documents and images -*- lexical-binding: t -*-

;; Copyright (C) 2020  Free Software Foundation, Inc

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

(ert-deftest scanner-test-scanimage-args ()
  "Test the argument list construction for scanimage."
  ;; minimum args list (no device-specific options are available)
  (let ((switches nil)
	(scanner-device-name "devname")
	(scanner-image-size '(200 250))
	(-compare-fn #'string=))
    ;; known values are included with their switches
    (should (-is-infix-p '("-d" "devname") (scanner--scanimage-args :image
								    switches
								    "jpeg")))
    (should (-contains-p (scanner--scanimage-args :image switches "jpeg")
			 "--format=jpeg"))
    ;; device-specific options are not included in the argument list
    (should-not (-contains-p (scanner--scanimage-args :image switches "jpeg")
			     "--mode"))
    (should-not (-contains-p (scanner--scanimage-args :image switches "jpeg") 			     "--resolution"))
    (should-not (-contains-p (scanner--scanimage-args :image switches "jpeg")
			     "-x"))
    (should-not (-contains-p (scanner--scanimage-args :image switches "jpeg")
			     "-y"))
    (should-not (-contains-p (scanner--scanimage-args :doc switches "jpeg")
			     "-x"))
    (should-not (-contains-p (scanner--scanimage-args :doc switches "jpeg")
			     "-y"))
    ;; without format and device name, these are not in the args list
    (let ((scanner-image-format nil)
	  (scanner-device-name nil))
      (should-not (-contains-p (scanner--scanimage-args :image switches "jpeg")
			       "--format="))
      (should-not (-contains-p (scanner--scanimage-args :image switches "jpeg")
			       "-d"))))
  ;; image args list with device-specific args
  (let ((switches '("--resolution" "-x" "-y" "--mode"))
	(scanner-resolution '(:doc 300 :image 600))
	(scanner-scan-mode '(:image "Color" :doc "Gray"))
	(scanner-doc-papersize :a4)
	(scanner-paper-sizes '(:a4 (210 297)))
	(scanner-image-size '(200 250))
	(-compare-fn #'string=))
    (should (-contains-p (scanner--scanimage-args  :image switches "jpeg")
			 "--format=jpeg"))
    (should (-contains-p (scanner--scanimage-args  :image switches "jpeg")
			 "--mode=Color"))
    (should (-contains-p (scanner--scanimage-args  :image switches "jpeg")
			 "--resolution=600"))
    (should (-is-infix-p '("-x" "210") (scanner--scanimage-args  :doc
								 switches "jpeg")))
    (should (-is-infix-p '("-y" "297") (scanner--scanimage-args  :doc
								 switches "jpeg")))
    (should (-is-infix-p '("-x" "200") (scanner--scanimage-args  :image
								 switches "jpeg")))
    (should (-is-infix-p '("-y" "250") (scanner--scanimage-args  :image
								 switches "jpeg")))
    (should (-contains-p (scanner--scanimage-args  :doc switches "jpeg")
			 "--format=jpeg"))
    (should (-contains-p (scanner--scanimage-args  :doc switches "jpeg")
			 "--mode=Gray"))
    (should (-contains-p (scanner--scanimage-args  :doc switches "jpeg")
			 "--resolution=300"))
    (let ((scanner-image-size nil))
      (should-not (-contains-p (scanner--scanimage-args  :image
							 switches "jpeg")
			       "-x"))
      (should-not (-contains-p (scanner--scanimage-args :image
							switches "jpeg")
			       "-y")))
    (let ((scanner-doc-papersize :whatever))
      (should-not (-contains-p (scanner--scanimage-args  :doc
							 switches "jpeg")
			       "-x"))
      (should-not (-contains-p (scanner--scanimage-args :doc
							switches "jpeg")
			       "-y")))))

(ert-deftest scanner-test-make-scanimage-command ()
  "Test the scanimage command construction."
  (let ((scanner--scanimage-version-o-switch "0"))
    (should (-is-infix-p '("-o" "outfile")
			 (scanner--make-scanimage-command
			  (scanner--scanimage-args :doc nil "pnm")
			  "outfile")))
    (should-not (-is-infix-p (list shell-file-name shell-command-switch)
			 (scanner--make-scanimage-command
			  (scanner--scanimage-args :doc nil "pnm")
			  "outfile")))
    (should-not (string-match " > outfile$"
			  (car (last (scanner--make-scanimage-command
				      (scanner--scanimage-args :doc nil "pnm")
				      "outfile"))))))
  (let ((scanner--scanimage-version-o-switch "10"))
    (should-not (-is-infix-p '("-o" "outfile")
			     (scanner--make-scanimage-command
			      (scanner--scanimage-args :doc nil "pnm")
			      "outfile")))
    (should (-is-infix-p (list shell-file-name shell-command-switch)
			 (scanner--make-scanimage-command
			  (scanner--scanimage-args :doc nil "pnm")
			  "outfile")))
    (should (string-match " > outfile$"
			  (car (last (scanner--make-scanimage-command
				      (scanner--scanimage-args :doc nil "pnm")
				      "outfile")))))))

(ert-deftest scanner-test-tesseract-args ()
  "Test the argument list construction for tesseract."
  (let ((scanner-resolution '(:image 600 :doc 300))
	(scanner-tesseract-languages '("eng" "deu"))
	(scanner-tesseract-switches '("--opt1" "--opt2"))
	(scanner-tesseract-outputs '("out1" "out2"))
	(scanner-tessdata-dir "/usr/share/tessdata/")
	(-compare-fn #'string=))
    (should (-is-infix-p '("-l" "eng+deu") (scanner--tesseract-args "infile"
								    "outfile")))
    (let ((scanner--tesseract-version-dpi-switch "0"))
      (should (-is-infix-p '("--dpi" "300") (scanner--tesseract-args "infile"
								     "outfile"))))
    (let ((scanner--tesseract-version-dpi-switch "1000"))
      (should-not (-is-infix-p '("--dpi" "300") (scanner--tesseract-args "infile"
									 "outfile"))))
    (should (-contains-p (scanner--tesseract-args "infile" "outfile") "--opt1"))
    (should (-contains-p (scanner--tesseract-args "infile" "outfile") "--opt2"))
    (should (-contains-p (scanner--tesseract-args "infile" "outfile") "infile"))
    (should (-contains-p (scanner--tesseract-args "infile" "outfile") "outfile"))
    (should (-contains-p (scanner--tesseract-args "infile" "outfile") "out1"))
    (should (-contains-p (scanner--tesseract-args "infile" "outfile") "out2"))
    (should (-is-infix-p '("--tessdata-dir" "/usr/share/tessdata/")
			 (scanner--tesseract-args "infile" "outfile")))))

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

(provide 'scanner-test)


;; Local variables:
;; eval: (flycheck-mode)
;; End:

;;; scanner-test.el ends here
