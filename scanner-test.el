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

(ert-deftest scanner-test-determine-image-format ()
  "Test format determination from extension."
  (let ((scanner-image-format '(:image "img-def" :doc "doc-def")))
    (should (string= "jpeg" (scanner--determine-image-format "jpg")))
    (should (string= "jpeg" (scanner--determine-image-format "jpeg")))
    (should (string= "tiff" (scanner--determine-image-format "tiff")))
    (should (string= "tiff" (scanner--determine-image-format "tif")))
    (should (string= "pnm" (scanner--determine-image-format "pnm")))
    (should (string= "png" (scanner--determine-image-format "png")))
    (should (string= "jpeg" (scanner--determine-image-format "JPG")))
    (should (string= "jpeg" (scanner--determine-image-format "jpg")))
    (should (string= "jpeg" (scanner--determine-image-format "jpeg")))
    (should (string= "tiff" (scanner--determine-image-format "tiff")))
    (should (string= "tiff" (scanner--determine-image-format "tif")))
    (should (string= "pnm" (scanner--determine-image-format "pnm")))
    (should (string= "png" (scanner--determine-image-format "png")))
    (should (string= "jpeg" (scanner--determine-image-format "JPG")))
    (should (string= "img-def" (scanner--determine-image-format nil)))
    (should (string= "img-def" (scanner--determine-image-format "")))
    (should (string= "img-def" (scanner--determine-image-format 42)))
    (should-error (scanner--determine-image-format '(42))
		  :type 'wrong-type-argument)))

;; FIXME the order of some arguments is relevant, test this.
(ert-deftest scanner-test-scanimage-args ()
  "Test the argument list construction for scanimage."
  ;; minimum args list (no required options)
  (let ((scanner--available-options nil)
	(scanner-image-format '(:image "fmt-img" :doc "fmt-doc"))
	(scanner-device-name "devname"))
    (should (cl-subsetp '("-d" "devname") (scanner--scanimage-args "file"
								   :image)
			:test #'string=))
    (should (cl-subsetp '("-o" "file") (scanner--scanimage-args "file"
								:image)
			:test #'string=))
    (should (member "--format=fmt-img" (scanner--scanimage-args "file" :image)))
    (should (member "--format=arg-fmt" (scanner--scanimage-args "file"
								:image
								"arg-fmt")))
    (should-not (member "--mode" (scanner--scanimage-args "file" :image)))
    (should-not (member "--resolution" (scanner--scanimage-args "file"
								:image)))
    (should-not (member "-x" (scanner--scanimage-args "file" :image)))
    (should-not (member "-y" (scanner--scanimage-args "file" :image)))
    (should-not (member "-x" (scanner--scanimage-args "file" :doc)))
    (should-not (member "-y" (scanner--scanimage-args "file" :doc)))
    (let ((scanner-image-format nil)
	  (scanner-device-name nil))
      (should-not (member "--format=" (scanner--scanimage-args "file"
							       :image)))
      (should-not (member "-d" (scanner--scanimage-args "file" :image)))))
  ;; image args list (present/missing args)
  (let ((scanner--available-options '("--resolution" "-x" "-y" "--mode"))
	(scanner-image-format '(:image "fmt-img" :doc "fmt-doc"))
	(scanner-resolution '(:doc 300 :image 600))
	(scanner-scan-mode '(:image "Color" :doc "Gray"))
	(scanner-doc-papersize :a4)
	(scanner-paper-sizes '(:a4 (210 297))))
    (should (cl-subsetp '("-o" "file") (scanner--scanimage-args "file" :image)
			:test #'string=))
    (should (member "--format=fmt-img" (scanner--scanimage-args "file"
								:image)))
    (should (member "--format=arg-fmt" (scanner--scanimage-args "file"
								:image
								"arg-fmt")))
    (should (member "--mode=Color" (scanner--scanimage-args "file"
							    :image)))
    (should (member "--resolution=600" (scanner--scanimage-args "file"
								:image)))
    (should (cl-subsetp '("-x" "210") (scanner--scanimage-args "file" :doc)
			:test #'string=))
    (should (cl-subsetp '("-y" "297") (scanner--scanimage-args "file" :doc)
			:test #'string=))
    )
  ;; doc args list (present/missing args)
  (let ((scanner--available-options '("--resolution" "-x" "-y" "--mode"))
	(scanner-image-format '(:image "fmt-img" :doc "fmt-doc"))
	(scanner-resolution '(:doc 300 :image 600))
	(scanner-scan-mode '(:image "Color" :doc "Gray"))
	(scanner-doc-papersize :a4)
	(scanner-paper-sizes '(:a4 (210 297))))
    (should (cl-subsetp '("-o" "file") (scanner--scanimage-args "file" :doc)
			:test #'string=))
    (should (member "--format=fmt-doc" (scanner--scanimage-args "file"
								:doc)))
    (should (member "--format=arg-fmt" (scanner--scanimage-args "file"
								:doc
								"arg-fmt")))
    (should (member "--mode=Gray" (scanner--scanimage-args "file"
							   :doc)))
    (should (member "--resolution=300" (scanner--scanimage-args "file"
								:doc)))
    (should (cl-subsetp '("-x" "210") (scanner--scanimage-args "file" :doc)
			:test #'string=))
    (should (cl-subsetp '("-y" "297") (scanner--scanimage-args "file" :doc)
			:test #'string=))))


;; FIXME the order of some arguments is relevant, test this.
(ert-deftest scanner-test-tesseract-args ()
  "Test the argument list construction for tesseract."
  (let ((scanner-resolution '(:image 600 :doc 300))
	(scanner-tesseract-languages '("eng" "deu"))
	(scanner-tesseract-options '("--opt1" "--opt2"))
	(scanner-tesseract-outputs '("out1" "out2")))
    (should (cl-subsetp '("-l" "eng+deu") (scanner--tesseract-args "infile"
								   "outfile")
			:test #'string=))
    (should (cl-subsetp '("--dpi" "300") (scanner--tesseract-args "infile"
								  "outfile")
			:test #'string=))
    (should (member "--opt1" (scanner--tesseract-args "infile" "outfile")))
    (should (member "--opt2" (scanner--tesseract-args "infile" "outfile")))
    (should (member "infile" (scanner--tesseract-args "infile" "outfile")))
    (should (member "outfile" (scanner--tesseract-args "infile" "outfile")))
    (should (member "out1" (scanner--tesseract-args "infile" "outfile")))
    (should (member "out2" (scanner--tesseract-args "infile" "outfile")))))


(provide 'scanner-test)


;; Local variables:
;; eval: (flycheck-mode)
;; End:

;;; scanner-test.el ends here
