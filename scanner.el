;;; scanner.el --- Scan documents and images -*- lexical-binding: t -*-

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

;; Scan documents and images...

;;; Code:

(require 'dash)
(require 'cl-lib)

(defgroup scanner nil
  "Scan documents and images."
  :group 'Multimedia)

(defcustom scanner-doc-resolution 300
  "Default resolution for documents."
  :type '(integer))

(defcustom scanner-image-resolution 600
  "Default resolution for images."
  :type '(integer))

(defcustom scanner-paper-sizes
  '(:a3
    (297 420)
    :a4
    (210 297)
    :a5
    (148 210)
    :a6
    (105 148)
    :tabloid
    (279.4 431.8)
    :legal
    (215.9 355.6)
    :letter
    (215.9 279.4))
  "List of paper sizes for documents.
The first of each pair of numbers is the paper width in mm,
the second is the height in mm."
  :type '(plist :value-type (group number number)))

(defcustom scanner-doc-papersize
  :a4
  "Default document paper size.
The value must be one of the keys in the paper sizes list."
  :type '(restricted-sexp :match-alternatives
			  ((lambda (k) (plist-member scanner-paper-sizes k)))))

(defcustom scanner-image-format
  "jpeg"
  "Default image file format."
  :type '(radio (const "jpeg")
		(const "png")
		(const "pnm")
		(const "tiff")))

(defcustom scanner-scanimage-program
  (executable-find "scanimage")
  "Path of the scanimage(1) program."
  :type '(string))

(defcustom scanner-img2pdf-program
  (executable-find "img2pdf")
  "Path of the img2pdf program."
  :type '(string))

(defcustom scanner-tesseract-program
  (executable-find "tesseract")
  "Path of the tesseract(1) program."
  :type '(string))

(defun scanner--validate-languages (widget)
  "Validate the language selection in customization WIDGET."
  (let ((val (widget-value widget))
	(langs (cdr (process-lines scanner-tesseract-program
				   "--list-langs"))))
    (if (cl-subsetp val langs :test #'string=)
	nil
      (widget-put widget
		  :error
		  (format "Unknown languages: %s; available are: %s"
			  (mapconcat #'identity val ", ")
			  (mapconcat #'identity langs ", ")))
      widget)))

(defcustom scanner-tesseract-languages
  '("eng" "deu")
  "List of languages passed to tesseract(1) for OCR."
  :type '(repeat :validate scanner--validate-languages string))

(defcustom scanner-tesseract-outputs
  '("pdf" "txt")
  "List of output formats to produce.
The output formats correspond to the names of config files that
come with tesseract(1).
The config files may reside in ‘/usr/share/tessdata/configs’."
  :type '(repeat string))

(defcustom scanner-tesseract-options
  '()
  "Additional options to pass to tesseract(1)."
  :type '(repeat string))

(defcustom scanner-scan-mode
  "Color"
  "Scan mode."
  :type '(radio (const "Color")
		(const "Gray")
		(const "Lineart")))

(defcustom scanner-scanimage-options
  '()
  "Additional options to be passed to scanimage(1)."
  :type '(repeat string))

(defcustom scanner-device-name
  nil
  "SANE scanner device name or nil."
  :type '(restricted-sexp :match-alternatives
			  (stringp 'nil)))

;; TODO: check for availability of -x and -y arguments and
;; use them according to the configured paper size
(defun scanner--scanimage-args (outfile format)
  "Construct the argument list for scanimage(1).
OUTFILE is the output filename and FORMAT is the output image format."
  (-flatten (list "--format" format
		  "--output-file" outfile
		  scanner-scanimage-options)))

(defun scanner--tesseract-args (input output-base)
  "Construct the argument list for ‘tesseract(1)’.
INPUT is the input file name, OUTPUT-BASE is the basename for the
output files.  Extensions are added automatically depending on the
selected output options, see ‘scanner-tesseract-outputs’."
  (-flatten (list input output-base
		  "-l" (mapconcat #'identity scanner-tesseract-languages "+")
		  "--dpi" (number-to-string scanner-doc-resolution)
		  scanner-tesseract-options
		  scanner-tesseract-outputs)))

(defun scanner-scan-document (&optional _filename)
  "Scan a document named FILENAME."
  (interactive)
  ;; loop in y-or-n-p over pages of the document
  ;; write scanned images to temp files
  ;; convert to temp pdf
  ;; ask for filename and write file?
  ;; open PDF if configured to do so
  ;; optional filename is for programmatic use (map over list of filenames)
  ;; optional pause for document change?
  ;; use prefix arg to decide whether to scan one or many docs
  ;; always find ways to design functions such that automation and
  ;;  functional programming is possible
  ;; write tests using ert
  )

(defun scanner-scan-image ()
  "Scan an image."
  (interactive)
  ;; write scanned image to temp file
  ;; ask for filename and write file
  ;; open image if configured to do so
  )

(provide 'scanner)

;; Local variables:
;; eval: (flycheck-mode)
;; End:

;;; scanner.el ends here
