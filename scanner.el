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

(defcustom scanner-doc-intermediate-format
  "png"
  "Intermediate image format for document scanning."
  :type '(radio (const "jpeg")
		(const "png")
		(const "pnm")
		(const "tiff")))

(defcustom scanner-image-format
  "jpeg"
  "Image file format."
  :type '(radio (const "jpeg")
		(const "png")
		(const "pnm")
		(const "tiff")))

(defcustom scanner-doc-intermediate-format
  "png"
  "Intermediate image format for document scanning."
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
		  (format "Unknown language(s): %s; available are: %s"
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
  "Additional options to pass to tesseract(1).
FORM?"
  :type '(repeat string))

(defcustom scanner-scan-mode
  "Color"
  "Scan mode."
  :type '(string))

(defcustom scanner-scanimage-options
  '()
  "Additional options to be passed to scanimage(1).
FORM?"
  :type '(repeat string))

(defcustom scanner-device-name
  nil
  "SANE scanner device name or nil.
If nil, auto-detection will be attempted."
  :type '(restricted-sexp :match-alternatives
			  (stringp 'nil)))

(defvar scanner--detected-devices
  nil
  "List of devices detected by SANE.
Each element of the list has the form (DEVICE TYPE MODEL) where
DEVICE is the SANE device name, TYPE the type of the device
\(e.g. \"flatbed scanner\",) and MODEL is the device's model
name.")

(eval-when-compile
  (defconst scanner--device-specific-options
    '("--mode" "--depth" "--resolution" "-x" "-y")
    "List of required device specific options.

These options are necessary for the full set of features offered
by the scanner package.  If one of these is missing, something may
not work as expected."))

(defconst scanner--device-option-re
  (eval-when-compile (regexp-opt scanner--device-specific-options t)))

(defvar scanner--available-options
  nil
  "List of required options implemented by the device backend.")

(defvar scanner--missing-options
  nil
  "List of required options missing from the device backend.")


(defun scanner--check-device-options ()
  "Return available and missing options provided by the device.

This function checks the SANE backend of the device selected by
‘scanner-device-name’ against the required options.  If
‘scanner-device-name’ is nil, it attempts auto-detection.  The
return value is a list comprising a list of the available options
and a list of the missing options.  As a side effect, these
results are cached."
  (let ((-compare-fn #'string=)
	opts)
    (with-temp-buffer
      (apply #'call-process scanner-scanimage-program nil t nil "-A"
	     (and scanner-device-name (list "-d" scanner-device-name)))
      (goto-char (point-min))
      (while (re-search-forward scanner--device-option-re nil t)
	(push (match-string 1) opts)))
    (setq scanner--available-options opts)
    (setq scanner--missing-options
	  (-difference scanner--device-specific-options opts))
    (list scanner--available-options scanner--missing-options)))

(defun scanner-detect-devices ()
  "Return a list of auto-detected scanning devices.

Each element of the list contains three elements: the SANE device
name, the device type, and the vendor and model names."
  (let ((scanners (process-lines scanner-scanimage-program "-f" "%d|%t|%v %m")))
    ;; attempt to filter out any spurious error output or other non-relevant
    ;; stuff
    (setq scanner--detected-devices
	  (--filter (eql 3 (length it))
		    (mapcar (lambda (x) (split-string x "|")) scanners)))))

(defun scanner-select-device (&optional detect)
  "Select a scanning device, maybe running auto-detection.
If DETECT is non-nil or a prefix argument is supplied, force
auto-detection.  Without an argument, auto-detect only if
no devices have been detected yet.

The selected device will be used for any future scan until a new
selection is made."
  (interactive "P")
  (let* ((devices (if detect
		      (scanner-detect-devices)
		    (or scanner--detected-devices
			(scanner-detect-devices))))
	 (choices (mapcar (lambda (dev)
			    (concat (caddr dev) " (" (car dev) ")"))
			  devices)))
    (setq scanner-device-name
	  (cadr (split-string
		 (completing-read "Select scanning device: " choices nil t)
		 "(" t ")")))))

(defun scanner--scanimage-args (outfile format type)
  "Construct the argument list for scanimage(1).
OUTFILE is the output filename and FORMAT is the output image
format.  TYPE is either ‘:image’ or ‘:doc’."
  (let ((opts scanner--available-options))
    (-flatten (list (and scanner-device-name
			 (list "-d" scanner-device-name))
		    (if (eq :image type)
			(concat "--format=" format)
		      (concat "--format=" scanner-doc-intermediate-format))
		    "-o" outfile
		    (and (eq :doc type)
			 (-when-let* ((x (car (member "-x" opts)))
				      (y (car (member "-y" opts)))
				      ((&plist scanner-doc-papersize size)
				       scanner-paper-sizes))
			   (list x (number-to-string (car size))
				 y (number-to-string (cadr size)))))
		    (and (member "--mode" opts)
			 (concat "--mode=" scanner-scan-mode))
		    (and (member "--resolution" opts)
			 (concat "--resolution=" (number-to-string
						  scanner-doc-resolution)))
		    scanner-scanimage-options))))

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

(defconst scanner--image-extensions
  '(("jpeg" . "jpeg")
    ("jpg" . "jpeg")
    ("png" . "png")
    ("pnm" . "pnm")
    ("tiff" . "tiff")
    ("tif" . "tiff"))
  "List of known image filename extensions with aliases.")

(defun scanner--determine-format (extension)
  "Determine image file format from EXTENSION."
  (let ((ext (if extension (downcase extension) "")))
    (or (cdr (assoc ext scanner--image-extensions))
	scanner-image-format)))

(defun scanner-scan-document (&optional _filename)
  "Scan a document named FILENAME."
  (interactive)
  ;; loop in y-or-n-p over pages of the document
  ;; scan multiple pages with configurable time delay
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
  (let* ((filename (read-file-name "Image file: "))
	 (fmt (scanner--determine-format filename))
	 (fname (if (file-name-extension filename)
		    filename
		  (concat (file-name-sans-extension filename) "." fmt)))
	 (args (scanner--scanimage-args fname fmt :image)))
    (cl-flet ((sentinel (process event)
			(message
			 (format "Scanner: %s" (string-trim event)))))
      (make-process :name "scanimage"
		    :command `(,scanner-scanimage-program ,@args)
		    :sentinel #'sentinel))))

(provide 'scanner)

;; Local variables:
;; eval: (flycheck-mode)
;; End:

;;; scanner.el ends here
