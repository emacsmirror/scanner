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

;; Scan documents and images using scanimage(1) from the SANE distribution
;; and tesseract(1) for OCR.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'subr-x)

(defgroup scanner nil
  "Scan documents and images."
  :group 'multimedia)

(defcustom scanner-resolution
  '(:image 600 :doc 300)
  "Default resolutions for images and documents."
  :type '(plist :value-type number))

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
  '(:image "jpeg" :doc "pnm")
  "Image file formats for images and documents (intermediate representation)."
  :type '(plist :value-type string))

(defcustom scanner-scanimage-program
  (executable-find "scanimage")
  "Path of the scanimage(1) program."
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
  '("deu")
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
  '(:image "Color" :doc "Color")
  "Scan modes for images and documents."
  :type '(plist :value-type string))

(defcustom scanner-scanimage-options
  '("--brightness" "50" "--contrast" "50")
  "Additional options to be passed to scanimage(1)."
  :type '(repeat string))

(defcustom scanner-device-name
  nil
  "SANE scanner device name or nil.
If nil, auto-detection will be attempted."
  :type '(restricted-sexp :match-alternatives
			  (stringp 'nil)))

(defcustom scanner-scan-delay
  3
  "Delay between document scans in multi-page mode."
  :type '(number))

(defvar scanner-menu
  (let ((map (make-sparse-keymap)))
    (define-key map [languages]
      '(menu-item "Select languages" scanner-select-languages
		  :key-sequence nil
		  :help "Select languages for OCR."))
    (define-key map [papersize]
      '(menu-item "Select paper size" scanner-select-papersize
		  :key-sequence nil
		  :help "Select a paper size for document scanning."))
    (define-key map [img-res]
      '(menu-item "Set image resolution" scanner-set-image-resolution
		  :key-sequence nil
		  :help "Set the resolution for image scanning."))
    (define-key map [doc-res]
      '(menu-item "Set document resolution" scanner-set-document-resolution
		  :key-sequence nil
		  :help "Set the resolution for document scanning."))
    (define-key map [seperator]
      '(menu-item "--"))
    (define-key map [image]
      '(menu-item "Scan an image" scanner-scan-image
		  :key-sequence nil))
    (define-key map [document]
      '(menu-item "Scan a document" scanner-scan-document
		  :key-sequence nil))
    map)
  "The scanner menu map.")

(define-key-after menu-bar-tools-menu [scanner]
  (list 'menu-item "Scanner" scanner-menu))

(defvar scanner--detected-devices
  nil
  "List of devices detected by SANE.
Each element of the list has the form (DEVICE TYPE MODEL) where
DEVICE is the SANE device name, TYPE the type of the device
\(e.g.  \"flatbed scanner\",) and MODEL is the device's model
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

Each entry of the list contains three elements: the SANE device
name, the device type, and the vendor and model names."
  (let ((scanners (process-lines scanner-scanimage-program "-f" "%d|%t|%v %m")))
    ;; attempt to filter out any spurious error output or other non-relevant
    ;; stuff
    (setq scanner--detected-devices
	  (--filter (eql 3 (length it))
		    (mapcar (lambda (x) (split-string x "|")) scanners)))))

(defun scanner-select-papersize ()
  "Select the papersize for document scanning."
  (interactive)
  (let ((choices (delq nil (mapcar (lambda (x) (and (keywordp x)
					       (substring (symbol-name x) 1)))
				   scanner-paper-sizes))))
    (setq scanner-doc-papersize
	  (intern (concat ":" (completing-read "Papersize: " choices nil t))))))

(defun scanner-select-languages ()
  "Select languages for optical character recognition."
  (interactive)
  (let ((langs (cdr (process-lines scanner-tesseract-program
				   "--list-langs"))))
    (setq scanner-tesseract-languages
	  (completing-read-multiple "Languages: " langs nil t))))

(defun scanner-set-image-resolution (resolution)
  "Set the RESOLUTION for scanning images."
  (interactive "NImage scan resolution: ")
  (plist-put scanner-resolution :image resolution))

(defun scanner-set-document-resolution (resolution)
  "Set the RESOLUTION for scanning documents."
  (interactive "NDocument scan resolution: ")
  (plist-put scanner-resolution :doc resolution))

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

(defun scanner--scanimage-args (outfile type &optional img-fmt)
  "Construct the argument list for scanimage(1).
OUTFILE is the output filename and IMG-FMT is the output image
format.  TYPE is either ‘:image’ or ‘:doc’.

When scanning documents (type :doc), the format argument is used
for the intermediate representation before conversion to the
document format.  If any of the required options from
‘scanner--device-specific-options’ are unavailable, they are
simply dropped."
  (let ((opts scanner--available-options))
    (-flatten (list (and scanner-device-name
			 (list "-d" scanner-device-name))
		    (-when-let (fmt (or img-fmt
					(plist-get scanner-image-format type)))
		      (concat "--format=" fmt))
		    "-o" outfile
		    (and (eq :doc type)
			 (-when-let* ((x (car (member "-x" opts)))
				      (y (car (member "-y" opts)))
				      ((&plist scanner-doc-papersize size)
				       scanner-paper-sizes))
			   (list x (number-to-string (car size))
				 y (number-to-string (cadr size)))))
		    (and (member "--mode" opts)
			 (concat "--mode=" (plist-get scanner-scan-mode type)))
		    (and (member "--resolution" opts)
			 (concat "--resolution=" (number-to-string
						  (plist-get
						   scanner-resolution
						   type))))
		    scanner-scanimage-options))))

(defun scanner--tesseract-args (input output-base)
  "Construct the argument list for ‘tesseract(1)’.
INPUT is the input file name, OUTPUT-BASE is the basename for the
output files.  Extensions are added automatically depending on the
selected output options, see ‘scanner-tesseract-outputs’."
  (-flatten (list input output-base
		  "-l" (mapconcat #'identity scanner-tesseract-languages "+")
		  "--dpi" (number-to-string (plist-get scanner-resolution :doc))
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

(defun scanner--determine-image-format (extension)
  "Determine image file format from EXTENSION."
  (let ((ext (if extension (downcase extension) "")))
    (or (cdr (assoc ext scanner--image-extensions))
	(plist-get scanner-image-format :image))))

(defun scanner--sentinel (process event)
  ""
  (let ((ev (string-trim event)))
    (unless (string= "finished" ev)
      (message "%s: %s" process ev))))

(defun scanner--ensure-init ()
  "Ensure that scanning device is initialized.
If no scanning device has been configured or the configured
device is not available, attempt auto-detection.  Check for
availability of required options."
  (let ((need-autodetect (cond ((null scanner-device-name) t)
			       ((< 0 (call-process scanner-scanimage-program
						   nil nil nil "-n"
						   "-d" scanner-device-name))
				t)
			       (t nil))))
    (when need-autodetect
      (let ((num-devices (length (scanner-detect-devices))))
	(cond ((eql 0 num-devices)
	       (user-error "No scanning device was found"))
	      ((eql 1 num-devices)
	       (setq scanner-device-name (caar scanner--detected-devices)))
	      (t (scanner-select-device)))))
    (scanner--check-device-options)))

;;;###autoload
(defun scanner-scan-document (npages filename)
  "Scan a number of NPAGES and write to a document named FILENAME.
Without a prefix argument, scan one page.  With a non-numeric
prefix argument, i.e. ‘\\[universal-argument]
\\[scanner-scan-document]’, scan a page and ask the user for
confirmation to scan another page, etc.  With a numeric prefix
argument, e.g. ‘\\[universal-argument] 3
\\[scanner-scan-document]’, scan that many pages.

If ‘scanner-device-name’ is nil or this device is unavailable,
attempt auto-detection.  If more than one scanning devices are
available, ask for a selection interactively."
  (interactive "P\nFDocument file name: ")
  (scanner--ensure-init)
  (let ((doc-file (file-name-sans-extension filename))
	(num-pages (prefix-numeric-value npages))
	(fmt (plist-get scanner-image-format :doc))
	(file-list '())
	(fl-file nil))
    (cl-labels ((cleanup
		 (process event)
		 (let ((ev (string-trim event)))
		   (unless (string= "finished" ev)
		     (message "%s: %s" process ev))
		   (dolist (file file-list)
		     (delete-file file))
		   (delete-file fl-file)))
		(tesseract
		 ()
		 (setq file-list (nreverse file-list))
		 (setq fl-file (make-temp-file "scanlist" nil ".txt"
					       (mapconcat #'identity
							  file-list
							  "\n")))
		 (let ((tesseract-args (scanner--tesseract-args fl-file doc-file)))
		   (make-process :name "Scanner (tesseract)"
				 :command `(,scanner-tesseract-program
					    ,@tesseract-args)
				 :sentinel #'cleanup)))
		(scan-or-finish
		 (process event)
		 (let ((ev (string-trim event)))
		   (unless (string= "finished" ev)
		     (error "%s: %s" process ev))
		   (cond ((consp npages) (if (y-or-n-p "Scan another page? ")
					     (scanimage)
					   (tesseract)))
			 ((> num-pages 1)
			  (cl-decf num-pages)
			  (run-at-time scanner-scan-delay nil #'scanimage))
			 (t (tesseract)))))
		(scanimage
		 ()
		 (let* ((img-file (make-temp-file "scanner" nil (concat "." fmt)))
			(scanimage-args (scanner--scanimage-args img-file
								 :doc fmt)))
		   (push img-file file-list)
		   (make-process :name "Scanner (scanimage)"
				 :command `(,scanner-scanimage-program
					    ,@scanimage-args)
				 :sentinel #'scan-or-finish))))
      (scanimage))))

;;;###autoload
(defun scanner-scan-image (img-file)
  "Scan an image, reading the file name IMG-FILE interactively.
If ‘scanner-device-name’ is nil or this device is unavailable,
attempt auto-detection.  If more than one scanning devices are
available, ask for a selection interactively."
  (interactive "FImage file name: ")
  (scanner--ensure-init)
  (let* ((fmt (scanner--determine-image-format img-file))
	 (fname (if (file-name-extension img-file)
		    img-file
		  (concat (file-name-sans-extension img-file) "." fmt)))
	 (args (scanner--scanimage-args fname :image fmt)))
    (make-process :name "Scanner (scanimage)"
		  :command `(,scanner-scanimage-program ,@args)
		  :sentinel #'scanner--sentinel)))

(provide 'scanner)

;; Local variables:
;; eval: (flycheck-mode)
;; End:

;;; scanner.el ends here
