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

;; Scan documents and images using scanimage(1) from the SANE distribution and
;; tesseract(1) for OCR and PDF export.
;;
;; The scanner package uses two sets of customizations for image mode and
;; document mode, with the former usually configured to use high resolution
;; and an image file format, like JPEG, and the latter to use lower resolution
;; and a document format, like PDF or text.  The available file formats are
;; provided by scanimage(1) for image mode and tesseract(1) for document mode.
;; The scanner package uses tesseract(1) to provide optical character
;; recognition (OCR).  You can select the language plugins with
;; ‘scanner-tesseract-languages’.
;;
;; The ‘scanner-scan-image’ command performs one scan in image mode.  This
;; function tries to guess the file format from the chosen file name or falls
;; back to the configured default, see ‘scanner-image-format’.
;;
;; In document mode, you can scan one or multiple pages that are then written
;; in a customizable output format, e.g. (searchable) PDF or text, or whatever
;; tesseract provides.  You can also customize resolution, intermediate image
;; format, and paper size.  The command ‘scanner-scan-document’ starts a
;; document scan.  Without a prefix argument, it scans one page.  With a
;; non-numeric argument, it asks the user after each scanned page for
;; confirmation to scan another page.  With a numeric argument, it scans that
;; many pages.  In the latter case, it observes a delay between scans that is
;; customizable using ‘scanner-scan-delay’.
;;
;; For both images and documents, you can customize the scan mode
;; (e.g. "Color" or "Gray") if your scanning device supports it.
;;
;; Finally, you can pass additional options to the backends using the
;; customization variables ‘scanner-scanimage-switches’ and
;; ‘scanner-tesseract-switches’.  The former variable is helpful for tuning
;; brightness and contrast, for instance.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'subr-x)

(defgroup scanner nil
  "Scan documents and images."
  :group 'multimedia)

(defcustom scanner-resolution
  '(:image 600 :doc 300)
  "Resolutions for images and documents."
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
  "Document paper size.
The value must be one of the keys in the paper sizes list."
  :type '(restricted-sexp :match-alternatives
			  ((lambda (k) (plist-member scanner-paper-sizes k)))))

(defcustom scanner-image-format
  '(:image "jpeg" :doc "pnm")
  "Image file formats for images and documents (intermediate representation)."
  :type '(plist :value-type string))

(defcustom scanner-scanimage-program
  (executable-find "scanimage")
  "Path and file name of the scanimage(1) program."
  :type '(string))

(defcustom scanner-tesseract-program
  (executable-find "tesseract")
  "Path and file name of the tesseract(1) program."
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
  '("eng")
  "List of languages passed to tesseract(1) for OCR."
  :type '(repeat :validate scanner--validate-languages string))

(defcustom scanner-tesseract-outputs
  '("pdf" "txt")
  "List of output formats to produce.
The output formats correspond to the names of config files that
come with tesseract(1).
The config files may reside in ‘/usr/share/tessdata/configs’."
  :type '(repeat string))

(defcustom scanner-tesseract-switches
  '()
  "Additional options to pass to tesseract(1)."
  :type '(repeat string))

(defcustom scanner-scan-mode
  '(:image "Color" :doc "Color")
  "Scan modes for images and documents.
This is usually \"Color\" or \"Gray\", but depends on your
scanning device."
  :type '(plist :value-type string))

(defcustom scanner-scanimage-switches
  '("--brightness" "50" "--contrast" "50")
  "Additional options to be passed to scanimage(1)."
  :type '(repeat string))

(defcustom scanner-device-name
  nil
  "SANE scanning device name or nil.
If nil, attempt auto-detection."
  :type '(restricted-sexp :match-alternatives
			  (stringp 'nil)))

(defcustom scanner-scan-delay
  3
  "Delay between document scans in multi-page mode."
  :type '(number))


;;;###autoload
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
    (define-key map [select-dev]
      '(menu-item "Select scanning device" scanner-select-device
		  :key-sequence nil
		  :help "Select a scanning device."))
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

;;;###autoload
(define-key-after menu-bar-tools-menu [scanner]
  (list 'menu-item "Scanner" scanner-menu))


(defvar scanner--detected-devices
  nil
  "List of devices detected by SANE.
Each element of the list has the form (DEVICE TYPE MODEL) where
DEVICE is the SANE device name, TYPE the type of the device
\(e.g.  \"flatbed scanner\",) and MODEL is the device's model
name.")

(eval-and-compile
  (defconst scanner--device-specific-switches
    '("--mode" "--depth" "--resolution" "-x" "-y")
    "List of required device specific options.

These options are necessary for the full set of features offered
by the scanner package.  If one of these is missing, something may
not work as expected."))

(defconst scanner--device-option-re
  (eval-when-compile (regexp-opt scanner--device-specific-switches t)))

(defvar scanner--available-switches
  nil
  "List of required options implemented by the device backend.")

(defvar scanner--missing-switches
  nil
  "List of required options missing from the device backend.")

(defun scanner--check-device-switches ()
  "Return available and missing options provided by the device.

This function checks the SANE backend of the device selected by
‘scanner-device-name’ against the required options.  If
‘scanner-device-name’ is nil, it attempts auto-detection.  The
return value is a list comprising a list of the available options
and a list of the missing options.  As a side effect, these
results are cached in ‘scanner--available-switches’ and
‘scanner--missing-switches’."
  (let ((-compare-fn #'string=)
	opts)
    (with-temp-buffer
      (apply #'call-process scanner-scanimage-program nil t nil "-A"
	     (and scanner-device-name (list "-d" scanner-device-name)))
      (goto-char (point-min))
      (while (re-search-forward scanner--device-option-re nil t)
	(push (match-string 1) opts)))
    (setq scanner--available-switches opts)
    (setq scanner--missing-switches
	  (-difference scanner--device-specific-switches opts))
    (list scanner--available-switches scanner--missing-switches)))

(defun scanner--detect-devices ()
  "Return a list of auto-detected scanning devices.

Each entry of the list contains three elements: the SANE device
name, the device type, and the vendor and model names."
  (let ((scanners (process-lines scanner-scanimage-program "-f" "%d|%t|%v %m")))
    ;; attempt to filter out any spurious error output or other non-relevant
    ;; stuff
    (setq scanner--detected-devices
	  (--filter (eql 3 (length it))
		    (mapcar (lambda (x) (split-string x "|")) scanners)))))

(defun scanner--scanimage-args (outfile type &optional img-fmt)
  "Construct the argument list for scanimage(1).
OUTFILE is the output filename and IMG-FMT is the output image
format.  TYPE is either ‘:image’ or ‘:doc’.

When scanning documents (type :doc), scanner uses the IMG-FMT
argument for the intermediate representation before conversion to
the document format.  If any of the required options from
‘scanner--device-specific-switches’ are unavailable, they are
simply dropped."
  (let ((opts scanner--available-switches))
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
		    scanner-scanimage-switches))))

(defun scanner--tesseract-args (input output-base)
  "Construct the argument list for ‘tesseract(1)’.
INPUT is the input file name, OUTPUT-BASE is the basename for the
output files.  Note that tesseract automatically adds file name
extensions depending on the selected output options, see
‘scanner-tesseract-outputs’."
  (-flatten (list input output-base
		  "-l" (mapconcat #'identity scanner-tesseract-languages "+")
		  "--dpi" (number-to-string (plist-get scanner-resolution :doc))
		  scanner-tesseract-switches
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
      (let ((num-devices (length (scanner--detect-devices))))
	(cond ((eql 0 num-devices)
	       (user-error "No scanning device was found"))
	      ((eql 1 num-devices)
	       (setq scanner-device-name (caar scanner--detected-devices)))
	      (t (call-interactively #'scanner-select-device)))))
    (scanner--check-device-switches)))


(defun scanner-select-papersize (size)
  "Select the papersize SIZE for document scanning."
  (interactive
   (let ((choices (delq nil (mapcar (lambda (x) (and (keywordp x)
					   (substring (symbol-name x) 1)))
				    scanner-paper-sizes))))
     (list (intern (concat ":"
			   (completing-read "Papersize: " choices nil t))))))
  (unless (plist-member scanner-paper-sizes size)
    (signal 'args-out-of-range `(,size)))
  (setq scanner-doc-papersize size))

(defun scanner-select-languages (languages)
  "Select LANGUAGES for optical character recognition."
  (interactive
   (let ((langs (cdr (process-lines scanner-tesseract-program
				    "--list-langs"))))
     (list (completing-read-multiple "Languages: " langs nil t))))
  (unless (consp languages)
    (signal 'wrong-type-argument `(consp ,languages)))
  (setq scanner-tesseract-languages languages))

(defun scanner-set-image-resolution (resolution)
  "Set the RESOLUTION for scanning images."
  (interactive "NImage scan resolution: ")
  (unless (numberp resolution)
    (signal 'wrong-type-argument `(numberp ,resolution)))
  (plist-put scanner-resolution :image resolution))

(defun scanner-set-document-resolution (resolution)
  "Set the RESOLUTION for scanning documents."
  (interactive "NDocument scan resolution: ")
  (unless (numberp resolution)
    (signal 'wrong-type-argument `(numberp ,resolution)))
  (plist-put scanner-resolution :doc resolution))

(defun scanner-select-device (device)
  "Select a scanning DEVICE.
If a prefix argument is supplied, force auto-detection.
Otherwise, auto-detect only if no devices have been detected
previously.

The selected device will be used for any future scan until a new
selection is made."
  (interactive
   (let* ((devices (if current-prefix-arg
		       (scanner--detect-devices)
		     (or scanner--detected-devices
			 (scanner--detect-devices))))
	  (choices (mapcar (lambda (dev)
			     (concat (caddr dev) " (" (car dev) ")"))
			   devices)))
     (list (cadr (split-string
		  (completing-read "Select scanning device: " choices nil t)
		  "(" t ")")))))
  (setq scanner-device-name device))

;;;###autoload
(defun scanner-scan-document (npages filename)
  "Scan NPAGES pages and write the result to FILENAME.
Without a prefix argument, scan one page.  With a non-numeric
prefix argument, i.e. ‘\\[universal-argument]
\\[scanner-scan-document]’, scan a page and ask the user for
confirmation to scan another page, etc.  With a numeric prefix
argument, e.g. ‘\\[universal-argument] 3
\\[scanner-scan-document]’, scan that many pages (in this case,
3).

If ‘scanner-device-name’ is nil or this device is unavailable,
attempt auto-detection.  If more than one scanning device is
available, ask for a selection interactively."
  (interactive "P\nFDocument file name: ")
  (scanner--ensure-init)
  (let ((doc-file (file-name-sans-extension filename))
	(num-pages (prefix-numeric-value npages))
	(fmt (plist-get scanner-image-format :doc))
	(file-list '())
	(fl-file nil))
    (cl-labels ((cleanup
		 ()
		 (and file-list (dolist (file file-list)
				  (delete-file file)))
		 (and fl-file (delete-file fl-file)))
		(finish
		 (process event)
		 (unwind-protect
		     (let ((ev (string-trim event)))
		       (unless (string= "finished" ev)
			 (error "%s: %s" process ev)))
		   (cleanup)))
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
				 :sentinel #'finish)))
		(scan-or-finish
		 (process event)
		 (condition-case err
		     (let ((ev (string-trim event)))
		       (unless (string= "finished" ev)
			 (error "%s: %s" process ev))
		       (cond ((consp npages) (if (y-or-n-p "Scan another page? ")
						 (scanimage)
					       (tesseract)))
			     ((> num-pages 1)
			      (cl-decf num-pages)
			      (run-at-time scanner-scan-delay nil #'scanimage))
			     (t (tesseract))))
		   (error
		    (cleanup)
		    (signal (car err) (cdr err)))))
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
  "Scan an image, and write the result to IMG-FILE.
If ‘scanner-device-name’ is nil or this device is unavailable,
attempt auto-detection.  If more than one scanning device is
available, ask for a selection interactively."
  (interactive "FImage file name: ")
  (scanner--ensure-init)
  (let* ((fmt (scanner--determine-image-format img-file))
	 (fname (if (file-name-extension img-file)
		    img-file
		  (concat (file-name-sans-extension img-file) "." fmt)))
	 (args (scanner--scanimage-args fname :image fmt)))
    (cl-labels ((sentinel (process event)
			  (let ((ev (string-trim event)))
			    (unless (string= "finished" ev)
			      (error "%s: %s" process ev)))))
     (make-process :name "Scanner (scanimage)"
		   :command `(,scanner-scanimage-program ,@args)
		   :sentinel #'sentinel))))

(provide 'scanner)

;; Local variables:
;; eval: (flycheck-mode)
;; End:

;;; scanner.el ends here
