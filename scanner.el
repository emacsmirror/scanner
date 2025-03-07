;;; scanner.el --- Scan documents and images -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2021, 2025  Free Software Foundation, Inc

;; Author: Raffael Stocker <r.stocker@mnet-mail.de>
;; Maintainer: Raffael Stocker <r.stocker@mnet-mail.de>
;; Created: 05. Feb 2020
;; Version: 0.3
;; Package-Requires: ((emacs "25.1") (dash "2.12.0"))
;; Keywords: hardware, multimedia
;; URL: https://codeberg.org/rstocker/scanner.git

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

;; Scan documents and images using scanimage(1) from the SANE distribution and
;; tesseract(1) for OCR and PDF export.  Enhance the scan with unpaper(1), see
;; https://github.com/unpaper/unpaper.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'cus-edit)
(eval-when-compile (require 'subr-x))
(require 'menu-bar)


;;;; customization
(defgroup scanner nil
  "Scan documents and images."
  :group 'multimedia)

(defcustom scanner-resolution
  '(:image 600 :doc 300 :preview 75)
  "Resolutions for images and documents."
  :type '(plist :value-type integer))

(defcustom scanner-brightness
  20
  "Brightness setting for the scan."
  :type '(integer))

(defcustom scanner-contrast
  50
  "Contrast setting for the scan."
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
    (215.9 279.4)
	:a3-landscape
    (420 297)
    :a4-landscape
    (297 210)
    :a5-landscape
    (210 148)
    :a6-landscape
    (148 105)
    :tabloid-landscape
    (431.8 279.4)
    :legal-landscape
    (355.6 215.9)
    :letter-landscape
    (279.4 215.9))
  "List of paper sizes for documents.
The first of each pair of numbers is the paper width in mm,
the second is the height in mm."
  :type '(plist :value-type (group number number)))

(defcustom scanner-doc-papersize
  :a4
  "Document paper size.
The value must be one of the keys in the paper sizes list, or ‘:whatever’ to
use whatever scanimage thinks is right."
  :type '(restricted-sexp :match-alternatives
						  ((lambda (k) (or (plist-member scanner-paper-sizes k)
									  (eq k :whatever))))))

(defcustom scanner-image-size
  '(200 250)
  "Image size as list of (width height) in mm or nil."
  :type '(list number number))

(defcustom scanner-reverse-pages
  nil
  "If non-nil, reverse pages in document mode."
  :type '(boolean))

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

(defcustom scanner-unpaper-program
  (executable-find "unpaper")
  "Path and file name of the unpaper(1) program."
  :type '(string))

(defun scanner--widget-validate-subset (error-msg widget set)
  "Issue ERROR-MSG if the value of WIDGET is a not subset of SET.
ERROR-MSG is passed to ‘format’ with two string arguments: the
widget's values and the elements of SET."
  (let ((values (widget-value widget)))
    (unless (cl-subsetp values set :test #'string=)
      (widget-put widget :error (format error-msg
										(mapconcat #'identity values ", ")
										(mapconcat #'identity set ", ")))
      widget)))

(defcustom scanner-tessdata-dir
  (let ((prefix (getenv "TESSDATA_PREFIX")))
	(if prefix
		(if (string= (file-name-nondirectory (directory-file-name prefix))
					 "tessdata")
			(file-name-as-directory prefix)
		  (concat (file-name-as-directory prefix) "tessdata/"))
	  "/usr/share/tessdata/"))
  "Tesseract data directory prefix."
  :type '(string))

(defcustom scanner-tesseract-configdir
  (if scanner-tessdata-dir
	  (concat (file-name-as-directory scanner-tessdata-dir) "configs/")
	"/usr/share/tessdata/configs/")
  "Config file directory for tesseract."
  :type '(string))

(defun scanner--validate-languages (widget)
  "Validate the language selection in customization WIDGET."
  (cl-assert scanner-tesseract-program)
  (scanner--widget-validate-subset
   "Unknown language(s): %s; available are: %s" widget
   (condition-case err
	   (cdr (process-lines scanner-tesseract-program
						   "--list-langs"
						   "--tessdata-dir"
						   scanner-tessdata-dir))
	 (error (error "No language definitions found %s" (cdr err))))))

(defcustom scanner-tesseract-languages
  '("eng")
  "List of languages passed to tesseract(1) for OCR."
  :type '(repeat :validate scanner--validate-languages string))

(defun scanner--validate-outputs (widget)
  "Validate the output selection in customization WIDGET."
  (scanner--widget-validate-subset
   "Unknown output(s): %s; available are: %s" widget
   (directory-files scanner-tesseract-configdir nil "[^.]")))

(defcustom scanner-tesseract-outputs
  '("pdf" "txt")
  "List of output formats to produce.
The output formats correspond to the names of config files that
come with tesseract(1).  The config files are assumed to reside
in ‘scanner-tesseract-configdir’."
  :type '(repeat :validate scanner--validate-outputs string))

(defcustom scanner-tesseract-switches
  '()
  "Additional options to pass to tesseract(1)."
  :type '(repeat string))

(defcustom scanner-scan-mode
  '(:image "Color" :doc "Color" :preview "Gray")
  "Scan modes for images and documents.
This is usually \"Color\" or \"Gray\", but depends on your
scanning device."
  :type '(plist :value-type string))

(defcustom scanner-scanimage-switches
  '()
  "Additional options to be passed to scanimage(1)."
  :type '(repeat string))

(defcustom scanner-device-name
  nil
  "SANE scanning device name or nil.
If nil, attempt auto-detection.  Note that some devices, like USB
scanners, may receive a different name every time they are
plugged in.  For these, auto-detection will always be done."
  :type '(restricted-sexp :match-alternatives
						  (stringp 'nil)))

(defcustom scanner-scan-delay
  3
  "Delay in seconds between document scans in multi-page mode."
  :type '(number))

(defcustom scanner-use-unpaper
  nil
  "Use unpaper(1) for post-processing of the scans before OCR."
  :type '(boolean))

(defcustom scanner-unpaper-page-layout
  "none"
  "Page layout to assume in post-processing."
  :type '(choice (const "single")
				 (const "double")
				 (const "none")))

(defcustom scanner-unpaper-input-pages
  1
  "Input pages per sheet."
  :type '(choice (const 1)
				 (const 2)))

(defcustom scanner-unpaper-output-pages
  1
  "Output pages per sheet."
  :type '(choice (const 1)
				 (const 2)))

(defcustom scanner-unpaper-pre-rotation
  nil
  "Pre-rotation to apply before post-processing."
  :type '(choice (const :tag "clockwise" 90)
				 (const :tag "counter-clockwise" -90)
				 (const :tag "none" nil)))

(defcustom scanner-unpaper-post-rotation
  nil
  "Post-rotation to apply after post-processing."
  :type '(choice (const :tag "clockwise" 90)
				 (const :tag "counter-clockwise" -90)
				 (const :tag "none" nil)))

(defcustom scanner-unpaper-scan-direction
  "left,bottom"
  "Edges from which to scan for rotation."
  :type '(string))

(defcustom scanner-unpaper-scan-size
  3000
  "Size of virtual line for rotation detection."
  :type '(number))

(defcustom scanner-unpaper-scan-step
  0.01
  "Steps between single rotation-angle detections."
  :type '(number))

(defconst scanner--unpaper-sizes
  '(:a5 :a4 :a3 :letter :legal :a5-landscape :a4-landscape
		:a3-landscape :letter-landscape :legal-landscape)
  "Paper size names understood by unpaper.")

(defcustom scanner-unpaper-pre-size
  :a4
  "Change sheet size before post-processing.
Either choose one of the pre-defined options (see
‘scanner--unpaper-sizes’), or enter width and height values as a
string; e.g. ‘\"21cm,29.7cm\"’."
  :type `(choice (string)
				 (const :tag "none" nil)
				 ,@(mapcar (lambda (x) (list 'const x))
						   scanner--unpaper-sizes)))

(defcustom scanner-unpaper-post-size
  :a4
  "Change sheet size after post-processing.
Either choose one of the pre-defined options (see
‘scanner--unpaper-sizes’), or enter width and height values as a
string; e.g. ‘\"21cm,29.7cm\"’."
  :type `(choice (string)
				 (const :tag "none" nil)
				 ,@(mapcar (lambda (x) (list 'const x))
						   scanner--unpaper-sizes)))

(defcustom scanner-unpaper-border
  '(10 10 10 10)
  "Define a border at the sheet edges to be set to white.
The border is specified as a list of four integers (widths in
pixels) for left, top, right, and bottom edge, respectively.
This border is before any further processing."
  :type '(list integer integer integer integer))

(defcustom scanner-unpaper-mask-size
  nil
  "Define the mask size to be used if ‘scanner-unpaper-page-layout’
  is \"none\".  The mask is necessary for deskewing; any pixel
  outside the mask will be set to white.  If nil, attempt to set
  the mask from ‘scanner-unpaper-pre-size’, otherwise this option
  must be set to a list ‘(x1 y1 x2 y2)’ specifying the corners of
  the mask in pixels, e.g. ‘(0 0 300 300)’ specifies a mask of
  one square inch in the upper left corner, if the resolution is
  300 dpi."
  :type '(choice (const :tag "From paper size" nil)
				 (list integer integer integer integer)))

(defcustom scanner-unpaper-switches
  '()
  "Additional options to be passed to unpaper(1)."
  :type '(repeat string))


;;;; menu and keymap
;;;###autoload
(defvar scanner-enhancement-menu
  (let ((map (make-sparse-keymap "Scan Enhancement")))
	(define-key map [post-size]
	  '(menu-item "Select page size after processing"
				  scanner-select-post-size
				  :help "Stretch to a page size after processing is done."
				  :enable (and (boundp 'scanner-use-unpaper)
							   scanner-use-unpaper)))
	(define-key map [pre-size]
	  '(menu-item "Select page size before processing"
				  scanner-select-pre-size
				  :help "Stretch to a page size before all other processing."
				  :enable (and (boundp 'scanner-use-unpaper)
							   scanner-use-unpaper)))
	(define-key map [post-rotation]
	  '(menu-item "Select page rotation after processing"
				  scanner-select-post-rotation
				  :help "Rotate the page after processing is done."
				  :enable (and (boundp 'scanner-use-unpaper)
							   scanner-use-unpaper)))
	(define-key map [pre-rotation]
	  '(menu-item "Select page rotation before processing"
				  scanner-select-pre-rotation
				  :help "Rotate the page before all other processing."
				  :enable (and (boundp 'scanner-use-unpaper)
							   scanner-use-unpaper)))
	(define-key map [output-pages]
	  '(menu-item "Select number of output pages"
				  scanner-select-output-pages
				  :help "Select the number of output pages (to split pages)."
				  :enable (and (boundp 'scanner-use-unpaper)
							   scanner-use-unpaper)))
	(define-key map [input-pages]
	  '(menu-item "Select number of input pages"
				  scanner-select-input-pages
				  :help "Select the number of input pages (to combine pages)."
				  :enable (and (boundp 'scanner-use-unpaper)
							   scanner-use-unpaper)))
	(define-key map [page-layout]
	  '(menu-item "Select page layout"
				  scanner-select-page-layout
				  :help "Select the page layout (pages per scanned sheet)."
				  :enable (and (boundp 'scanner-use-unpaper)
							   scanner-use-unpaper)))
	(define-key map [use-unpaper]
	  '(menu-item "Use unpaper to enhance document scans"
				  scanner-toggle-use-unpaper
				  :help "Enable scan post-processing using unpaper."
				  :button (:toggle . (and (boundp 'scanner-use-unpaper)
										  scanner-use-unpaper))))
	map))

;;;###autoload
(defvar scanner-menu
  (let ((map (make-sparse-keymap)))
    (define-key map [show-config]
      '(menu-item "Show current configuration" scanner-show-config
				  :help "Show the current configuration."))
	(define-key map [image-size]
      '(menu-item "Select image size" scanner-select-image-size
				  :help "Select a size for image scanning."))
	(define-key map [img-res]
      '(menu-item "Set image resolution" scanner-set-image-resolution
				  :help "Set the resolution for image scanning."))
	(define-key map [seperator4]
      '(menu-item "--"))
	(define-key map [languages]
      '(menu-item "Select OCR languages" scanner-select-languages
				  :help "Select languages for OCR."))
    (define-key map [outputs]
      '(menu-item "Select document outputs" scanner-select-outputs
				  :help "Select document output formats."))
    (define-key map [papersize]
      '(menu-item "Select paper size" scanner-select-papersize
				  :help "Select a paper size for document scanning."))
    (define-key map [doc-res]
      '(menu-item "Set document resolution" scanner-set-document-resolution
				  :help "Set the resolution for document scanning."))
    (define-key map [seperator3]
      '(menu-item "--"))
	(define-key map [contrast]
	  '(menu-item "Set contrast" scanner-set-contrast
				  :help "Set the scanner's contrast."))
    (define-key map [brightness]
	  '(menu-item "Set brightness" scanner-set-brightness
				  :help "Set the scanner's brightness."))
	(define-key map [scan-delay]
	  '(menu-item "Set delay between scans" scanner-set-scan-delay
				  :help "Set the delay between multi-page scans."))
    (define-key map [select-dev]
      '(menu-item "Select scanning device" scanner-select-device
				  :help "Select a scanning device."))
    (define-key map [seperator2]
      '(menu-item "--"))
	(define-key map [scan-enhancement]
	  (list 'menu-item "Scan enhancement" scanner-enhancement-menu))
	(define-key map [seperator1]
      '(menu-item "--"))
	(define-key map [preview]
      '(menu-item "Make a preview scan" scanner-scan-preview))
	(define-key map [image-multi]
      '(menu-item "Scan multiple images" scanner-scan-multi-images))
    (define-key map [image]
      '(menu-item "Scan an image" scanner-scan-image))
    (define-key map [document-multi]
      '(menu-item "Scan a multi-page document" scanner-scan-multi-doc))
    (define-key map [document]
      '(menu-item "Scan a document" scanner-scan-document))
    map)
  "The scanner menu map.")

;;;###autoload
(define-key-after menu-bar-tools-menu [scanner]
  (list 'menu-item "Scan" scanner-menu))

;;;###autoload
(defvar scanner-map
  (let ((map (make-sparse-keymap)))
	(define-key map "d" #'scanner-scan-document)
	(define-key map "i" #'scanner-scan-image)
	(define-key map "D" #'scanner-scan-multi-doc)
	(define-key map "I" #'scanner-scan-multi-images)
	(define-key map "p" #'scanner-scan-preview)
	(define-key map "r" #'scanner-set-document-resolution)
	(define-key map "R" #'scanner-set-image-resolution)
	(define-key map "s" #'scanner-select-papersize)
	(define-key map "S" #'scanner-select-image-size)
	(define-key map "u" #'scanner-toggle-use-unpaper)
	(define-key map "l" #'scanner-select-languages)
	(define-key map "o" #'scanner-select-outputs)
	(define-key map "C" #'scanner-show-config)
	(define-key map "cd" #'scanner-select-device)
	(define-key map "ct" #'scanner-set-scan-delay)
	(define-key map "ci" #'scanner-select-input-pages)
	(define-key map "co" #'scanner-select-output-pages)
	(define-key map "cl" #'scanner-select-page-layout)
	(define-key map "cr" #'scanner-select-pre-rotation)
	(define-key map "cR" #'scanner-select-post-rotation)
	(define-key map "cs" #'scanner-select-pre-size)
	(define-key map "cS" #'scanner-select-post-size)
	(define-key map "cb" #'scanner-set-brightness)
	(define-key map "cc" #'scanner-set-contrast)
	map)
  "The scanner keymap.")


;;;; internal variables and functions
(defvar scanner--detected-devices
  nil
  "List of devices detected by SANE.
Each element of the list has the form (DEVICE TYPE MODEL) where
DEVICE is the SANE device name, TYPE the type of the device
\(e.g.  \"flatbed scanner\",) and MODEL is the device's model
name.")

(eval-and-compile
  (defconst scanner--device-specific-switches
    '("--mode" "--resolution" "-x" "-y" "--brightness" "--contrast")
    "List of required device specific options.

These options are necessary for the full set of features offered
by the scanner package.  If one of these is missing, this is
ignored, but something may not work as expected."))

(defun scanner--detect-devices ()
  "Return a list of auto-detected scanning devices.

Each entry of the list contains three elements: the SANE device
name, the device type, and the vendor and model names."
  (cl-assert scanner-scanimage-program)
  (let ((scanners (process-lines scanner-scanimage-program "-f" "%d|%t|%v %m%n")))
    ;; attempt to filter out any spurious error output or other non-relevant
    ;; stuff
    (setq scanner--detected-devices
		  (--filter (= 3 (length it))
					(mapcar (lambda (x) (split-string x "|")) scanners)))))

(defmacro scanner--when-switch (switch args form)
  "Evaluate FORM if SWITCH is in ‘(plist-get ARGS :device-dependent)’."
  (declare (indent 2))
  `(when (member ,switch (plist-get ,args :device-dependent))
	 ,form))

(defun scanner--size (scan-type selector)
  "Return the size for SCAN-TYPE as given by SELECTOR.
SCAN-TYPE may be either ‘:doc’ or ‘:image’.  Use ‘car’ as
selector for the x-dimension and ‘cadr’ as selector for the
y-dimension.  If no size is configured, return nil."
  (-when-let (size (cl-case scan-type
					 (:doc (plist-get scanner-paper-sizes
									  scanner-doc-papersize))
					 (:image scanner-image-size)
					 (t nil)))
	(funcall selector size)))

(defvar scanner--scanimage-argspec
  (list "-d" 'scanner-device-name
		"--format=" (lambda (args)
					  (or (plist-get args :img-fmt)
						  (plist-get scanner-image-format
									 (plist-get args :scan-type))))
		"--mode=" (lambda (args)
					(scanner--when-switch "--mode" args
					  (plist-get scanner-scan-mode
								 (plist-get args :scan-type))))
		"--resolution=" (lambda (args)
						  (scanner--when-switch "--resolution" args
							(plist-get scanner-resolution
									   (plist-get args :scan-type))))
		"-x" (lambda (args)
			   (scanner--when-switch "-x" args
				 (scanner--size (plist-get args :scan-type) #'car)))
		"-y" (lambda (args)
			   (scanner--when-switch "-y" args
				 (scanner--size (plist-get args :scan-type) #'cadr)))
		"--brightness=" (lambda (args)
						  (scanner--when-switch "--brightness" args
							scanner-brightness))
		"--contrast=" (lambda (args)
						(scanner--when-switch "--contrast" args
						  scanner-contrast))
		'user-switches 'scanner-scanimage-switches)
  "The arguments list specification for scanimage.")

(defvar scanner--scanimage-preview-argspec
  (list "-d" 'scanner-device-name
		"--format=" "pnm"
		"--mode" (lambda (args)
				   (scanner--when-switch "--mode" args
					 "Gray"))
		"--resolution=" (lambda (args)
						  (scanner--when-switch "--resolution" args
							(plist-get scanner-resolution :preview)))
		"-x" (lambda (args)
			   (scanner--when-switch "-x" args
				 (scanner--size (plist-get args :scan-type) #'car)))
		"-y" (lambda (args)
			   (scanner--when-switch "-y" args
				 (scanner--size (plist-get args :scan-type) #'cadr)))
		"--brightness=" (lambda (args)
						  (scanner--when-switch "--brightness" args
							scanner-brightness))
		"--contrast=" (lambda (args)
						(scanner--when-switch "--contrast" args
						  scanner-contrast))
		'user-switches 'scanner-scanimage-switches)
  "The arguments list used for preview scans.")

(defun scanner--size-in-cm (size)
  (cond ((and (keywordp size)
			  (plist-member scanner-paper-sizes size))
		 (mapcar (lambda (num) (/ num 10.0))
				 (plist-get scanner-paper-sizes size)))
		((stringp size) (mapcar (lambda (size-str)
								  (let ((idx (string-match
											  "\\([[:digit:]]+\\(\\.[[:digit:]]+\\)?\\)cm"
											  size-str)))
									(if idx
										(string-to-number (match-string 1 size-str))
									  (user-error "Unknown size format: %s" size-str))))
								(split-string size ",")))
		(t (error "Unknown paper size: %s" size))))

(defun scanner--cm-to-pixels (cm resolution)
  (floor (* (/ cm 2.54) resolution)))

(defun scanner--corner-pixels (size resolution)
  (let ((coords (list 0
					  0
					  (scanner--cm-to-pixels (car size) resolution)
					  (scanner--cm-to-pixels (cadr size) resolution))))
	(unless (cl-notany #'cl-minusp coords)
	  (user-error "Size must be non-negative: %s" size))
	coords))

(defun scanner--program-args (argspec &rest args)
  "Return an arguments list as specified in ARGSPEC, assuming ARGS.

ARGSPEC is expected to be a list of the form:
   (\"--switch1\" \\='argument1
    \"--switch2=\" (lambda (args) \"bar\"))
    \"--never-used\" nil
    \"--switch-without-argument\" t
    \\='symbol \"--always-there\"
    \\='other-symbol (\"baz\" \"quux\"))

Assuming ‘argument1’ is ‘\"foo\"’, this specification will be
translated into the arguments list:
   (\"--switch1\" \"foo\" \"--switch2=bar\" \"--always-there\" \"baz\"
   \"quux\")"
  (cl-labels ((make-option (sw val)
						   (when val
							 (let ((sval (if (numberp val)
											 (number-to-string val)
										   val)))
							   (if (stringp sw)
								   (if (stringp sval)
									   (if (string-match ".=\\'" sw)
										   (list (concat sw sval))
										 (list sw sval))
									 (list sw))
								 (list sval)))))
			  (process-option (switch value)
							  (cond ((functionp value)
									 (make-option switch (funcall value args)))
									((and (symbolp value) (boundp value))
									 (make-option switch (symbol-value value)))
									(value (make-option switch value))
									(t nil)))
			  (process-argspec (spec)
							   (when spec
								 (nconc (process-option (car spec) (cadr spec))
										(process-argspec (cddr spec))))))
	(-flatten (process-argspec argspec))))

(defun scanner--program-version (program version-switch)
  "Determine the version of PROGRAM using VERSION-SWITCH."
  (condition-case err
	  (let ((version-re "[.[:digit:]]+$")
			(version-output (car (process-lines program version-switch))))
		(when (string-match version-re version-output)
		  (match-string 0 version-output)))
	(error
	 (error "Could not determine program version: %s" (cadr err)))))

(defconst scanner--scanimage-version-o-switch "1.0.28"
  "Minimum scanimage(1) version to have the --output-file switch.")

;; Old (< 1.0.28) versions of scanimage don't have an --output-file switch.
;; For these versions, we have to use the shell to redirect the output.  As
;; this is not very elegant, this is supposed to be removed in the future,
;; once everyone has caught up.
(defun scanner--make-scanimage-command (args outfile)
  "Make the scanimage command using ARGS and OUTFILE.
The arguments list ARGS should be supplied by
‘scanner--program-args’ and the output file name is given by
OUTFILE.  This function checks the installed version of
scanimage(1) and returns a command directly callable by
‘make-process’.  For old versions of scanimage this will
construct a shell command."
  (if (version< (scanner--program-version scanner-scanimage-program "-V")
				scanner--scanimage-version-o-switch)
	  (list shell-file-name
			shell-command-switch
			(concat scanner-scanimage-program
					" "
					(mapconcat 'identity args " ")
					" > "
					outfile))
	`(,scanner-scanimage-program "-o" ,outfile ,@args)))

(defconst scanner--tesseract-version-dpi-switch "4.0.0"
  "Minimum tesseract(1) version to have the --dpi switch.")

(defvar scanner--tesseract-argspec
  (list 'input-files (lambda (args) (plist-get args :input))
		'output-filename (lambda (args) (plist-get args :output))
		"-l" (lambda (_)
			   (mapconcat #'identity scanner-tesseract-languages "+"))
		"--dpi" (lambda (_)
				  (unless (version< (scanner--program-version
									 scanner-tesseract-program
									 "--version")
									scanner--tesseract-version-dpi-switch)
					(plist-get scanner-resolution :doc)))
		"--tessdata-dir" 'scanner-tessdata-dir
		'user-switches 'scanner-tesseract-switches
		'outputs 'scanner-tesseract-outputs)
  "The arguments list specification for tesseract.")

(defun scanner--keyword-string (arg)
  (if (keywordp arg)
	  (substring (symbol-name arg) 1)
	arg))

(defvar scanner--unpaper-argspec
  (list "--layout" 'scanner-unpaper-page-layout
		"--dpi" (lambda (_) (plist-get scanner-resolution :doc))
		"--input-pages" 'scanner-unpaper-input-pages
		"--output-pages" 'scanner-unpaper-output-pages
		"--pre-rotate" 'scanner-unpaper-pre-rotation
		"--post-rotate" 'scanner-unpaper-post-rotation
		"--size" (lambda (_)
				   (scanner--keyword-string scanner-unpaper-pre-size))
		"--post-size" (lambda (_)
						(scanner--keyword-string scanner-unpaper-post-size))
		"--pre-border" (lambda (_) (mapconcat #'number-to-string
										 scanner-unpaper-border
										 ","))
		"--no-mask-scan" (lambda (_) (string= "none" scanner-unpaper-page-layout))
		"--mask" (lambda (_)
				   (when (string= "none" scanner-unpaper-page-layout)
					 (mapconcat #'number-to-string
								(if scanner-unpaper-mask-size
									scanner-unpaper-mask-size
								  (scanner--corner-pixels
								   (scanner--size-in-cm
									scanner-unpaper-pre-size)
								   (plist-get scanner-resolution :doc)))
								",")))
		"--deskew-scan-direction" 'scanner-unpaper-scan-direction
		"--deskew-scan-size" 'scanner-unpaper-scan-size
		"--deskew-scan-step" 'scanner-unpaper-scan-step
		'user-switches 'scanner-unpaper-switches
		'input (lambda (args) (concat (file-name-as-directory
								  (plist-get args :tmp-dir))
								 "input%04d.pnm"))
		'output (lambda (args) (concat (file-name-as-directory
								   (plist-get args :tmp-dir))
								  "output%04d.pnm")))
  "The arguments list specification for unpaper.")

(defvar scanner--unpaper-preview-argspec
  (list "--layout" 'scanner-unpaper-page-layout
		"--dpi" (lambda (_) (plist-get scanner-resolution :preview))
		"--input-pages" 'scanner-unpaper-input-pages
		"--output-pages" 'scanner-unpaper-output-pages
		"--pre-rotate" 'scanner-unpaper-pre-rotation
		"--post-rotate" 'scanner-unpaper-post-rotation
		"--size" 'scanner-unpaper-pre-size
		"--post-size" 'scanner-unpaper-post-size
		"--pre-border" (lambda (_) (mapconcat #'number-to-string
										 scanner-unpaper-border
										 ","))
		'user-switches 'scanner-unpaper-switches
		'input (lambda (args) (concat (file-name-as-directory
								  (plist-get args :tmp-dir))
								 "input%04d.pnm"))
		'output (lambda (args) (concat (file-name-as-directory
								   (plist-get args :tmp-dir))
								  "output%04d.pnm")))
  "The arguments list specification for unpaper in preview scans.")

(defun scanner--ensure-init ()
  "Ensure that scanning device is initialized.
If no scanning device has been configured or the configured
device is not available, attempt auto-detection and maybe ask for
a device selection.

This function checks the SANE backend of the selected device
against the required options.  The return value is a list of the
available options."
  (cl-assert scanner-scanimage-program)
  (let ((-compare-fn #'string=)
		(switches-re (eval-when-compile
					   (regexp-opt scanner--device-specific-switches t)))
		opts)
    (unless (and scanner-device-name
				 (eql 0 (call-process scanner-scanimage-program
									  nil nil nil "-n"
									  "-d" scanner-device-name)))
      (let ((num-devices (length (scanner--detect-devices))))
		(cond ((= 0 num-devices)
			   (user-error "No scanning device was found"))
			  ((= 1 num-devices)
			   (setq scanner-device-name (caar scanner--detected-devices)))
			  (t (call-interactively #'scanner-select-device)))))
    (with-temp-buffer
      (apply #'call-process scanner-scanimage-program nil t nil "-A"
			 (and scanner-device-name (list "-d" scanner-device-name)))
      (goto-char (point-min))
      (while (re-search-forward switches-re nil t)
		(push (match-string 1) opts)))
    (-when-let (missing (-difference scanner--device-specific-switches
									 opts))
      (scanner--log "Some required options are not supported by the device: %S"
					missing))
    (nreverse opts)))

(defun scanner--log (msg &rest args)
  "Write a log message MSG to the process log buffer.
MSG is a format string, with ARGS passed to ‘format’."
  (with-current-buffer (scanner--log-buffer)
    (goto-char (point-max))
    (insert (apply #'format (concat "[" (current-time-string) "]: " msg) args)
			"\n")))

;; FIXME use special mode in the log buffer
(defun scanner--log-buffer ()
  "Return scanner log buffer or create it."
  (get-buffer-create "*Scanner*"))

(defun scanner--confirm-filenames (file &optional formats)
  "Confirm that FILE using the provided list of FORMATS may be overwritten.
If no formats are provided, FILE is used as-is.  Return t either
when the files do not exist or the user allows overwriting all of
them.  Otherwise, return nil."
  (cl-flet ((confirm
			 (filename)
			 (or (not (file-exists-p filename))
				 (y-or-n-p (format "File ‘%s’ exists; overwrite? " filename)))))
	(if (consp formats)
		(and (confirm (concat (file-name-sans-extension file) "." (car formats)))
			 (if (cdr formats)
				 (scanner--confirm-filenames file (cdr formats))
			   t))
	  (confirm file))))



;;;; commands
;;;###autoload
(defun scanner-select-papersize (size)
  "Select the papersize SIZE for document scanning."
  (interactive
   (let ((choices (append (delq nil
								(mapcar #'scanner--keyword-string
										scanner-paper-sizes))
						  '("whatever"))))
     (list (intern (concat ":"
						   (completing-read "Papersize: " choices nil t))))))
  (setq scanner-doc-papersize size))

;;;###autoload
(defun scanner-select-image-size (x y)
  "Select the size for image scanning as X and Y dimensions."
  (interactive "nImage size in x-dimension: \nnImage size in y-dimension: ")
  (setq scanner-image-size (list x y)))

;;;###autoload
(defun scanner-select-languages (languages)
  "Select LANGUAGES for optical character recognition."
  (interactive
   (let ((langs (condition-case err
					(progn
					  (cl-assert scanner-tesseract-program)
					  (cdr (process-lines scanner-tesseract-program
										  "--list-langs"
										  "--tessdata-dir"
										  scanner-tessdata-dir)))
				  (error
				   (error "Could not query language list %s" (cdr err))))))
     (list (completing-read-multiple "Languages: " langs nil t))))
  (setq scanner-tesseract-languages languages))

;;;###autoload
(defun scanner-select-outputs (outputs)
  "Select OUTPUTS for tesseract."
  (interactive
   (let ((configs (condition-case err
					  (directory-files scanner-tesseract-configdir nil "[^.]")
					(error
					 (error "Could not find output configurations %s"
							(cdr err))))))
     (list (completing-read-multiple "Outputs: " configs nil t))))
  (setq scanner-tesseract-outputs outputs))

;;;###autoload
(defun scanner-set-image-resolution (resolution)
  "Set the RESOLUTION for scanning images."
  (interactive "NImage scan resolution: ")
  (setq scanner-resolution
		(plist-put scanner-resolution :image resolution)))

;;;###autoload
(defun scanner-set-document-resolution (resolution)
  "Set the RESOLUTION for scanning documents."
  (interactive "NDocument scan resolution: ")
  (setq scanner-resolution
		(plist-put scanner-resolution :doc resolution)))

;;;###autoload
(defun scanner-set-brightness (brightness)
  "Set the BRIGHTNESS."
  (interactive "NBrightness: ")
  (setq scanner-brightness brightness))

;;;###autoload
(defun scanner-set-contrast (contrast)
  "Set the CONTRAST."
  (interactive "NContrast: ")
  (setq scanner-contrast contrast))

;;;###autoload
(defun scanner-set-scan-delay (delay)
  "Set the scan DELAY."
  (interactive "NDelay between page scans: ")
  (setq scanner-scan-delay (max delay 0)))

;;;###autoload
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
(defun scanner-toggle-use-unpaper ()
  "Toggle use of unpaper."
  (interactive)
  (setq scanner-use-unpaper (not scanner-use-unpaper))
  (message "Processing with unpaper is %sabled"
		   (if scanner-use-unpaper "en" "dis")))

;;;###autoload
(defun scanner-select-page-layout (layout)
  "Select the page LAYOUT."
  (interactive (list (completing-read "Select page layout: "
									  '("single" "double" "none")
									  nil t)))
  (setq scanner-unpaper-page-layout layout))

;;;###autoload
(defun scanner-select-input-pages (pages)
  "Select the number of input PAGES."
  (interactive "NSelect the number of input pages: ")
  (setq scanner-unpaper-input-pages (min (max pages 1) 2)))

;;;###autoload
(defun scanner-select-output-pages (pages)
  "Select the number of output PAGES."
  (interactive "NSelect the number of output pages: ")
  (setq scanner-unpaper-output-pages (min (max pages 1) 2)))

(defun scanner--select-rotation (prompt)
  "Select rotation displaying PROMPT."
  (let ((choice (completing-read prompt
								 '("clockwise" "counter-clockwise"
								   "none")
								 nil t)))
	(list (cond ((string= choice "clockwise") 90)
				((string= choice "counter-clockwise") -90)
				(t nil)))))

;;;###autoload
(defun scanner-select-pre-rotation (rotation)
  "Select the pre-rotation ROTATION (cw, ccw, none)."
  (interactive (scanner--select-rotation "Select pre-rotation: "))
  (setq scanner-unpaper-pre-rotation rotation))

;;;###autoload
(defun scanner-select-post-rotation (rotation)
  "Select the post-rotation ROTATION (cw, ccw, none)."
  (interactive (scanner--select-rotation "Select post-rotation: "))
  (setq scanner-unpaper-post-rotation rotation))

(defun scanner--process-unpaper-size (size)
  (cond ((string= "none" size) nil)
		((string= ":" (substring size 0 1)) (intern size))
		(t (if (or (not (string-match-p "[[:alnum:]]+,[[:alnum:]]+" size))
				   (cl-some #'string-empty-p (split-string size ",")))
			   (user-error "Size must have two values, separated by comma: %s" size)
			 size))))

;;;###autoload
(defun scanner-select-pre-size (size)
  "Select the page SIZE before processing."
  (interactive (list (completing-read "Select a pre-processing page size: "
									  (cons "none"
											scanner--unpaper-sizes)
									  nil 'confirm)))
  (setq scanner-unpaper-pre-size (scanner--process-unpaper-size size)))

;;;###autoload
(defun scanner-select-post-size (size)
  "Select the page SIZE after processing."
  (interactive (list (completing-read "Select a post-processing page size: "
									  (cons "none"
											scanner--unpaper-sizes)
									  nil 'confirm)))
  (setq scanner-unpaper-post-size (scanner--process-unpaper-size size)))

;;;###autoload
(defun scanner-show-config ()
  "Show the current configuration."
  (interactive)
  (with-current-buffer-window "*scanner-config*" nil nil
	(let ((variables (mapcar (lambda (variable)
							   (cons (car variable)
									 (replace-regexp-in-string "-" " "
															   (symbol-name (car variable))
															   nil nil nil
															   (length "scanner-"))))
							 (custom-group-members 'scanner nil))))
	  (widget-create 'push-button
					 :notify (lambda (&rest _)
							   (customize-group 'scanner))
					 "Customize scanner")
	  (insert "\n\nCurrent configuration:\n\n")
	  (mapc (lambda (variable)
			  (insert (format "%25s: %s\n"
							  (cdr variable)
							  (symbol-value (car variable)))))
			variables)
	  (use-local-map widget-keymap)
	  (widget-setup))))

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
available, ask for a selection interactively.

If ‘scanner-use-unpaper’ is non-nil, silenty force the image
format to \"pnm\" and post-process the scans using unpaper before
performing OCR."
  (interactive "P\nFDocument file name: ")
  (cl-assert scanner-scanimage-program)
  (cl-assert scanner-tesseract-program)
  (let ((doc-file (file-name-sans-extension filename))
		(num-pages (prefix-numeric-value npages))
		(fmt (if scanner-use-unpaper
				 "pnm"
			   (plist-get scanner-image-format :doc)))
		(switches (scanner--ensure-init))
		(file-list '())
		(fl-file nil)
		(tmp-dir (make-temp-file "scanner" t))
		(page-num 0))
    (cl-labels ((scanimage
				 ()
				 (let* ((img-file (format "%sinput%04d.%s"
										  (file-name-as-directory tmp-dir)
										  (cl-incf page-num)
										  fmt))
						(scanimage-args (scanner--program-args
										 scanner--scanimage-argspec
										 :scan-type :doc
										 :img-fmt fmt
										 :device-dependent switches))
						(scanimage-command (scanner--make-scanimage-command
											scanimage-args img-file)))
				   (push img-file file-list)
				   (scanner--log "scanimage command: %s" scanimage-command)
				   (make-process :name "Scanner (scanimage)"
								 :command scanimage-command
								 :sentinel #'scan-or-process
								 :buffer (scanner--log-buffer)
								 :std-err nil)))
				(scan-or-process
				 (process event)
				 (condition-case err
					 (let ((ev (string-trim event)))
					   (unless (string= "finished" ev)
						 (error "%s: %s" process ev))
					   (cond ((consp npages)
							  (if (y-or-n-p "Scan another page? ")
								  (scanimage)
								(if scanner-use-unpaper
									(unpaper)
								  (tesseract))))
							 ((> num-pages 1)
							  (cl-decf num-pages)
							  (run-at-time scanner-scan-delay nil #'scanimage))
							 (t (if scanner-use-unpaper
									(unpaper)
								  (tesseract)))))
				   (error
					(cleanup)
					(signal (car err) (cdr err)))))
				(unpaper
				 ()
				 (cl-assert scanner-unpaper-program)
				 (let ((unpaper-args (scanner--program-args
									  scanner--unpaper-argspec
									  :tmp-dir tmp-dir)))
				   (scanner--log "unpaper arguments: %s" unpaper-args)
				   (make-process :name "Scanner (unpaper)"
								 :command `(,scanner-unpaper-program
											,@unpaper-args)
								 :sentinel #'unpaper-sentinel
								 :buffer (scanner--log-buffer)
								 :std-err nil)))
				(unpaper-sentinel
				 (process event)
				 (condition-case err
					 (let ((ev (string-trim event)))
					   (unless (string= "finished" ev)
						 (error "%s: %s" process ev))
					   (setq file-list
							 (nreverse
							  (directory-files tmp-dir
											   t
											   "output[[:digit:]]+\\.pnm")))
					   (tesseract))
				   (error
					(cleanup)
					(signal (car err) (cdr err)))))
				(tesseract
				 ()
				 (unless scanner-reverse-pages
				   (setq file-list (nreverse file-list)))
				 (setq fl-file (make-temp-file (concat (file-name-as-directory
														tmp-dir)
													   "scanlist")
											   nil ".txt"
											   (mapconcat #'identity
														  file-list
														  "\n")))
				 (let ((tesseract-args (scanner--program-args
										scanner--tesseract-argspec
										:input fl-file :output doc-file)))
				   (scanner--log "tesseract arguments: %s" tesseract-args)
				   (make-process :name "Scanner (tesseract)"
								 :command `(,scanner-tesseract-program
											,@tesseract-args)
								 :sentinel #'finish
								 :buffer (scanner--log-buffer)
								 :std-err nil)))
				(finish
				 (process event)
				 (unwind-protect
					 (let ((ev (string-trim event)))
					   (unless (string= "finished" ev)
						 (error "%s: %s" process ev)))
				   (cleanup)))
				(cleanup
				 ()
				 (and tmp-dir (delete-directory tmp-dir t))))
	  (when (scanner--confirm-filenames doc-file scanner-tesseract-outputs)
		(scanner--log "Scanning document to file(s) \"%s.*\"" doc-file)
		(scanimage)))))

;;;###autoload
(defun scanner-scan-multi-doc (filename)
  "Scan a multi-page document, writing them to FILENAME."
  (interactive "FDocument file name: ")
  (scanner-scan-document (list 4) filename))

;;;###autoload
(defun scanner-scan-image (nscans filename)
  "Scan NSCANS images, and write the result to FILENAME.
Without a prefix argument, scan one image.  With a non-numeric
prefix argument, i.e. ‘\\[universal-argument]
\\[scanner-scan-document]’, scan an image and ask the user for
confirmation to scan another image, etc.  With a numeric prefix
argument, e.g. ‘\\[universal-argument] 3
\\[scanner-scan-document]’, scan that many images (in this case,
3).  A numerical suffix is added to FILENAME for each scanned
image.

If ‘scanner-device-name’ is nil or this device is unavailable,
attempt auto-detection.  If more than one scanning device is
available, ask for a selection interactively."
  (interactive "P\nFImage file name: ")
  (cl-assert scanner-scanimage-program)
  (let ((derived-fmt (cdr (assoc (downcase (file-name-extension filename t))
								 '((".jpeg" . "jpeg")
								   (".jpg" . "jpeg")
								   (".png" . "png")
								   (".pnm" . "pnm")
								   (".tiff" . "tiff")
								   (".tif" . "tiff")))))
		(num-scans (prefix-numeric-value nscans))
		(switches (scanner--ensure-init))
		(page-count 1))
    (cl-labels ((scanimage
				 (multi-scan)
				 (let* ((img-ext (if derived-fmt
									 (file-name-extension filename t)
								   (concat "."
										   (plist-get scanner-image-format
													  :image))))
						(img-base (if derived-fmt
									  (file-name-sans-extension filename)
									filename))
						(img-file (if multi-scan
									  (prog1
										  (concat img-base "-"
												  (number-to-string page-count)
												  img-ext)
										(cl-incf page-count))
									(concat img-base img-ext)))
						(scanimage-args (scanner--program-args
										 scanner--scanimage-argspec
										 :img-fmt derived-fmt
										 :scan-type :image
										 :device-dependent switches))
						(scanimage-command (scanner--make-scanimage-command
											scanimage-args img-file)))
				   (when (scanner--confirm-filenames img-file)
					 (scanner--log "Scanning image to file \"%s\"" img-file)
					 (scanner--log (format "scanimage command: %s"
										 scanimage-command))
					 (make-process :name "Scanner (scanimage)"
								   :command scanimage-command
								   :sentinel #'scan-or-finish
								   :buffer (scanner--log-buffer)
								   :std-err nil))))
				(scan-or-finish
				 (process event)
				 (let ((ev (string-trim event)))
				   (unless (string= "finished" ev)
					 (error "%s: %s" process ev))
				   (cond ((consp nscans) (when (y-or-n-p "Scan another page? ")
										   (scanimage t)))
						 ((> num-scans 1)
						  (cl-decf num-scans)
						  (run-at-time scanner-scan-delay nil #'scanimage t))))))
      (scanimage (or (> num-scans 1) (consp nscans))))))

;;;###autoload
(defun scanner-scan-multi-images (filename)
  "Scan multiple images, writing them to FILENAME.
A numerical suffix is added to FILENAME for each scanned image."
  (interactive "FImage file name: ")
  (scanner-scan-image (list 4) filename))


;;;###autoload
(defun scanner-scan-preview ()
  "Make a preview scan.
If ‘scanner-use-unpaper’ is non-nil, also post-process with
unpaper.  On graphical displays, this command opens the preview
scan in an image buffer.  Otherwise it opens a dired buffer of
the directory containing the image files."
  (interactive)
  (cl-assert scanner-scanimage-program)
  (let* ((switches (scanner--ensure-init))
		 (tmp-dir (make-temp-file "scanner" t))
		 (img-file (concat (file-name-as-directory tmp-dir)
						   "input0001.pnm")))
    (cl-labels ((scanimage
				 ()
				 (let* ((scanimage-args (scanner--program-args
										 scanner--scanimage-preview-argspec
										 :scan-type :doc
										 :device-dependent switches))
						(scanimage-command (scanner--make-scanimage-command
											scanimage-args img-file)))
				   (scanner--log "Scanning preview to file \"%s\"" img-file)
				   (scanner--log (format "scanimage command: %s"
										 scanimage-command))
				   (make-process :name "Scanner (scanimage)"
								 :command scanimage-command
								 :sentinel #'process-or-finish
								 :stderr (scanner--log-buffer))))
				(process-or-finish
				 (process event)
				 (condition-case err
					 (let ((ev (string-trim event)))
					   (unless (string= "finished" ev)
						 (error "%s: %s" process ev))
					   (if scanner-use-unpaper
						   (unpaper)
						 (finish)))
				   (error
					(cleanup)
					(signal (car err) (cdr err)))))
				(unpaper
				 ()
				 (cl-assert scanner-unpaper-program)
				 (let ((unpaper-args (scanner--program-args
									  scanner--unpaper-preview-argspec
									  :tmp-dir tmp-dir)))
				   (scanner--log "unpaper arguments: %s" unpaper-args)
				   (make-process :name "Scanner (unpaper)"
								 :command `(,scanner-unpaper-program
											,@unpaper-args)
								 :sentinel #'unpaper-sentinel
								 :std-err (scanner--log-buffer))))
				(unpaper-sentinel
				 (process event)
				 (condition-case err
					 (let ((ev (string-trim event)))
					   (unless (string= "finished" ev)
						 (error "%s: %s" process ev))
					   (finish))
				   (error
					(cleanup)
					(signal (car err) (cdr err)))))
				(finish
				 ()
				 (let ((output-file (if scanner-use-unpaper
										(concat (file-name-as-directory tmp-dir)
												"output0001.pnm")
									  img-file)))
				   (if (and (display-images-p)
							(image-type-available-p 'pbm))
					   (with-current-buffer-window "*scan preview*" nil nil
						 (insert-image-file output-file nil)
						 (image-mode)
						 (cleanup))
					 (dired tmp-dir))))
				(cleanup
				 ()
				 (and tmp-dir (delete-directory tmp-dir t))))
      (scanimage))))

(provide 'scanner)

;;; scanner.el ends here

;; Local Variables:
;; tab-width: 4
;; indent-tabs-mode: t
;; End:
