;;; scanner.el --- Scan documents and images -*- lexical-binding: t -*-

;; Copyright (C) 2020 Raffael Stocker

;; Author: Raffael Stocker <r.stocker@mnet-mail.de>
;; Maintainer: Raffael Stocker <r.stocker@mnet-mail.de>
;; Created: 05. Feb 2020
;; Version: 0.0
;; Package-Requires: ((emacs "24"))
;; Keywords: hardware multimedia
;; URL: https://gitlab.com/rstocker/scanner

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

(defgroup scanner nil
  "Scan documents and images."
  :group 'Multimedia)

(defcustom scanner-doc-resolution 300
  "Default resolution for documents."
  :type '(integer))

(defcustom scanner-image-resolution 600
  "Default resolution for images."
  :type '(integer))

;; FIXME: type declaration seems to be wrong
(defcustom scanner-paper-sizes
  '((:a3 (297 420))
    (:a4 (210 297))
    (:a5 (148 210))
    (:a6 (105 148))
    (:tabloid (279.4 431.8))
    (:legal (215.9 355.6))
    (:letter (215.9 279.4)))
  "List of paper sizes for documents (in mm)."
  :type '(plist :key-type symbol :value-type (group number number)))

;; FIXME: how to synchronise this with the above?
(defcustom scanner-doc-papersize
  :a4
  "Default document paper size."
  :type '(symbol))

(defcustom scanner-image-format
  "jpeg"
  "Default image file format."
  :type '(radio (const "jpeg")
		(const "png")
		(const "pnm")
		(const "tiff")))

(defcustom scanner-scanimage-program
  (executable-find "scanimage")
  "Path of the ‘scanimage’ program."
  :type '(sexp))

(defcustom scanner-mode
  "Color"
  "Scan mode."
  :type '(radio (const "Color")
		(const "Gray")
		(const "Lineart")))

(defcustom scanner-scanimage-options
  ""
  "Additional options to be passed to ‘scanimage’."
  :type '(string))

(defcustom scanner-img2pdf-program
  (executable-find "img2pdf")
  "Path of the ‘img2pdf’ program."
  :type '(sexp))

(defcustom scanner-device-name
  nil
  "SANE scanner device name or nil."
  :type '(restricted-sexp :match-alternatives
			  (stringp 'nil)))

(defun scanner-scan-document ()
  "Scan a document.")

(defun scanner-scan-image ()
  "Scan an image.")





(provide 'scanner)

;; Local variables:
;; eval: (flycheck-mode)
;; End:

;;; scanner.el ends here

