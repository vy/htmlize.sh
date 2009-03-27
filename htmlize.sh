#!/usr/bin/emacs --script
;;; -*- Mode: EMACS-LISP; -*-

;;; Copyright (c) 2007, Volkan YAZICI <volkan.yazici@gmail.com>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:

;;; - Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.

;;; - Redistributions in binary form must reproduce the above copyright notice,
;;;   this list of conditions and the following disclaimer in the documentation
;;;   and/or other materials provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initialization Routines
;;;

(require 'cl)

;;; Find where are we residing actually.
(defvar *library-directory* (file-name-directory load-file-name))

;;; Below is the associative list of all available URI lookup hooks for specific
;;; modes in `(MODE HOOKS ...)' format.
(defvar *avail-uri-lookup-hooks-table* (make-hash-table))

;;; URI lookup hooks for current mode.
(defvar *uri-lookup-hooks*)

;;; Shortcut for loading library files.
(defun load-library-file (file-name)
  (load (expand-file-name file-name *library-directory*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command Line Argument Parsing Routines
;;;

(defvar *input-file*)
(defvar *output-file*)
(defvar *mode*)

(or (ignore-errors
      (prog1 t
	(destructuring-bind (input-file output-file &optional mode)
	    command-line-args-left
	  (setq *input-file* input-file
		*output-file* output-file
		*mode* mode))))
    (error
     (concat
      "Invalid arguments: %s\n"
      "Usage: htmlize.sh <INPUT-FILE> <OUTPUT-FILE> [<MODE>]")
     command-line-args-left))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Htmlize Stuff
;;;

(load-library-file "htmlize.el")

;;; We overwrite `htmlize-buffer-1' of htmlize to get rid of <HTML>, <HEAD>,
;;; <BODY>, <PRE>, etc. stuff.
(defun htmlize-buffer-1 ()
  ;; Internal function; don't call it from outside this file.  Htmlize
  ;; current buffer, writing the resulting HTML to a new buffer, and
  ;; return it.  Unlike htmlize-buffer, this doesn't change current
  ;; buffer or use switch-to-buffer.
  (save-excursion
    ;; Protect against the hook changing the current buffer.
    (save-excursion
      (run-hooks 'htmlize-before-hook))
    ;; Convince font-lock support modes to fontify the entire buffer
    ;; in advance.
    (htmlize-ensure-fontified)
    (clrhash htmlize-extended-character-cache)
    (clrhash htmlize-memoization-table)
    (let* ((buffer-faces (htmlize-faces-in-buffer))
	   (face-map (htmlize-make-face-map (adjoin 'default buffer-faces)))
	   ;; Generate the new buffer.  It's important that it inherits
	   ;; default-directory from the current buffer.
	   (htmlbuf (generate-new-buffer (if (buffer-file-name)
					     (htmlize-make-file-name
					      (file-name-nondirectory
					       (buffer-file-name)))
					   "*html*")))
	   ;; Having a dummy value in the plist allows writing simply
	   ;; (plist-put places foo bar).
	   (places '(nil nil))
	   (title (if (buffer-file-name)
		      (file-name-nondirectory (buffer-file-name))
		    (buffer-name))))
      ;; Initialize HTMLBUF and insert the HTML prolog.
;      (with-current-buffer htmlbuf
;	(buffer-disable-undo)
;	(insert (htmlize-method doctype) ?\n
;		(format "<!-- Created by htmlize-%s in %s mode. -->\n"
;			htmlize-version htmlize-output-type)
;		"<html>\n  ")
;	(plist-put places 'head-start (point-marker))
;	(insert "<head>\n"
;		"    <title>" (htmlize-protect-string title) "</title>\n"
;		(if htmlize-html-charset
;		    (format (concat "    <meta http-equiv=\"Content-Type\" "
;				    "content=\"text/html; charset=%s\">\n")
;			    htmlize-html-charset)
;		  "")
;		htmlize-head-tags)
;	(htmlize-method insert-head buffer-faces face-map)
;	(insert "  </head>")
;	(plist-put places 'head-end (point-marker))
;	(insert "\n  ")
;	(plist-put places 'body-start (point-marker))
;	(insert (htmlize-method body-tag face-map)
;		"\n    ")
;	(plist-put places 'content-start (point-marker))
;	(insert "<pre>\n"))
      (let ((insert-text-method
	     ;; Get the inserter method, so we can funcall it inside
	     ;; the loop.  Not calling `htmlize-method' in the loop
	     ;; body yields a measurable speed increase.
	     (htmlize-method-function 'insert-text))
	    ;; Declare variables used in loop body outside the loop
	    ;; because it's faster to establish `let' bindings only
	    ;; once.
	    next-change text face-list fstruct-list trailing-ellipsis)
	;; This loop traverses and reads the source buffer, appending
	;; the resulting HTML to HTMLBUF with `princ'.  This method is
	;; fast because: 1) it doesn't require examining the text
	;; properties char by char (htmlize-next-change is used to
	;; move between runs with the same face), and 2) it doesn't
	;; require buffer switches, which are slow in Emacs.
	(goto-char (point-min))
	(while (not (eobp))
	  (setq next-change (htmlize-next-change (point) 'face))
	  ;; Get faces in use between (point) and NEXT-CHANGE, and
	  ;; convert them to fstructs.
	  (setq face-list (htmlize-faces-at-point)
		fstruct-list (delq nil (mapcar (lambda (f)
						 (gethash f face-map))
					       face-list)))
	  ;; Extract buffer text, sans the invisible parts.  Then
	  ;; untabify it and escape the HTML metacharacters.
	  (setq text (htmlize-buffer-substring-no-invisible
		      (point) next-change))
	  (when trailing-ellipsis
	    (setq text (htmlize-trim-ellipsis text)))
	  ;; If TEXT ends up empty, don't change trailing-ellipsis.
	  (when (> (length text) 0)
	    (setq trailing-ellipsis
		  (get-text-property (1- (length text))
				     'htmlize-ellipsis text)))
	  (setq text (htmlize-untabify text (current-column)))
	  (setq text (htmlize-protect-string text))
	  ;; Don't bother writing anything if there's no text (this
	  ;; happens in invisible regions).
	  (when (> (length text) 0)
	    ;; Insert the text, along with the necessary markup to
	    ;; represent faces in FSTRUCT-LIST.
	    (funcall insert-text-method text fstruct-list htmlbuf))
	  (goto-char next-change)))

;      ;; Insert the epilog and post-process the buffer.
;      (with-current-buffer htmlbuf
;	(insert "</pre>")
;	(plist-put places 'content-end (point-marker))
;	(insert "\n  </body>")
;	(plist-put places 'body-end (point-marker))
;	(insert "\n</html>\n")
;	(when htmlize-generate-hyperlinks
;	  (htmlize-make-hyperlinks))
;	(htmlize-defang-local-variables)
;	(when htmlize-replace-form-feeds
;	  ;; Change each "\n^L" to "<hr />".
;	  (goto-char (point-min))
;	  (let ((source
;		 ;; ^L has already been escaped, so search for that.
;		 (htmlize-protect-string "\n\^L"))
;		(replacement
;		 (if (stringp htmlize-replace-form-feeds)
;		     htmlize-replace-form-feeds
;		   "</pre><hr /><pre>")))
;	    (while (search-forward source nil t)
;	      (replace-match replacement t t))))
;	(goto-char (point-min))
;	(when htmlize-html-major-mode
;	  ;; What sucks about this is that the minor modes, most notably
;	  ;; font-lock-mode, won't be initialized.  Oh well.
;	  (funcall htmlize-html-major-mode))
;	(set (make-local-variable 'htmlize-buffer-places) places)
;	(run-hooks 'htmlize-after-hook)
;	(buffer-enable-undo))
      htmlbuf)))

;;; We overwrite `htmlize-css-insert-text' to extract and place proper links for
;;; CLHS and MOP builtins.
(defun htmlize-css-insert-text (text fstruct-list buffer)
  ;; Insert TEXT colored with FACES into BUFFER.  In CSS mode, this is
  ;; easy: just nest the text in one <span class=...> tag for each
  ;; face in FSTRUCT-LIST.
  (let (keyword-uri)
    (dolist (fstruct fstruct-list)
      (princ "<span class=\"" buffer)
      (princ (htmlize-fstruct-css-name fstruct) buffer)
      (let ((hooks *uri-lookup-hooks*)
	    (text (downcase text)))
	(while (and (null keyword-uri) hooks)
	  (setq keyword-uri (funcall (first hooks) text)
		hooks (rest hooks))))
      (princ "\">" buffer))
    (when keyword-uri
      (princ "<a href=\"" buffer)
      (princ keyword-uri buffer)
      (princ "\">" buffer))
    (princ text buffer)
    (when keyword-uri
      (princ "</a>" buffer)))
  (dolist (fstruct fstruct-list)
    (ignore fstruct)			; shut up the byte-compiler
    (princ "</span>" buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLHS & MOP Symbol Recognition Routines
;;;

(load-library-file "symbols.el")
(load-library-file "symbols-lisp-mop.el")
(load-library-file "symbols-lisp-clhs.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Produce The HTML Output
;;;

(condition-case err
    (progn
      (find-file *input-file*)
      ;; Load the mode.
      (if *mode*
	  (funcall (intern *mode*))
	(normal-mode))
      (font-lock-mode 1)
      (unless font-lock-mode
	;; In GNU Emacs (font-lock-mode 1) doesn't force font-lock, contrary to the
	;; documentation.  This seems to work.
	(font-lock-fontify-buffer))
      ;; Load `*uri-lookup-hooks*'.
      (let ((*uri-lookup-hooks* (gethash major-mode *avail-uri-lookup-hooks-table*)))
	;; Process buffer.
	(with-current-buffer (htmlize-buffer)
	  (run-hooks 'htmlize-file-hook)
	  (write-region (point-min) (point-max) *output-file*))))
  (error
   (backtrace)
   (error (error-message-string err))))
