;;; org-forester-section.el --- Show sections using org-mode  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2024 Hanwen Guo <g.hanwen@outlook.com>

;; Author: Hanwen Guo <g.hanwen@outlook.com>
;; URL: https://github.com/g-hanwen/org-forester
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org-mode, note

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Show sections using Org-mode.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'dash)
(require 'eieio)
(require 'subr-x)

(require 'org)

;;; Options
(defgroup org-forester-section nil
  "Expandable sections, in Org-mode."
  :group 'org-forester)

;;; Variables
(defvar-local org-forester-section-inhibit-markers nil)
(defvar-local org-forester-section-insert-in-reverse nil)

;;; Classes
(defclass org-forester-section ()
  ((keymap :initform nil)
   (type :initform nil :initarg :type)
   (value :initform nil :initarg :value)
   (start :initform nil :initarg :start)
   (content :initform nil)
   (end :initform nil)
   (parent :initform nil :initarg :parent)
   (children :initform nil)
   (file :initform nil)))

;;; Mode
(defvar org-forester-section-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (keymap-set map "TAB" #'org-forester-section-cycle)
    (keymap-set map "n" #'org-next-visible-heading)
    (keymap-set map "p" #'org-previous-visible-heading)
    (keymap-set map "M-n" #'org-forward-heading-same-level)
    (keymap-set map "M-p" #'org-backward-heading-same-level)
    (keymap-set map "u" #'outline-up-heading)
    (keymap-set map "RET" #'org-forester-buffer-visit-thing)
    (keymap-set map "g" #'org-forester-buffer-refresh)
    (keymap-set map "q" #'quit-window)
    map)
  "Parent keymap for all keymaps of org-forester sections.")

(define-derived-mode org-forester-section-mode org-mode "Org-forester"
  "Major mode for navigating between org-forester sections."
  :group 'org-forester-section
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky))

;;; Core
(defvar-local org-forester-section-root-section nil
  "The root section in the current buffer.")

(put 'org-forester-section-root-section 'permanent-local t)

(defun org-forester-section-current-section ()
  "Return the section at point."
  (or (org-forester-section-at)
      org-forester-section-root-section))

(defun org-forester-section-at (&optional position)
  "Return the section at POSITION, defaulting to point."
  (get-text-property (or position (point)) 'org-forester-section))

(defun org-forester-section-ident (section)
  "Return an unique identifier for SECTION.
The returen value has the form ((TYPE . VALUE)...)."
  (cons (cons (oref section type)
              (org-forester-section-ident-value section))
        (and-let* ((parent (oref section parent)))
          (org-forester-section-ident parent))))

(cl-defgeneric org-forester-section-ident-value (object)
  "Return OBJECT's value, making it constant and unique if necessary.

This is used to correlate different incarnations of the same
section, see `org-forester-section-ident' and `org-forester-get-section'.

Sections whose values that are not constant and/or unique should
implement a method that return a value that can be used for this
purpose.")

(cl-defmethod org-forester-section-ident-value ((section org-forester-section))
  "Return the value unless it is an object.

Different object incarnations representing the same value then to
not be equal, so call this generic function on the object itself
to determine a constant value."
  (let ((value (oref section value)))
    (if (eieio-object-p value)
        (org-forester-section-ident-value value)
      value)))

(cl-defmethod org-forester-section-ident-value ((object eieio-default-superclass))
  "Simply return the object itself.  That likely isn't
good enough, so you need to implement your own method."
  object)

(defun org-forester-section-get-section (ident &optional root)
  "Return the section identified by IDENT.
IDENT has to be a list as returned by `org-forester-section-ident'.
If optional ROOT is non-nil, then search in that section tree
instead of in the one whose root `org-forester-section-root-section' is."
  (setq ident (reverse ident))
  (let ((section (or root org-forester-section-root-section)))
    (when (eq (car (pop ident))
              (oref section type))
      (while (and ident
                  (pcase-let ((`(,type . ,value) (car ident)))
                    (setq section
                          (cl-find-if
                           (lambda (section)
                             (and (eq (oref section type) type)
                                  (equal (org-forester-section-ident-value section)
                                         value)))
                           (oref section children)))))
        (pop ident))
      section)))

(defun org-forester-section-lineage (section &optional raw)
  "Return the lineage of SECTION.
If optional RAW is non-nil, return a list of section object
beginning with SECTION, otherwise return a list of section
types."
  (cons (if raw section (oref section type))
        (and-let* ((parent (oref section parent)))
          (org-forester-section-lineage parent raw))))

(defvar org-forester-section-insert-section--current nil "For internal use only.")
(defvar org-forester-section-insert-section--parent  nil "For internal use only.")
(defvar org-forester-section-insert-section--oldroot nil "For internal use only.")

;;; Commands
;;;; Visibility
(defun org-forester-section-cycle ()
  (interactive)
  (unless (org-at-heading-p)
    (org-previous-visible-heading 1))
  (org-cycle))

;;; Match
(cl-defun org-forester-section-match
    (condition &optional (section (org-forester-section-current-section)))
  "Return t if SECTION matches CONDITION.

SECTION defaults to the section at point.  If SECTION is not
specified and there also is no section at point, then return
nil.

CONDITION can take the following forms:
  (CONDITION...)  matches if any of the CONDITIONs matches.
  [CLASS...]      matches if the section's class is the same
                  as the first CLASS or a subclass of that;
                  the section's parent class matches the
                  second CLASS; and so on.
  [* CLASS...]    matches sections that match [CLASS...] and
                  also recursively all their child sections.
  CLASS           matches if the section's class is the same
                  as CLASS or a subclass of that; regardless
                  of the classes of the parent sections.

Each CLASS should be a class symbol, identifying a class that
derives from `org-forester-section'.  For backward compatibility CLASS
can also be a \"type symbol\".  A section matches such a symbol
if the value of its `type' slot is `eq'.

Note that it is not necessary to specify the complete section
lineage as printed by `org-forester-describe-section-briefly', unless
of course you want to be that precise."
  (and section (org-forester-section-match-1 condition section)))

(defun org-forester-section-match-1 (condition section)
  "Return t if SECTION matches CONDITION.
See `org-forester-section-match' for details."
  (cl-assert condition)
  (and section
       (if (listp condition)
           (--first (org-forester-section-match-1 it section) condition)
         (org-forester-section-match-2 (if (symbolp condition)
                                           (list condition)
                                         (cl-coerce condition 'list))
                                       section))))

(defun org-forester-section-match-2 (condition section)
  (if (eq (car condition) '*)
      (or (org-forester-section-match-2 (cdr condition) section)
          (and-let* ((parent (oref section parent)))
            (org-forester-section-match-2 condition parent)))
    (and (let ((c (car condition)))
           (if (class-p c)
               (cl-typep section c)
             (eq (oref section type) c)))
         (or (not (setq condition (cdr condition)))
             (and-let* ((parent (oref section parent)))
               (org-forester-section-match-2 condition parent))))))

(defmacro org-forester-section-case (&rest clauses)
  "Choose among CLAUSES on the type of the section at point.

  Each clause looks like (CONDITION BODY...).  The type of the
  section is compared against each CONDITION; the BODY forms of the
  first match are evaluated sequentially and the value of the last
  form is returned.  Inside BODY the symbol `it' is bound to the
  section at point.  If no clause succeeds or if there is no
  section at point, return nil.

  See `org-forester-section-match' for the forms CONDITION can take.
  Additionally a CONDITION of t is allowed in the final clause, and
  matches if no other CONDITION match, even if there is no section
  at point."
  (declare (indent 0)
           (debug (&rest (sexp body))))
  `(let* ((it (org-forester-section-current-section)))
     (cond ,@(mapcar (lambda (clause)
                       `(,(or (eq (car clause) t)
                              `(and it
                                    (org-forester-section-match-1 ',(car clause) it)))
                         ,@(cdr clause)))
                     clauses))))

;;; Create
(defmacro org-forester-section-insert-section (&rest args)
  "Insert a section at point.

\(fn [NAME] (CLASS &optional VALUE HIDE) &rest BODY)"
  (declare (indent defun)
           (debug ([&optional symbolp]
                   (&or [("eval" form) &optional form form]
                        [symbolp &optional form form])
                   body)))
  (let ((tp (cl-gensym "type"))
        (s* (and (symbolp (car args))
                 (pop args)))
        (s  (cl-gensym "section")))
    `(let* ((,tp ,(let ((type (nth 0 (car args))))
                    (if (eq (car-safe type) 'eval)
                        (cadr type)
                      `',type)))
            (,s (funcall (if (class-p ,tp)
                             ,tp
                           'org-forester-section)
                         :type
                         ,tp
                         :value ,(nth 1 (car args))
                         :start (if org-forester-section-inhibit-markers
                                    (point)
                                  (point-marker))
                         :parent org-forester-section-insert-section--parent)))
       (let ((org-forester-section-insert-section--current ,s)
             (org-forester-section-insert-section--parent  ,s)
             (org-forester-section-insert-section--oldroot
              (or org-forester-section-insert-section--oldroot
                  (and (not org-forester-section-insert-section--parent)
                       (prog1 org-forester-section-root-section
                         (setq org-forester-section-root-section ,s))))))
         (catch 'cancel-section
           ,@(if s*
                 `((let ((,s* ,s))
                     ,@(cdr args)))
               (cdr args))
           (unless org-forester-section-inhibit-markers
             (set-marker-insertion-type (oref ,s start) t))
           (let* ((end (oset ,s end
                             (if org-forester-section-inhibit-markers
                                 (point)
                               (point-marker))))
                  (class-map (oref ,s keymap))
                  (org-forester-map (intern (format "org-forester-%s-section-map"
                                                    (oref ,s type))))
                  (map (and class-map (symbol-value class-map))))
             (unless map
               (setq map (and (boundp org-forester-map) (symbol-value org-forester-map)))
               (oset ,s keymap map))
             (save-excursion
               (goto-char (oref ,s start))
               (while (< (point) end)
                 (let ((next (or (next-single-property-change
                                  (point) 'org-forester-section)
                                 end)))
                   (unless (org-forester-section-at)
                     (put-text-property (point) next 'org-forester-section ,s)
                     (when map
                       (put-text-property (point) next 'keymap map)))
                   ;; (org-forester-section-maybe-add-heading-map ,s)
                   (goto-char next)))))
           (cond
            ((eq ,s org-forester-section-root-section)
             (when (eq org-forester-section-inhibit-markers 'delay)
               (setq org-forester-section-inhibit-markers nil)
               (org-forester-section-map-sections
                (lambda (section)
                  (oset section start (copy-marker (oref section start) t))
                  (oset section end   (copy-marker (oref section end) t))))))
            (org-forester-section-insert-in-reverse
             (push ,s (oref (oref ,s parent) children)))
            ((let ((parent (oref ,s parent)))
               (oset parent children
                     (nconc (oref parent children)
                            (list ,s)))))))
         (when org-forester-section-insert-in-reverse
           (setq org-forester-section-insert-in-reverse nil)
           (oset ,s children (nreverse (oref ,s children))))
         ,s))))

(defun org-forester-section-cancel-section ()
  "Cancel inserting the section that is currently being inserted.
Remove all traces of that section."
  (when org-forester-section-insert-section--current
    (if (not (oref org-forester-section-insert-section--current parent))
        (insert "(empty)\n")
      (delete-region (oref org-forester-section-insert-section--current start)
                     (point))
      (setq org-forester-section-insert-section--current nil)
      (throw 'cancel-section nil))))

(cl-defun org-forester-section-insert-heading (&key (level nil) (args nil))
  "Insert the heading for the section currently being inserted.

This function should only be used inside `org-forester-section-insert-section'.

With LEVEL, insert heading into LEVEL.

When called without any arguments, then just set the `content'
slot of the object representing the section being inserted to
a marker at `point'.  The section should only contain a single
line beginning with stars when this function is used like this.

When called with arguments ARGS, which have to be strings, or
nil, then insert those strings at point. These strings do not
need to contain stars at the beginning. The section should not
contain any text before this happens and afterwards it should
again only contain a single line.

The `content' property of the section object is the end of the
heading (which lasts from `start' to `content') and the beginning
of the the body (which lasts from `content' to `end'). If the
value of `content' is nil, then the section has no body. If a
section does have a heading, then its height must be exactly one
line, including a trailing newline character. This isn't
enforced, you are responsible for getting it right. The only
exception is that this function does insert a newline character
if necessary."
  (declare (indent defun))
  (if args
      (let ((heading (apply #'concat args)))
        (insert (if level
                    (make-string level ?*)
                  (make-string (1+ (or (org-get-previous-line-level)
                                       0)) ?*))
                " "
                heading)
        )
    (save-excursion
      (beginning-of-line)
      (insert (if level
                  (make-string level ?*)
                (make-string (1+ (or (org-get-previous-line-level)
                                     0)) ?*))
              " ")))
  (if (not (bolp))
      (insert ?\n))
  (oset org-forester-section-insert-section--current content
        (if org-forester-section-inhibit-markers (point) (point-marker))))

(defmacro org-forester-section-insert-section-body (&rest body)
  "Use BODY to insert the section body, once the section is expanded.
If the section is expanded when it is created, then this is
like `progn'.  Otherwise BODY isn't evaluated until the section
is explicitly expanded."
  (declare (indent 0))
  (let ((f (cl-gensym))
        (s (cl-gensym))
        (l (cl-gensym)))
    `(let ((,f (lambda () ,@body))
           (,s org-forester-section-insert-section--current))
       (if (oref ,s hidden)
           (oset ,s washer
                 (lambda ()
                   (let ((,l (org-forester-section-lineage ,s t)))
                     (dolist (s ,l)
                       (set-marker-insertion-type (oref s end) t))
                     (funcall ,f)
                     (dolist (s ,l)
                       (set-marker-insertion-type (oref s end) nil))
                     ;; (org-forester-section-maybe-remove-heading-map ,s)
                     ;;(org-forester-section-maybe-remove-visibility-indicator ,s)
                     )))
         (funcall ,f)))))

;;; Utilities
(defun org-forester-section-map-sections (function &optional section)
  "Apply FUNCTION to all sections for side effects only, depth first.
If optional SECTION is non-nil, only map over that section and
its descendants, otherwise map over all sections in the current
buffer, ending with `org-forester-section-root-section'."
  (let ((section (or section org-forester-section-root-section)))
    (mapc (lambda (child) (org-forester-section-map-sections function child))
          (oref section children))
    (funcall function section)))

(defun org-forester-section-section-position-in-heading-p (&optional section pos)
  "Return t if POSITION is inside the heading of SECTION.
POSITION defaults to point and SECTION defaults to the
current section."
  (unless section
    (setq section (org-forester-section-current-section)))
  (unless pos
    (setq pos (point)))
  (ignore-errors ; Allow navigating broken sections.
    (and section
         (>= pos (oref section start))
         (<  pos (or (oref section content)
                     (oref section end)))
         t)))

(provide 'org-forester-section)
;;; org-forester-section.el ends here
