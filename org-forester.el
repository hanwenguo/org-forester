;;; org-forester.el --- Create your forest in org-mode  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2024 Hanwen Guo <g.hanwen@outlook.com>

;; Author: Hanwen Guo <g.hanwen@outlook.com>
;; URL: https://github.com/g-hanwen/org-forester
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.4") (org-transclusion "1.3"))
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

;; Org-forester let you create your forest in org-mode. Dependencies
;; include org-roam and org-transclusion.

;;; Code:
(require 'seq)
(require 'cl-lib)

(require 'xref)

(require 'org-transclusion)

(require 'org-forester-section)

;;;; Declarations
;; (defvar ...)

;;;; Options
(defgroup org-forester nil
  "Create your forest of notes in Org-mode."
  :group 'org
  :prefix "org-forester-")

(defcustom org-forester-buffer-sections
  (list #'org-forester-contexts-section
        #'org-forester-backlinks-section
        #'org-forester-references-section
        #'org-forester-related-notes-section)
  "A list of sections for the `org-forester-mode' buffers.
Each section is a function that is passed the file name for which
the section will be constructed for as the first argument."
  :group 'org-forester
  :type `(repeat (symbol :tag "Function")))

(defcustom org-forester-buffer-postrender-functions (list)
  "Functions to run after the Org-forester buffer is rendered.
Each function accepts no arguments, and is run with the
Org-forester buffer as the current buffer."
  :group 'org-forester
  :type 'hook)

(defcustom org-forester-buffer-prerender-functions (list)
  "Functions to run before the Org-forester buffer is rendered.
Each function accepts no arguments, and is run with the
Org-forester buffer as the current buffer."
  :group 'org-forester
  :type 'hook)

(defgroup org-forester-faces nil
  "Faces used by Org-forester."
  :group 'org-forester
  :group 'faces)

(defcustom org-forester-lighter " org-forester"
  "The lighter string used by `org-forester-mode'."
  :group 'org-forester
  :type '(string))

(defcustom org-forester-date-format "%Y-%m-%d"
  "The date format string to be used for creating date field.
See help of `format-time-string' for possible replacements."
  :group 'org-forester
  :type '(string))

(defcustom org-forester-default-author (or user-full-name user-login-name "Anonymous")
  "The default author.
Used when creating notes."
  :group 'org-forester
  :type '(string))

(defcustom org-forester-author-shortnames (list)
  "The association list specifying the mapping between authors and shortnames."
  :group 'org-forester
  :type '(alist :key-type string :value-type string))

(defcustom org-forester-preview-postrender-functions
  '((org-forester-file-section org-forester-preview-postrender)
    (org-forester-contexts-section org-forester-preview-postrender)
    (org-forester-backlinks-section org-forester-preview-postrender)
    (org-forester-references-section org-forester-preview-postrender)
    (org-forester-related-nodes-section org-forester-preview-postrender))
  "The map between section and postrender functions.
Each key is a section type, and each value is a function which
accepts two arguments: the begin and end position of the preview
content in current buffer, and is run with the Org-forester
buffer as the current buffer."
  :group 'org-forester
  :type '(alist :key-type function :value-type function))

;;;; Faces

;;;; Buffer
(defvar org-forester-buffer-current-buffer nil
  "The buffer for which an `org-forester-section-mode' buffer corresponds to.")

(put 'org-forester-buffer-current-buffer 'permanent-local t)

(defvar org-forester-buffer-current-directory nil
  "The `default-directory' of `org-forester-buffer-current-buffer'.")

(put 'org-forester-buffer-current-directory 'permanent-local t)

(defvar org-forester-buffer-current-file nil
  "The file of `org-forester-buffer-current-buffer'.")

(put 'org-forester-buffer-current-title 'permanent-local t)

(defvar org-forester-buffer-current-title nil
  "The title of `org-forester-buffer-current-buffer'.")

(put 'org-forester-buffer-current-title 'permanent-local t)

(defun org-forester-buffer-visit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be visited"))

(defun org-forester-buffer-file-at-point (&optional assert)
  "Return the file at point in the current `org-forester-section-mode' buffer."
  (if-let ((file (org-forester-section-case
                   (org-forester-context-section (oref it file))
                   (org-forester-backlink-section (oref it file))
                   (org-forester-reference-section (oref it file))
                   (org-forester-related-node-section (oref it file))
                   (t (cl-assert (derived-mode-p 'org-forester-mode))))))
      file
    (when assert
      (user-error "No file at point"))))

(defun org-forester-file-visit (file &optional other-window)
  "Visit FILE and return the visited buffer.
With OTHER_WINDOW non-nil do so in another window.
In interactive calls OTHER-WINDOW is set with `universal-argument'."
  (interactive (list (org-forester-buffer-file-at-point 'assert)
                     (oref (org-forester-section-current-section) point)
                     current-prefix-arg))
  (let ((buf (find-file-noselect file))
        (display-buffer-fn (if other-window
                               #'switch-to-buffer-other-window
                             #'pop-to-buffer-same-window)))
    (funcall display-buffer-fn buf)
    (with-current-buffer buf
      (widen)
      (goto-char (point-min))
      (org-next-visible-heading 1))
    (when (org-invisible-p) (org-fold-show-context))
    buf))

(defun org-forester-buffer-refresh ()
  "Refresh the contents of the currently selected Org-forester buffer."
  (interactive)
  (cl-assert (derived-mode-p 'org-forester-section-mode))
  (save-excursion (org-forester-buffer-render-contents)))

(defun org-forester-buffer-render-contents ()
  "Recompute and render the contents of an Org-forester buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-forester-section-mode)
    (setq-local default-directory org-forester-buffer-current-directory)
    (run-hooks 'org-forester-buffer-prerender-functions)
    (org-forester-section-insert-section (org-forester)
      (insert (format "#+title: Sections for %s\n" org-forester-buffer-current-title))
      (dolist (section org-forester-buffer-sections)
        (funcall section org-forester-buffer-current-file)))
    (run-hooks 'org-forester-buffer-postrender-functions)
    (goto-char 0)))

(defvar org-forester-buffer "*org-forester*"
  "The Org-forester buffer name. Must be surround with \"*\".")

;;;###autoload
(defun org-forester-buffer-toggle ()
  "Toggle display of the Org-forester buffer."
  (interactive)
  (pcase (org-forester-buffer--visibility)
    ('visible
     (quit-window nil (get-buffer-window org-forester-buffer)))
    ((or 'exists 'none)
     (progn
       (display-buffer (get-buffer-create org-forester-buffer))
       (when-let ((buffer (current-buffer)))
         (unless (equal buffer org-forester-buffer-current-buffer)
           (setq org-forester-buffer-current-buffer buffer
                 org-forester-buffer-current-directory (file-name-directory buffer-file-name)
                 org-forester-buffer-current-file (expand-file-name buffer-file-name)
                 org-forester-buffer-current-title (org-get-title buffer))
           (with-current-buffer (get-buffer-create org-forester-buffer)
             (org-forester-buffer-render-contents))))))))

(define-inline org-forester-buffer--visibility ()
  "Return the current visibility state of the Org-forester buffer."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((get-buffer-window org-forester-buffer) 'visible)
    ((get-buffer org-forester-buffer) 'exists)
    (t 'none))))

;;;; Sections
;;;;; Utilities
(defun org-forester-collect-links (&optional filter)
  "Return all linked files in the current buffer.
Use FILTER to filter links."
  (let ((links (list)))
    (save-excursion
      (org-with-point-at 1
        (while (re-search-forward org-link-any-re nil :no-error)
          (backward-char)
          (let* ((begin (match-beginning 0))
                 (element (org-element-context))
                 (type (org-element-type element))
                 link)
            (cond
             ((eq type 'link)
              (setq link element))
             ((and (member type '(node-property keyword))
                   (not (member-ignore-case (org-element-property :key element)
                                            '("transclude"))))
              (setq link (save-excursion
                           (goto-char begin)
                           (save-match-data (org-element-link-parser))))))
            (when (and link
                       (or (not filter) (funcall filter link)))
              (cl-pushnew link links))))))
    (mapcar (lambda (link)
              (pcase (org-element-property :type link)
                ("file"
                 (org-element-property :path link))
                ("id"
                 (car (org-id-find (org-element-property :path link))))
                ("denote"
                 (when (fboundp 'denote)
                   (let* ((link-text (org-element-property :path link))
                          (search-option (and (string-match "::\\(.*\\)\\'" link-text)
                                              (match-string 1 link-text)))
                          (id (if (and search-option (not (string-empty-p search-option)))
                                  (substring link-text 0 (match-beginning 0))
                                link-text)))
                     (denote-get-path-by-id id))))
                (_ "")))
            links)))

(defun org-forester-collect-citations ()
  "Collect all citation to files in current buffer."
  (let ((info (org-element-parse-buffer))
        (references (list)))
    (org-element-map info 'citation-reference
      (lambda (cite)
        (dolist (file (mapcar (lambda (match-item)
                                (plist-get match-item :file))
                              (org-forester-matches-in-files
                               (format "^#\\+reference[ \\t]*:[ \\t]*%s" (org-element-property :key cite)))))
          (cl-pushnew file references))))
    (cl-remove-duplicates references)))

(defun org-forester-directory-files (&optional files-matching-regexp omit-current)
  "Return list of absolute file paths in `org-forester-buffer-current-directory'.

With optional FILES-MATCHING-REGEXP, restrict files to those
matching the given regular expression.

With optional OMIT-CURRENT, do not include the current file."
  (let ((files (mapcar
                #'expand-file-name
                (seq-filter
                 (lambda (file)
                   (not (backup-file-name-p file)))
                 (directory-files-recursively
                  org-forester-buffer-current-directory
                  directory-files-no-dot-files-regexp
                  nil
                  nil
                  :follow-symlinks)))))
    (when (and omit-current buffer-file-name)
      (setq files (delete buffer-file-name files)))
    (when files-matching-regexp
      (setq files (seq-filter
                   (lambda (f)
                     (string-match-p files-matching-regexp
                                     (when-let (((file-name-absolute-p f))
                                                (file-name (expand-file-name f))
                                                ((string-prefix-p org-forester-buffer-current-directory file-name)))
                                       (substring-no-properties file-name (length org-forester-buffer-current-directory)))))
                   files)))
    files))

(defvar org-forester-matches-re
  (rx (group (one-or-more anything))
      ":"
      (group (one-or-more digit))
      ":"
      (group (one-or-more digit))
      ":"
      (group (zero-or-more anything)))
  "Regex for the return result of a ripgrep query.")

(defun org-forester-matches-in-files (regexp &optional match-result filter-fn omit-current)
  "Find all matches for REGEXP in `org-forester-buffer-current-directory'.
When MATCH-RESULT is non-nil, also return match result.
Particularly, if MATCH-RESULT is symbol `line', return the whole
line of match result instead of only matched part.

When FILTER-FN is non-nil, use it for filtering the results.

When OMIT-CURRENT is non-nil, ignore `org-forester-buffer-current-file'.

Use ripgrep for searching."
  (when (executable-find "rg")
    (let* ((rg-command (concat "rg -L -n --column -H --no-heading -i "
                               (if match-result
                                   (if (eq match-result 'line)
                                       ""
                                     "-o ")
                                 "-o -r '' ")
                               (mapconcat (lambda (glob) (concat "-g " glob))
                                          '("\"*.org\"" "\"*.org.gpg\"" "\"*.org.age\"")
                                          " ")
                               " "
                               (shell-quote-argument regexp)
                               " "
                               org-forester-buffer-current-directory))
           (results (split-string (string-trim (shell-command-to-string rg-command)) "\n")))
      (seq-filter (if filter-fn
                      (lambda (res)
                        (if (and omit-current
                                 (equal (plist-get res :file)
                                        org-forester-buffer-current-file))
                            nil
                          (funcall filter-fn res)))
                    (lambda (res)
                      (if (and omit-current
                               (equal (plist-get res :file)
                                      org-forester-buffer-current-file))
                          nil
                        res)))
                  (mapcar (lambda (line)
                            (save-match-data
                              (when (string-match org-forester-matches-re line)
                                (append
                                 `(:file ,(expand-file-name (match-string 1 line) org-forester-buffer-current-directory)
                                   :row ,(string-to-number (match-string 2 line))
                                   :column ,(string-to-number (match-string 3 line)))
                                 (when match-result
                                   (list :match (match-string 4 line)))))))
                          results)))))

(defmacro org-forester-with-file (file keep-buf-p &rest body)
  "Execute BODY within FILE in Org-mode.
If FILE is nil, execute BODY in the current buffer.
Kills the buffer if KEEP-BUF-P is nil, and FILE is not yet visited."
  (declare (indent 2) (debug t))
  `(let* (new-buf
          (auto-mode-alist nil)
          (find-file-hook nil)
          (buf (or (and (not ,file)
                        (current-buffer)) ;If FILE is nil, use current buffer
                   (find-buffer-visiting ,file) ; If FILE is already visited, find buffer
                   (progn
                     (setq new-buf t)
                     (find-file-noselect ,file)))) ; Else, visit FILE and return buffer
          res)
     (with-current-buffer buf
       (unless (derived-mode-p 'org-mode)
         (delay-mode-hooks
           (let ((org-inhibit-startup t)
                 (org-agenda-files nil))
             (org-mode)
             (hack-local-variables))))
       (setq res (progn ,@body))
       (unless (and new-buf (not ,keep-buf-p))
         (save-buffer)))
     (if (and new-buf (not ,keep-buf-p))
         (when (find-buffer-visiting ,file)
           (kill-buffer (find-buffer-visiting ,file))))
     res))

(defun org-forester-file-keyword-value (keyword &optional default seperator file)
  "Get the value of KEYWORD in FILE.
If KEYWORD does not exist in FILE, return DEFAULT.
If there are several values, join them with SEPERATOR.
If FILE is nil, use the file of current buffer."
  (save-excursion
    (org-forester-with-file (or file (buffer-file-name)) nil
      (if-let ((raw-values (cdr (assoc (upcase keyword)
                                       (org-collect-keywords (list keyword)))))
               (values (cl-loop for value in raw-values
                                if (not (string-empty-p value))
                                collect value)))
          (string-join values seperator)
        default))))

(defun org-forester-file-date (&optional file)
  "Get the \"#+date\" value in FILE.
If FILE is nil, use the file of current buffer.
If there is no date, return the date of today."
  (org-forester-file-keyword-value "date"
                                   (format-time-string org-forester-date-format (current-time))
                                   ", "
                                   file))

(defun org-forester-file-author (&optional file)
  "Get the \"#+author\" value in FILE.
If FILE is nil, use the file of current buffer.
If there is no author, return the value of `org-forester-default-author'."
  (org-forester-file-keyword-value "author"
                                   org-forester-default-author
                                   ", "
                                   file))

(defun org-forester-file-category (&optional file)
  "Get the \"#+author\" value in FILE.
If FILE is nil, use the file of current buffer.
If there is no author, return the value of `org-forester-default-author'."
  (org-forester-file-keyword-value "category"
                                   ""
                                   " & "
                                   file))

(defun org-forester-file-id (&optional file)
  "Get the id of FILE.
If FILE is nil, use the file of current buffer."
  (save-excursion
    (org-forester-with-file (or file (buffer-file-name)) nil
      (org-id-get 0))))

(defun org-forester-file-title (&optional file)
  "Get the \"#+title\" value in FILE or the first heading.
If FILE is nil, use the file of current buffer."
  (save-excursion
    (org-forester-with-file (or file (buffer-file-name)) nil
      (let ((file-title (string-join (cdr (assoc "TITLE" (org-collect-keywords '("title")))) " "))
            (file-category (org-forester-file-category)))
        (when (string-empty-p file-title)
          (setq file-title nil))
        (concat (if (not (string-empty-p file-category))
                    (concat (capitalize file-category) ". ")
                  "")
                (or file-title
                    (progn
                      (goto-char (point-min))
                      (when (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
                        (cdr (assoc "ITEM" (org-entry-properties)))))
                    ""))))))

(defun org-forester-file-content (&optional file)
  "Get the content in FILE.
If FILE is nil, use the file of current buffer."
  (save-excursion
    (let (content
          obj
          headline-levels
          highest-headline)
      (org-forester-with-file (or file (buffer-file-name)) nil
        (setq obj (org-element-parse-buffer))
        (setq obj (org-element-map obj org-element-all-elements
                    (lambda (data)
                      (org-element-map data '(keyword drawer)
                        (lambda (d) (org-element-extract-element d)))
                      data)
                    nil nil org-element-all-elements nil))
        (org-element-map obj 'link
          (lambda (link)
            (when (string-equal "file" (org-element-property :type link))
              (let ((path (org-element-property :path link)))
                (unless (file-name-absolute-p path)
                  (org-element-put-property
                   link :path
                   (expand-file-name
                    path
                    (file-name-directory (buffer-file-name (current-buffer))))))))))
        (org-element-map obj 'headline
          (lambda (h)
            (push (org-element-property :level h) headline-levels)))
        (setq highest-headline (when headline-levels
                                 (seq-min headline-levels)))
        (setq content (substring-no-properties (org-element-interpret-data obj))))
      (with-temp-buffer
        ;; This temp buffer needs to be in Org Mode
        ;; Otherwise, subtree won't be recognized as a Org subtree
        (delay-mode-hooks (org-mode))
        (insert content)
        (org-with-point-at 1
          (let* ((to-level 3)
                 (diff (when (and highest-headline to-level) (- highest-headline to-level))))
            (when diff
              (cond ((< diff 0) ; demote
                     (org-map-entries (lambda ()
                                        (dotimes (_ (abs diff))
                                          (org-do-demote)))))
                    ((> diff 0) ; promote
                     (org-map-entries (lambda ()
                                        (dotimes (_ diff)
                                          (org-do-promote))))))))
          (setq content (buffer-string))))
      content)))

(defun org-forester-file-transclusion (&optional file level)
  "Use Org-transclusion to insert FILE content.
LEVEL is initial level."
  (format "#+transclude: [[file:%s]] :exclude-elements \"drawer keyword\" :expand-links :level %d" (or file org-forester-buffer-current-file) (or level 2)))

;;;;; File
(defvar org-forester-file-map
  (let ((map (make-keymap)))
    (set-keymap-parent map org-forester-section-mode-map)
    (keymap-set map "C-RET" 'org-forester-file-visit)
    map)
  "Keymap for `org-forester-file-section's.")

(defclass org-forester-file-section (org-forester-section)
  ((keymap :initform 'org-forester-file-map))
  "A `magit-section' used by `org-forester' to outline NODE in its own heading.")

(cl-defun org-forester-file-insert-section (&key file title-fn preview-fn)
  "Insert section for a link about FILE.

TITLE-FN is the function used to preview title.
PREVIEW-FN is the function used to provide preview contents."
  (org-forester-section-insert-section section (org-forester-file-section file)
    (insert (funcall title-fn file))
    (org-forester-section-insert-heading :level 2)
    (oset section file file)
    (let ((beg (point))
          (end))
      (org-forester-section-insert-section section (org-forester-preview-section nil nil)
        (insert (funcall preview-fn file))
        (setq end (point))
        (unless (bolp)
          (insert "\n"))
        (oset section file file))
      (funcall (cadr (assoc (oref section type) org-forester-preview-postrender-functions)) beg end))))

;;;;; Preview
(defclass org-forester-preview-section (org-forester-file-section)
  ((keymap :initform 'org-forester-file-map))
  "A `magit-section' used by `org-forester' to contain preview content.
The preview content comes from FILE, and the link is at POINT.")

(defun org-forester-preview-visit (file &optional other-window)
  "Visit FILE and return the visited buffer.
With OTHER_WINDOW non-nil do so in another window.
In interactive calls OTHER-WINDOW is set with `universal-argument'."
  (interactive (list (org-forester-buffer-file-at-point 'assert)
                     (oref (org-forester-section-current-section) point)
                     current-prefix-arg))
  (let ((buf (find-file-noselect file))
        (display-buffer-fn (if other-window
                               #'switch-to-buffer-other-window
                             #'pop-to-buffer-same-window)))
    (funcall display-buffer-fn buf)
    (with-current-buffer buf
      (widen)
      (goto-char (point-min))
      (org-next-visible-heading 1))
    (when (org-invisible-p) (org-fold-show-context))
    buf))

(defun org-forester-preview-postrender (beg end)
  "This function acts as a placeholder.")

;;;;; Context (transcluding parent)
(defun org-forester-contexts-regexp (file)
  "Regexp that matches links to FILE."
  (let* ((rel-file (when-let (((file-name-absolute-p file))
                              (file-name (expand-file-name file))
                              ((string-prefix-p org-forester-buffer-current-directory file-name)))
                     (substring-no-properties file-name (length org-forester-buffer-current-directory))))
         (res (list (format "file:%s" file)
                    (format "file:%s" rel-file))))
    (when-let (id (org-forester-file-id file))
      (cl-pushnew (format "id:%s" id) res))
    (when (fboundp 'denote)
      (require 'denote)
      (when-let ((denote-file-has-identifier-p file)
                 (identifier (denote-retrieve-filename-identifier file)))
        (cl-pushnew (format "denote:%s" identifier) res)))
    (concat "^[ \\t]*#\\+transclude:[ \t]*\\[\\[("
            (mapconcat 'regexp-quote res "|")
            ")\\](\\[([^a&&b]+?)\\])?\\]")))

(defun org-forester-contexts-get (file)
  "Return the contexts for FILE."
  (let ((regexp (org-forester-contexts-regexp file)))
    (mapcar (lambda (match-result)
              (plist-get match-result :file))
            (org-forester-matches-in-files regexp))))

(cl-defun org-forester-contexts-section (file &key (show-context-p nil))
  "The contexts section for FILE.

When SHOW-CONTEXT-P is not null, only show contexts for which
this predicate is not nil."
  (when-let ((contexts (org-forester-contexts-get file)))
    (org-forester-section-insert-section (org-forester-contexts-section)
      (org-forester-section-insert-heading :level 1 :args '("Contexts"))
      (dolist (context contexts)
        (when (or (null show-context-p)
                  (and (funcall show-context-p context)))
          (org-forester-file-insert-section
           :file context
           :title-fn #'org-forester-file-title
           :preview-fn #'org-forester-file-content)
          )))))

;;;;; Backlink
(defun org-forester-backlinks-regexp (file)
  "Regexp that matches links to FILE."
  (let* ((rel-file (when-let (((file-name-absolute-p file))
                              (file-name (expand-file-name file))
                              ((string-prefix-p org-forester-buffer-current-directory file-name)))
                     (substring-no-properties file-name (length org-forester-buffer-current-directory))))
         (res (list (format "file:%s" file)
                    (format "file:%s" rel-file))))
    (when-let (id (org-forester-file-id file))
      (cl-pushnew (format "id:%s" id) res))
    (when (fboundp 'denote)
      (require 'denote)
      (when-let ((denote-file-has-identifier-p file)
                 (identifier (denote-retrieve-filename-identifier file)))
        (cl-pushnew (format "denote:%s" identifier) res)))
    (concat "\\[\\[("
            (mapconcat 'regexp-quote res "|")
            ")\\](\\[([^a&&b]+?)\\])?\\]")))

(defun org-forester-backlinks-get (file)
  "Return the backlinks for FILE."
  (let ((regexp (org-forester-backlinks-regexp file)))
    (mapcar (lambda (match-result)
              (plist-get match-result :file))
            (org-forester-matches-in-files regexp 'line
                                           (lambda (result)
                                             (not (string-prefix-p "#+transclude:" (plist-get result :match))))))))

(defun org-forester-backlinks-section (file)
  "The backlinks section for FILE."
  (when-let ((backlinks (org-forester-backlinks-get file)))
    (org-forester-section-insert-section (org-forester-backlinks-section)
      (org-forester-section-insert-heading :level 1 :args '("Backlinks"))
      (dolist (backlink backlinks)
        (org-forester-file-insert-section
         :file backlink
         :title-fn #'org-forester-file-title
         :preview-fn #'org-forester-file-content)))))

;;;;; Related notes (notes current note links to)
;; TODO: Test Related notes
(defun org-forester-related-notes-get (file)
  "Return the related notes for FILE."
  (org-forester-with-file file nil
    (org-forester-collect-links)))

(defun org-forester-related-notes-section (file)
  "The related notes section for FILE."
  (when-let ((related-notes (org-forester-related-notes-get file)))
    (org-forester-section-insert-section (org-forester-related-notes)
      (org-forester-section-insert-heading :level 1 :args '("Related Notes"))
      (dolist (related-note related-notes)
        (org-forester-file-insert-section
         :file related-note
         :title-fn #'org-forester-file-title
         :preview-fn #'org-forester-file-content)))))

;;;;; References (ref nodes that this node cites)
;; TODO: Test bibliography
(defun org-forester-references-get (file)
  "Return the references for FILE."
  (org-forester-with-file file nil
    (org-forester-collect-citations)))

(defun org-forester-references-section (file)
  "The references section for FILE."
  (when-let ((references (org-forester-references-get file)))
    (org-forester-section-insert-section (org-forester-refereces)
      (org-forester-section-insert-heading :level 1 :args '("References"))
      (dolist (reference references)
        (org-forester-file-insert-section
         :file reference
         :title-fn #'org-forester-file-title
         :preview-fn #'org-forester-file-content)))))

;;;;; TODO: Neighbors (nodes with common source node)

;;;; Transclusion
(defun org-forester-transclusion-content-buffer (plist)
  "Return a list of payload for transclusion.

It always includes the first section,
always exclude the keywords, and append as a headline the value
of the \"#+title:\" keyword.

The payload is a plist that consists of the following properties:
- :src-content
- :src-buf
- :src-beg
- :src-end

This function applies multiple filters on the Org elements before
construting the payload based on PLIST. It is the
\"keyword-plist\" for the transclusion being worked on; each
property controls the filter applied to the transclusion."
  (let ((exclude-elements
         (org-transclusion-keyword-plist-to-exclude-elements plist))
        (expand-links (plist-get plist :expand-links))
        title
        obj)
    (setq obj (org-element-parse-buffer))
    (setq title (org-forester-file-title))
    (let ((org-transclusion-exclude-elements
           (append '(keyword) exclude-elements org-transclusion-exclude-elements)))
      (setq obj (org-element-map obj org-element-all-elements
                  #'org-transclusion-content-filter-org-exclude-elements
                  nil nil org-element-all-elements nil)))
    (when expand-links
      (org-element-map obj 'link #'org-transclusion-content-filter-expand-links))
    (let ((raw-content (org-element-interpret-data obj)))
      (list :src-content (concat "* " title "\n" (string-trim-left raw-content))
            :src-buf (current-buffer)
            :src-beg (point-min)
            :src-end (point-max)))))

(defun org-forester-transclusion-content-denote (link plist)
  "Return a list of payload from Org Denote LINK object and PLIST."
  (save-excursion
    (let* ((link-text (org-element-property :path link))
           (search-option (and (string-match "::\\(.*\\)\\'" link-text)
                               (match-string 1 link-text)))
           (id (if (and search-option (not (string-empty-p search-option)))
                   (substring link-text 0 (match-beginning 0))
                 link-text))
           (path (denote-get-path-by-id id))
           (buf (find-file-noselect path)))
      (with-current-buffer buf
        (org-with-wide-buffer
         (org-forester-transclusion-content-buffer plist))))))

(defun org-forester-transclusion-content-org-link (link plist)
  "Return a list of payload from Org file LINK object and PLIST."
  (save-excursion
    (let* ((path (org-element-property :path link))
           (buf (find-file-noselect path)))
      (with-current-buffer buf
        (org-with-wide-buffer
         (org-forester-transclusion-content-buffer plist))))))

(defun org-forester-transclusion-content-org-id (file plist)
  "Return a list of payload fro Org FILE and PLIST."
  (save-excursion
    (let* ((buf (find-file-noselect file)))
      (with-current-buffer buf
        (org-with-wide-buffer
         (org-forester-transclusion-content-buffer plist))))))

(defun org-forester-transclusion-add (link plist)
  "Return a list for Org Denote LINK object and PLIST.
Return nil if not found."
  (cond
   ((string= "denote" (org-element-property :type link))
    (let* ((identifier (org-element-property :path link))
           (file (denote-get-path-by-id identifier))
           (payload '(:tc-type "org-denote")))
      (if file
          (append payload
                  (org-forester-transclusion-content-denote link plist))
        (message
         (format "No transclusion done for this note. Ensure it works at point %d, line %d"
                 (point) (org-current-line))))))
   ((org-transclusion-org-file-p (org-element-property :path link))
    (append '(:tc-type "org-link")
            (org-forester-transclusion-content-org-link link plist)))
   ((string= "id" (org-element-property :type link))
    (let* ((id (org-element-property :path link))
           (file (ignore-errors (car (org-id-find id))))
           (payload '(:tc-type "org-id")))
      (if file
          (append payload
                  (org-forester-transclusion-content-org-id file plist))
        (message
         (format "No transclusion done for this ID. Ensure it works at point %d, line %d"
                 (point) (org-current-line)))
        nil)))))

;;;; Commands

(provide 'org-forester)
;;; org-forester.el ends here
