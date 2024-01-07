;;; org-forester.el --- create your forest in org-mode  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Hanwen Guo <g.hanwen@outlook.com>

;; Author: Hanwen Guo <g.hanwen@outlook.com>
;; URL: https://github.com/g-hanwen/org-forester
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (org "9.4") (org-roam "2.2") (org-transclusion "1.3"))
;; Keywords: org-mode, forester, note

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

(require 'magit-section)

(require 'emacsql)

(require 'org-roam-mode)
(require 'org-roam-db)

(require 'org-transclusion)

;;;; Declarations
;; (defvar ...)

;;;; Options
(defgroup org-forester nil
  "Create your forest of notes in Org-mode."
  :group 'org
  :prefix "org-forester-")

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

(defcustom org-forester-author-shortnames nil
  "The association list specifying the mapping between authors and shortnames."
  :group 'org-forester
  :type '(alist :key-type string :value-type string))

(defcustom org-forester-roam-sections
  '(org-forester-roam-contexts-section org-forester-citations-section org-forester-related-nodes-section)
  "The extra sections to show in Org-roam buffer."
  :group 'org-forester
  :type '(sexp))

(defcustom org-forester-roam-metadata-functions
  '(('org-forester-roam-contexts-section #'org-forester-roam-node-metadata)
    ('org-forester-citations-section #'org-forester-roam-node-metadata)
    ('org-forester-related-nodes-section #'org-forester-roam-node-metadata))
  "The map between section and metadata functions."
  :group 'org-forester
  :type '(alist :key-type function :value-type function))

(defcustom org-forester-roam-preview-functions
  '((#'org-forester-roam-contexts-section . #'org-forester-roam-preview-get-contents)
    (#'org-forester-citations-section . #'org-forester-roam-preview-get-contents)
    (#'org-forester-related-nodes-section . #'org-forester-roam-preview-get-contents))
  "The map between section and preview functions."
  :group 'org-forester
  :type '(alist :key-type function :value-type function))

;;;; Faces
(defface org-forester-metadata
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the metadata (date, author, taxon, etc.) of the node."
  :group 'org-forester-faces)

;;;; Hack Org-roam database to store transclusions specially
(defun org-forester-roam-db-map-transclusions (fns)
  "Run FNS over all transcluding links in the current buffer."
  (org-with-point-at 1
    (while (re-search-forward org-link-any-re nil :no-error)
      ;; `re-search-forward' let the cursor one character after the link, we need to go backward one char to
      ;; make the point be on the link.
      (backward-char)
      (let* ((begin (match-beginning 0))
             (element (org-element-context))
             (type (org-element-type element))
             link)
        (when (and (eq type 'keyword)
                   (org-string-equal-ignore-case
                    (org-element-property :key element)
                    "transclude"))
          (setq link (save-excursion
                       (goto-char begin)
                       (save-match-data (org-element-link-parser)))))
        (when link
          (dolist (fn fns)
            (funcall fn link)))))))

(defun org-forester-roam-db-insert-transclusion (link)
  "Insert transclusion data for LINK at current point into the Org-roam cache."
  (save-excursion
    (goto-char (org-element-property :begin link))
    (beginning-of-line)
    (let ((type (org-element-property :type link))
          (path (org-element-property :path link))
          (source (org-roam-id-at-point))
          (properties (list :outline (ignore-errors
                                       ;; This can error if link is not under any headline
                                       (org-get-outline-path 'with-self 'use-cache)))))
      (when (and source path (equal type "id"))
        (org-roam-db-query
         [:insert :into links
          :values $v1]
         (vector (point) source path "transclusion" properties))))))

(defun org-forester-roam-db-map-links (old-fun &rest args)
  "Insert all links in the current buffer.
Deal with transclusions properly.
Meant to be used as an advice around `org-roam-db-map-links'
which would be the OLD-FUN and passes ARGS to it.."
  ;; Disable org-transclusion-mode before scanning for links
  (org-transclusion-before-save-buffer)
  (apply old-fun args)
  (org-forester-roam-db-map-transclusions
   (list #'org-forester-roam-db-insert-transclusion))
  ;; Enable org-transclusion-mode after scanning for links
  (org-transclusion-after-save-buffer))

;;;; Org-roam buffer sections
;;;;; Metadata
(defalias 'org-forester--file-title #'org-roam-db--file-title)

(defun org-forester--file-date ()
  "In current Org buffer, get the date.
If there is no date, return the date of today."
  (if-let ((raw-dates (cdr (assoc "DATE" (org-collect-keywords '("date")))))
           (dates (cl-loop for date in raw-dates
                           if (not (string-empty-p date))
                           collect date)))
      (string-join dates  ", ")
    (format-time-string org-forester-date-format (current-time))))

(defun org-forester--file-author ()
  "In current Org-buffer, get the author.
If there is no author, return the value of `org-forester-default-author'."
  (if-let ((raw-authors (cdr (assoc "AUTHOR" (org-collect-keywords '("author")))))
           (authors (cl-loop for author in raw-authors
                             if (not (string-empty-p author))
                             collect author)))
      (string-join authors ", ")
    org-forester-default-author))

(cl-defmethod org-forester-node-date ((node org-roam-node))
  "Get the date of NODE."
  (save-excursion (org-roam-with-file (org-roam-node-file node) nil
                    (org-forester--file-date))))

(cl-defmethod org-forester-node-author ((node org-roam-node))
  "Get the author of NODE."
  (save-excursion (org-roam-with-file (org-roam-node-file node) nil
                    (org-forester--file-author))))

(defun org-forester-roam-node-metadata (node)
  "Get the metadata string for NODE."
  (concat (format " created on %s"
                  (propertize
                   (org-forester-node-date node) 'font-lock-face 'org-forester-metadata))
          (format " by %s"
                  (propertize
                   (org-forester-node-author node) 'font-lock-face 'org-forester-metadata))))

(defclass org-forester-roam-node-section (org-roam-node-section)
  ((keymap :initform 'org-roam-node-map))
  "A `magit-section' used by `org-forester' to outline NODE in its own heading.")

(cl-defun org-forester-roam-node-insert-section (&key node point metadata-fn preview-fn)
  "Insert section for a link about NODE.

NODE is an `org-roam-node' to be shown.

POINT is a character position where the link is located.

METADATA-FN is the function used to render metadata.

PREVIEW-FN is the function used to provide preview contents.

Despite the name, this function actually inserts 2 sections at
the same time:

1. `org-forester-roam-node-section' for a heading that describes
   NODE. Acts as a parent section of the following one.

2. `org-forester-roam-preview-section' for a preview content that comes
   from NODE's file. Acts a child section of the previous
   one."
  (magit-insert-section section (org-roam-node-section)
    (insert (propertize (org-roam-node-title node)
                        'font-lock-face 'org-roam-title))
    (insert (funcall metadata-fn node))
    (magit-insert-heading)
    (oset section node node)
    (magit-insert-section section (org-forester-roam-preview-section nil t)
      (insert (string-trim (org-roam-fontify-like-in-org-mode
                            (funcall preview-fn (org-roam-node-file node))))
              "\n")
      (oset section file (org-roam-node-file node))
      (oset section point point))))

;;;;; Preview contexts
(defvar org-forester-roam-preview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-preview-map)
    (define-key map [remap org-roam-preview-visit] 'org-forester-roam-preview-visit)
    map)
  "Keymap for `org-forester-roam-preview-section's.")

(defclass org-forester-roam-preview-section (org-roam-preview-section)
  ((keymap :initform 'org-forester-roam-context-preview-map)
   ;; (file :initform nil)
   ;; (point :initform nil)
   )
  "A `magit-section' used by `org-forester' to contain preview content.
The preview content comes from FILE, and the link is at POINT.")

(defun org-forester-roam-preview-get-contents (file)
  "Get preview content for FILE.
This function returns the all contents under the first headline
up to the end of file."
  (save-excursion
    (org-roam-with-temp-buffer file
      (org-with-wide-buffer
       (org-with-point-at 1
         (let ((beg (save-excursion
                      (org-next-visible-heading 1)
                      (forward-line)
                      (point)))
               (end (point-max)))
           (string-trim (buffer-substring-no-properties beg end))))))))

(defun org-forester-roam-context-preview-visit (file point &optional other-window)
  "Visit FILE and return the visited buffer.
If `org-forester-visit-context-inplace' is non-nil, visit FILE at POINT.
With OTHER_WINDOW non-nill do so in another window.
In interactive calls OTHER-WINDOW is set with `universal-argument'."
  (interactive (list (org-roam-buffer-file-at-point 'assert)
                     (oref (magit-current-section) point)
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

;;;;; Context (transcluding parent)
(cl-defstruct (org-forester-roam-context (:constructor org-forester-roam-context-create)
                                         (:copier nil))
  source-node point properties)

(cl-defmethod org-roam-populate ((context org-forester-roam-context))
  "Populate CONTEXT from database."
  (setf (org-forester-roam-context-source-node context)
        (org-roam-populate (org-forester-roam-context-source-node context)))
  context)

(defun org-forester-roam-contexts-get (node)
  "Return the contexts for NODE."
  (let* ((sql [:select :distinct [source dest pos properties]
               :from links
               :where (= dest $s1)
               :and (= type "transclusion")
               :group :by source
               :having (funcall min pos)])
         (contexts (org-roam-db-query sql (org-roam-node-id node))))
    (cl-loop for context in contexts
             collect (pcase-let ((`(,source-id ,_ ,pos ,properties) context))
                       (org-roam-populate
                        (org-forester-roam-context-create
                         :source-node (org-roam-node-create :id source-id)
                         :point pos
                         :properties properties))))))

(defun org-forester-roam-contexts-sort (a b)
  "Default sorting function for contexts A and B.
Sorts by title."
  (string< (org-roam-node-title (org-forester-roam-context-source-node a))
           (org-roam-node-title (org-forester-roam-context-source-node b))))

(cl-defun org-forester-roam-contexts-section (node &key (show-context-p nil))
  "The contexts section for NODE.

When SHOW-CONTEXT-P is not null, only show contexts for which
this predicate is not nil."
  (when-let ((contexts (seq-sort #'org-forester-roam-contexts-sort
                                 (org-forester-roam-contexts-get node))))
    (magit-insert-section (org-forester-roam-contexts)
      (magit-insert-heading "Context: ")
      (dolist (context contexts)
        (when (or (null show-context-p)
                  (and (not (null show-context-p))
                       (funcall show-context-p context)))
          (let ((source-node (org-forester-roam-context-source-node context))
                (point (org-forester-roam-context-point context)))
            (org-forester-roam-node-insert-section
             :node source-node
             :point point
             :metadata-fn #'org-forester-roam-node-metadata
             :preview-fn #'org-forester-roam-preview-get-contents))))
      (insert ?\n))))

;;;;; Related nodes (nodes current node links to)
;; TODO: Test Related nodes
(cl-defstruct (org-forester-related-node (:constructor org-forester-related-node-create)
                                         (:copier nil))
  target-node pos properties)

(cl-defmethod org-roam-populate ((related-node org-forester-related-node))
  "Populate RELATED-NODE from database."
  (setf (org-forester-related-node-target-node related-node)
        (org-roam-populate (org-forester-related-node-target-node related-node)))
  related-node)

(defun org-forester-related-nodes-get (node)
  "Return the related nodes for NODE."
  (let ((links (org-roam-db-query
                ;; [:select :distinct [links:dest links:pos links:properties]
                ;;  :from links
                ;;  :where (= links:source $s1)
                ;;  :and :not-exists
                ;;  (:select 1
                ;;   :from refs
                ;;   :where (= refs:node-id links:dest))]
                [:select :distinct [links:dest links:pos links:properties]
                 :from links
                 :left-join refs :on (= links:dest refs:node-id)
                 :where (= links:source $s1)
                 :and (= refs:node-id nil)]
                (org-roam-node-id node)))
        related-nodes)
    (pcase-dolist (`(,target-id ,pos ,properties) links)
      (push (org-roam-populate
             (org-forester-related-node-create
              :target-node (org-roam-node-create :id target-id)
              :pos pos
              :properties properties))
            related-nodes))
    related-nodes))

(defun org-forester-related-nodes-sort (a b)
  "Default sorting function for citations A and B.
Sorts by occurance order in source."
  (< (org-forester-related-node-pos a)
     (org-forester-related-node-pos b)))

(defun org-forester-related-nodes-section (node)
  "The related nodes section for NODE."
  (when-let ((related-nodes (seq-sort #'org-forester-related-nodes-sort
                                      (org-forester-related-nodes-get node))))
    (magit-insert-section (org-forester-roam-related-nodes)
      (magit-insert-heading "Related: ")
      (dolist (related-node related-nodes)
        (let ((target-node (org-forester-related-node-target-node related-node))
              (point (org-forester-related-node-pos related-node)))
          (org-forester-roam-node-insert-section
           :node target-node
           :point point
           :metadata-fn #'org-forester-roam-node-metadata
           :preview-fn #'org-forester-roam-preview-get-contents)))
      (insert ?\n))))

;;;;; Bibliography (ref nodes that this node cites)
;; TODO: Test bibliography
(cl-defstruct (org-forester-citation (:constructor org-forester-citation-create)
                                     (:copier nil))
  target-node ref pos properties)

(cl-defmethod org-roam-populate ((citation org-forester-citation))
  "Populate CITATION from database."
  (setf (org-forester-citation-target-node citation)
        (org-roam-populate (org-forester-citation-target-node citation)))
  citation)

(defun org-forester-citations-get (node)
  "Return the bibliography for NODE."
  (let ((refs (org-roam-db-query [:select :distinct [links:dest refs:ref links:pos links:properties]
                                  :from links
                                  :left-join refs
                                  :where (= links:source $s1)
                                  :and (= refs:node-id links:dest)
                                  :union
                                  :select :distinct [citations:node-id refs:ref citations:pos citations:properties]
                                  :from citations
                                  :left-join refs
                                  :where (= citations:note-id $s1)
                                  :and (= citations:cite-key refs:ref)]
                                 (org-roam-node-id node)))
        citations)
    (pcase-dolist (`(,target-id ,ref ,pos ,properties) refs)
      (push (org-roam-populate
             (org-forester-citation-create
              :target-node (org-roam-node-create :id target-id)
              :ref ref
              :pos pos
              :properties properties))
            citations))
    citations))

(defun org-forester-citations-sort (a b)
  "Default sorting function for citations A and B.
Sorts by occurance order in source."
  (< (org-forester-citation-pos a)
     (org-forester-citation-pos b)))

(defun org-forester-citations-section (node)
  "The citations section for NODE."
  (when-let ((citations (seq-sort #'org-forester-citations-sort
                                  (org-forester-citations-get node))))
    (magit-insert-section (org-forester-roam-citations)
      (magit-insert-heading "References: ")
      (dolist (citation citations)
        (let ((target-node (org-forester-citation-target-node citation))
              (point (org-forester-citation-pos citation)))
          (org-forester-roam-node-insert-section
           :node target-node
           :point point
           :metadata-fn #'org-forester-roam-node-metadata
           :preview-fn #'org-forester-roam-preview-get-contents)))
      (insert ?\n))))

;;;;; TODO: Neighbors (nodes with common source node)

;;;; Commands
(defun org-forester-add-transclusion (&optional filter-fn sort-fn)
  "Add a transclusion at point. The target node must already exists.

FILTER-FN is a function to filter out nodes: it takes an org-roam-node,
and when nil is returned the node will be filtered out.
SORT-FN is a function to sort nodes. See `org-roam-node-read-sort-by-file-mtime'
for an example sort function.

If current line is not empty, insert under it."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* ((node (org-roam-node-read "" filter-fn sort-fn t))
               (description (org-roam-node-formatted node)))
          (if (org-roam-node-id node)
              (let ((id (org-roam-node-id node)))
                (beginning-of-line)
                (skip-chars-forward "[ \t]")
                (when (not (eolp))
                  (end-of-line)
                  (insert ?\n))
                (insert
                 "#+transclu: "
                 (org-link-make-string
                  (concat "id:" id)
                  description)
                 " :level 2 :exclude-elements \"keyword\"")
                (run-hook-with-args 'org-roam-post-node-insert-hook
                                    id
                                    description))
            (user-error "Transcluding needs an existing node"))))
    (deactivate-mark)))

(defun org-forester-add-citation ()
  "TODO: Add a citation at point."
  (user-error "Unimplemented"))

(defvar org-forester-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c n t") #'org-forester-add-transclusion)
    map)
  "Keymap for `org-forester-mode'.")

;;;###autoload
(define-minor-mode org-forester-mode
  "Toggle `org-forester-mode'."
  :keymap org-forester-mode-map
  :lighter org-forester-lighter
  :global nil
  :init-value nil
  :interactive nil
  :group 'org-forester
  (unless (derived-mode-p 'org-mode)
    (error "`org-forester-mode' should be enabled only in `org-mode'"))
  (cond
   (org-forester-mode
    (org-forester-enable))
   (t
    (org-forester-disable))))

(defun org-forester-remove-advice ()
  "Remove advices added by Org-forester."
  (interactive)
  (advice-remove #'org-roam-db-map-links #'org-forester-roam-db-map-links))

(defun org-forester--maybe-clean-advice ()
  "Remove advices if there is no buffer with org-forester-mode enabled.
This runs in `kill-buffer-hook'."
  (when (eq 1 (cl-count-if
               (lambda (buf) (buffer-local-value 'org-forester-mode buf))
               (buffer-list)))
    (org-forester-remove-advice)))

;;;###autoload
(define-globalized-minor-mode global-org-forester-mode
  org-forester-mode #'org-forester-enable
  :group 'org-forester
  :predicate '(#'org-mode #'org-roam-mode)
  (if global-org-forester-mode
      (when (not (advice-member-p #'org-forester-roam-db-map-links #'org-roam-db-map-links))
        (advice-add #'org-roam-db-map-links :around #'org-forester-roam-db-map-links))
    (org-forester--maybe-clean-advice)))

;;;###autoload
(defun org-forester-enable ()
  "Enable the Org-forester minor mode."
  (interactive)
  (cond
   ((derived-mode-p #'org-mode)
    (add-to-invisibility-spec 'org-forester)
    (when (not (advice-member-p #'org-forester-roam-db-map-links #'org-roam-db-map-links))
      (advice-add #'org-roam-db-map-links :around #'org-forester-roam-db-map-links))
    (add-hook 'kill-buffer-hook #'org-forester--maybe-clean-advice))
   ((derived-mode-p #'org-roam-mode)
    ;; (make-local-variable 'org-roam-mode-sections)
    (dolist (section org-forester-roam-sections)
      (when (not (memq section org-roam-mode-sections))
        (add-to-list 'org-roam-mode-sections section))))))

(defun org-forester-disable ()
  "Disable the Org-forester minor mode."
  (interactive)
  (cond
   ((derived-mode-p #'org-mode)
    (remove-from-invisibility-spec 'org-forester)
    (remove-hook 'kill-buffer-hook #'org-forester--maybe-clean-advice)
    (org-forester--maybe-clean-advice))
   ((derived-mode-p #'org-roam-mode)
    (kill-local-variable 'org-roam-mode-sections))))

(provide 'org-forester)
;;; org-forester.el ends here
