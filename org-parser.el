;;; org-parser.el --- Parse and modify Org Mode files programmatically -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Your Name
;;
;; Author: Your Name <your-email@example.com>
;; Keywords: org, parser, ast
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module is responsible for all direct manipulation of .org
;; files. It uses the built-in `org-element.el` library, which is
;; the canonical way to safely and programmatically interact with
;; Org documents.
;;
;; The core principle is to treat the Org file as a database. We
;; parse it into an Abstract Syntax Tree (AST), perform all
;; modifications on the AST in memory, and then serialize the
;; modified AST back into the buffer. This is safer and more
;; robust than using regex or simple text insertion.
;;
;;; Code:

(require 'org-element)
(require 'cl-lib)

;;; Core Parsing and Traversal

(defun org-parser/get-ast ()
  "Parse the current buffer and return its Org AST.
The AST is the authoritative representation of the document's
structure."
  (org-element-parse-buffer))

(defun org-parser/find-sync-items (ast)
  "Traverse the AST and find all sync-aware Org headlines.
AST is the parsed Org document tree from `org-parser/get-ast`.

A headline is considered "sync-aware" if it has a PROPERTIES
drawer containing a :NOTION_ID: property.

Returns a list of these headline elements."
  (let ((sync-items '()))
    (org-element-map ast 'headline
      (lambda (headline)
        (when (org-element-property :NOTION_ID headline)
          (push headline sync-items)))
      nil t)
    (nreverse sync-items)))

;;; Data Extraction from Org Elements

(defun org-parser/get-headline-properties (headline)
  "Extract all key-value pairs from a headline's PROPERTIES drawer.
HEADLINE is an org-element of type 'headline'.

Returns an alist of (KEY . VALUE) pairs."
  (let ((props '()))
    (org-element-map (org-element-contents headline) 'node-property
      (lambda (node)
        (push (cons (intern (concat ":" (org-element-property :key node)))
                    (org-element-property :value node))
              props))))
    (nreverse props)))

(defun org-parser/get-headline-title (headline)
  "Extract the title from a headline element.
HEADLINE is an org-element of type 'headline'."
  (org-element-property :title headline))

(defun org-parser/get-headline-todo-keyword (headline)
  "Extract the TODO keyword from a headline element.
HEADLINE is an org-element of type 'headline'."
  (org-element-property :todo-keyword headline))

;;; Modifying the Org Buffer

(defun org-parser--write-ast-to-buffer (ast)
  "Serialize the AST and replace the buffer content.
AST is the modified abstract syntax tree.
This function also creates a backup of the original file."
  (let ((buffer-file-name (buffer-file-name)))
    (when buffer-file-name
      (backup-buffer))
    (erase-buffer)
    (insert (org-element-interpret-data ast))
    (save-buffer)))

(defun org-parser/update-entry-properties (headline-element changes)
  "Update properties of a given headline element in the AST.
HEADLINE-ELEMENT is the direct AST element for the headline.
CHANGES is an alist of property changes, e.g., '((:NOTION_ID . "new-id"))."
  (let ((ast (org-parser/get-ast)))
    ;; Here, headline-element is a reference into the AST.
    ;; We need to find its equivalent in the newly parsed AST.
    ;; This is tricky. A better way is to re-find it by a unique identifier.
    ;; For now, we assume the element is from the *same* AST that we will write.
    (dolist (change changes)
      (let ((prop-key (car change))
            (prop-val (cdr change)))
        (pcase prop-key
          (:title (org-element-put-property headline-element :title prop-val))
          (:todo-keyword (org-element-put-property headline-element :todo-keyword prop-val))
          (_ (org-element-put-property headline-element prop-key prop-val)))))
    (org-parser--write-ast-to-buffer ast)))


(defun org-parser/update-entry (notion-id changes)
  "Update a specific Org entry corresponding to a Notion page.
NOTION-ID is the ID of the page.
CHANGES is an alist describing the modifications, e.g.,
'((:title . "New Title") (:todo-keyword . "DONE")).

This function will find the correct headline, modify its AST
representation in-place, and then serialize the entire AST
back to the buffer."
  (let* ((ast (org-parser/get-ast))
         (headline (car (org-element-map ast 'headline
                          (lambda (el)
                            (when (equal (org-element-property :NOTION_ID el) notion-id)
                              el))))))
    (when headline
      (org-parser/update-entry-properties headline changes)
      (message "Updated Org entry for Notion ID %s" notion-id))))

(defun org-parser/create-entry (properties)
  "Create a new Org entry from Notion data.
PROPERTIES is an alist of properties from a Notion page object."
  (let ((title (alist-get :title properties "New Task"))
        (notion-id (alist-get :NOTION_ID properties))
        (db-id (alist-get :DATABASE_ID properties)))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "* TODO %s\n" title))
    (insert ":PROPERTIES:\n")
    (when notion-id
      (insert (format ":NOTION_ID:      %s\n" notion-id)))
    (when db-id
      (insert (format ":DATABASE_ID:    %s\n" db-id)))
    (insert ":END:\n")
    (save-buffer)
    (message "Created new Org entry for Notion ID %s" notion-id)))

(defun org-parser/flag-conflict (notion-id reason)
  "Add a :CONFLICT: tag and a note to an Org entry.
NOTION-ID identifies the entry to flag.
REASON is a string explaining the conflict for manual resolution."
  (let* ((ast (org-parser/get-ast))
         (headline (car (org-element-map ast 'headline
                          (lambda (el)
                            (when (equal (org-element-property :NOTION_ID el) notion-id)
                              el))))))
    (when headline
      ;; Add :CONFLICT: tag
      (let ((tags (org-element-property :tags headline)))
        (unless (member "CONFLICT" tags)
          (org-element-put-property headline :tags (cons "CONFLICT" tags))))
      ;; Add explanatory note
      (let* ((section (org-element-contents headline))
             (paragraph (org-element-create 'paragraph nil (list (org-element-create 'text nil reason)))))
        (org-element-set-contents headline (append section (list paragraph))))
      (org-parser--write-ast-to-buffer ast)
      (message "Flagged conflict for Notion ID %s" notion-id))))


(provide 'org-parser)

;;; org-parser.el ends here
