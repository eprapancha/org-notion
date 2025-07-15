;;; org-notion-sync.el --- Core synchronization engine -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Your Name
;;
;; Author: Your Name <your-email@example.com>
;; Keywords: sync, org-mode, notion
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module is the heart of the synchronization tool. It contains
;; the stateful logic that orchestrates the entire process. It uses
;; the stateless `notion-api.el` and `org-parser.el` modules to
;; perform the actual I/O.
;;
;; The core logic is based on a three-way merge, comparing three states:
;; 1. The Local State: The current content in the .org file.
;; 2. The Remote State: The current content in Notion.
;; 3. The Base State: The cached state from the last successful sync.
;;
;;; Code:

(require 'notion-api)
(require 'org-parser)
(require 'cl-lib)

;;; Cache Management

(defvar-local org-notion-sync--cache nil
  "In-memory cache for the current sync session.
This is a hash table mapping Notion IDs to cache entries.
Cache Entry Schema (alist):
- :notion-last-edited (string): The last_edited_time from Notion.
- :org-hash (string): A SHA256 hash of the Org entry's content.")

(defun org-notion-sync--get-cache-file-path ()
  "Return the absolute path to the cache file for the current project."
  (expand-file-name ".org-notion-cache.el" (project-root (project-current))))

(defun org-notion-sync--load-cache ()
  "Load the sync state cache from the project's cache file.
If the file doesn't exist, initialize an empty cache."
  (let ((cache-file (org-notion-sync--get-cache-file-path)))
    (if (file-exists-p cache-file)
        (with-temp-buffer
          (insert-file-contents cache-file)
          (setq org-notion-sync--cache (read (current-buffer))))
      (setq org-notion-sync--cache (make-hash-table :test 'equal)))))

(defun org-notion-sync--save-cache ()
  "Save the current state of the sync cache to its file."
  (let ((cache-file (org-notion-sync--get-cache-file-path)))
    (with-temp-file cache-file
      (prin1 org-notion-sync--cache (current-buffer)))))

;;; Core Synchronization Logic (Stubs)

(defun org-notion-sync--get-local-state ()
  "Scan the Org file and return the current state of all sync-aware items.
Returns a hash table mapping Notion IDs to local state alists.
Local State Alist:
- :hash (string): A new hash of the Org entry content.
- :properties (alist): The current properties from the Org entry."
  ;; 1. Parse the buffer using `org-parser/get-ast`.
  ;; 2. Find all sync items using `org-parser/find-sync-items`.
  ;; 3. For each item, calculate its content hash and extract its properties.
  ;; 4. Return a hash table of NotionID -> {hash, properties}.
  (make-hash-table :test 'equal))

(defun org-notion-sync--get-remote-state ()
  "Fetch all pages from the configured Notion database.
Returns a hash table mapping Notion IDs to remote state alists.
Remote State Alist:
- :last-edited (string): The last_edited_time from the API.
- :properties (alist): The properties from the Notion page object."
  ;; 1. Call `notion-api/query-database` with `org-notion-database-id`.
  ;; 2. Process the list of page objects into the required hash table format.
  (make-hash-table :test 'equal))

(defun org-notion-sync--compute-changes (local-state remote-state base-cache)
  "Compare local, remote, and base states to compute necessary actions.
This is the core of the three-way merge. It iterates through all
known Notion IDs and determines what has changed.

Returns a list of change-sets. Each change-set is an alist like:
'((:id . \"...\") (:action . :update-notion) (:data . ...))
'((:id . \"...\") (:action . :update-org) (:data . ...))
'((:id . \"...\") (:action . :conflict) (:reason . ...))
'((:id . \"...\") (:action . :create-in-org))
'((:id . \"...\") (:action . :create-in-notion))"
  ;; This is the most complex function. The logic will be:
  ;; - For each ID present in remote-state:
  ;;   - Was it in base-cache? If not -> new in Notion (:create-in-org).
  ;;   - Was it in local-state? If not -> deleted in Org (:delete-from-notion).
  ;;   - Compare remote last-edited with cached last-edited. Changed in Notion?
  ;;   - Compare local hash with cached hash. Changed in Org?
  ;;   - Based on the booleans above, determine the outcome:
  ;;     - No change: do nothing.
  ;;     - Changed only in Org: :update-notion.
  ;;     - Changed only in Notion: :update-org.
  ;;     - Changed in both: :conflict (or apply resolution policy).
  ;; - For each ID present only in local-state:
  ;;   - It must be a new entry created in Org -> :create-in-notion.
  '())

(defun org-notion-sync--apply-changes (changes)
  "Execute the actions computed by the reconciliation logic.
CHANGES is the list of change-sets from `org-notion-sync--compute-changes`."
  (dolist (change changes)
    (let ((id (alist-get :id change))
          (action (alist-get :action change))
          (data (alist-get :data change)))
      (cond
       ((eq action :update-notion)
        ;; TODO: Convert Org properties (`data`) to Notion API format.
        (notion-api/update-page id data))
       ((eq action :update-org)
        ;; TODO: Convert Notion properties (`data`) to Org format.
        (org-parser/update-entry id data))
       ((eq action :create-in-notion)
        (notion-api/create-page org-notion-database-id data))
       ((eq action :create-in-org)
        (org-parser/create-entry data))
       ((eq action :conflict)
        (org-parser/flag-conflict id (alist-get :reason change)))
       ;; etc.
       ))))

(defun org-notion-sync--start ()
  "The main entry point for the synchronization process."
  (message "Starting Notion sync...")
  ;; 1. Load the cache
  (org-notion-sync--load-cache)
  ;; 2. Get current state from Org and Notion
  (let* ((local-state (org-notion-sync--get-local-state))
         (remote-state (org-notion-sync--get-remote-state))
         ;; 3. Compare and compute changes
         (changes (org-notion-sync--compute-changes local-state remote-state org-notion-sync--cache)))
    ;; 4. Apply changes
    (org-notion-sync--apply-changes changes)
    ;; 5. Update and save the cache for the next run
    ;;    (This needs to be done carefully after all actions succeed)
    (org-notion-sync--save-cache)
    (message "Notion sync finished.")))

(provide 'org-notion-sync)

;;; org-notion-sync.el ends here
