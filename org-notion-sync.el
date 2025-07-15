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
(require 'secure-hash)

;;; Constants
(defconst org-notion-sync--log-buffer-name "*org-notion-sync-log*"
  "The name of the buffer for logging sync activity.")

;;; Logging
(defun org-notion-sync--log (message &rest args)
  "Log a timestamped MESSAGE to the sync log buffer.
ARGS are passed to `format` to format the message."
  (let ((log-buffer (get-buffer-create org-notion-sync--log-buffer-name)))
    (with-current-buffer log-buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
        (insert (apply #'format message args))
        (insert "\n")))))

(defun org-notion-sync--prepare-log-buffer ()
  "Clear and prepare the log buffer for a new sync run."
  (let ((log-buffer (get-buffer-create org-notion-sync--log-buffer-name)))
    (with-current-buffer log-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Starting Org-Notion sync for file: %s\n" (buffer-file-name)))
        (insert "--------------------------------------------------\n")))))

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
  (org-notion-sync--log "Loading cache...")
  (let ((cache-file (org-notion-sync--get-cache-file-path)))
    (if (file-exists-p cache-file)
        (with-temp-buffer
          (insert-file-contents cache-file)
          (setq org-notion-sync--cache (read (current-buffer)))
          (org-notion-sync--log "Cache loaded successfully from %s" cache-file))
      (setq org-notion-sync--cache (make-hash-table :test 'equal))
      (org-notion-sync--log "No cache file found. Initialized empty cache."))))

(defun org-notion-sync--save-cache ()
  "Save the current state of the sync cache to its file."
  (org-notion-sync--log "Saving cache...")
  (let ((cache-file (org-notion-sync--get-cache-file-path)))
    (with-temp-file cache-file
      (prin1 org-notion-sync--cache (current-buffer)))
    (org-notion-sync--log "Cache saved successfully to %s" cache-file)))

(defun org-notion-sync--update-cache-entry (id org-hash notion-edited-time)
  "Update or create a cache entry for a given Notion ID."
  (org-notion-sync--log "Updating cache for ID %s" id)
  (puthash id
           `((:org-hash . ,org-hash)
             (:notion-last-edited . ,notion-edited-time))
           org-notion-sync--cache))

;;; State Retrieval

(defun org-notion-sync--hash-org-entry (headline)
  "Compute a SHA256 hash for the content of an Org headline."
  (secure-hash 'sha256 (org-element-interpret-data headline)))

(defun org-notion-sync--get-local-state ()
  "Scan the Org file and return the current state of all sync-aware items."
  (org-notion-sync--log "Scanning local Org file for items...")
  (let ((local-state (make-hash-table :test 'equal))
        (ast (org-parser/get-ast)))
    (dolist (headline (org-parser/find-sync-items ast))
      (let* ((notion-id (org-element-property :NOTION_ID headline))
             (title (org-parser/get-headline-title headline))
             (hash (org-notion-sync--hash-org-entry headline)))
        (when notion-id
          (org-notion-sync--log "Found local item: '%s' (ID: %s)" title notion-id)
          (puthash notion-id
                   `((:hash . ,hash)
                     (:element . ,headline)
                     (:title . ,title)
                     (:todo-keyword . ,(org-parser/get-headline-todo-keyword headline)))
                   local-state))))
    ;; TODO: Process new items (those with DB_ID but no NOTION_ID)
    (org-notion-sync--log "Found %d local items with Notion IDs." (hash-table-count local-state))
    local-state))

(defun org-notion-sync--get-remote-state ()
  "Fetch all pages from the configured Notion database."
  (org-notion-sync--log "Querying Notion database: %s" org-notion-database-id)
  (let ((remote-state (make-hash-table :test 'equal))
        (response (notion-api/query-database org-notion-database-id)))
    (when response
      (let ((results (alist-get 'results response)))
        (org-notion-sync--log "Received %d pages from Notion." (length results))
        (dolist (page results)
          (let* ((notion-id (alist-get 'id page))
                 (last-edited (alist-get 'last_edited_time page))
                 (props (alist-get 'properties page))
                 (title-prop (alist-get 'Name props))
                 (title (car (alist-get 'title (car (alist-get 'title title-prop)))))
                 (status-prop (alist-get 'Status props))
                 (status (alist-get 'name (alist-get 'status status-prop))))
            (puthash notion-id
                     `((:last-edited . ,last-edited)
                       (:title . ,title)
                       (:status . ,status)
                       (:properties . ,props))
                     remote-state)))))
    remote-state))

;;; Change Computation (3-Way Merge)

(defun org-notion-sync--compute-changes (local-state remote-state base-cache)
  "Compare local, remote, and base states to compute necessary actions."
  (org-notion-sync--log "Computing changes...")
  (let ((changes '())
        (all-ids (let ((keys '()))
                   (maphash (lambda (k v) (push k keys)) local-state)
                   (maphash (lambda (k v) (push k keys)) remote-state)
                   (maphash (lambda (k v) (push k keys)) base-cache)
                   (seq-uniq keys))))
    (dolist (id all-ids)
      (let* ((local (gethash id local-state))
             (remote (gethash id remote-state))
             (base (gethash id base-cache))
             (local-hash (alist-get :hash local))
             (base-hash (alist-get :org-hash base))
             (remote-edited (alist-get :last-edited remote))
             (base-edited (alist-get :notion-last-edited base))
             (local-changed-p (and local base (not (equal local-hash base-hash))))
             (remote-changed-p (and remote base (not (equal remote-edited base-edited)))))

        (cond
         ;; Conflict: Changed in both places
         ((and local-changed-p remote-changed-p)
          (push `((:id . ,id) (:action . :conflict) (:reason . "Changed in both Org and Notion since last sync.")) changes))
         ;; Changed only in Org -> Update Notion
         (local-changed-p
          (push `((:id . ,id) (:action . :update-notion) (:data . ,local)) changes))
         ;; Changed only in Notion -> Update Org
         (remote-changed-p
          (push `((:id . ,id) (:action . :update-org) (:data . ,remote)) changes))
         ;; New in Notion -> Create in Org
         ((and remote (not local))
          (push `((:id . ,id) (:action . :create-in-org) (:data . ,remote)) changes))
         ;; New in Org -> Create in Notion
         ((and local (not remote))
          (push `((:id . ,id) (:action . :create-in-notion) (:data . ,local)) changes))
         ;; Deleted in Org -> Flag for now
         ((and remote base (not local))
          (push `((:id . ,id) (:action . :conflict) (:reason . "Deleted in Org but still exists in Notion.")) changes))
         ;; Deleted in Notion -> Flag for now
         ((and local base (not remote))
          (push `((:id . ,id) (:action . :conflict) (:reason . "Deleted in Notion but still exists in Org.")) changes)))))
    (org-notion-sync--log "Found %d changes to apply." (length changes))
    changes))

;;; Data Transformation and Change Application

(defun org-notion-sync--org-to-notion-properties (org-data)
  "Convert Org data alist to Notion API properties format."
  (let ((title (alist-get :title org-data))
        (status (alist-get :todo-keyword org-data)))
    `((Name . ((title . ((text . ((content . ,title)))))))
      (Status . ((status . ((name . ,status))))))))

(defun org-notion-sync--notion-to-org-properties (notion-data)
  "Convert Notion data alist to Org parser properties format."
  `((:title . ,(alist-get :title notion-data))
    (:todo-keyword . ,(alist-get :status notion-data))))

(defun org-notion-sync--apply-changes (changes)
  "Execute the actions computed by the reconciliation logic."
  (org-notion-sync--log "Applying changes...")
  (dolist (change changes)
    (let ((id (alist-get :id change))
          (action (alist-get :action change))
          (data (alist-get :data change)))
      (pcase action
        (:update-notion
         (org-notion-sync--log "Action: Update Notion page %s" id)
         (let ((notion-props (org-notion-sync--org-to-notion-properties data)))
           (when-let (updated-page (notion-api/update-page id notion-props))
             (org-notion-sync--update-cache-entry
              id
              (alist-get :hash data)
              (alist-get 'last_edited_time updated-page)))))
        (:update-org
         (org-notion-sync--log "Action: Update Org entry for Notion ID %s" id)
         (let ((org-props (org-notion-sync--notion-to-org-properties data)))
           (org-parser/update-entry id org-props)
           (org-notion-sync--update-cache-entry
            id
            (org-notion-sync--hash-org-entry (car (org-parser/find-sync-items (org-parser/get-ast)))) ;; Re-hash after update
            (alist-get :last-edited data))))
        (:create-in-notion
         (org-notion-sync--log "Action: Create Notion page for new Org entry '%s'" (alist-get :title data))
         (let* ((notion-props (org-notion-sync--org-to-notion-properties data))
                (new-page (notion-api/create-page org-notion-database-id notion-props))
                (new-id (alist-get 'id new-page))
                (org-element (alist-get :element data)))
           (when (and new-id org-element)
             (org-parser/update-entry-properties org-element `((:NOTION_ID . ,new-id)))
             (org-notion-sync--log "Successfully created Notion page %s and updated Org entry." new-id)
             (org-notion-sync--update-cache-entry
              new-id
              (org-notion-sync--hash-org-entry org-element) ;; Re-hash after update
              (alist-get 'last_edited_time new-page)))))
        (:create-in-org
         (org-notion-sync--log "Action: Create Org entry for new Notion page %s" id)
         (let ((org-props (org-notion-sync--notion-to-org-properties data)))
           (org-parser/create-entry (append `((:NOTION_ID . ,id) (:DATABASE_ID . ,org-notion-database-id)) org-props))
           (org-notion-sync--update-cache-entry
            id
            (org-notion-sync--hash-org-entry (car (org-parser/find-sync-items (org-parser/get-ast)))) ;; Re-hash after update
            (alist-get :last-edited data))))
        (:conflict
         (org-notion-sync--log "Action: Flag conflict for Notion ID %s" id)
         (org-parser/flag-conflict id (alist-get :reason change)))))))

;;; Main Entry Point

(defun org-notion-sync--start ()
  "The main entry point for the synchronization process."
  (org-notion-sync--prepare-log-buffer)
  (message "Starting Notion sync... (see %s for details)" org-notion-sync--log-buffer-name)
  (org-notion-sync--load-cache)
  (let* ((local-state (org-notion-sync--get-local-state))
         (remote-state (org-notion-sync--get-remote-state))
         (changes (org-notion-sync--compute-changes local-state remote-state org-notion-sync--cache)))
    (if (not changes)
        (progn
          (org-notion-sync--log "Everything is already in sync.")
          (message "Everything is already in sync."))
      (org-notion-sync--apply-changes changes)
      (org-notion-sync--save-cache)
      (message "Notion sync finished. See %s for details." org-notion-sync--log-buffer-name))))

(provide 'org-notion-sync)

;;; org-notion-sync.el ends here
