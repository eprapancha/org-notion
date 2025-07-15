;;; org-notion.el --- Main entry point for Notion synchronization -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Your Name
;;
;; Author: Your Name <your-email@example.com>
;; Keywords: notion, org-mode, sync, project-management
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides a robust, stateful, bidirectional synchronization
;; tool between Notion and Emacs Org Mode. It is designed to leverage the
;; strengths of both platforms for an efficient project management workflow.
;;
;; See README.md for the full project brief and system design.
;;
;;; Code:

(require 'notion-api)
(require 'org-parser)
(require 'org-notion-sync)

;;;###autoload
(defgroup org-notion nil
  "Settings for the Notion synchronization tool."
  :group 'org)

;;;###autoload
(defcustom org-notion-database-id nil
  "The ID of the Notion database to synchronize with.
This must be set by the user for the synchronization to work.
You can find the database ID in the Notion URL. It's the part
between the last '/' and the '?'."
  :type 'string
  :group 'org-notion)

;;;###autoload
(defcustom org-notion-api-token nil
  "The Notion API token.
This is a secret token for an 'Internal Integration'. It is
recommended to set this via a secure method like `auth-source`
rather than customizing it directly here. If this variable is nil,
the tool will attempt to retrieve the token from `auth-source`."
  :type 'string
  :group 'org-notion)

;;;###autoload
(defun notion-sync ()
  "Run the bidirectional synchronization between the current Org
file and the configured Notion database.

This command orchestrates the entire sync process:
1. Loads the state cache.
2. Fetches changes from both the local Org file and the remote
   Notion database.
3. Reconciles the changes using a three-way merge algorithm.
4. Applies the updates to Notion (via API) and the Org buffer.
5. Saves the new state cache."
  (interactive)
  (unless (buffer-file-name)
    (error "Buffer is not visiting a file"))
  (unless org-notion-database-id
    (error "Please set `org-notion-database-id`"))
  (org-notion-sync--start))

(provide 'org-notion)

;;; org-notion.el ends here
