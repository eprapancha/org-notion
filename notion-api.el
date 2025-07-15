;;; notion-api.el --- A stateless client for the Notion REST API -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Your Name
;;
;; Author: Your Name <your-email@example.com>
;; Keywords: api, notion, web
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module provides a low-level, stateless interface for all
;; communication with the Notion REST API. Its purpose is to abstract
;; away the details of HTTP requests, authentication, and JSON parsing.
;;
;; It does not hold any state related to the synchronization process.
;;
;; See: https://developers.notion.com/reference/intro
;;
;;; Code:

(require 'json)
(require 'url-http)
(require 'auth-source)

;;; Custom Variables
(defcustom org-notion-api-token nil
  "The Notion API token.
This is a secret token for an 'Internal Integration'. It is
recommended to set this via a secure method like `auth-source`
rather than customizing it directly here. If this variable is nil,
the tool will attempt to retrieve the token from `auth-source`."
  :type 'string
  :group 'org-notion)

;;; Constants

(defconst notion-api-version "2022-06-28"
  "The version of the Notion API to use.")

(defconst notion-api-base-url "https://api.notion.com/v1/"
  "The base URL for all Notion API endpoints.")

;;; Core API Functions

(defun notion-api--get-auth-token ()
  "Retrieve the Notion API token.
This function will be responsible for securely fetching the token,
ideally from `auth-source` (e.g., ~/.authinfo.gpg). For now,
it can prompt the user if `org-notion-api-token` is not set."
  (or org-notion-api-token
      (let ((found (auth-source-search :host "api.notion.com" :user "token")))
        (if found
            (funcall (plist-get (car found) :secret))
          (read-passwd "Notion API Token: ")))))

(defun notion-api--request (method endpoint &optional data)
  "Send a request to the Notion API and parse the JSON response.
METHOD is the HTTP method (e.g., 'GET', 'POST', 'PATCH').
ENDPOINT is the API endpoint path (e.g., \"databases/DB_ID/query\").
DATA is an optional alist to be encoded as the JSON request body."
  (let* ((url (concat notion-api-base-url endpoint))
         (token (notion-api--get-auth-token))
         (url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(format "Bearer %s" token))
            ("Notion-Version" . ,notion-api-version)
            ("Content-Type" . "application/json")))
         (url-request-data (and data (json-encode data))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (re-search-forward "^$" nil t)
        (let ((json-object-type 'alist)
              (json-key-type 'symbol))
          (json-read))))))


(defun notion-api/query-database (db-id)
  "Query a Notion database to retrieve a list of its pages.
DB-ID is the UUID of the database.

Returns a list of page objects as alists.
See: https://developers.notion.com/reference/post-database-query"
  (let ((endpoint (format "databases/%s/query" db-id)))
    (notion-api--request "POST" endpoint)))

(defun notion-api/get-page (page-id)
  "Retrieve a specific Notion page object by its ID.
PAGE-ID is the UUID of the page.

Returns the full page object as an alist.
See: https://developers.notion.com/reference/retrieve-a-page"
  (let ((endpoint (format "pages/%s" page-id)))
    (notion-api--request "GET" endpoint)))

(defun notion-api/update-page (page-id properties)
  "Update the properties of a specific Notion page.
PAGE-ID is the UUID of the page to update.
PROPERTIES is an alist representing the new property values in
the format required by the Notion API.

Example PROPERTIES:
'((Status . ((status . ((name . \"In Progress\"))))))

Returns the updated page object as an alist.
See: https://developers.notion.com/reference/patch-page"
  (let ((endpoint (format "pages/%s" page-id))
        (data `((properties . ,properties))))
    (notion-api--request "PATCH" endpoint data)))

(defun notion-api/create-page (parent-db-id properties)
  "Create a new page within a Notion database.
PARENT-DB-ID is the UUID of the database where the page will be created.
PROPERTIES is an alist representing the properties for the new page.

Returns the newly created page object as an alist.
See: https://developers.notion.com/reference/post-page"
  (let ((endpoint "pages")
        (data `((parent . ((database_id . ,parent-db-id)))
                (properties . ,properties))))
    (notion-api--request "POST" endpoint data)))

(defun notion-api/search (query)
  "Search for Notion pages and databases accessible to the integration.
QUERY is the string to search for.

Returns a list of page or database objects as alists.
See: https://developers.notion.com/reference/post-search"
  (let ((endpoint "search")
        (data `((query . ,query))))
    (notion-api--request "POST" endpoint data)))

(provide 'notion-api)

;;; notion-api.el ends here
