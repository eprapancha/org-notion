# **Project Brief & System Design: Bidirectional Notion & Emacs Org Mode Sync Tool**

## **1\. Project Goal & Philosophy**

### **1.1. Objective**

To create a robust, stateful, bidirectional synchronization tool between Notion and Emacs Org Mode. The primary focus is on project management workflows, enabling the use of Org Mode as a power-user front-end for Notion databases used in a collaborative work environment.

### **1.2. Guiding Principle: "Show Notion Who's Boss"**

The core philosophy is to leverage the unique strengths of each platform. Notion will be used for its collaborative features, web/mobile accessibility, and user-friendly UI for team members. Emacs, however, will serve as the primary, high-efficiency "command center" for the power user. The tool should feed structured data from Notion into the superior text-manipulation and ecosystem-integration capabilities of Org Mode.

### **1.3. Development Tooling**

This project will be developed in Emacs Lisp. The **Gemini CLI** will be used as an AI coding assistant to help generate, debug, and refactor the necessary Elisp code based on this system design.

## **2\. Core Architectural Decisions**

### **2.1. The Paradigm Mismatch**

The central challenge is bridging two fundamentally different ecosystems:

* **Notion:** A structured, database-driven, API-first web application where data objects have stable UUIDs.  
* **Org Mode:** A parser-driven, plain-text system where structure is emergent from a formal grammar interpreted by Emacs.

This requires a true synchronizer, not a simple converter.

### **2.2. Stateful, Three-Way Merge Sync Engine**

The tool must be **stateful**. It cannot simply overwrite data. The logic will be based on a **three-way merge** principle, comparing three states for every synchronized item:

1. **The Local State:** The current content in the .org file.  
2. **The Remote State:** The current content in Notion, identified by its last\_edited\_time timestamp.  
3. **The Base State:** The cached state from the last successful sync.

### **2.3. Independent, File-Based Cache**

To manage state, the tool will use a custom, self-contained cache.

* **Implementation:** A simple Emacs Lisp hash table saved to a file (e.g., .notion-sync-cache.el) in the user's project directory.  
* **Schema:** For each Notion Page ID, the cache will store:  
  * The last\_edited\_time from the Notion API.  
  * A cryptographic hash (e.g., SHA-256) of the relevant Org entry's content to detect local changes.  
* **Rationale:** This approach avoids external dependencies like org-roam, making the tool lightweight, self-contained, and usable by any Org Mode user, regardless of their personal knowledge management setup.

## **3\. System Components & Implementation Plan**

The tool will be modular, broken into distinct Elisp files.

### **3.1. Module 1: notion-api.el (The API Client)**

* **Purpose:** A stateless module to handle all communication with the Notion REST API.  
* **Authentication:** Use an **Internal Integration Token**. The tool will prompt the user for this token and should support storing it securely in auth-source (\~/.authinfo.gpg).  
* **Core Functions:**  
  * notion-api/query-database(db-id)  
  * notion-api/get-page(page-id)  
  * notion-api/update-page(page-id, properties)  
  * notion-api/create-page(parent-db-id, properties)  
  * notion-api/search(query)  
* **Dependencies:** url.el (or request.el), json.el.

### **3.2. Module 2: org-parser.el (The Org Manipulator)**

* **Purpose:** To programmatically parse, analyze, and modify .org files.  
* **Core Technology:** The built-in org-element.el library is the definitive choice.  
* **Workflow:**  
  1. Parse the entire buffer into an Abstract Syntax Tree (AST) using org-element-parse-buffer.  
  2. Use org-element-map to traverse the AST and find all sync-aware headlines.  
  3. Modify the AST in-memory based on changes from Notion.  
  4. Use org-element-interpret-data to serialize the modified AST back into text, replacing the buffer's content.  
* **Safety:** Before writing changes, the tool **must** create a backup of the original .org file (e.g., myfile.org.bak) to prevent data loss from bugs.

### **3.3. Module 3: org-notion-sync.el (The Core Engine)**

* **Purpose:** The main module that orchestrates the entire process.  
* **Main Command:** M-x notion-sync.  
* **Logic:** Implements the main synchronization loop:  
  1. Load the state cache.  
  2. Scan local Org files for new and modified items.  
  3. Scan remote Notion databases for new and modified items (using last\_edited\_time for efficiency).  
  4. Reconcile changes and resolve conflicts.  
  5. Execute API calls to Notion and AST modifications to Org files.  
  6. Save the updated Org buffer and the new state cache.

## **4\. Data Schemas & Mapping**

### **4.1. Notion API Data Model**

The tool will primarily interact with the following Notion objects and their properties relevant to project management.

| PM Feature | Notion Property Type | API type Value | Example update JSON Snippet |
| :---- | :---- | :---- | :---- |
| Task Title | Title | title | {"Name": {"title":}} |
| Status | Status | status | {"Status": {"status": {"name": "In Progress"}}} |
| Due Date | Date | date | {"Due Date": {"date": {"start": "2024-09-15"}}} |
| Assignee | Person | people | {"Assignee": {"people": \[{"id": "user-uuid"}\]}}} |
| Relation | Relation | relation | {"Related": {"relation": \[{"id": "page-uuid"}\]}}} |

### **4.2. Org Mode Canonical Schema**

A sync-aware Org entry **must** use a PROPERTIES drawer directly under the headline.  
`* TODO [#A] Design the Sync Engine :project:`  
  `:PROPERTIES:`  
  `:NOTION_ID:      d093f1d2-0046-4ce7-8b36-e58a3f0d8043`  
  `:DATABASE_ID:    af5f89b5-a8ff-4c56-a5e8-69797d11b9f8`  
  `:LAST_SYNC_TIMESTAMP: 2023-10-27T10:00:00Z`  
  `:END:`  
  `SCHEDULED: <2024-10-28 Mon>`

| Org Mode Construct | org-element Property | Notion Property |
| :---- | :---- | :---- |
| Headline Title | :title | Title |
| TODO Keyword | :todo-keyword | Status |
| SCHEDULED: Timestamp | :scheduled | Date |
| DEADLINE: Timestamp | :deadline | Date |
| Tags (:tag:) | :tags | Multi-select |
| Custom Property (:ASSIGNEE:) | node-property | Person |

## **5\. Conflict Resolution Strategy**

A hybrid strategy is required to balance automation and data safety.

1. **Automatic Merge (Default):** If changes are made to *different* properties in Org and Notion (e.g., deadline changed in Org, status changed in Notion), the changes will be automatically merged.  
2. **"Notion Wins" Policy (User-Defined Default):** For any **direct conflict** where the *same* property has been changed in both locations, the tool will **always prioritize the version from Notion**. The local change in Org Mode will be overwritten. This provides a predictable outcome and prevents the sync process from halting.  
3. **Deletion/Modification Conflict:** If an item is deleted in one system while modified in the other, the deletion will be halted and the item will be flagged in Org Mode with a :CONFLICT: tag and an explanatory comment for manual resolution.

## **6\. "Emacsy" Integration & User Experience**

The tool must integrate seamlessly into a modern Emacs workflow.

* **org-agenda:** Synced items with SCHEDULED or DEADLINE timestamps and TODO keywords will automatically appear in all agenda views, providing a unified dashboard of personal and work tasks.  
* **org-capture:** A custom org-capture template will be defined to allow for the rapid creation of new Notion tasks from anywhere within Emacs. The template will pre-fill the :DATABASE\_ID: property, and the sync engine will handle creating the page in Notion and back-filling the :NOTION\_ID:.  
* **consult & embark Integration:**  
  * A custom consult source will be created to search for Notion pages and databases directly from the minibuffer by querying the Notion API's search endpoint.  
  * embark actions will be defined for these Notion results, allowing the user to immediately:  
    * Sync the selected page to a local Org file.  
    * Open the Notion page in a web browser.  
    * Copy the Notion page URL.

## **7\. Advanced Concepts & "Superpowers"**

This architecture unlocks workflows impossible in Notion alone:

* **Executable Tasks (org-babel):** Embed and execute live code blocks (shell scripts, Python, etc.) directly within the body of a task in your Org file.  
* **Universal Linking:** Link tasks to anything Emacs can point to: a specific line in a source file, an email in mu4e, another Org heading, etc., providing deep context that is invisible but linked to the high-level task in Notion.  
* **Text-Based Refactoring:** Use the full power of Emacs text manipulation (keyboard macros, org-refile, promote/demote subtree) to restructure projects at the speed of thought.

## **8\. Exclusions & Future Scope**

* **V1 Scope:** The initial version will focus exclusively on synchronizing headline-level metadata (title, status, dates, properties, tags).  
* **V2 Scope (Future):** Full synchronization of page *content* (the body of the Org entry and the blocks on the Notion page) is a significant undertaking and is deferred to a future version. This would require a complex, bidirectional translator between Org markup and Notion's block object array.

#### **Works cited**

1\. Notion API Overview, https://developers.notion.com/docs/getting-started 2\. richardwesthaver/org-notion: Notion.so integration with ... \- GitHub, https://github.com/richardwesthaver/org-notion 3\. Page \- Notion API, https://developers.notion.com/reference/page 4\. Org Element API, https://orgmode.org/worg/dev/org-element-api.html 5\. How to modify content of org-mode document using org-element API?, https://emacs.stackexchange.com/questions/31011/how-to-modify-content-of-org-mode-document-using-org-element-api 6\. Database \- Notion API, https://developers.notion.com/reference/database 7\. Page properties \- Notion API, https://developers.notion.com/reference/page-property-values 8\. Database properties \- Notion API, https://developers.notion.com/reference/property-object 9\. Drawers (The Org Manual), https://orgmode.org/manual/Drawers.html 10\. Conflict resolution for two-way sync \- Software Engineering Stack Exchange, https://softwareengineering.stackexchange.com/questions/153806/conflict-resolution-for-two-way-sync 11\. Conflict resolution strategies in Data Synchronization | by Mobterest Studio \- Medium, https://mobterest.medium.com/conflict-resolution-strategies-in-data-synchronization-2a10be5b82bc 12\. David O'Toole Org tutorial \- Org mode, https://orgmode.org/worg/org-tutorials/orgtutorial\_dto.html 13\. Org mode: Adding a properties drawer to a capture template? \- Emacs Stack Exchange, https://emacs.stackexchange.com/questions/26119/org-mode-adding-a-properties-drawer-to-a-capture-template 14\. What can orgmode do that notion or obsidian can't \- Reddit, https://www.reddit.com/r/orgmode/comments/10tl4xa/what\_can\_orgmode\_do\_that\_notion\_or\_obsidian\_cant/ 15\. souvikinator/notion-to-md: Convert Notion pages, blocks, or entire lists into any format: Markdown, MDX, JSX, HTML, LaTeX, and more. A powerful Notion conversion engine that lets you write once in Notion and publish seamlessly anywhere, in any format. Compatible with your favorite CMS, \- GitHub, https://github.com/souvikinator/notion-to-md 16\. How do I extract content from a Notion page using the Notion API? \- Latenode community, https://community.latenode.com/t/how-do-i-extract-content-from-a-notion-page-using-the-notion-api/5474
