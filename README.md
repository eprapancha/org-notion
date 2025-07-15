## 7. Logging & Debugging

To ensure transparency and aid in debugging, the tool will implement a two-tiered logging strategy:

1.  **Dedicated Log Buffer:** A dedicated, unlisted buffer named `*org-notion-sync-log*` will be created for detailed, timestamped logs of the entire synchronization process. This buffer will be cleared at the start of each sync and will contain information about API requests, state comparisons, and actions taken. This is the primary location for debugging information.
2.  **User-Facing Messages:** The `*Messages*` buffer will only be used for high-level, concise status updates (e.g., "Sync started," "Sync finished," "Error occurred"). This keeps the main feedback channel clean and unobtrusive.