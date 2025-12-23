;;; sjy2-gptel.el --- LLM Integration for Emacs (Late 2025) -*- lexical-binding: t; -*-

;; The 2025 landscape: gptel + mcp.el reigns supreme
;; gptel: Universal chat client (anywhere in Emacs)
;; mcp.el: Agentic tools (file/web/GitHub access)
;; Focus: Grok (xAI) as default, easy swaps to ChatGPT/Gemini/Perplexity

;;; Code:

;;; 1. gptel - Universal LLM Client
;; Winner: Mature, extensible, everywhere-in-Emacs integration
;; Supports: Grok, ChatGPT, Claude, Gemini, Perplexity, Ollama/local
(use-package gptel
  :ensure t
  :demand t
  :custom
  ;; Core
  (gptel-default-mode 'org-mode)              ; Org for structured chats
  (gptel-use-curl t)                          ; Faster curl if available
  (gptel-stream t)                            ; Live responses
  ;; Tool use (agents)
  (gptel-use-tools t)
  (gptel-confirm-tool-calls 'always)          ; Prompt before running tools
  (gptel-include-tool-results 'auto)          ; Auto-include results in context
  ;; Display
  (gptel-prompt-prefix-alist
   '((org-mode . "* ")                        ; Org-style prompts
     (markdown-mode . "### ")
     (text-mode . "### ")))
  :bind
  (("C-c g" . gptel)                          ; Open Grok chat
   ("C-c G" . gptel-menu)                     ; Full menu (models/tools)
   ("C-c g s" . gptel-send)                   ; Send buffer/region
   ("C-c g r" . gptel-rewrite))               ; Rewrite selection
  :init
  ;; Update transients for Emacs 31+
  (when (< emacs-major-version 32)
    (setopt package-install-upgrade-built-in t))
  :config
  ;; Backends — Grok default (easy xAI key setup)
  (setq-default gptel-model 'grok-3           ; Latest Grok (no -latest suffix in 2025)
                gptel-backend (gptel-make-xai "Grok"
                                 :key (lambda () (auth-source-pick-first-password :host "api.x.ai"))
                                 :stream t
                                 :models '(grok-3 grok-3-mini)))  ; Grok-3 + mini

  ;; Quick swaps (comment/uncomment as needed)
  ;; ChatGPT (OpenAI)
  ;; (setq-default gptel-model 'gpt-4o
  ;;               gptel-backend (gptel-make-openai "ChatGPT"
  ;;                                 :key (lambda () (auth-source-pick-first-password :host "api.openai.com"))
  ;;                                 :stream t
  ;;                                 :models '(gpt-4o gpt-4o-mini o1)))

  ;; Gemini (Google)
  ;; (setq-default gptel-model 'gemini-1.5-pro
  ;;               gptel-backend (gptel-make-gemini "Gemini"
  ;;                                 :key (lambda () (auth-source-pick-first-password :host "aip.googleapis.com"))
  ;;                                 :stream t))

  ;; Perplexity (research beast)
  ;; (setq-default gptel-model 'llama-3.1-sonar-large-128k-online
  ;;               gptel-backend (gptel-make-perplexity "Perplexity"
  ;;                                 :key (lambda () (auth-source-pick-first-password :host "api.perplexity.ai"))
  ;;                                 :stream t))

  ;; Local Ollama (free, offline)
  ;; (setq-default gptel-model 'llama3.2:3b
  ;;               gptel-backend (gptel-make-ollama "Ollama"
  ;;                                 :host "localhost:11434"
  ;;                                 :stream t))

  ;; Org enhancements (branching chats)
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)
  (add-hook 'gptel-mode-hook #'auto-save-visited-mode)  ; Auto-save chats

  ;; Pro tip: Org branching — each heading = conversation branch
  (setopt gptel-org-branching-context t))

;;; 2. mcp.el - Agentic Tools (Model Context Protocol)
;; 2025 standard for LLM tools (file/web/GitHub access)
(use-package mcp
  :ensure t
  :after gptel
  :demand t
  :custom
  ;; MCP servers (tools for Grok/etc.)
  (mcp-hub-servers
   `(;; Filesystem (read/write files)
     ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,(expand-file-name "~"))))
     ;; Web search (DuckDuckGo)
     ("duckduckgo" . (:command "uvx" :args ("duckduckgo-mcp-server")))
     ;; Fetch web pages
     ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
     ;; GitHub (needs PAT token)
     ;; ("github" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-github")
     ;;                       :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(auth-source-pick-first-password :host "github.com" :user "token"))))
     ;; Sequential thinking (chain-of-thought)
     ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))))
  :config
  (require 'mcp-hub)
  ;; Start MCP servers after init
  (add-hook 'after-init-hook #'mcp-hub-start-all-servers)
  :bind
  (("C-c m h" . mcp-hub)                      ; Manage servers
   ("C-c m l" . mcp-hub-log-view)             ; View logs
   ("C-c m r" . mcp-hub-restart)))            ; Restart

;;; 3. Optional: ellama (task-specific workflows)
;; Use if you need pre-baked functions (summarize, translate)
;; Pros: Easy workflows. Cons: Less flexible than gptel.
;; (use-package ellama
;;   :ensure t
;;   :custom
;;   (ellama-provider (make-llm-xai :key (lambda () (auth-source-pick-first-password :host "api.x.ai"))))  ; Grok backend
;;   :bind
;;   (("C-c e c" . ellama-chat)       ; Chat
;;    ("C-c e s" . ellama-summarize)  ; Summarize region
;;    ("C-c e t" . ellama-translate)  ; Translate
;;    ("C-c e r" . ellama-code-review)))  ; Code review

;;; 4. Authentication (Secure Keys)
;; Store in ~/.authinfo.gpg (encrypted)
;; Format: machine HOST login USER password TOKEN
;; Examples:
;; machine api.x.ai login apikey password xai-abc123...
;; machine api.anthropic.com login apikey password sk-ant-...
;; machine api.openai.com login apikey password sk-openai-...
;; machine github.com login token password ghp_...

;;; 5. Prerequisites (Install Once)
;; MCP needs Node/Python tools:
;; Ubuntu: sudo apt install nodejs npm
;; macOS: brew install node
;; Python: pip install uv
;; Verify: npx --version && uvx --version

;;; 6. Quick Start
;; Chat: C-c g → type prompt → C-c RET
;; Rewrite: Select region → C-c g r → "Make concise" → RET
;; Tools: C-c G → toggle "Use tools" → Ask "Search for Emacs LLM tips"
;; Switch Models: C-c G → select backend/model

;;; 7. Pro Tips
;; Org Branching: Each heading = conversation branch (set gptel-org-branching-context t)
;; Auto-save Chats: Chats are Org files — C-x C-s to save/reopen
;; Custom Prompts: C-c G → "System message" for per-chat instructions
;; Context: M-x gptel-add-file → add files to context
;; MCP GitHub: Uncomment "github" server + add PAT to authinfo

(provide 'sjy2-gptel)
;;; sjy2-gptel.el ends here