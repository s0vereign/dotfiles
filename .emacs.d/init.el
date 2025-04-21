;; Init el
;; Author: Maximilian Peter Boehme
;; Date: 2025/04/20




;; Startup preformance




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init housekeeping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-default-coding-systems 'utf-8)


;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))



(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar


(setq ring-bell-function 'ignore) ; disable the fucking annoying alarm

(column-number-mode)
(global-display-line-numbers-mode t)


;; Disable line numbers for some modes
    (dolist (mode '(org-mode-hook
                    term-mode-hook
                    vterm-mode-hook
                    shell-mode-hook
                   treemacs-mode-hook
                    eshell-mode-hook))
      (add-hook mode (lambda() (display-line-numbers-mode 0))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; UI configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package command-log-mode
  :commands command-log-mode)

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;(use-package which-key
;;  :defer 0
;  :diminish which-key-mode
;  :config
;  (which-key-mode)
;  (setq which-key-idle-delay 1))


;; ==============================
;; macOS Specific Optimizations
;; ==============================

;; Enable mac-specific key bindings
(when (eq system-type 'darwin)
  ;; Command key as Meta (⌘ as M)
  (setq mac-command-modifier 'meta)
  ;; Option key as Super (⌥ as s)
  (setq mac-option-modifier 'super)
  ;; Function key as Hyper (fn as H)
  (setq mac-function-modifier 'hyper)
  ;; Right Option key as regular Option for special chars
  (setq mac-right-option-modifier nil))

;; Use Mac-style fullscreen
(setq ns-use-native-fullscreen t)

;; Smoother scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Enable clipboard integration
(setq select-enable-clipboard t)


;; ==============================
;; Packages for Basic Improvements
;; ==============================

;; Improved help
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; Keybinding helper
;;(use-package which-key
;;  :init (which-key-mode)
;;  :config
;;  (setq which-key-idle-delay 0.5))

;; Better completion UI
(use-package vertico
  :init
  (vertico-mode))

;; Better minibuffer completion
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Additional minibuffer information
(use-package marginalia
  :init
  (marginalia-mode))

;; Modern theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Improved modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))


;; ==============================
;; macOS Optimized Key Bindings
;; ==============================

;; Common Mac-like key bindings
;;(global-set-key (kbd "M-c") 'kill-ring-save)  ;; ⌘-c to copy
;;(global-set-key (kbd "M-v") 'yank)            ;; ⌘-v to paste
;;(global-set-key (kbd "M-x") 'kill-region)     ;; ⌘-x to cut
(global-set-key (kbd "M-a") 'mark-whole-buffer) ;; ⌘-a to select all
;;(global-set-key (kbd "M-z") 'undo)            ;; ⌘-z to undo
;;(global-set-key (kbd "M-Z") 'redo)            ;; ⌘-Z to redo (if available)
;;(global-set-key (kbd "M-s") 'save-buffer)     ;; ⌘-s to save
;;(global-set-key (kbd "M-f") 'isearch-forward) ;; ⌘-f to search
;;(global-set-key (kbd "M-g") 'isearch-repeat-forward) ;; ⌘-g to find next
;;(global-set-key (kbd "M-o") 'find-file)       ;; ⌘-o to open file
;;(global-set-key (kbd "M-w") 'delete-frame)    ;; ⌘-w to close window

;; Additional convenient bindings
(global-set-key (kbd "M-<return>") 'toggle-frame-fullscreen) ;; ⌘-return for fullscreen
(global-set-key (kbd "M-+") 'text-scale-increase)  ;; ⌘-+ to zoom in
(global-set-key (kbd "M--") 'text-scale-decrease)  ;; ⌘-- to zoom out


;; ==============================
;; Performance Optimizations
;; ==============================

;; Faster startup
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Increase the amount of data which Emacs reads from processes
(setq read-process-output-max (* 1024 1024))

;; Keep customizations in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))





;; =============================
;; Helm Config
;; ============================

(use-package helm
  :diminish
  :init
 ;; (require 'helm-config)
  :bind
  (("M-x" . helm-M-x)                      ;; Replace default M-x with helm version
   ("C-x C-f" . helm-find-files)           ;; Replace find-file
   ("C-x b" . helm-mini)                   ;; Replace switch-to-buffer
   ("M-y" . helm-show-kill-ring)           ;; Show kill ring
   ("C-x r b" . helm-filtered-bookmarks)   ;; Bookmarks
   ("C-c h o" . helm-occur)                ;; Occur
   ("C-c h g" . helm-google-suggest)       ;; Google suggest
   :map helm-map
   ("<tab>" . helm-execute-persistent-action) ;; Make TAB work in helm
   ("C-i" . helm-execute-persistent-action)   ;; Same as TAB
   ("C-z" . helm-select-action))           ;; Choose actions
  :config
  (helm-mode 1)
  (setq helm-split-window-inside-p t       ;; Open helm buffer inside current window
        helm-move-to-line-cycle-in-source t ;; Cycle through sources
        helm-ff-search-library-in-sexp t    ;; Search for library in `require' and `declare-function'
        helm-scroll-amount 8                ;; Scroll 8 lines when using scroll commands
        helm-ff-file-name-history-use-recentf t ;; Use recentf
        helm-autoresize-mode t             ;; Auto-resize the helm window
        helm-autoresize-max-height 25      ;; Max height %
        helm-autoresize-min-height 15      ;; Min height %
        helm-M-x-fuzzy-match t            ;; Fuzzy matching for M-x
        helm-buffers-fuzzy-matching t     ;; Fuzzy matching for buffers
        helm-recentf-fuzzy-match t        ;; Fuzzy matching for recent files
        helm-semantic-fuzzy-match t       ;; Fuzzy matching for semantic
        helm-imenu-fuzzy-match t))        ;; Fuzzy matching for imenu



;; Helm Projectile integration
(use-package helm-projectile
  :after (helm projectile)
  :init
  (helm-projectile-on)
  :bind
  (("C-c p p" . helm-projectile-switch-project)
   ("C-c p f" . helm-projectile-find-file)
   ("C-c p g" . helm-projectile-grep)))

;; Helm Swoop - improved search
(use-package helm-swoop
  :after helm
  :bind
  (("C-s" . helm-swoop)
   ("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all))
  :config
  (setq helm-swoop-split-with-multiple-windows t
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-speed-or-color t
        helm-swoop-move-to-line-cycle t
        helm-swoop-use-line-number-face t
        helm-swoop-use-fuzzy-match t))

;; Add helm-ag for fast searching
(use-package helm-ag
  :after helm
  :bind
  (("C-c a" . helm-ag)
   ("C-c A" . helm-ag-project-root))
  :config
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case"
        helm-ag-command-option "--all-text"
        helm-ag-insert-at-point 'symbol))

;; Descbinds - describe key bindings with helm
(use-package helm-descbinds
  :after helm
  :config
  (helm-descbinds-mode))


; =========================
; C++ Mode & IDEs
; ========================

;; Modern C++ mode
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;; Project management with Projectile
(use-package projectile
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Completion framework
(use-package company
 :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t))

;; Helm extensions
(use-package helm-projectile
  :after (helm projectile)
  :config (helm-projectile-on))


;;;(use-package helm-swoop
;  :after helm
;  :bind (("C-s" . helm-swoop)
;         ("M-i" . helm-swoop)
;
;         ("M-I" . helm-swoop-back-to-last-point)))

(require 'helm)
(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; If you prefer fuzzy matching
;; (setq helm-swoop-use-fuzzy-match t)

;;; config-helm-swoop ends here

;; Fix: Modified helm-lsp binding to avoid conflict with lsp-mode prefix
(use-package helm-lsp
  :after (helm lsp-mode)
  :commands helm-lsp-workspace-symbol
  :bind (:map lsp-mode-map
              ("C-c h s" . helm-lsp-workspace-symbol)))


(use-package helm-xref
  :after helm
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))


;; Syntax checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-display-errors-delay 0.3))

;; On-the-fly syntax checking for C/C++
(use-package flycheck-clang-analyzer
  :after flycheck
  :config (flycheck-clang-analyzer-setup))




;; LSP support
(use-package lsp-mode
  :hook ((c++-mode . lsp)
         (c-mode . lsp))
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-headerline-breadcrumb-enable t)
  )


(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t))


;; Enhanced completion with LSP
;;(use-package company-lsp
;;  :after (company lsp-mode)
;;  :config
;;  (push 'company-lsp company-backends))

;; C++ headers completion
(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))


;; CMake support
(use-package cmake-mode)

(use-package cmake-font-lock
  :after cmake-mode
  :hook (cmake-mode . cmake-font-lock-activate))




;; ============================================================================
;; Enhanced GDB setup with Graphical Annex Protocol (GAP) support
;; ============================================================================

;; Basic GDB configuration
(setq gdb-many-windows t
      gdb-show-main t
      gdb-use-separate-io-buffer t
      gdb-non-stop-setting t
      gdb-show-changed-values t
      gdb-delete-out-of-scope nil
      gdb-use-colon-colon-notation t
      gdb-restore-window-configuration-after-quit t)

;; Enable GDB-MI interface which is required for GDB Graphical Annex Protocol
(setq gdb-mi-decode-strings t)



;; GDB-UI setup - create custom layout for debugging
(defadvice gdb-setup-windows (after setup-gdb-windows activate)
  "Custom GDB window layout."
  (gdb-setup-my-windows))

(defun gdb-setup-my-windows ()
  "Set up my preferred window layout for debugging."
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window-horizontally
               (floor (* 0.5 (window-width)))))
        (win2 (split-window-vertically
               (floor (* 0.65 (window-height))))))
    
    ;; Setup source, locals, and stack windows
    (select-window win1)
    (set-window-buffer
     win1
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         (list-buffers-noselect))))
    
    ;; Setup variables window
    (select-window win2)
    (gdb-set-window-buffer (gdb-locals-buffer-name))
    
    ;; Setup stack window
    (let ((win3 (split-window-horizontally)))
      (select-window win3)
      (gdb-set-window-buffer (gdb-stack-buffer-name)))
    
    ;; Setup breakpoints window
    (select-window win0)
    (let ((win4 (split-window-vertically
                 (floor (* 0.65 (window-height))))))
      (select-window win4)
      (gdb-set-window-buffer (gdb-breakpoints-buffer-name)))
    
    ;; Return to the command buffer
    (select-window win0)))



;; Add GDB-MI extensions for visualization
(defun gdb-setup-gap-mode ()
  "Setup visualization options for GDB using GAP mode."
  (interactive)
  ;; Tell GDB to use the Graphical Annex Protocol
  (gud-gdb-set-options (concat gud-gdb-options
                              (when gud-gdb-options " ")
                              "-i=mi"))
  
  ;; Enable TUI support if GDB was compiled with it
  (gdb-input "-interpreter-exec console \"tui enable\"")
  (gdb-input "-interpreter-exec console \"layout split\"")
  (gdb-input "-interpreter-exec console \"set print pretty on\"")
  (gdb-input "-interpreter-exec console \"set print object on\"")
  (gdb-input "-interpreter-exec console \"set print array on\"")
  (gdb-input "-interpreter-exec console \"set print array-indexes on\"")
  (gdb-input "-interpreter-exec console \"set print demangle on\"")
  (gdb-input "-interpreter-exec console \"set demangle-style gnu-v3\"")
  (gdb-input "-interpreter-exec console \"set pagination off\""))

;; Add advice to automatically run our setup when GDB starts
(defadvice gdb (after activate-gap-mode activate)
  "Activate GAP mode after GDB starts."
  (gdb-setup-gap-mode))

;; Define a more integrated debugging function
(defun gdb-debug-buffer ()
  "Run GDB on the current buffer's file."
  (interactive)
  (when (buffer-file-name)
    (let ((file (file-name-nondirectory (buffer-file-name)))
          (dir default-directory))
      ;; Check if the file is a C++ file
      (when (string-match "\\.\\(cpp\\|cc\\|cxx\\|C\\|c\\|h\\|hpp\\)$" file)
        ;; Ensure the file is compiled before debugging
        (let ((exec-name (file-name-sans-extension file)))
          ;; For simplicity, use a standard compilation command
          (compile (concat "g++ -g -std=c++17 -o " exec-name " " file))
          ;; Run GDB on the executable when compilation finishes
          (add-hook 'compilation-finish-functions
                    (lambda (buffer status)
                      ;; Remove ourselves from the hook
                      (remove-hook 'compilation-finish-functions
                                  (lambda (buffer status) nil))
                      ;; Only run GDB if compilation succeeded
                      (when (string-match "finished" status)
                        (gdb (concat "gdb -i=mi --fullname -q " exec-name))))
                    nil t))))))

;; Custom function to insert a breakpoint
(defun gdb-insert-breakpoint ()
  "Insert a breakpoint at the current line."
  (interactive)
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer))
      (progn
        (gud-basic-call
         (concat "break " (file-name-nondirectory (buffer-file-name))
                 ":" (number-to-string (line-number-at-pos))))
        (message "Breakpoint set at line %d" (line-number-at-pos)))
    (message "GDB is not running")))

;; Enhanced GDB keybindings
(defun my-c++-debug-keys ()
  "Set key bindings for debugging C++ code."
  (local-set-key (kbd "C-c d d") 'gdb-debug-buffer)
  (local-set-key (kbd "C-c d g") 'gdb)
  (local-set-key (kbd "C-c d b") 'gdb-insert-breakpoint)
  (local-set-key (kbd "C-c d r") 'gud-run)
  (local-set-key (kbd "C-c d n") 'gud-next)
  (local-set-key (kbd "C-c d s") 'gud-step)
  (local-set-key (kbd "C-c d f") 'gud-finish)
  (local-set-key (kbd "C-c d c") 'gud-cont)
  (local-set-key (kbd "C-c d p") 'gud-print)
  (local-set-key (kbd "C-c d q") 'gud-quit))

(add-hook 'c-mode-common-hook 'my-c++-debug-keys)


;; Helper tool for pretty-printing complex C++ objects
(use-package gdb-mi
  :defer t
  :config
  (setq gdb-many-windows t
        gdb-show-main t))

;; Git integration
(use-package magit
  :bind ("C-x g" . magit-status))

;; Tree-based file explorer
(use-package treemacs
  :bind
  (:map global-map
        ("C-c t" . treemacs))
  :config
  (setq treemacs-width 30))

(use-package treemacs-projectile
  :after (treemacs projectile))

;; Helm integration with Treemacs
(use-package treemacs-magit
  :after (treemacs magit))



;; Code folding
(use-package origami
  :hook (prog-mode . origami-mode)
  :bind
  (:map origami-mode-map
        ("C-c o t" . origami-toggle-node)
        ("C-c o a" . origami-toggle-all-nodes)))


;; Snippets
(use-package yasnippet
  :hook ((c-mode c++-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets)


;; C/C++ tags for Helm
(use-package helm-gtags
  :after helm
  :hook ((c-mode c++-mode) . helm-gtags-mode)
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t)
  :bind (:map helm-gtags-mode-map
              ("M-." . helm-gtags-find-tag)
              ("M-," . helm-gtags-pop-stack)
              ("C-c g t" . helm-gtags-find-tag)
              ("C-c g r" . helm-gtags-find-rtag)
              ("C-c g s" . helm-gtags-find-symbol)
              ("C-c g a" . helm-gtags-tags-in-this-function)))

;; CMake project integration
(use-package cmake-ide
  :after projectile
  :config
  (cmake-ide-setup))



;; Nicer visualization of differences
(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

;; Modern theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Smart mode line
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup))

;; Better help with Helm
(use-package helm-describe-modes
  :after helm
  :bind ([remap describe-mode] . helm-describe-modes))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))




;; C/C++ specific key bindings
(defun my-c++-mode-hook ()
  "Custom C++ mode configurations."
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-c C-r") 'recompile))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)

;; Custom compile command function
(defun set-compile-command-for-cpp ()
  "Set the compile command based on project type."
  (let ((makefile-path (locate-domainname "Makefile" default-directory))
        (cmakefile-path (locate-domainname "CMakeLists.txt" default-directory)))
    (cond (makefile-path
           (setq compile-command "make -j8"))
          (cmakefile-path
           (setq compile-command "cmake --build build"))
          (t
           (setq compile-command 
                 (concat "g++ -std=c++17 -Wall -Wextra -g "
                         (file-name-nondirectory buffer-file-name)
                         " -o "
                         (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))))

(add-hook 'c++-mode-hook 'set-compile-command-for-cpp)

;; Additional C++ configuration
(add-hook 'c++-mode-hook (lambda ()
                           (setq flycheck-gcc-language-standard "c++17")
                           (setq flycheck-clang-language-standard "c++17")
                           (setq company-clang-arguments '("-std=c++17"))))



;; Add llvm and gnu to company c-headers directories
(with-eval-after-load 'company-c-headers
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/")
  (add-to-list 'company-c-headers-path-system "/usr/local/include/"))

;; Helm smart search
(use-package helm-ag
  :after helm
  :bind ("C-c h g" . helm-do-ag-project-root))

;; Show function arguments in echo area
(use-package eldoc
  :hook ((c-mode c++-mode emacs-lisp-mode) . eldoc-mode))


;;(require 'bash-completion)
;;(bash-completion-setup)

(add-hook 'eshell-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      'bash-completion-capf-nonexclusive nil t)))


;; Move global mode string to the tab-bar and hide tab close buttons
(setq tab-bar-close-button-show nil
      tab-bar-separator " "
      tab-bar-format '(tab-bar-format-menu-bar
                       tab-bar-format-tabs-groups
                       tab-bar-separator
                       tab-bar-format-align-right
                       tab-bar-format-global))

;; Turn on the tab-bar
(tab-bar-mode 1)


;; Customize time display
(setq display-time-load-average nil
      display-time-format "%l:%M %p %b %d W%U"
      display-time-world-time-format "%a, %d %b %I:%M %p %Z"
      display-time-world-list
      '(("Etc/UTC" "UTC")
        ("Europe/Athens" "Athens")
        ("America/Los_Angeles" "Seattle")
        ("America/Denver" "Denver")
        ("America/New_York" "New York")
        ("Pacific/Auckland" "Auckland")
        ("Asia/Shanghai" "Shanghai")
        ("Asia/Kolkata" "Hyderabad")))




(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))


(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))


(use-package vterm
  :commands vterm
  :config
  ;;(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))



(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))


; ================
; Pythoon
; ================

(use-package pyvenv
  :config
  (pyvenv-mode 1))


(provide 'init)
;;; init.el ends here








