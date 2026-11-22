;;; init.el --- Everyday IDE config (C/C++, CMake, Python)  -*- lexical-binding: t; -*-
;; Author: Maximilian Peter Boehme
;;
;; Stack:  vertico + orderless + marginalia + consult   (minibuffer completion)
;;         corfu + cape                                  (in-buffer autocomplete)
;;         eglot (built in) + clangd / pylsp / pyright   (language servers)
;;         flymake (built in)                            (diagnostics)
;;         projectile, magit, treemacs, yasnippet, vterm
;;
;; Language servers are NOT Emacs packages -- install them once:
;;   brew install llvm                 ; clangd for C/C++   (already installed)
;;   pip install python-lsp-server     ; or: npm i -g pyright
;;   brew install ripgrep              ; fast project search (consult-ripgrep)

;;; Code:

;; ---------------------------------------------------------------------------
;; Startup performance
;; ---------------------------------------------------------------------------
(setq gc-cons-threshold (* 100 1000 1000))          ; relaxed during init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1000 1000))
            (message "Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(setq read-process-output-max (* 1024 1024))        ; LSP sends big messages

;; Native compilation (Emacs 30+/31 does it in the background on first use).
;; It needs a working C toolchain.  If Emacs.app was started with Finder's
;; "Open using Rosetta" ticked, every tool it spawns (xcrun, cc, make) runs as
;; x86_64 and fails against the arm64-only Command Line Tools -- which floods
;; you with "native-ice" errors.  Detect that, switch JIT compilation off and
;; say what to do, instead of spamming errors on every start.
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p)
           (eq system-type 'darwin)
           (executable-find "xcrun")
           (not (zerop (condition-case nil
                           (call-process "xcrun" nil nil nil "--find" "cc")
                         (error 1)))))
  (setq native-comp-jit-compilation nil
        native-comp-async-report-warnings-errors 'silent)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (display-warning
               'init
               (concat "C toolchain unusable from Emacs, native compilation and "
                       "vterm builds are disabled.\n"
                       "Most likely Emacs.app is set to \"Open using Rosetta\": quit Emacs, "
                       "Finder > Applications > Emacs.app > Cmd-I > untick it, relaunch.")
               :warning))))
(setq native-comp-async-report-warnings-errors 'silent) ; warnings from 3rd-party packages are noise

;; ---------------------------------------------------------------------------
;; Unicode / encoding  (part of the "broken characters" fix)
;; ---------------------------------------------------------------------------
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)

;; ---------------------------------------------------------------------------
;; Packages
;; ---------------------------------------------------------------------------
;; This machine sits behind the LLNL TLS-inspection proxy, which re-signs HTTPS
;; certificates with the LLNL CA.  curl uses the macOS keychain (trusts it),
;; Emacs does not -> "Could not create connection to melpa.org:443".
;; ~/.emacs.d/llnl-ca.pem holds the LLNL CA certificates exported from the
;; System keychain; verification stays ON, we just trust that CA too.
(require 'gnutls)
(let ((ca (expand-file-name "llnl-ca.pem" user-emacs-directory)))
  (when (file-exists-p ca)
    (add-to-list 'gnutls-trustfiles ca)))

(require 'package)
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents
  (ignore-errors (package-refresh-contents)))        ; don't die when offline

(require 'use-package)
(setq use-package-always-ensure t)                  ; auto-install missing packages

;; Keep Custom's output out of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file nil t))

;; GUI Emacs on macOS does not inherit your shell PATH, so it can't find
;; clangd, pylsp, rg, cmake ...  This copies PATH from your login shell.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
  (exec-path-from-shell-initialize))

;; Tools installed with `uv tool install' / pipx (pylsp, ...) live here.
(let ((local-bin (expand-file-name "~/.local/bin"))
      (juliaup-bin (expand-file-name "~/.juliaup/bin")))
  (dolist (dir (list local-bin juliaup-bin))
    (add-to-list 'exec-path dir)
    (setenv "PATH" (concat dir path-separator (getenv "PATH")))))

;; ---------------------------------------------------------------------------
;; Basic UI
;; ---------------------------------------------------------------------------
(setq inhibit-startup-message t
      ring-bell-function 'ignore
      use-short-answers t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

;; Start with a maximized window that fills the screen (menu bar and Dock stay
;; visible), like VS Code.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(dolist (hook '(org-mode-hook term-mode-hook vterm-mode-hook shell-mode-hook
                eshell-mode-hook treemacs-mode-hook help-mode-hook
                compilation-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

;; Font: first one that exists.
(when (display-graphic-p)
  (let ((font (seq-find #'find-font
                        (mapcar (lambda (n) (font-spec :name n))
                                '("JetBrains Mono" "SF Mono" "Menlo")))))
    (when font
      (set-face-attribute 'default nil :font font :height 140)))
  ;; Colour emoji fallback (so e.g. in commit messages they don't show as boxes)
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t doom-themes-enable-italic t
        ;; doom-one's default comment grey (#5B6268 on #282C34) is only 2.3:1 contrast.
        ;; The theme's own "brighter comments" switch uses cyan instead of grey,
        ;; without a tinted background behind comment lines.
        doom-one-brighter-comments t
        doom-one-comment-bg nil)
  (load-theme 'doom-one t)
  ;; A little brighter again: 6.5:1 contrast, still clearly not code-coloured
  ;; (strings are green, numbers orange, keywords blue).
  (custom-theme-set-faces
   'user
   '(font-lock-comment-face           ((t (:foreground "#78bbd1" :slant italic))))
   '(font-lock-comment-delimiter-face ((t (:foreground "#78bbd1" :slant italic)))))
  (doom-themes-org-config))

;; Icons.  doom-modeline, treemacs, corfu and marginalia use `nerd-icons'.
;; It needs the "Symbols Nerd Font Mono" font; without it every icon is a
;; broken box.  Install it once with:  M-x nerd-icons-install-fonts
(use-package nerd-icons
  :config
  (when (and (display-graphic-p)
             (not (find-font (font-spec :name nerd-icons-font-family))))
    (add-hook 'emacs-startup-hook
              (lambda ()
                (message "Nerd Font missing -- run M-x nerd-icons-install-fonts, then restart Emacs")))))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-minor-modes nil)
  ;; Only draw icons when the font is really there -> never broken characters.
  (doom-modeline-icon (and (display-graphic-p)
                           (find-font (font-spec :name "Symbols Nerd Font Mono"))
                           t))
  (doom-modeline-major-mode-icon t)
  (doom-modeline-unicode-fallback t))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package which-key
  :init (which-key-mode 1)
  :custom (which-key-idle-delay 0.6))

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key]      . helpful-key)))

(use-package command-log-mode :commands command-log-mode)

;; Time / world clock (M-x world-clock)
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

;; ---------------------------------------------------------------------------
;; macOS
;; ---------------------------------------------------------------------------
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta          ; Cmd    -> Meta
        mac-option-modifier 'super          ; Option -> Super
        mac-function-modifier 'hyper
        mac-right-option-modifier nil)      ; right Option types special chars
  (setq ns-use-native-fullscreen t
        mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        select-enable-clipboard t
        insert-directory-program "gls"      ; GNU ls if present (brew coreutils)
        dired-use-ls-dired t)
  (unless (executable-find "gls")           ; fall back to BSD ls without warnings
    (setq insert-directory-program "ls" dired-use-ls-dired nil)))

(global-set-key (kbd "M-a") #'mark-whole-buffer)
(global-set-key (kbd "M-<return>") #'toggle-frame-fullscreen)
(global-set-key (kbd "M-+") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)

;; ---------------------------------------------------------------------------
;; Sane editing defaults
;; ---------------------------------------------------------------------------
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100)

(let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
      (auto-save-dir (expand-file-name "auto-save/" user-emacs-directory)))
  (make-directory backup-dir t)
  (make-directory auto-save-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        backup-by-copying t
        create-lockfiles nil))

(global-auto-revert-mode 1)             ; reload files changed on disk (git checkout...)
(save-place-mode 1)                     ; remember cursor position
(savehist-mode 1)                       ; remember minibuffer history
(recentf-mode 1)
(setq recentf-max-saved-items 200)
(electric-pair-mode 1)                  ; auto-close brackets and quotes
(show-paren-mode 1)
(delete-selection-mode 1)
(global-hl-line-mode 1)
(global-so-long-mode 1)                 ; don't freeze on minified / huge lines
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t
      sentence-end-double-space nil)

;; Code folding (built in; replaces the unmaintained origami)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map (kbd "C-c o t") #'hs-toggle-hiding)
  (define-key hs-minor-mode-map (kbd "C-c o a") #'hs-show-all)
  (define-key hs-minor-mode-map (kbd "C-c o h") #'hs-hide-all))

;; ---------------------------------------------------------------------------
;; Window management  (tiling-window-manager feel, keyboard only)
;; Remember: Cmd = Meta (M-), Option = Super (s-) on this setup.
;; ---------------------------------------------------------------------------
;; Jump to any window by letter: Cmd-o, then the letter shown in the window.
;; In the prompt: x = delete window, m = swap with current, s = swap buffers,
;; v/b = split, o = other frame, ? = show all options.
(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))   ; home-row labels
  (aw-scope 'frame)
  (aw-dispatch-always t))                   ; always show the prompt, even with 2 windows

;; Option + arrows moves between windows; Option + Shift + arrows swaps buffers.
(windmove-default-keybindings 'super)
(windmove-swap-states-default-keybindings '(super shift))

;; Undo / redo window layouts: C-c <left> / C-c <right>
(winner-mode 1)

;; After C-x { } ^ etc. you can just keep tapping the last key (e.g. C-x } } } }).
(repeat-mode 1)

;; Quick split/close on Cmd-digit (like workspaces in i3/sway).
;; Plain Emacs splits CLONE the current buffer into the new tile ("duplicate
;; tiles").  These versions behave like i3: the new tile gets a different buffer
;; straight away and asks what you want to put there (C-g keeps the other buffer).
(defun my/split-and-pick (split-fn)
  "Call SPLIT-FN, select the new window, show another buffer, then offer a choice."
  (select-window (funcall split-fn))
  (switch-to-buffer (other-buffer (window-buffer (previous-window)) t))
  (condition-case nil
      (call-interactively #'consult-buffer)     ; pick buffer / recent file / bookmark
    (quit nil)))

(defun my/split-below-pick () (interactive) (my/split-and-pick #'split-window-below))
(defun my/split-right-pick () (interactive) (my/split-and-pick #'split-window-right))

(global-set-key (kbd "M-0") #'delete-window)
(global-set-key (kbd "M-1") #'delete-other-windows)
(global-set-key (kbd "M-2") #'my/split-below-pick)
(global-set-key (kbd "M-3") #'my/split-right-pick)
(global-set-key (kbd "M-9") #'balance-windows)

;; Where popups go.  Without rules, help / compilation / REPL buffers split
;; whichever window happens to be big, rearranging your layout.  Now they all
;; land in ONE slot at the bottom (like an i3 scratchpad) and re-use it.
;; C-x 1 / M-1 leaves that slot alone; close it with M-0 while it is selected.
(setq display-buffer-alist
      '(((or (derived-mode . comint-mode)       ; Python REPL, M-x shell
             (derived-mode . term-mode)         ; Julia REPL (ansi-term backend)
             (derived-mode . compilation-mode)  ; F5 build output, grep
             (derived-mode . help-mode)
             (derived-mode . helpful-mode))
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom) (slot . 0) (window-height . 0.3)
         (window-parameters . ((no-delete-other-windows . t)))))
      ;; Never open the same buffer in a second tile when one already shows it.
      display-buffer-base-action '((display-buffer-reuse-window)
                                   (reusable-frames . nil))
      switch-to-buffer-obey-display-actions nil)

;; Tabs = workspaces.  C-x t 2 new, C-<tab> / C-S-<tab> next/prev, C-x t 0 close,
;; C-x t r rename, C-x t RET pick by name.
(setq tab-bar-show 1                        ; only show the bar when 2+ tabs exist
      tab-bar-close-button-show nil
      tab-bar-new-tab-choice "*scratch*")
(tab-bar-mode 1)

;; Open the cheat sheet any time with C-c ?
(global-set-key (kbd "C-c ?")
                (lambda () (interactive)
                  (find-file-read-only
                   (expand-file-name "cheatsheet.md" user-emacs-directory))))

;; ---------------------------------------------------------------------------
;; i3wm-style keys.   mod = Option (Super, "s-") on this setup.
;;   focus       s-h/j/k/l   (also Option+arrows, see windmove above)
;;   move tile   s-H/J/K/L   swap this buffer with the neighbour in that direction
;;   fullscreen  s-f         zoom this tile; press again to restore the layout
;;   resize      s-r         then h/j/k/l repeatedly; any other key leaves resize mode
;;   workspace   s-1 ... s-9 switch (created on demand, like i3), shown in the tab bar
;;   split       M-2 / M-3   (Cmd) new tile below / right, then pick what goes in it
;;   close tile  s-d         close this tile (the buffer stays open)
;;   kill tile   s-D         close this tile AND kill its buffer
;; ---------------------------------------------------------------------------
;; macOS binds s-q (quit Emacs) and s-w (close frame) by default; with Option as
;; Super that is one slip away from losing a session.  Disable both.
(dolist (key '("s-q" "s-w"))
  (global-unset-key (kbd key)))

;; Focus and move (these replace macOS defaults like s-h = hide, s-k = kill buffer).
(global-set-key (kbd "s-h") #'windmove-left)
(global-set-key (kbd "s-j") #'windmove-down)
(global-set-key (kbd "s-k") #'windmove-up)
(global-set-key (kbd "s-l") #'windmove-right)
(global-set-key (kbd "s-H") #'windmove-swap-states-left)
(global-set-key (kbd "s-J") #'windmove-swap-states-down)
(global-set-key (kbd "s-K") #'windmove-swap-states-up)
(global-set-key (kbd "s-L") #'windmove-swap-states-right)

;; Fullscreen toggle for one tile (i3: mod+f).
(defvar my/zoom-saved-config nil "Window layout saved by `my/window-zoom-toggle'.")
(defun my/window-zoom-toggle ()
  "Maximize the current tile; call again to restore the previous layout."
  (interactive)
  (if my/zoom-saved-config
      (progn (set-window-configuration my/zoom-saved-config)
             (setq my/zoom-saved-config nil)
             (message "layout restored"))
    (if (one-window-p t)
        (message "only one tile")
      (setq my/zoom-saved-config (current-window-configuration))
      (delete-other-windows))))
(global-set-key (kbd "s-f") #'my/window-zoom-toggle)

;; Resize mode (i3: mod+r, then h/j/k/l).
(defvar my/resize-map
  (let ((m (make-sparse-keymap)))
    (define-key m "h" (lambda () (interactive) (shrink-window-horizontally 4)))
    (define-key m "l" (lambda () (interactive) (enlarge-window-horizontally 4)))
    (define-key m "j" (lambda () (interactive) (enlarge-window 2)))
    (define-key m "k" (lambda () (interactive) (shrink-window 2)))
    m)
  "Keys active while in resize mode.")
(defun my/resize-mode ()
  "Resize the current tile with h/j/k/l until another key is pressed."
  (interactive)
  (message "resize: h narrower, l wider, j taller, k shorter (any other key exits)")
  (set-transient-map my/resize-map t))
(global-set-key (kbd "s-r") #'my/resize-mode)

;; Closing tiles.
(defun my/close-tile () (interactive) (unless (one-window-p t) (delete-window)))
(defun my/kill-tile ()
  "Close this tile and kill its buffer."
  (interactive)
  (kill-current-buffer)
  (my/close-tile))
(global-set-key (kbd "s-d") #'my/close-tile)
(global-set-key (kbd "s-D") #'my/kill-tile)

;; ---------------------------------------------------------------------------
;; Edwina: dwm-style AUTOMATIC tiling (master + stack).  Off by default.
;;   s-e   toggle it on/off.  While on, it re-tiles every window itself and its
;;         keys (below) replace the i3 ones that share a letter.  Off = i3 style.
;;   s-j / s-k          focus next / previous tile (cycles through the stack)
;;   s-J / s-K          swap this tile with the next / previous one
;;   s-h / s-l          shrink / grow the master area
;;   s-i / s-d          more / fewer tiles in the master area
;;   s-RET              zoom: swap this tile into (or out of) the master slot
;;   s-S-RET            new tile (clone), s-C close this tile
;;   s-r                re-arrange everything,  s-t  switch tall <-> stacked layout
;; ---------------------------------------------------------------------------
(use-package edwina
  :commands edwina-mode
  :config
  (edwina-setup-dwm-keys 'super)
  ;; Edwina rebuilds every window it can see, including the bottom popup slot,
  ;; which then shows up twice.  Make it blind to side windows, and do not re-tile
  ;; when the window that was just displayed is a side window (popup slot).
  (advice-add 'edwina--window-list :filter-return
              (lambda (windows)
                (seq-remove (lambda (w) (window-parameter w 'window-side)) windows)))
  (advice-add 'edwina--display-buffer :override
              (lambda (display-buffer &rest args)
                (let ((win (apply display-buffer args)))
                  (if (and (windowp win) (window-parameter win 'window-side))
                      win
                    (edwina--respective-window win (edwina-arrange))))))
  ;; New tile like in i3: split, show another buffer, ask what should go there.
  (defun my/edwina-new-tile ()
    "Clone the tile, show a different buffer in it, and offer a buffer choice."
    (interactive)
    (let ((origin (selected-window)))
      (edwina-clone-window)
      (unless (eq (selected-window) origin)
        (switch-to-buffer (other-buffer (window-buffer origin) t)))
      (condition-case nil
          (call-interactively #'consult-buffer)
        (quit nil))))
  (define-key edwina-mode-map (kbd "<s-S-return>") #'my/edwina-new-tile)
  (defun my/edwina-cycle-layout ()
    "Switch Edwina between the tall (master + stack) and fully stacked layout."
    (interactive)
    (setq edwina-layout (if (eq edwina-layout 'edwina-tall-layout)
                            'edwina-stack-layout
                          'edwina-tall-layout))
    (edwina-arrange)
    (message "Edwina layout: %s" (edwina-layout-name edwina-layout)))
  (define-key edwina-mode-map (kbd "s-t") #'my/edwina-cycle-layout))

(defun my/edwina-toggle ()
  "Turn dwm-style automatic tiling on or off."
  (interactive)
  (require 'edwina)
  (edwina-mode 'toggle)
  (message "Edwina (auto tiling) %s" (if edwina-mode "ON: dwm keys active" "OFF: i3 keys active")))
(global-set-key (kbd "s-e") #'my/edwina-toggle)

;; Workspaces = tab-bar tabs, numbered, created on demand (i3: mod+1..9).
(setq tab-bar-tab-hints t)                 ; show 1 2 3 in the tab names
(defun my/workspace (n)
  "Switch to workspace N, creating tabs up to N if they do not exist yet."
  (let ((count (length (funcall tab-bar-tabs-function))))
    (while (< count n)
      (tab-bar-new-tab-to (1+ count))
      (setq count (1+ count)))
    (tab-bar-select-tab n)))
(dotimes (i 9)
  (let ((n (1+ i)))
    (global-set-key (kbd (format "s-%d" n))
                    (lambda () (interactive) (my/workspace n)))))

;; ---------------------------------------------------------------------------
;; Minibuffer completion: vertico + orderless + marginalia + consult
;; (replaces Helm; Helm and Vertico were both active and fighting each other)
;; ---------------------------------------------------------------------------
(use-package vertico
  :init (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-count 14)
  (vertico-preselect 'first)       ; always highlight the first entry (like helm); C-p selects the prompt itself
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :bind (:map vertico-map
              ;; Helm-style: C-n / C-p / up / down move the highlight, then
              ("<right>" . my/vertico-right)            ; folder -> go into it
              ("<left>"  . my/vertico-left)             ; go to the parent folder
              ("TAB"     . vertico-insert)              ; copy suggestion into the prompt
              ("<tab>"   . vertico-insert)
              ("RET"     . vertico-directory-enter)     ; file -> open, folder -> step into it
              ("DEL"     . vertico-directory-delete-char); backspace removes a whole "folder/" at once
              ("M-DEL"   . vertico-directory-delete-word)
              ("M-j"     . vertico-quick-insert)))      ; Cmd-j: label each suggestion with a letter
(with-eval-after-load 'vertico
  (require 'vertico-directory)
  (require 'vertico-quick)
  (require 'vertico-mouse)
  (vertico-mouse-mode 1)                                ; click a suggestion to pick it
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (setq vertico-quick1 "asdfghjkl" vertico-quick2 "asdfghjkl")

  (defun my/vertico-right ()
    "In file prompts, enter the highlighted folder.  Otherwise move the cursor right.
The special entries \"./\" (ignored) and \"../\" (go up) are handled sensibly."
    (interactive)
    (if (and minibuffer-completing-file-name (>= vertico--index 0))
        (let ((cand (vertico--candidate)))
          (cond ((string-suffix-p "../" cand) (my/vertico-left))
                ((string-suffix-p "./" cand) nil)
                ((string-suffix-p "/" cand) (vertico-insert))
                ((< (point) (point-max)) (forward-char 1))))
      (when (< (point) (point-max)) (forward-char 1))))

  (defun my/vertico-left ()
    "In file prompts with the cursor at the end, go to the parent folder.
Any half-typed name after the last slash is dropped first.  Otherwise move left."
    (interactive)
    (if (and minibuffer-completing-file-name (eobp))
        (progn
          (let ((beg (save-excursion
                       (skip-chars-backward "^/" (minibuffer-prompt-end))
                       (point))))
            (delete-region beg (point)))      ; drop the partial name, keep "dir/"
          (vertico-directory-up))
      (when (> (point) (minibuffer-prompt-end)) (backward-char 1)))))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package nerd-icons-completion
  :after marginalia
  :config (nerd-icons-completion-mode 1)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package consult
  :bind (("C-s"     . consult-line)               ; was helm-swoop
         ("M-i"     . consult-line)
         ("C-x b"   . consult-buffer)             ; was helm-mini
         ("C-x 4 b" . consult-buffer-other-window)
         ("M-y"     . consult-yank-pop)           ; was helm-show-kill-ring
         ("C-x r b" . consult-bookmark)
         ("M-g g"   . consult-goto-line)
         ("M-g i"   . consult-imenu)              ; jump to function/class in file
         ("M-g I"   . consult-imenu-multi)
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flymake)            ; list diagnostics
         ("M-s r"   . consult-ripgrep)            ; project-wide search
         ("M-s g"   . consult-grep)
         ("M-s f"   . consult-fd)
         ("C-c h o" . consult-line)
         ("C-c a"   . consult-ripgrep))
  :custom
  (consult-narrow-key "<")
  (xref-show-xref-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

;; ---------------------------------------------------------------------------
;; In-buffer autocomplete: corfu + cape
;; Popup appears automatically while typing; TAB accepts, C-n/C-p or M-n/M-p move.
;; ---------------------------------------------------------------------------
(use-package corfu
  :custom
  (corfu-auto t)                  ; pop up by itself
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)           ; after 2 typed characters
  (corfu-cycle t)
  (corfu-preselect 'first)        ; first candidate is highlighted
  (corfu-quit-no-match 'separator)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :bind (:map corfu-map
              ("TAB"     . corfu-complete)   ; accept (or expand common prefix)
              ("<tab>"   . corfu-complete)
              ("RET"     . nil)              ; Enter keeps inserting a newline
              ("<return>". nil)
              ("S-TAB"   . corfu-previous)
              ("<backtab>" . corfu-previous))
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)        ; show docs next to the popup
  (corfu-history-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(setq tab-always-indent 'complete        ; TAB indents, or completes if already indented
      completion-cycle-threshold 3
      read-extended-command-predicate #'command-completion-default-include-p)

;; Extra completion sources that work in every buffer, even without an LSP:
;; words from open buffers, file names, language keywords.
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword))

;; Snippets (eglot expands function-argument templates through yasnippet)
(use-package yasnippet
  :init (yas-global-mode 1))
(use-package yasnippet-snippets :after yasnippet)

;; ---------------------------------------------------------------------------
;; Language servers via Eglot  (starts automatically when you open a file)
;; ---------------------------------------------------------------------------
(defvar my/eglot-server-executables
  '((c-mode          "clangd")
    (c++-mode        "clangd")
    (c-ts-mode       "clangd")
    (c++-ts-mode     "clangd")
    (python-mode     "pylsp" "pyright-langserver" "basedpyright-langserver" "jedi-language-server")
    (python-ts-mode  "pylsp" "pyright-langserver" "basedpyright-langserver" "jedi-language-server")
    (cmake-mode      "cmake-language-server")
    (sh-mode         "bash-language-server")
    (rust-mode       "rust-analyzer")
    (go-mode         "gopls")
    (julia-mode      "julia" "julialauncher"))
  "Major mode -> executables, any one of which lets Eglot start for that mode.")

(defun my/eglot-ensure-maybe ()
  "Start Eglot for this buffer if a language server for its mode is installed.
Stays quiet (no error popups) when none is found."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name))
             (seq-some #'executable-find
                       (cdr (assq major-mode my/eglot-server-executables))))
    (eglot-ensure)))

(use-package eglot
  :ensure nil                                       ; built into Emacs 29
  :hook ((prog-mode . my/eglot-ensure-maybe))
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)
              ("C-c l F" . eglot-format-buffer)
              ("C-c l h" . eldoc)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l t" . eglot-find-typeDefinition)
              ("C-c l R" . eglot-reconnect))
  :custom
  (eglot-autoshutdown t)                            ; stop server with last buffer
  (eglot-sync-connect nil)                          ; never block Emacs while it starts
  (eglot-events-buffer-size 0)                      ; no giant log buffer
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c-ts-mode c++-ts-mode)
                 . ("clangd"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--function-arg-placeholders=true"
                    "--header-insertion=iwyu")))
  ;; In Eglot buffers: server completions first (merged with words from open
  ;; buffers), then file names; orderless does the matching.
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-category-defaults nil)
              (setq-local completion-at-point-functions
                          (list (cape-capf-super #'eglot-completion-at-point
                                                 #'cape-dabbrev)
                                #'cape-file)))))

(use-package eldoc
  :ensure nil
  :custom (eldoc-idle-delay 0.3)
  :config (global-eldoc-mode 1))

;; Diagnostics (Flymake is what Eglot uses; Flycheck is not needed)
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; ---------------------------------------------------------------------------
;; Projects, Git, file tree
;; ---------------------------------------------------------------------------
(use-package projectile
  :init (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'default)           ; use vertico
  (projectile-project-search-path '("~/code/" "~/projects/" "~/src/"))
  (projectile-switch-project-action #'projectile-find-file))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (diff-hl-flydiff-mode 1))

(use-package treemacs
  :bind ("C-c t" . treemacs)
  :custom (treemacs-width 30))
(use-package treemacs-projectile :after (treemacs projectile))
(use-package treemacs-magit :after (treemacs magit))
(use-package treemacs-nerd-icons
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

;; ---------------------------------------------------------------------------
;; C / C++ / CMake
;; ---------------------------------------------------------------------------
(setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "stroustrup")))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode)

(defun my/c-mode-keys ()
  (local-set-key (kbd "C-c C-c") #'projectile-compile-project)
  (local-set-key (kbd "C-c C-r") #'recompile))
(add-hook 'c-mode-common-hook #'my/c-mode-keys)
(global-set-key (kbd "<f5>") #'projectile-compile-project)
(global-set-key (kbd "<f6>") #'projectile-run-project)

(setq compilation-scroll-output 'first-error
      compilation-ask-about-save nil)

(defun my/cmake-export-compile-commands ()
  "Configure the CMake project in ./build and link compile_commands.json to the root.
clangd (and therefore completion/diagnostics) needs that file to understand
your include paths and flags.  Run once per CMake project, then M-x eglot-reconnect."
  (interactive)
  (let ((root (or (projectile-project-root) default-directory)))
    (let ((default-directory root))
      (compile "cmake -S . -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON && ln -sf build/compile_commands.json compile_commands.json"))))
(global-set-key (kbd "C-c c c") #'my/cmake-export-compile-commands)

;; Debugging: macOS ships lldb, not gdb.  M-x lldb works through gud.
;; (The old gdb "GAP" layout code was removed -- it used functions that don't exist.)

;; ---------------------------------------------------------------------------
;; Python
;; ---------------------------------------------------------------------------
(use-package pyvenv
  :config (pyvenv-mode 1))

(defun my/python-activate-project-venv ()
  "Use <project>/.venv if there is one, so the language server sees your packages."
  (when-let* ((root (and (fboundp 'projectile-project-root) (projectile-project-root)))
              (venv (expand-file-name ".venv" root)))
    (when (file-directory-p venv)
      (pyvenv-activate venv))))
;; Must run before Eglot starts (hook order: depth -90 = early).
(add-hook 'python-mode-hook #'my/python-activate-project-venv -90)

;; --- VS Code style "send to REPL" ---------------------------------------------
;;   S-<return>  send region, or the statement/block at point, then move to the next line
;;   C-<return>  same, but stay where you are
;; The Python REPL is started (beside the code) on first use.
(setq python-shell-interpreter "python3"
      python-shell-completion-native-enable nil)   ; avoids macOS "readline" warning

(defun my/python--ensure-repl ()
  "Start an inferior Python process next to the code if none is running.
After a fresh start, wait until the REPL has stopped printing, otherwise the
first line sent is swallowed by Python's startup probing."
  (let ((fresh (not (python-shell-get-process))))
    (when fresh
      (save-selected-window (run-python)))
    (let ((proc (python-shell-get-process)))
      (unless (get-buffer-window (process-buffer proc))
        (save-selected-window (display-buffer (process-buffer proc))))
      (when fresh
        (let ((tries 0))
          (while (and (< tries 40) (accept-process-output proc 0.4))
            (setq tries (1+ tries)))))
      proc)))

(defun my/python-send-dwim (&optional step)
  "Send the active region, or the statement / block at point, to the Python REPL.
Single lines are typed into the REPL so expression values are printed.
Multi-line blocks (def, for, if, ...) are sent whole.  With STEP non-nil,
move point to the next line of code afterwards."
  (interactive)
  (let ((proc (my/python--ensure-repl)) end)
    (if (use-region-p)
        (progn (setq end (region-end))
               (python-shell-send-region (region-beginning) end)
               (deactivate-mark))
      (let* ((beg (save-excursion (python-nav-beginning-of-statement) (point))))
        (setq end (save-excursion
                    (goto-char beg)
                    (if (python-info-looking-at-beginning-of-block)
                        (python-nav-end-of-block)
                      (python-nav-end-of-statement))
                    (point)))
        (let ((text (buffer-substring-no-properties beg end)))
          (if (string-match-p "\n" text)
              (python-shell-send-region beg end)
            (comint-send-string proc (concat text "\n"))))))
    (when step
      (goto-char end)
      (forward-line 1)
      (while (and (not (eobp)) (looking-at-p "[ \t]*\\(#.*\\)?$"))   ; skip blanks and comments
        (forward-line 1)))))

(defun my/python-send-and-step () (interactive) (my/python-send-dwim t))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "S-<return>") #'my/python-send-and-step)
  (define-key python-mode-map (kbd "C-<return>") #'my/python-send-dwim))

;; ---------------------------------------------------------------------------
;; Julia
;; ---------------------------------------------------------------------------
;; juliaup normally provides a `julia' command; on this machine only
;; `julialauncher' is present in ~/.juliaup/bin, so fall back to that.
(defvar my/julia-executable
  (or (executable-find "julia")
      (executable-find "julialauncher")
      "julia")
  "Julia executable used by the REPL and the language server.")

(use-package julia-mode
  :mode "\\.jl\\'"
  :custom (julia-indent-offset 4))
;; In .jl buffers: type \alpha then TAB (or pick from the popup) to get α.
;; C-\ toggles the "julia" input method for typing \alpha<space> style input.

;; Language server (LanguageServer.jl) through Eglot.  The server's own Julia
;; environment is installed on first use by `eglot-jl-init' (needs network, takes
;; a few minutes once).
(use-package eglot-jl
  :after eglot
  :custom
  (eglot-jl-julia-command my/julia-executable)
  ;; Own environment (survives eglot-jl upgrades).  Set up once with:
  ;;   julia --project=~/.emacs.d/julia-ls -e 'import Pkg; Pkg.instantiate(); Pkg.precompile()'
  (eglot-jl-language-server-project (expand-file-name "julia-ls/" user-emacs-directory))
  :config
  (setq eglot-connect-timeout 120)            ; Julia's first start is slow
  (eglot-jl-init))

;; REPL integration: send code from the buffer to a running Julia session.
(use-package julia-repl
  :hook (julia-mode . julia-repl-mode)
  :custom
  (julia-repl-executable-records
   `((default ,my/julia-executable)))
  (julia-repl-switches "--threads=auto"))
(with-eval-after-load 'julia-repl
  (julia-repl-set-terminal-backend 'ansi-term))      ; no extra build needed (vterm is optional)

;; ---------------------------------------------------------------------------
;; Markdown
;; ---------------------------------------------------------------------------
;; Three ways to "render":
;;   1. In the same buffer:  C-c C-x C-m  hides the markup (# ** [](...)) and shows
;;      sized headings, bold/italic, links, native code-block colours.
;;      C-c C-x C-i toggles inline images.
;;   2. Live side-by-side preview (updates as you type):  C-c C-c l
;;   3. Full HTML page in your browser:                    C-c C-c p
;; 2 and 3 need pandoc:  brew install pandoc
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode) ("\\.markdown\\'" . gfm-mode))
  :hook (markdown-mode . visual-line-mode)
  :custom
  (markdown-command '("pandoc" "--from=gfm+tex_math_dollars" "--to=html5" "--mathml"))
  (markdown-header-scaling t)                    ; big # headings
  (markdown-fontify-code-blocks-natively t)      ; ```python blocks get Python colours
  (markdown-enable-math t)
  (markdown-asymmetric-header t)
  (markdown-live-preview-delete-export 'delete-on-export)
  (markdown-xhtml-header-content
   (concat "<style>"
           "body{max-width:50em;margin:2em auto;padding:0 1em;line-height:1.6;"
           "font-family:-apple-system,BlinkMacSystemFont,'Helvetica Neue',sans-serif}"
           "pre,code{background:#f4f4f5;border-radius:4px;font-size:90%}"
           "pre{padding:.8em;overflow:auto} code{padding:.1em .3em} pre code{padding:0}"
           "table{border-collapse:collapse} td,th{border:1px solid #ccc;padding:.3em .7em}"
           "blockquote{border-left:4px solid #ccc;margin-left:0;padding-left:1em;color:#555}"
           "img{max-width:100%}"
           "</style>")))

;; ---------------------------------------------------------------------------
;; PDF viewing (pdf-tools)
;; ---------------------------------------------------------------------------
;; One-time setup for the native renderer (`epdfinfo'):
;;   brew install poppler automake pkg-config   then   M-x pdf-tools-install
;; Without it Emacs falls back to the slower built-in `doc-view-mode'.
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-use-scaling t)                       ; sharp on Retina
  (pdf-view-use-imagemagick nil)
  :hook (pdf-view-mode . (lambda ()
                           (display-line-numbers-mode 0)
                           (setq-local global-hl-line-mode nil)
                           (blink-cursor-mode -1)))
  :config
  (pdf-loader-install))                          ; builds/loads lazily on first PDF

;; ---------------------------------------------------------------------------
;; Terminals
;; ---------------------------------------------------------------------------
(use-package vterm
  :commands vterm
  :custom
  (vterm-shell "zsh")
  (vterm-max-scrollback 10000)
  ;; Always build the module for the CPU Emacs runs on.  A stale CMake cache once
  ;; produced x86_64 objects linked against an arm64 libvterm.
  (vterm-module-cmake-args (if (string-match-p "\\`\\(aarch64\\|arm64\\)" system-configuration)
                               "-DCMAKE_OSX_ARCHITECTURES=arm64"
                             "")))

(use-package eshell
  :ensure nil
  :custom (eshell-destroy-buffer-when-process-dies t))

;; ---------------------------------------------------------------------------
;; Claude Code in vterm: send what you are looking at, with one key
;; ---------------------------------------------------------------------------
;; No extra package: Claude Code runs in a normal vterm buffer, these commands
;; just type into it.  Nothing is submitted for you; you finish the prompt and
;; press RET yourself.
;;
;;   C-c j j   start (or switch to) Claude for this project, in a side-by-side vterm
;;   C-c j r   send a REFERENCE: @file#L12-20 for the region, @file#L12 for this line
;;   C-c j f   send a reference to the whole file: @file
;;   C-c j s   send the selected TEXT as a code block (with file name)
;;   C-c j m   mark the current vterm as "the Claude buffer" (if you started it yourself)
;;   C-c j z   jump to the Claude buffer
;; With several Claude buffers open, the one for the current project is used,
;; otherwise you are asked which one.
(defvar my/claude-command "claude"
  "Command that starts Claude Code in the vterm.")
(defvar my/claude-marked-buffer nil
  "A vterm buffer the user marked as running Claude Code (see `my/claude-mark').")

(defun my/claude--root ()
  "Project root of the current buffer, or its directory."
  (file-name-as-directory
   (expand-file-name
    (or (and (fboundp 'projectile-project-root) (ignore-errors (projectile-project-root)))
        default-directory))))

(defun my/claude--project-name ()
  (file-name-nondirectory (directory-file-name (my/claude--root))))

(defun my/claude--candidates ()
  "Live vterm buffers that run Claude Code."
  (seq-filter (lambda (b)
                (and (buffer-live-p b)
                     (provided-mode-derived-p (buffer-local-value 'major-mode b) 'vterm-mode)
                     (or (eq b my/claude-marked-buffer)
                         (string-prefix-p "*claude" (buffer-name b)))))
              (buffer-list)))

(defun my/claude--target ()
  "Pick the Claude buffer: the only one, the one for this project, or ask."
  (let* ((cands (my/claude--candidates))
         (mine (format "*claude:%s*" (my/claude--project-name))))
    (cond ((null cands)
           (user-error "No Claude buffer yet: C-c j j starts one, C-c j m marks an existing vterm"))
          ((null (cdr cands)) (car cands))
          ((get-buffer mine) (get-buffer mine))
          (t (get-buffer (completing-read "Claude buffer: " (mapcar #'buffer-name cands) nil t))))))

(defun my/claude--rel-file ()
  "Current file relative to the project root.
Both paths are resolved first: on macOS /tmp is a symlink to /private/tmp, and a
mismatch would give paths like ../../../tmp/x."
  (unless buffer-file-name (user-error "This buffer is not visiting a file"))
  (file-relative-name (file-truename buffer-file-name)
                      (file-truename (my/claude--root))))

(defun my/claude--reference ()
  "Text like @src/a.py#L12-20 for the region, or @src/a.py#L12 for the current line."
  (let ((file (my/claude--rel-file)))
    (if (use-region-p)
        (let ((a (line-number-at-pos (region-beginning)))
              (b (line-number-at-pos (max (region-beginning) (1- (region-end))))))
          (if (= a b) (format "@%s#L%d " file a) (format "@%s#L%d-%d " file a b)))
      (format "@%s#L%d " file (line-number-at-pos)))))

(defun my/claude--code-block ()
  "The region as a fenced code block headed by its file name and line numbers."
  (unless (use-region-p) (user-error "Select some text first"))
  (let* ((lang (replace-regexp-in-string "-ts-mode\\'\\|-mode\\'" "" (symbol-name major-mode)))
         (a (line-number-at-pos (region-beginning)))
         (text (string-trim-right (buffer-substring-no-properties (region-beginning) (region-end)))))
    (format "%s (line %d):\n```%s\n%s\n```\n" (my/claude--rel-file) a lang text)))

(defun my/claude--send (text &optional paste)
  "Type TEXT into the Claude buffer (as a bracketed paste when PASTE) and show it."
  (let ((buf (my/claude--target)))
    (with-current-buffer buf (vterm-send-string text paste))
    (deactivate-mark)
    (select-window (or (get-buffer-window buf) (display-buffer buf)))))

(defun my/claude-start ()
  "Start Claude Code for this project in a vterm beside the code, or switch to it."
  (interactive)
  (require 'vterm)
  (let* ((name (format "*claude:%s*" (my/claude--project-name)))
         (buf (get-buffer name)))
    (if (and buf (get-buffer-process buf))
        (select-window (or (get-buffer-window buf) (display-buffer buf)))
      (let ((default-directory (my/claude--root))
            (vterm-buffer-name name))
        (vterm-other-window)
        (vterm-send-string my/claude-command)
        (vterm-send-return)))))

(defun my/claude-send-reference ()
  "Send @file#Lstart-end (region) or @file#Lline (no region) to Claude."
  (interactive) (my/claude--send (my/claude--reference)))
(defun my/claude-send-file ()
  "Send @file (the whole file) to Claude."
  (interactive) (my/claude--send (format "@%s " (my/claude--rel-file))))
(defun my/claude-send-region-text ()
  "Send the selected text as a fenced code block to Claude."
  (interactive) (my/claude--send (my/claude--code-block) t))
(defun my/claude-mark ()
  "Mark the current vterm buffer as the one running Claude Code."
  (interactive)
  (unless (derived-mode-p 'vterm-mode) (user-error "Not a vterm buffer"))
  (setq my/claude-marked-buffer (current-buffer))
  (message "Marked %s as the Claude buffer" (buffer-name)))
(defun my/claude-jump ()
  "Jump to the Claude buffer."
  (interactive)
  (let ((buf (my/claude--target)))
    (select-window (or (get-buffer-window buf) (display-buffer buf)))))

(global-set-key (kbd "C-c j j") #'my/claude-start)
(global-set-key (kbd "C-c j r") #'my/claude-send-reference)
(global-set-key (kbd "C-c j f") #'my/claude-send-file)
(global-set-key (kbd "C-c j s") #'my/claude-send-region-text)
(global-set-key (kbd "C-c j m") #'my/claude-mark)
(global-set-key (kbd "C-c j z") #'my/claude-jump)

;; ---------------------------------------------------------------------------
;; claude-code-ide.el: full IDE integration (MCP).  Complements the keys above.
;;   C-c C-'   transient menu with every command (start, resume, continue, stop, switch...)
;; What it adds over plain vterm: Claude is told which file you are viewing and
;; what is selected, can read Flymake/Eglot diagnostics, and shows proposed edits
;; as an ediff you can modify before accepting.  Buffers are named
;; *claude-code[project]*, so the C-c j commands above can type into them too.
;;
;; Not on MELPA, so it is installed straight from GitHub.  The revision is PINNED
;; to a commit I read through (2026-11-20); move it forward on purpose, not with
;; :newest.  It runs a WebSocket server on 127.0.0.1 only, protected by a random
;; token written to ~/.claude/ide/.
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el"
       :rev "3e742ae7da47ef61091cc45d0f9e11172c376431")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-terminal-backend 'vterm)
  ;; The package can expose an `executeCode' tool that EVALUATES ELISP inside this
  ;; Emacs for the agent.  On by default upstream; off here on purpose.
  (claude-code-ide-enable-execute-code nil))

(provide 'init)
;;; init.el ends here
