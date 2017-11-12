(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/elpa")

(setq package-enable-at-startup nil)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes
   (quote
	("5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "51277c9add74612c7624a276e1ee3c7d89b2f38b1609eed6759965f9d4254369" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
	(solarized-color-blend it "#002b36" 0.25)
	(quote
	 ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
	(("#073642" . 0)
	 ("#546E00" . 20)
	 ("#00736F" . 30)
	 ("#00629D" . 50)
	 ("#7B6000" . 60)
	 ("#8B2C02" . 70)
	 ("#93115C" . 85)
	 ("#073642" . 100))))
 '(hl-bg-colors
   (quote
	("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
	("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(ido-enable-flex-matching t)
 '(initial-buffer-choice "~/git")
 '(magit-diff-use-overlays nil)
 '(neo-theme (quote ascii))
 '(nrepl-message-colors
   (quote
	("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
	(material-theme jedi-direx babel org-edna htmlize ox-reveal pep8 ein zone-nyan zlc tabbar swiper solarized-theme slime-theme slime-annot rtags projectile osx-org-clock-menubar osx-lib osx-clipboard org neotree multi-term magit latex-pretty-symbols latex-extra kv js2-mode jedi helm flycheck-irony exec-path-from-shell evil-tabs elpy cuda-mode company-math company-jedi company-irony-c-headers company-irony company-ghc company-cmake color-theme-solarized cmake-project cmake-mode cmake-ide autopair auto-complete-clang aurora-theme auctex-latexmk airline-themes ac-clang ac-cake2)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(weechat-color-list
   (quote
	(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq c-basic-offset 6)
(setq-default c-electric-flag nil)
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)
(setq-default c-electric-flag nil)
(require 'cl)
(require 'cc-mode)
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;;(require 'ac-clang)
;;(ac-clang-initialize)
;;(ac-clang-activate)
(require 'yasnippet)
(yas-global-mode 1)


;; Using the CMD key as Meta Key on my macbook 
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)

;; Linum mode for showing lines
(put 'upcase-region 'disabled nil)
(global-linum-mode t)

;;; Use Airline theme
(require 'airline-themes)
(load-theme 'airline-light)

;;; Use the Projectile Addon in Emacs
(require 'projectile)
(projectile-global-mode)

(add-hook 'term-mode-hook
		  (lambda ()
			(setq term-buffer-maximum-size 10000)))

(global-flycheck-mode)

(add-hook 'after-init-hook 'global-company-mode)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;;; Using Helm
(require 'helm-config)
(require 'helm)
(helm-mode 1)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal

;;; Indentation for python

;; Ignoring electric indentation
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

(add-hook 'after-init-hook #'global-flycheck-mode)


;slime setup
(add-to-list 'load-path "~/.emacs.d/elpa/slime-20151118.1044/")
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime-autoloads)
(slime-setup '(slime-fancy))
(set-face-attribute 'default nil :height 150)

(setq ring-bell-function 'ignore)
(require 'ox-reveal)
(setq org-reveal-root "file:////Users/max/git/reveal.js/js/reveal.js")
(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; call function now
(set-exec-path-from-shell-PATH)
(flyspell-mode 1)
(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
