(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(line-number-mode t)
(column-number-mode t)
(setq scroll-step 1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-o") 'other-window)
(set-face-background 'mode-line "color-18")
(set-face-foreground 'mode-line "color-124")
(set-face-bold-p 'mode-line t)
(set-face-background 'mode-line-inactive "color-18")
(set-face-foreground 'mode-line-inactive "color-28")
(global-auto-revert-mode t)
(transient-mark-mode -1)

(push (expand-file-name "~/.emacs.d/auto-install/") load-path)
(let ((default-directory (expand-file-name "~/.emacs.d/elpa")))
 (normal-top-level-add-subdirs-to-load-path))

(require 'whitespace)
(setq whitespace-style '(face tabs spaces trailing empty))
(setq whitespace-space-regexp "\\(\u3000+\\)")
(set-face-foreground 'whitespace-tab "yellow")
(set-face-background 'whitespace-tab nil)
(set-face-underline  'whitespace-tab t)
(set-face-background 'whitespace-space "red")
(set-face-background 'whitespace-trailing "red")
(set-face-background 'whitespace-empty "red")
(global-whitespace-mode t)

(show-paren-mode t)

(setq flymake-run-in-place nil)

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(push '("\\.php5?$" . php-mode) auto-mode-alist)

(setq-default tab-width 4 indent-tabs-mode nil)
(add-hook 'php-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (c-set-offset 'case-label' 4)
            (c-set-offset 'arglist-intro' 4)
            (c-set-offset 'arglist-cont-nonempty' 4)
            (c-set-offset 'arglist-close' 0)
            ))
(set-face-foreground 'font-lock-function-name-face "color-27")

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

(require 'dired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

(require 'sequential-command-config)
(sequential-command-setup-keys)

(require 'smart-tab)
(global-smart-tab-mode t)

(require 'key-combo)
(defun my-key-combo-paren ()
  (progn
    (key-combo-define-local (kbd "{") '("{`!!'}" "{"))
    (key-combo-define-local (kbd "{}") "{}")
    (key-combo-define-local (kbd "(") '("(`!!')" "("))
    (key-combo-define-local (kbd "()") "()")
    (key-combo-define-local (kbd "[") '("[`!!']" "["))
    (key-combo-define-local (kbd "[]") "[]")
    ))
(defun my-key-combo-quote ()
  (key-combo-define-local (kbd "\"") '("\"`!!'\"" "\""))
  (key-combo-define-local (kbd "'") '("'`!!''" "'"))
  )
(defun my-key-combo-operator ()
  (key-combo-define-local (kbd "+") '(" + " "++"))
  (key-combo-define-local (kbd "+=") " += ")
  (key-combo-define-local (kbd "-") '(" - " "--"))
  (key-combo-define-local (kbd "-=") " -= ")
  (key-combo-define-local (kbd "=") '(" = " " == " " === " "="))
  )
(defun my-key-combo-others ()
  (key-combo-define-local (kbd ",") '(", " ","))
  (key-combo-define-local (kbd "\\") '("\\" "function "))
  (key-combo-define-local (kbd "<") '("<" "return "))
  )
(global-key-combo-mode)
(add-hook 'php-mode-hook
          (lambda ()
            (my-key-combo-paren)
            (my-key-combo-quote)
            (my-key-combo-operator)
            (my-key-combo-others)
            (key-combo-define-local (kbd "$") '("$" "$this" "$this->"))
            (key-combo-define-local (kbd ">") '(">" "->" " => "))
            (key-combo-define-local (kbd "@") '("@" "array(`!!')"))
            (flymake-mode t)
            (setq require-final-newline t)
            (set-face-background 'flymake-errline nil)
            (set-face-underline 'flymake-errline t)
            )
)

(require 'magit)
(require 'magit-blame)
(global-set-key (kbd "M-S") 'magit-status)

(require 'tempbuf)
(setq tempbuf-minimum-timeout (* 60 60 24 7))
(add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)

(setq browse-url-browser-function 'w3m-browse-url)

(setq recentf-max-saved-items 1000)
(require 'recentf-ext)
(global-set-key (kbd "M-F") 'recentf-open-files)

(global-set-key (kbd "M-R") 'revert-buffer)
(global-set-key (kbd "M-G") 'find-grep)
(global-set-key (kbd "M-D") 'make-directory)
(global-set-key (kbd "M-W") 'w3m-find-file)
(global-set-key (kbd "M-Q") 'w3m-search)

(ffap-bindings)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (my-key-combo-paren)
            (my-key-combo-quote)
            (my-key-combo-operator)
            (my-key-combo-others)
            (local-unset-key (kbd "C-a"))
            (local-unset-key (kbd "C-e"))
            (flymake-mode t)
            (setq require-final-newline t)
            )
)

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'coffee-mode)

(require 'markdown-mode)

(require 'yascroll)
(global-yascroll-bar-mode 1)

(require 'popwin)
(push "*Backtrace*" popwin:special-display-config)
(push '("*Buffer List*" :position left :width 25 :dedicated t) popwin:special-display-config)
(popwin-mode 1)

(require 'direx)
(global-set-key (kbd "C-x C-j") 'direx:find-directory-reuse-other-window)
(setq direx:leaf-icon "* " direx:open-icon "\u25be " direx:closed-icon "\u25b8 ")
(push '(direx:direx-mode :position left :width 25 :dedicated t) popwin:special-display-config)

(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(push '("*Ibuffer*" :position left :width 25 :dedicated t) popwin:special-display-config)

(require 'google-translate)
(require 'google-translate-default-ui)
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "ja")
(global-set-key "\C-ct" 'google-translate-at-point)

(require 'relocate-window)
(require 'slice)
(require 'swap-buffer)
(global-set-key (kbd "M-B") 'swap-buffer)
(require 'restore-window)
(global-set-key (kbd "C-x 1") 'restore-window-delete-other-windows-or-restore-window)
