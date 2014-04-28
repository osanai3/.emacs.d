;;; -*- lexical-binding: t; -*-
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(line-number-mode t)
(column-number-mode t)
(setq scroll-step 1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(push '(background-color . "black") default-frame-alist)
(push '(foreground-color . "white") default-frame-alist)
(global-set-key (kbd "C-h") 'delete-backward-char)
(set-face-background 'mode-line "#000087")
(set-face-foreground 'mode-line "#af0000")
(set-face-bold-p 'mode-line t)
(set-face-background 'mode-line-inactive "#000087")
(set-face-foreground 'mode-line-inactive "#008700")
(global-auto-revert-mode t)
(transient-mark-mode -1)
(setq ispell-program-name "aspell")

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

(require 'diff-mode)
(set-face-background 'diff-added nil)
(set-face-foreground 'diff-added "green")
(set-face-background 'diff-removed nil)
(set-face-foreground 'diff-removed "red")

(require 'tramp)
(setq-default tramp-remote-path (cons 'tramp-own-remote-path tramp-remote-path))

(require 'magit)
(require 'magit-blame)
(global-set-key (kbd "M-S") 'magit-status)
(set-face-background 'magit-item-highlight "#121212")
(setq magit-git-executable "git")

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

(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

(require 'direx)
(global-set-key (kbd "C-x C-j") 'direx:find-directory-reuse-other-window)
(setq direx:leaf-icon "* " direx:open-icon "\u25be " direx:closed-icon "\u25b8 ")

(require 'popwin)
(setq popwin:special-display-config
      '(
        help-mode
        grep-mode
        (completion-list-mode :noselect t)
        "*Shell Command Output*"
        "*Backtrace*"
        "*eshell*"
        ("*Buffer List*" :position left :dedicated t)
        ("*Ibuffer*" :position left :dedicated t)
        (direx:direx-mode :position left :dedicated t)
        ))
(setq display-buffer-function 'popwin:display-buffer)

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
(require 'dimensional-command)
(dimensional-command-register-global
 (mapcar
  (lambda (l) (mapcar (lambda (list) (lambda () (relocate-window-split-window (lambda () (delete-other-windows) (slice-h list))))) l))
  '(
    ((1))
    ((1 1) (2 1))
    ((1 1 1) (2 (2 1)))
    )
  ))
(global-set-key (kbd "C-x 2") (dimensional-command-invoke-command 1))
(global-set-key (kbd "C-x 3") (dimensional-command-invoke-command 0))

(desktop-save-mode 1)

(require 'vc-git)
(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize (concat user-login-name "@" (car (split-string system-name "\\."))) 'face '(foreground-color . "green"))
         (propertize (concat " " (abbreviate-file-name (eshell/pwd))) 'face '(foreground-color . "yellow"))
         (when (vc-git-root (eshell/pwd)) (propertize (concat " (" (car (vc-git-branches)) ")") 'face '(foreground-color . "cyan")))
         "\n"
         (if (= (user-uid) 0) "# " "$ ")
         )))
(setq eshell-prompt-regexp "^[#$] ")
(setq eshell-highlight-prompt nil)

(defun eshell/img (filename)
 (propertize " " 'display (create-image (expand-file-name filename))))
(defun eshell/e (filename)
  (find-file-other-window (expand-file-name filename)))
(global-set-key (kbd "C-z") 'eshell)
