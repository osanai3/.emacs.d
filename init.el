;;; -*- lexical-binding: t; -*-

(package-initialize)

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(line-number-mode t)
(column-number-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(push '(background-color . "black") default-frame-alist)
(push '(foreground-color . "white") default-frame-alist)
(global-set-key (kbd "C-h") 'delete-backward-char)
(set-face-background 'mode-line "#000087")
(set-face-foreground 'mode-line "#af0000")
(set-face-bold 'mode-line t)
(set-face-background 'mode-line-inactive "#000087")
(set-face-foreground 'mode-line-inactive "#008700")
(global-auto-revert-mode t)
(transient-mark-mode -1)
(set-face-attribute 'default nil :height 150)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "s-}") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "s-{") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "s-r") 'revert-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(browse-url-browser-function 'eww-browse-url)
 '(desktop-restore-eager 10)
 '(help-window-select t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(make-backup-files nil)
 '(marginalia-mode t)
 '(neo-autorefresh nil)
 '(neo-show-hidden-files t)
 '(neo-show-updir-line t)
 '(neo-smart-open t)
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-quickstart t)
 '(package-selected-packages
   '(eglot multi-vterm vterm embark-consult embark marginalia orderless icomplete-vertical consult projectile neotree pipe-to-emacsclient restore-window swap-buffer typescript-mode dockerfile-mode yaml-mode purescript-mode exec-path-from-shell restart-emacs key-combo js2-mode sequential-command php-mode markdown-mode haskell-mode))
 '(recentf-max-saved-items 1000)
 '(require-final-newline t)
 '(revert-without-query '(".*"))
 '(scroll-step 1)
 '(sgml-basic-offset 4)
 '(shell-file-name "/usr/local/bin/bash")
 '(tab-width 4)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets nil (uniquify))
 '(uniquify-ignore-buffers-re "*[^*]+*")
 '(vterm-keymap-exceptions
   '("C-z" "C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y"))
 '(whitespace-space-regexp "\\(　+\\)")
 '(whitespace-style '(face tabs spaces trailing empty)))

(defun package-install-from-my-github (package)
  (let* ((name (symbol-name package))
        (url (concat "https://raw.githubusercontent.com/osanai3/" name "/master/" name ".el")))
    (with-current-buffer (url-retrieve-synchronously url) (package-install-from-buffer)))
  )
(unless (file-exists-p "~/.emacs.d/elpa")
  (mapc 'package-install-from-my-github '(swap-buffer restore-window pipe-to-emacsclient))
  (progn (package-refresh-contents) (package-install-selected-packages))
  )

(require 'server)
(unless (server-running-p)
  (server-start))

(with-eval-after-load 'whitespace
  (set-face-foreground 'whitespace-tab "yellow")
  (set-face-background 'whitespace-tab nil)
  (set-face-underline  'whitespace-tab t)
  (set-face-background 'whitespace-space "red")
  (set-face-background 'whitespace-trailing "red")
  (set-face-background 'whitespace-empty "red")
)
(global-whitespace-mode t)

(show-paren-mode t)

(with-eval-after-load 'dired
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
)

(require 'sequential-command-config)
(sequential-command-setup-keys)

(defun my-key-combo-others ()
  (key-combo-define-local (kbd "\\") '("\\" "function "))
  (key-combo-define-local (kbd "<") '("<" "return "))
  )
(add-hook 'php-mode-hook
          (lambda ()
            (my-key-combo-others)
            (key-combo-define-local (kbd "$") '("$" "$this" "$this->"))
            (key-combo-define-local (kbd ">") '(">" "->" " => "))
            (key-combo-define-local (kbd "@") '("@" "array(`!!')"))
            (key-combo-mode 1)
            )
)

(with-eval-after-load 'diff-mode
  (set-face-background 'diff-added nil)
  (set-face-foreground 'diff-added "green")
  (set-face-background 'diff-removed nil)
  (set-face-foreground 'diff-removed "red")
)

(global-set-key (kbd "M-R") 'revert-buffer)
(global-set-key (kbd "M-D") 'make-directory)

(ffap-bindings)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (my-key-combo-others)
            (key-combo-define-local (kbd ">") '(">" " => "))
            (key-combo-mode 1)
            (local-unset-key (kbd "C-a"))
            (local-unset-key (kbd "C-e"))
            (flymake-mode t))
)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'swap-buffer)
(global-set-key (kbd "M-B") 'swap-buffer)
(require 'restore-window)
(global-set-key (kbd "C-x 1") 'restore-window-delete-other-windows-or-restore-window)

(desktop-save-mode 1)

(add-to-list 'vc-handled-backends 'Git)

(push '("\\.twig$" . html-mode) auto-mode-alist)

(exec-path-from-shell-initialize)

(require 'pipe-to-emacsclient)
(add-hook 'find-file-hook 'pipe-to-emacsclient-format)

(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))

(global-set-key (kbd "C-x C-b") 'neotree-toggle)
(with-eval-after-load 'neotree
  (require 'projectile)
  (define-key neotree-mode-map "\C-g" 'neotree-hide)
)

(defadvice neo-open-file (after auto-hide (full-path &optional arg))
  "hide neotree after open file"
  (neotree-hide))
(ad-activate 'neo-open-file)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(icomplete-mode)
(icomplete-vertical-mode)
(define-key icomplete-minibuffer-map "\t" 'icomplete-force-complete)
(define-key icomplete-minibuffer-map "\C-n" 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map "\C-p" 'icomplete-backward-completions)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "C-s") 'consult-isearch)
(global-set-key (kbd "C-x b") 'consult-buffer)
(setq completion-styles '(orderless))
(global-set-key (kbd "C-;") 'embark-act)
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)))
(global-set-key (kbd "C-z") 'multi-vterm-dedicated-toggle)
(with-eval-after-load 'multi-vterm
  (define-key vterm-mode-map (kbd "C-g") 'multi-vterm-dedicated-close))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))
