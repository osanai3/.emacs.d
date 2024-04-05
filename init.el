;;; -*- lexical-binding: t; -*-

(set-language-environment "Japanese")
(setenv "LANG" "ja_JP.UTF-8")
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
(global-set-key (kbd "s-w") nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(browse-url-browser-function 'eww-browse-url)
 '(css-indent-offset 2)
 '(desktop-restore-eager 10)
 '(display-buffer-alist
   '(("\\*vterm\\*" display-buffer-in-side-window
      (window-height . 20))
     ("\\*Embark Actions\\*" display-buffer-in-side-window
      (side . right)
      (window-width . 80))
     ("\\*Help\\*" display-buffer-in-side-window
      (side . right)
      (window-width . 80))
     ("\\*pager\\*" display-buffer-in-side-window
      (side . right)
      (window-width . 80))
     ("\\*Ibuffer\\*" display-buffer-in-side-window
      (side . left)
      (window-width . 30))))
 '(gofmt-command "goimports")
 '(gofmt-show-errors 'echo)
 '(haskell-stylish-on-save t)
 '(help-window-select t)
 '(ibuffer-use-other-window t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(make-backup-files nil)
 '(marginalia-mode t)
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-quickstart t)
 '(package-selected-packages
   '(editorconfig corfu quelpa protobuf-mode rust-mode renda go-mode multi-vterm vterm embark marginalia orderless pipe-to-emacsclient swap-buffer dockerfile-mode yaml-mode purescript-mode exec-path-from-shell js2-mode markdown-mode haskell-mode))
 '(recentf-max-saved-items 1000)
 '(require-final-newline t)
 '(revert-without-query '(".*"))
 '(ruby-align-to-stmt-keywords t)
 '(rust-format-on-save t)
 '(scroll-step 1)
 '(sgml-basic-offset 4)
 '(shell-file-name "/opt/homebrew/bin/bash")
 '(tab-always-indent 'complete)
 '(tab-width 4)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets nil (uniquify))
 '(uniquify-ignore-buffers-re "*[^*]+*")
 '(view-read-only t)
 '(vterm-keymap-exceptions
   '("C-z" "C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y"))
 '(whitespace-space-regexp "\\(　+\\)")
 '(whitespace-style '(face tabs spaces trailing empty))
 '(window-combination-limit t)
 '(window-sides-vertical t))

(unless (file-exists-p "~/.emacs.d/elpa")
  (progn (package-refresh-contents) (package-install-selected-packages))
  (if (fboundp 'quelpa)
      (progn
        (quelpa '(swap-buffer :repo "osanai3/swap-buffer" :fetcher github))
        (quelpa '(pipe-to-emacsclient :repo "osanai3/pipe-to-emacsclient" :fetcher github))
        (quelpa '(renda :repo "osanai3/renda" :fetcher github)))))

(require 'server)
(unless (server-running-p)
  (server-start))

(with-eval-after-load 'whitespace
  (set-face-foreground 'whitespace-tab "yellow")
  (set-face-background 'whitespace-tab nil)
  (set-face-underline  'whitespace-tab t)
  (set-face-background 'whitespace-space "red")
  (set-face-background 'whitespace-trailing "red")
  (set-face-background 'whitespace-empty "red"))
(global-whitespace-mode t)

(show-paren-mode t)

(with-eval-after-load 'dired
  (defvar dired-mode-map)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

(with-eval-after-load 'diff-mode
  (set-face-background 'diff-added nil)
  (set-face-foreground 'diff-added "green")
  (set-face-background 'diff-removed nil)
  (set-face-foreground 'diff-removed "red"))

(global-set-key (kbd "M-R") 'revert-buffer)
(global-set-key (kbd "M-D") 'make-directory)

(ffap-bindings)
(if (boundp 'ffap-url-regexp) (setq ffap-url-regexp nil))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-a"))
            (local-unset-key (kbd "C-e"))
            (flymake-mode t)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(global-set-key (kbd "M-B") 'swap-buffer)

(desktop-save-mode 1)

(add-to-list 'vc-handled-backends 'Git)

(if (fboundp 'exec-path-from-shell-initialize) (exec-path-from-shell-initialize))

;;(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(icomplete-mode)
(fido-vertical-mode)
(defvar icomplete-minibuffer-map)
(define-key icomplete-minibuffer-map "\t" 'icomplete-force-complete)
(define-key icomplete-minibuffer-map "\C-n" 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map "\C-p" 'icomplete-backward-completions)
(setq completion-styles '(orderless))
(global-set-key (kbd "C-;") 'embark-act)
(global-set-key (kbd "C-z") 'vterm)

(with-eval-after-load 'vterm
  (defvar vterm-mode-map)
  (defvar vterm-copy-mode-map)
  (define-key vterm-mode-map (kbd "C-g") 'delete-window)
  (define-key vterm-copy-mode-map (kbd "C-g") 'delete-window))

(with-eval-after-load 'flymake
  (defvar flymake-mode-map)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(add-hook 'typescript-ts-base-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'ruby-mode-hook 'eglot-ensure)

(with-eval-after-load 'go-mode
  (defvar go-mode-map)
  (if (fboundp 'renda-str)
      (progn
        (define-key go-mode-map ":" (renda-str '(":" ":=")))
        (define-key go-mode-map "!" (renda-str '("!" "!=")))
        (define-key go-mode-map "<" (renda-str '("<" "return")))))
  (add-hook 'before-save-hook 'gofmt-before-save))

(with-eval-after-load 'rust-mode
  (defvar rust-mode-map)
  (if (fboundp 'renda-str)
      (progn
        (define-key rust-mode-map "<" (renda-str '("<" "return")))
        (define-key rust-mode-map ">" (renda-str '(">" "->" "=>"))))))

(with-eval-after-load 'haskell-mode
  (defvar haskell-mode-map)
  (if (fboundp 'renda-str)
      (progn
        (define-key haskell-mode-map ">" (renda-str '(">" "->" "=>")))
        (define-key haskell-mode-map "<" (renda-str '("<" "<-"))))))

(if (fboundp 'renda-pos)
    (progn
      (global-set-key (kbd "C-a") (renda-pos '(beginning-of-line beginning-of-buffer)))
      (global-set-key (kbd "C-e") (renda-pos '(end-of-line end-of-buffer)))))

(add-hook 'flymake-mode-hook 'corfu-mode)

(if (fboundp 'editorconfig-mode) (editorconfig-mode t))

(provide 'init)
;;; init.el ends here
