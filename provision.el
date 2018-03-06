(progn
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents))

(mapc
 (lambda (package) (unless (package-installed-p package) (package-install package)))
 '(haskell-mode markdown-mode php-mode sequential-command smart-tab recentf-ext js2-mode coffee-mode google-translate key-combo vagrant-tramp restart-emacs exec-path-from-shell purescript-mode csharp-mode)
 )

(mapc
 (lambda (url)
  (let ((name (and (string-match "\\([a-z0-9-]+\\)\\.el" url) (match-string 1 url))))
    (unless (package-installed-p (intern name))
     (with-current-buffer (url-retrieve-synchronously url)
       (package-install-from-buffer)))))
 '(
   "https://raw.githubusercontent.com/osanai3/swap-buffer/master/swap-buffer.el"
   "https://raw.githubusercontent.com/osanai3/restore-window/master/restore-window.el"
   "https://raw.githubusercontent.com/osanai3/tempwin/master/tempwin.el"
   "https://raw.githubusercontent.com/osanai3/eshell-tree/master/eshell-tree.el"
   )
 )

;; tar
(mapc
 (lambda (url)
   (cl-destructuring-bind (name . version)
       (and
        (string-match "\\([a-z0-9-]+\\)-\\([0-9.]+\\)\\.tar" url)
        (cons (match-string 1 url) (match-string 2 url)))
     (unless (package-installed-p (intern name))
       (let ((temp-file-name
              (concat temporary-file-directory name "-" version ".tar")))
         (url-copy-file url temp-file-name t)
         (package-install-file temp-file-name)))))
 '(
   "https://github.com/osanai3/eshell-git/releases/download/0.1.7/eshell-git-0.1.tar"
   ))

;;(eval-current-buffer)
