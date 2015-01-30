(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)
(package-refresh-contents)

(mapc
 (lambda (package) (unless (package-installed-p package) (package-install package)))
 '(haskell-mode markdown-mode php-mode sequential-command smart-tab recentf-ext js2-mode coffee-mode google-translate key-combo vagrant-tramp)
 )

(mapc
 (lambda (url)
  (let ((name (and (string-match "\\([a-z0-9-]+\\)\\.el" url) (match-string 1 url))))
    (unless (package-installed-p (intern name))
     (with-current-buffer (url-retrieve-synchronously url)
       (if (version< emacs-version "24.3.90.1")
           (package-install-from-buffer (package-buffer-info) 'single)
         (package-install-from-buffer))))))
 '(
   "http://www.emacswiki.org/emacs/download/tempbuf.el"
   "https://raw.githubusercontent.com/osanai3/swap-buffer/master/swap-buffer.el"
   "https://raw.githubusercontent.com/osanai3/restore-window/master/restore-window.el"
   "https://raw.githubusercontent.com/osanai3/tempwin/master/tempwin.el"
   "https://raw.githubusercontent.com/osanai3/eshell-git/master/eshell-git.el"
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
