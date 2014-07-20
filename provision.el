(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)
(package-refresh-contents)

(mapc
 (lambda (package) (unless (package-installed-p package) (package-install package)))
 '(magit haskell-mode markdown-mode w3m php-mode sequential-command smart-tab recentf-ext js2-mode coffee-mode google-translate key-combo)
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
   )
 )

;;(eval-current-buffer)
