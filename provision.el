(progn
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents))

(mapc
 (lambda (package) (unless (package-installed-p package) (package-install package)))
 '(haskell-mode markdown-mode php-mode sequential-command smart-tab recentf-ext js2-mode coffee-mode google-translate key-combo restart-emacs exec-path-from-shell purescript-mode csharp-mode yaml-mode dockerfile-mode typescript-mode neotree)
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
   "https://raw.githubusercontent.com/osanai3/pipe-to-emacsclient/master/pipe-to-emacsclient.el"
   )
 )

;;(eval-buffer)
