(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)
(package-refresh-contents)

(mapc
 (lambda (package) (unless (package-installed-p package) (package-install package)))
 '(magit haskell-mode markdown-mode w3m php-mode sequential-command smart-tab recentf-ext js2-mode coffee-mode yascroll direx popwin google-translate key-combo)
 )

(mapc
 (lambda (url)
  (let ((name (and (string-match "\\([a-z0-9-]+\\)\\.el" url) (match-string 1 url))))
    (unless (package-installed-p (intern name))
     (with-current-buffer (url-retrieve-synchronously url)
       (package-install-from-buffer (package-buffer-info) 'single)))))
 '("http://www.emacswiki.org/emacs/download/tempbuf.el"))

;;(eval-current-buffer)
