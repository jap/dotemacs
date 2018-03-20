(setq debug-on-error t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package auto-package-update
  :ensure t
  :config
  (auto-package-update-maybe)
)

;;(unless (package-installed-p 'use-package)
;;  (unless (assoc 'use-package package-archive-contents)
;;    (package-refresh-contents))
;;  (package-install 'use-package))
;;(require 'use-package)
(setq use-package-verbose t)

(use-package ido
  :init
  (use-package flx-ido
      :ensure t
      :init (flx-ido-mode 1)
  )
  :config
  (progn
   (ido-mode 1)
   (ido-everywhere 1)
   )
)
(use-package color-theme-modern
  :ensure t
  :init
  (use-package solarized-theme
    :ensure t)
  :config
  (load-theme 'solarized-dark t)
)

(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
)

(use-package diminish
  :ensure t)
(use-package ansible
  :ensure t)
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

(use-package web-mode
  :ensure t)

(use-package mmm-mode)
;;(use-package mmm-jinja2
;;  :config ((add-to-list 'auto-mode-alist '("\\.j2\\'" . yaml-mode))
;;           (mmm-add-mode-ext-class 'yaml-mode "\\.j2\\'" 'jinja2)))
(use-package projectile
  :config
  (projectile-mode 1)
  :diminish Projectile
)

(use-package projectile-speedbar)
(use-package python-mode
  :ensure t)
(use-package sphinx-mode
  :ensure t)
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" . yaml-mode))
(use-package rainbow-identifiers
  :ensure t)

(setq-default indent-tabs-mode nil)

;; make s-backspace remove the current line
(defun nuke-line ()
  (interactive)
;  (if (looking-at "\\s-")
;      (delete-region (point) (progn (skip-chars-forward " \t\r\n") (point)))
  (setq previous-column (current-column)
        (kill-whole-line 1)
        (move-to-column previous-column)))

(use-package emojify
  :ensure t
)

(use-package org
  :ensure t
  ;;  :config
  ;;  (use-package org-mind-map
  ;;    :ensure t)
  ;; (use-package plantuml-mode
  ;;   :ensure t
  ;;   :config
  ;;   (add-to-list
  ;;    'org-src-lang-modes '("plantuml" . plantuml))
  ;;   (setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2017.20/libexec/plantuml.jar")
  ;;   (setq org-plantuml-jar-path plantuml-jar-path)
  ;;   )
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((plantuml . t)))
)

(use-package htmlize
  :ensure t
)

(use-package dockerfile-mode
  :ensure t
  )

(use-package js2-mode
  :ensure t
  )

;(use-package elpy
;  :ensure t
;  :config
;  (elpy-enable)
;  )

;; (use-package auto-complete
;;   :ensure t
;;   :config
;;   (setq ac-auto-start nil)
;;   (global-auto-complete-mode)
;;   (ac-flyspell-workaround)
;;   (defun indent-or-complete ()
;;     (interactive)
;;     (if (looking-at-p "\\_>")
;;         (auto-complete)
;;       (indent-according-to-mode)))
;; ;  (global-set-key [tab] 'indent-or-complete)
;; )

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

(setq py-python-command "python3")
(setq python-shell-interpreter "python3")

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))

(use-package pyvenv
  :ensure t
)


(global-set-key [S-backspace] 'nuke-line)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell yasnippet-snippets jedi yaml-mode web-mode use-package sphinx-mode solarized-theme rainbow-identifiers python-mode projectile-speedbar plantuml-mode org-mind-map mmm-jinja2 markdown-mode magit json-mode js2-mode htmlize flx-ido emojify elpy edit-server dockerfile-mode diminish color-theme-modern auto-package-update auto-complete ansible))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
