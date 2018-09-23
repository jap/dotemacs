(setq debug-on-error t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(defun apply-to-my-hooks (mode)
  (setq my-hooks '(prog-mode-hook text-mode-hook conf-mode-hook flycheck-mode))
  (dolist (hook my-hooks) (add-hook hook mode))
)

(use-package auto-package-update
  :ensure t
  :config
  (auto-package-update-maybe)
)

;; remove trailing whitespace
(apply-to-my-hooks (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(use-package highlight-symbol
  :ensure t
  :bind (("<f7>" . highlight-symbol-prev)
         ("<f8>" . highlight-symbol)
         ("<f9>" . highlight-symbol-next)
;;         ("M-<f8>" . highlight-symbol-query-replace)
  )
  :init
  (apply-to-my-hooks 'highlight-symbol-mode)
  :diminish highlight-symbol-mode
)

(use-package magit
  :ensure t
  :bind (("<f12>" . magit-status)
         :map magit-mode-map
         ("<tab>" . magit-section-toggle) ;; override indent-or-complete
        )
  :init
  (add-hook 'magit-process-mode-hook 'goto-address-mode)
)

(use-package whitespace
  :config
;;  (apply-to-my-hooks 'whitespace-mode)
;;  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 132) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  :diminish whitespace-mode
)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode +1)
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))
        projectile-completion-system 'ivy)
)
(use-package projectile-speedbar
  :ensure t)

(use-package ido
  :init
  (use-package flx-ido
      :ensure t
      :config
      (flx-ido-mode 1)
      ; disable ido faces to see flx highlights.
      (setq ido-enable-flex-matching t)
      (setq ido-use-faces nil)

  )
  :config
  (progn
   (ido-mode 1)
   (ido-everywhere 1)
   )
)

(use-package flx
  :ensure t
)

(use-package org
  :ensure t
  )


;; begin stolen from https://stackoverflow.com/a/26297700
(require 'org-table)

(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))
    ))

(add-hook 'markdown-mode-hook 'orgtbl-mode)
(add-hook 'markdown-mode-hook
          (lambda()
            (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))
;; end stolen

(use-package plantuml-mode
  :ensure t
)

(use-package markdown-mode
  :ensure t
)

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook 'turn-off-auto-fill)
)

(use-package python-mode
  :ensure t
)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
)

(use-package emojify
  :ensure t
  :config (global-emojify-mode)
)

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  :diminish super-save-mode
)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq magit-completing-read-function 'ivy-completing-read)
)


(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (global-auto-complete-mode 1)
  (defun some-auto-complete-mode-hook ()
    ;; NOTE the following line is recommended by some modes but it does seem troublesome now
    (setq completion-at-point-functions '(auto-complete))
    ;; NOTE: Setting assigning TAB here triggers the error
    (ac-set-trigger-key "TAB"))
  (add-hook 'auto-complete-mode-hook
            'some-auto-complete-mode-hook)
)


(use-package highlight-parentheses
  :ensure t
  :config
  (setq hl-paren-background-colors '("yellow1"))
  (setq hl-paren-colors '("red1"))
;;  (apply-to-my-hooks highlight-parentheses-mode)
  :diminish highlight-parentheses-mode
)

(use-package fill-column-indicator
  :ensure t
)

(use-package go-mode
  :ensure t
)

(use-package elm-mode
  :ensure t)

(use-package flyspell
  :ensure t
  :config
  (setq ispell-list-command "--list")
  (setq ispell-program-name "/usr/local/bin/aspell")
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '(".php'" . web-mode))
)

; ;; stuff not on melpa
; (add-to-list 'load-path "~/.emacs.d/elisp/")

(use-package flycheck
  :ensure t
  :config
  (add-hook 'text-mode-hook 'flycheck-mode)
 ;;  (add-hook 'flycheck-mode-hook 'flycheck-proselint-setup)

)
;;(use-package flycheck-proselint
;;  :load-path "elisp"
;;  :config
;;)

;;(use-package daily-todo
;;  :bind ("C-c C-t" . open-todo-file-interactive))

(setq-default
 c-default-style "linux"
 c-basic-offset 4
 indent-tabs-mode nil
 py-indent-tabs-mode nil
 fci-rule-column 80
 )

(setq visible-bell 1)
(add-hook 'text-mode-hook 'auto-fill-mode)

;(setq use-package-verbose t)

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

(use-package web-mode
  :ensure t)

(use-package mmm-mode
  :ensure t)
;;(use-package mmm-jinja2
;;  :config ((add-to-list 'auto-mode-alist '("\\.j2\\'" . yaml-mode))
;;           (mmm-add-mode-ext-class 'yaml-mode "\\.j2\\'" 'jinja2)))

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
  (setq previous-column (current-column))
  (kill-whole-line)
  (move-to-column previous-column))
(global-set-key [S-backspace] 'nuke-line)

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

(use-package toml-mode
  :ensure t)

(use-package rust-mode
  :init
  (use-package cargo
    :ensure t
    :config
    (add-hook 'rust-mode-hook 'cargo-minor-mode))

  :ensure t)

; super special key binding
(global-set-key (kbd "C-'") (lambda () (interactive) (insert "ʼ")))
(global-set-key (kbd "C-!") (lambda () (interactive) (insert "‽")))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(gnutls-trustfiles
   (quote
    ("/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/usr/local/share/certs/ca-root-nss.crt" "/private/etc/ssl/cert.pem")))
 '(package-selected-packages
   (quote
    (cargo toml-mode rust-mode rust slack yasnippet-snippets yaml-mode web-mode use-package super-save sphinx-mode solarized-theme rainbow-identifiers python-mode projectile-speedbar plantuml-mode org-mind-map mmm-jinja2 markdown-mode magit json-mode js2-mode jedi htmlize highlight-symbol highlight-parentheses go-mode flycheck flx-ido fill-column-indicator exec-path-from-shell emojify elpy elm-mode edit-server dockerfile-mode diminish diff-hl color-theme-modern auto-package-update ansible))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
