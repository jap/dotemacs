(setq exec-path (append exec-path '("/usr/local/bin/")))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(defun apply-to-my-hooks (mode)
  (setq my-hooks '(prog-mode-hook text-mode-hook conf-mode-hook flycheck-mode))
  (dolist (hook my-hooks) (add-hook hook mode))
)

(use-package auto-package-update
  :ensure t
  :config
  (auto-package-update-maybe)
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

;; remove trailing whitespace
(apply-to-my-hooks (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(use-package highlight-symbol
  :ensure t
  :bind (
         ("<f7>" . highlight-symbol-prev)
         ("<f8>" . highlight-symbol)
         ("<f9>" . highlight-symbol-next)
         ;("M-<f8>" . highlight-symbol-query-replace)
  )
  :config
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

(use-package python-mode
  :ensure t
)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode +1)
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))
        projectile-completion-system 'ivy)
)

(use-package ido
   :ensure t
   :config
   (ido-mode +1)
   (ido-everywhere +1)
)

(use-package flx-ido
   :ensure t
   :config
   (flx-ido-mode +1)
   ; disable ido faces to see flx highlights.
   (setq ido-enable-flex-matching t)
   (setq ido-use-faces nil)
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

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

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


;; ;(use-package anaconda-mode
;; ;  :ensure t
;; ;  :config
;; ;  (add-hook 'python-mode-hook 'anaconda-mode)
;; ;  :diminish anaconda-mode
;; ;)

;; ;(use-package company-anaconda
;; ;  :ensure t
;; ;)

;; (use-package company
;;   :ensure t
;;   :config
;;   ;; ((defun check-expansion ()
;;   ;;   (save-excursion
;;   ;;     (if (looking-at "\\_>") t
;;   ;;       (backward-char 1)
;;   ;;       (if (looking-at "\\.") t
;;   ;;         (backward-char 1)
;;   ;;         (if (looking-at "->") t nil)))))

;;   ;; (defun do-yas-expand ()
;;   ;;   (let ((yas/fallback-behavior 'return-nil))
;;   ;;     (yas/expand)))

;;   ;; (defun tab-indent-or-complete ()
;;   ;;   (interactive)
;;   ;;   (if (minibufferp)
;;   ;;       (minibuffer-complete)
;;   ;;     (if (or (not yas/minor-mode)
;;   ;;             (null (do-yas-expand)))
;;   ;;         (if (check-expansion)
;;   ;;             (company-complete-common-or-cycle)
;;   ;;           (indent-for-tab-command)))))
;;   (apply-to-my-hooks 'company-mode)
;;   (setq company-backends '(company-dabbrev-code))
;;   ;; (add-to-list 'company-backends '(company-anaconda :with company-capf))
;;   (setq company-idle-delay 0) ;; 10 days should be enough

;;   :bind (:map prog-mode-map
;; 	      ("<tab>" . company-indent-or-complete-common))
;;  )


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

;;(global-hl-line-mode +1)
;;(set-face-background 'hl-line "dim gray")
;;(set-face-foreground 'highlight nil)

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
  (add-hook 'flycheck-mode-hook 'flycheck-proselint-setup)
)
(use-package flycheck-proselint
  :load-path "elisp"
  :config
  (add-hook 'text-mode-hook 'flycheck-mode))

(use-package daily-todo
  :bind ("C-c C-t" . open-todo-file-interactive))



(setq-default
 c-default-style "linux"
 c-basic-offset 4
 indent-tabs-mode nil
 py-indent-tabs-mode nil
 fci-rule-column 80
 )

(setq visible-bell 1)
(add-hook 'text-mode-hook 'auto-fill-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(flycheck-flake8-maximum-line-length 120)
 '(flycheck-python-flake8-executable "/usr/local/bin/flake8")
 '(package-selected-packages
   (quote
    (auto-complete autocomplete emojify org-present hide-mode-line elm-mode hide-show hide-show-mode go-mode plantuml-mode auto-package-update company-anaconda anaconda-mode flycheck org org-jira web-mode color-theme-modern solarized-theme yasnippet yaml-mode use-package swiper super-save python-mode projectile markdown-mode magit jinja2-mode highlight-symbol highlight-parentheses flx-ido fill-column-indicator diff-hl company)))
 '(plantuml-jar-path "/usr/local/Cellar/plantuml/1.2017.16/libexec/plantuml.jar"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
