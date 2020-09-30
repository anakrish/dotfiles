
;; Anand Krishnamoorthi's emacs setup.


;; Increase garbage collection threshold
;;(setq gc-cons-threshold (* 50 1000 1000))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216
		  gc-cons-percentage 0.1)))


(defun defer-garbage-collection ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection ()
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection)

;; Setup up MELPA.
;; Load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))

(package-initialize)

;; For installing vterm
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(unless
     (require 'use-package nil 'no-error)
   (progn
     (package-refresh-contents)
     (package-install 'use-package)))

(use-package benchmark-init
  :ensure t
    :config
    (add-hook 'after-init-hook 'benchmark-init/deactivate))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(use-package which-key
  :ensure t
    :config
    (which-key-mode))

;;(use-package hydra
;;  :ensure t)

(setq inhibit-startup-screen t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))


;; backup to this folder rather than littering all
;; visited directories
;; backup folder
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(use-package doom-modeline
  :defer t
  :config (setq doom-modeline-workspace-name t
		doom-modeline-buffer-file-name-stype 'truncate-from-project)
  :init (doom-modeline-mode t))

(require 'display-line-numbers)
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(font .  "Iosevka Term:weight=normal:size=17"))
(add-to-list 'default-frame-alist '(variable-pitch . "Iosevka:size=17"))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))


(use-package doom-themes
  :config (load-theme 'doom-nord t))

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq select-enable-clipboard t
      select-enable-primary t)

(defcustom
  display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode help-mode ansi-term-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'."
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (member (buffer-name) '("*scratch*" "*Messages*")))
       (not (minibufferp)))
      (display-line-numbers-mode)))

(global-display-line-numbers-mode)

(if (display-graphic-p)
    (progn
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (display-line-numbers-mode t)))

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq help-window-select t)

;; (use-package ace-window
;;   :defer t
;;   :config (ace-window-display-mode)
;;   :bind (("M-o" . ace-window)))

(use-package winum
  :ensure t
  :config (winum-mode))


(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

;; ace-jump-mode
(use-package ace-jump-mode
  :defer t
  :bind (("C-c c" . ace-jump-char-mode)
	 ("C-c v" . ace-jump-word-mode)
	 ("C-c l" . ace-jump-line-mode)))

;;(use-package swiper
;;  :defer t
;;  :bind ("C-
;;  c s b" . swiper-isearch))

;; When cursor jumps to a window, show a beacon
;; as a visual

(use-package beacon
 ;; :blackout beacon-mode
  :custom
  (beacon-push-mark 10)
  :config
  (beacon-mode +1))

(set-default 'truncate-lines t)

(setq c-default-style "linux"
      c-basic-offset 4)

(use-package magit)

(use-package projectile
  :ensure t
  :config
  (setq projectile-indexing-method 'hybrid
	projectile-enable-caching t
	projectile-require-project-root nil
	projectile-project-compilation-dir "build"
	rojectile-roject-compilation-cmd "make -j 16")
  (projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  :bind ("C-c p" . 'projectile-command-map))


(show-paren-mode 1)
(use-package smartparens
  :ensure t
  :config (smartparens-global-mode))

(blink-cursor-mode 0)

;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(use-package lsp-mode
  :ensure t
  :diminish lsp-ui
  :hook (c-mode-common . lsp)
  :commands lsp
  :bind ("C-h p" . lsp-describe-thing-at-point)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-enable-symbol-highlighting nil
	lsp-ui-doc-show-with-cursor nil
	lsp-ui-sideline-enable nil
	lsp-eldoc-enable-hover nil)
  (lsp-ui-mode -1))

;; (use-package lsp-ui
;;   :config
;;   (setq lsp-ui-sideline-ignore-duplicate t
;; 	lsp-ui-sideline-enable nil
;; 	lsp-ui-use-webkit nil))
(size-indication-mode 1)

(setq save-place-file (locate-user-emacs-file "places" ".emacs-places"))
(setq save-place-forget-unreadable-files nil)

(save-place-mode 1)
(savehist-mode 1)

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

;; (use-package highlight-numbers
;;   :ensure t
;;   :hook (prog-mode . hightlight-numbers-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;(use-package rainbow-identifiers
;;  :ensure t
;;  :hook (prog-mode . rainbow-identifiers-mode))

(use-package counsel
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-height 18
        ivy-regex "ivy-reg"
        ;ivy-initial-inputs-alist nil
        )
  (ivy-mode t)
  (counsel-mode t)
  :bind (("M-x" . 'counsel-M-x)
         ("C-h b" . 'counsel-descbinds)

         ("C-h f" . 'counsel-describe-function)
         ("C-h v" . 'counsel-describe-variable)
	 ("C-x b" . 'counsel-switch-buffer)))

;; Description for help choices
(use-package ivy-rich
  :ensure t
  :config (ivy-rich-mode))


(defun my/vterm-exit-hook (buffer process)
  (let ((window (get-buffer-window buffer)))
    (when window
      (let ((frame (window-frame window)))
	(if (= 1 (length (window-list frame)))
	    (delete-frame frame t)
	  (delete-window window)))
      (kill-buffer buffer))))

(use-package vterm
  :straight (vterm :host github :repo "akermu/emacs-libvterm")
  :config
  (setq vterm-shell (executable-find "fish")
        vterm-kill-buffer-on-exit nil
        vterm-buffer-name-string "vt:%s"
        vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no"
        vterm-max-scrollback 100000)
   (add-hook 'vterm-exit-functions #'my/vterm-exit-hook))

(use-package clang-format+
  :straight (clang-format+
	     :host github
	     :repo "SavchenkoValeriy/emacs-clang-format-plus")
  :ensure t
  :config (setq clang-format+-always-enable t
		clang-format-executable (executable-find "clang-format-7"))
  :hook (c-mode-common . clang-format+-mode))

(use-package popwin
  :ensure t
  :config
  (setq popwin:popup-window-position 'bottom
	popwin:popup-window-height 24
	popwin:popup-window-width 42)
  (popwin-mode t))

(message (emacs-init-time))
(global-set-key (kbd "s-x") 'counsel-M-x)
