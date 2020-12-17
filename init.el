;;; Code:

;; Increase garbage collection threshold during init.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 32 1024 1024)
		  gc-cons-percentage 0.1)))

(defun defer-garbage-collection ()
  "Increase garbage collection threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection ()
  "Restore garbage collection thresholds."
  (run-at-time
   15 nil (lambda () (setq gc-cons-threshold
			  ;; (* 2 1024 1024)
			  100000000 ;lsp-mode/doom/spacemacs
			  ))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection)

;; General configuration
(setq inhibit-startup-screen t
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups")))
      help-window-select t
      save-place-file (locate-user-emacs-file "places" ".emacs-places")
      save-place-forget-unreadable-files nil
      truncate-lines t
      ;;      initial-scratch-message "(emacs-init-time)"
      c-default-style "linux"
      c-basic-offset 4
      echo-keystrokes 0)

;; More general configuration
(add-to-list 'default-frame-alist '(font .  "Iosevka Term:weight=normal:size=17"))
(add-to-list 'default-frame-alist '(variable-pitch . "Iosevka:size=10"))
					;(add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
					;(add-to-list 'default-frame-alist '(background-color . "#2e3440"))
;;(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
;; Disable ui elements via frame parameters. Faster.
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(menu-bar-lines . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines . nil))
(add-to-list 'default-frame-alist '(tooltip- . nil))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-file-no-ext
       (expand-file-name "straight/repos/straight.el/bootstrap" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))

      (eval-print-last-sexp)))
  (load bootstrap-file-no-ext nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Benchmark init
(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Window switching
(use-package switch-window
  :bind (("M-o" . switch-window)
	 ("M-p" . switch-window-then-swap-buffere)))

;; Jump to windows using numbers.
;; In exwm, provides a way to reliably jump to minibuffer.
(defun my/select-minibuffer ()
  (interactive)
  (select-window (minibuffer-window)))

(use-package winum
  :ensure t
  :bind  (("M-0" . my/select-minibuffer)
          ("M-1" . winum-select-window-1)
          ("M-2" . winum-select-window-2)
          ("M-3" . winum-select-window-3)
          ("M-4" . winum-select-window-4)
          ("M-5" . winum-select-window-5)
          ("M-6" . winum-select-window-6)
          ("M-7" . winum-select-window-7)
          ("M-8" . winum-select-window-8))
  :config
  (winum-mode t)
  (setq winum-scope 'frame-local))


;; Collection of pleasing themes
(use-package doom-themes
  :config
  (load-theme 'doom-city-lights t)
  (tooltip-mode -1))

;; Doom modeline is cool.
(use-package doom-modeline
  :after doom-themes
  :config (setq doom-modeline-workspace-name t
		doom-modeline-buffer-file-name-stype 'truncate-from-project)
  :init (doom-modeline-mode t))


;; dashboard
(use-package dashboard
  :after doom-themes
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-insert-startupify-lists)
  (switch-to-buffer "*dashboard*"))


;; Apply emacs theme to terminals and rest of linux
(use-package theme-magic
  :ensure t
  :after doom-themes
  :config
  (setq theme-magic--same-color-threshold 0.2)
  (theme-magic-export-theme-mode))


(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode))

;; After line number module has been loaded,
;; customize it.
(defcustom
  display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode help-mode ansi-term-mode)
  "Major modes on whichto disable the linum mode, exempts them from global requirement."
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

;; Automatically insert matching parentheses
;; Quite useful for wtiting lisp code.
(use-package smartparens
  :defer 1
  :config (smartparens-global-mode))

;; enable visualization of matching parentheses.
(show-paren-mode 1)

;; Do not blink cursor
(blink-cursor-mode 0)

;; Which key. When partial keys for a command are pressed,
;; minibuffer shows all possible completions.
(use-package which-key
  :defer 0.1
  :config
  (setq which-key-side-window-location 'top
	which-key-side-window-slot 1)
  (which-key-mode))

;; ;; Turn on code completion (?) for all source code.
;; ;; TODO determine exact purpose
(use-package company
  :defer 0.5
  :config (global-company-mode))

;; Turn on code navigation
;; TODO determine exact capabilities
(use-package counsel
  :defer 0.5
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-height 18
	ivy-display-style 'fancy
					; ivy-initial-inputs-alist ((counsel-M-x . "^")))
	)
  (set-face-attribute 'ivy-current-match nil
		      :weight 'demibold :box t)
  (ivy-mode t)
  (counsel-mode t)
  :custom-face
  (ivy-minibuffer-match-face-1 ((t (:inherit 'hl-line))))
  :bind (("M-x" . 'counsel-M-x)
         ("C-h b" . 'counsel-descbinds)
         ("C-h f" . 'counsel-describe-function)
         ("C-h v" . 'counsel-describe-variable)
	 ("C-x b" . 'counsel-switch-buffer)
         ("M-s s" . 'swiper)
         ("C-c r" . 'ivy-resume)
	 ("C-<return>" . 'ivy-immediate-done)))

;; (defun recentf-open-files+ ()
;;   "Use `completing-read' to open a recent file."
;;   (interactive)
;;   (let ((files (mapcar 'abbreviate-file-name recentf-list)))
;;     (find-file (completing-read "Find recent file: " files nil t))))

;; (use-package selectrum
;;   :ensure t
;;   :bind (("C-x r" . 'selectrum-repeat)
;; 	 ("C-x C-r" . 'recentf-open-files+))
;;   :config
;;   (setq selectrum-num-candidates-displayed 20
;; 	selectrum-extend-current-candidate-highlight nil
;; 	selectrum-show-indices t
;; 	)
;; ;	selectrum-secondary-highlight t)
;;   (selectrum-mode +1))

;; (use-package selectrum-prescient
;;   :after selectrum
;;   :ensure t
;;   :config
;;   (selectrum-prescient-mode +1)
;;   (prescient-persist-mode +1))

(use-package mini-frame
  :ensure t
  :config
  (setq resize-mini-frames t)
  (setq mini-frame-show-parameters
	'((top . 0.3)
	  (width . 0.5)
	  (height . 0.3)
	  (left . 0.5)
	  (parent-frame . nil)))
  (mini-frame-mode +1))

(use-package ctrlf
  :after exwm
  :ensure t
  :hook
  ((exwm-mode . (lambda () (ctrlf-local-mode -1))))
  :config
  (ctrlf-mode +1))

;; (use-package swiper
;;   :ensure t)

;; Description for help choices
(use-package ivy-rich
  :after (counsel ivy)
  :config (ivy-rich-mode))

;; Highlight TODOs in all files
(use-package hl-todo
  :defer 0.5
  :config
  (global-hl-todo-mode))

;; Rainbow delimiters for visual delimiter pairing
(use-package rainbow-delimiters
  :defer 0.5
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Allow non-floating resizing with mouse.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2
      window-divider-default-places t)
(window-divider-mode)

;; vterm
;; Fast terminal within emacs
(defun my/vterm-exit-hook (buffer process)
  "Intelligently close window and frame when vterm (PROCESS) running in (BUFFER) exits."
  (let ((window (get-buffer-window buffer)))
    (when window
      (let ((frame (window-frame window)))
	(if (= 1 (length (window-list frame)))
	    (delete-frame frame t)
	  (delete-window window)))
      (kill-buffer buffer))))

(use-package vterm
  :defer 1
  :straight (vterm :type git :host github :repo "akermu/emacs-libvterm"
		   :straight-default-files t)
  :config
  (setq vterm-shell (executable-find "fish")
        vterm-kill-buffer-on-exit nil
        vterm-buffer-name-string "vt:%s"
        vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no"
        vterm-max-scrollback 100000
	vterm-prompt-regexp "λ|$|#"
	vterm-disable-bold-font t
	vterm-disable-inverse-video t
	vterm-term-environment-variable "xterm-24bit"
	vterm-timer-delay 0.15
	vterm-eval-cmds '(("find-file" find-file)
			  ("message" message)
			  ("vterm-clear-scrollback" vterm-clear-scrollback)
			  ("man" man)
			  ("make" compile)
			  ("compile" compile)))
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (vterm-send-string "source ~/emacs/vterm.fish\n")))
  (add-hook 'vterm-exit-functions #'my/vterm-exit-hook))

;Projectile projects
(use-package projectile
  :after doom-themes
  :config
  (setq projectile-indexing-method 'hybrid
	projectile-enable-caching t
	projectile-require-project-root nil
	projectile-project-compilation-dir "build"
	rojectile-roject-compilation-cmd "make -j 16")
  (projectile-mode)
  :custom
  (projectile-completion-system 'ivy-read)
  :bind ("C-c p" . 'projectile-command-map))

;; LSP for coding
(use-package lsp-mode
  :after projectile
  :diminish lsp-ui
  :hook (c-mode-common . lsp)
  :commands lsp
  :bind ("C-h p" . lsp-describe-thing-at-point)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-enable-symbol-highlighting nil
	lsp-ui-doc-show-with-cursor nil
	lsp-ui-sideline-enable nil
	lsp-eldoc-enable-hover nil
	;;performance
	read-process-output-max (* 5 1024 1024)
	lsp-completion-provider 'capf
	lsp-idle-delay 0.500))


;; Only allow keyboard use.
(use-package disable-mouse
  :hook (prog-mode . disable-mouse-mode))


;; Magit
(use-package magit
  :ensure t)

;; Recently used commands
(use-package amx
  :after ivy-rich)

;; Beautify
(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

;; Beautify
(use-package all-the-icons-ibuffer
  :after ivy-rich
  :init
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (all-the-icons-ibuffer-mode 1))

;; Beautify
(use-package balanced-windows
  :defer 1
  :config
  (balanced-windows-mode))

;; Browse packages
;; For browsing melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Heuristics for pop up windows.
(setq
 display-buffer-alist
 '(("\\*lsp-help\\*" display-buffer-below-selected)
   ("vt\:*" display-buffer-same-window (inhibit-same-window . nil))
   ("\\*Buffer List\\*" display-buffer-in-direction (direction . rightmost))
   ("\\*Help\\*" (display-buffer-reuse-window  display-buffer-below-selected))
   ("\\*compilation\\*" display-buffer-same-window)
   ("magit.+" display-buffer-same-window)
   ("\\*Man.+"  display-buffer-same-window (inhibit-same-window . nil))
   ("\\*Messages\\*" display-buffer-in-direction (direction . rightmost))))
;   (".+"  display-buffer-same-window (inhibit-same-window . nil))))


;; (setq
;;  display-buffer-alist
;;  '(
;;    ("\\*Help\\*" (display-buffer-reuse-window display-buffer-below-selected))
;;    ("vt\\:*" (display-buffer-same-window display-buffer-below-selected)
;;     ((inhibit-same-window . nil) ()))
;;    ("\\*lsp-help\\*" display-buffer-below-selected)
;; ;   ((lambda (n a) (eq (buffer-local-value ,'major-mode (get-buffer name))
;; ;		      ,'ibuffer-mode))
;;  ;   (display-buffer-reuse-window display-buffer-below-selected))
;;    ("\\*Ibuffer\\*"
;;     (display-buffer-reuse-window display-buffer-below-selected)
;;     ( (inhibit-same-window) ()))
;;    ;(my/display-buffer-reuse-mode-window-p display-buffer-reuse-mode-window)
;; ;   (my/display-buffer-reuse-window-p display-buffer-reuse-window)
;;  ;  (my/display-buffer-below-selected-p display-buffer-below-selected)

;;    ("\\*Buffer List\\*" display-buffer-in-direction (direction . right))
;; ;   ("\\*Help\\*" (display-buffer-reuse-window  display-buffer-below-selected)
;; ;    (()  ()));(direction . rightmost)))
;;    ("\\*compilation\\*" display-buffer-in-direction (direction . rightmost))
;;    ("\\*Messages\\*" display-buffer-in-direction (direction . rightmost))))

;; Focus new window after split
(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

(use-package ace-link
  :config
  (ace-link-setup-default))

;; (use-package ace-jump-mode
;;   :bind(("M-s c" . ace-jump-char-mode)
;; 	("M-s l" . ace-jump-line-mode)
;; 	("M-s w" . ace-jump-word-mode)))

(use-package avy
  :ensure t
  :bind (("C-c C-j" . 'avy-resume)
	 ("M-g f" . 'avy-goto-line)
	 ("C-'" . 'avy-goto-char-2))
  :config (avy-setup-default))

;; (use-package symon
;;   :straight t
;;   :config
;;   (setq symon-refresh-rate 5
;; 	symon-monitors '(symon-linux-memory-monitor
;; 			 symon-linux-cpu-monitor
;; 			 symon-linux-battery-monitor))
;;   (symon-mode 1))
(display-time)
(display-battery-mode)

(use-package hide-mode-line
  :after exwm)
;  :hook (after-change-major-mode . hide-mode-line-mode))

;; (use-package guru-mode
;;   :config
;;   (guru-global-mode t))

(use-package sudo-edit
  :straight t
  :bind (("C-c C-r e" . 'sudo-edit)
	 ("C-c C-r f" . 'sudo-edit-find-file)))


(use-package diredfl
  :straight t
  :config (diredfl-global-mode 1))


;; (use-package minibuffer-line
;;   :after exwm
;;   :config
;;   (defadvice select-window (after my/update-minibuffer-line activate)
;;     (minibuffer-line--update))
;;   (defadvice exwm-input-toggle-keyboard (after my/update-minibuffer-line activate)
;;     (minibuffer-line--update))
;;   (doom-modeline-def-segment current-time
;;     "Display exwm workspace number."
;;     (when (doom-modeline--active)
;;       (format " %s "display-time-string)))

;;   (doom-modeline-def-segment exwm-workspace
;;     "Display exwm workspace number."
;;     (when (doom-modeline--active)
;;       (format "[WS:%s] "
;; 	      (exwm-workspace--position (selected-frame)))))

;;   (doom-modeline-def-modeline 'my/minibuffer-modeline
;;     '(bar window-number modals matches buffer-info remote-host buffer-position
;; 	  parrot selection-info)
;;     '(objed-state current-time battery grip github debug repl lsp minor-modes
;; 		  input-method indent-info buffer-encoding major-mode process vcs
;; 		  checker
;; 		  exwm-workspace))
;;   (setq minibuffer-line-refresh-interval 5)
;;   (setq minibuffer-line-format '(:eval (doom-modeline-format--my/minibuffer-modeline))))

;; EXWM
;; Also shrink fringes to 1 pixel.
(fringe-mode 0)
;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil)
(display-time-mode t)

(defun list-external-commands ()
  "Creates a list of all external commands available on $PATH
  while filtering NixOS wrappers."
  (cl-loop
   for dir in (split-string (getenv "PATH") path-separator)
   when (and (file-exists-p dir) (file-accessible-directory-p dir))
   for lsdir = (cl-loop for i in (directory-files dir t)
                        for bn = (file-name-nondirectory i)
                        when (and (not (s-contains? "-wrapped" i))
                                  (not (member bn completions))
                                  (not (file-directory-p i))
                                  (file-executable-p i))
                        collect bn)
   append lsdir into completions
   finally return (sort completions 'string-lessp)))

(defun run-external-command (cmd)
  "Execute the specified command and notify the user when it
  finishes."
  (message "Starting %s..." cmd)
  (set-process-sentinel
   (start-process-shell-command cmd nil cmd)
   (lambda (process event)
     (when (string= event "finished\n")
       (message "%s process finished." process)))))

(defun ivy-run-external-command ()
  "Prompts the user with a list of all installed applications and
  lets them select one to launch."

  (interactive)
  (let ((external-commands-list (list-external-commands)))
    (ivy-read "Command:" external-commands-list
              :require-match t
              :history 'external-commands-history
              :action #'run-external-command)))

;; (defun ivy-run-external-command ()
;;   "Prompts the user with a list of all installed applications and
;;   lets them select one to launch."

;;   (interactive)
;;   (let* ((external-commands-list (list-external-commands))
;; 	(cmd  (selectrum-read "Command:" external-commands-list
;;               :require-match t
;;               :history 'external-commands-history
;;               )))
;;     (run-external-command cmd)))


(defun my/init--exwm ()
;  (run-at-time 1 nil (lambda () (run-external-command "guake")))
  (defun screen ()
    "Launch GNU Screen in external-terminal."
    (interactive)
    (run-at-time 3 nil (lambda ()
			 (message nil)))
    (run-external-command "guake -n (pwd) --show"))

  (setq exwm-input-global-keys
	`(([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
			(lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
	  ([?\s-!] . (lambda () (interactive) (exwm-workspace-move-window 1)))
	  ([?\s-@] . (lambda () (interactive) (exwm-workspace-move-window 2)))
	  ([?\s-#] . (lambda () (interactive) (exwm-workspace-move-window 3)))
	  ([?\s-$] . (lambda () (interactive) (exwm-workspace-move-window 4)))
	  ([?\s-%] . (lambda () (interactive) (exwm-workspace-move-window 5)))
	  ([?\s-^] . (lambda () (interactive) (exwm-workspace-move-window 6)))
	  ([?\s-&] . (lambda () (interactive) (exwm-workspace-move-window 7)))
	  ([?\s-*] . (lambda () (interactive) (exwm-workspace-move-window 8)))
	  ([?\s-\(] . (lambda () (interactive) (exwm-workspace-move-window 9)))
	  ([?\s-\)] . (lambda () (interactive) (exwm-workspace-move-window 0)))
	  ;; ([?\s-&] . (lambda (command)
	  ;; 	       (interactive (list (read-shell-command "$ ")))
	  ;; 	       (start-process-shell-command command nil command)))
	  ([?\s-d] . ivy-run-external-command)
	  ([s-f2] . (lambda ()
		      (interactive)
		      (start-process "" nil "/usr/bin/i3lock -c 000000 && sleep 1")))
	  ([?\s-g] . (lambda ()
		       (interactive)
		       (start-process "" nil "~/.config/polybar/kill.sh")))
	  ([?\s-y] . (lambda ()
		       (interactive)
		       (start-process "" nil "~/.config/polybar/launch.sh")))
	  ;; ([?\s-d] . (lambda ()
	  ;; 	       (interactive)
	  ;; 	       (start-process "" nil "dmenu_run"))); "-fn 'Ubuntu Mono:style=Regular:size=13'")))
	  ([?\s-b] . (lambda ()
		       (interactive)
		       (start-process "" nil "firefox")))
	  ([?\s-s] . (lambda ()
		       (interactive)
		       (run-external-command my/toggle-eterm--cmd)))
	  ([?\s-i] . exwm-input-toggle-keyboard)
	  ([?\s-Q] . kill-buffer-and-window)
	  ([?\s-H] . windmove-swap-states-left)
	  ([?\s-J] . windmove-swap-states-down)
	  ([?\s-K] . windmove-swap-states-up)
	  ([?\s-L] . windmove-swap-states-right)
	  ([?\s-h] . windmove-left)
	  ([?\s-j] . windmove-down)
	  ([?\s-k] . windmove-up)
	  ([?\s-l] . windmove-right)
  	  ([?\s-=] . my/toggle-eterm-0)
    	  (,(kbd "s-- 1") . my/toggle-eterm-1)
    	  (,(kbd "s-- 2") . my/toggle-eterm-2)
    	  (,(kbd "s-- 3") . my/toggle-eterm-3)
    	  (,(kbd "s-- 4") . my/toggle-eterm-4)
    	  (,(kbd "s-- 5") . my/toggle-eterm-5)
    	  (,(kbd "s-- 6") . my/toggle-eterm-6)
    	  (,(kbd "s-- 7") . my/toggle-eterm-7)
    	  (,(kbd "s-- 8") . my/toggle-eterm-8)
    	  (,(kbd "s-- 9") . my/toggle-eterm-9)
	  ))
  (setq exwm-input-simulation-keys
	'(
	  ;; movement([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
					; cut/paste.

          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])))
  (setq exwm-workspace-number 1)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-input-prefix-keys
	'(?\C-x ?\C-u ?\C-h ?\M-x ?\M-` ?\M-& ?\M-:
		?\M-o ?\M-0 ?\M-1 ?\M-2 ?\M-3 ?\M-4
		?\M-5 ?\M-6 ?\M-7 ?\M-8 ?\M-9
		?\s-= ))


  ;; You can hide the minibuffer and echo area when they're not used by
  ;; uncommenting the following.
  ;; (setq exwm-workspace-minibuffer-position 'top)
  (use-package exwm
    :straight t
    :config
    (exwm-enable)
;    (window-divider-mode)
    :hook
    ((exwm-update-class . (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
			;; (lambda ()
			;;   (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
			;; 	      (string= "gimp" exwm-instance-name))
			;;     (exwm-workspace-rename-buffer exwm-class-name))))
     (exwm-update-title . (lambda () (exwm-workspace-rename-buffer exwm-title)))
			;; (lambda ()
			;;   (when (or (not exwm-instance-name)
			;; 	    (string-prefix-p "sun-awt-X11-" exwm-instance-name)
			;; 	    (string= "gimp" exwm-instance-name))
			;;     (exwm-workspace-rename-buffer exwm-title))))
     ;; (exwm-floating-setup . exwm-layout-hide-mode-line)
     ;; (exwm-floating-exit . exwm-layout-show-mode-line))))
     )))

(use-package exwm-mff
  :init
  (xterm-mouse-mode)
  :after exwm
  :config
  (exwm-mff-mode))

(use-package ivy-posframe
  :straight t
  :after exwm
  :custom-face
  (ivy-posframe-border  ((t (:background "gray50"))))
  :config
  (setq ivy-posframe-display-functions-alist
	'((swiper          . ivy-posframe-display-at-window-bottom-left)
;          (complete-symbol . ivy-posframe-display-at-point)
;         (counsel-M-x     . ivy-posframe-display-at-
          (t               . ivy-posframe-display-at-frame-center))
	ivy-posframe-parameters '((parent-frame .  nil)
				  (left-fringe . 10)
				  (right-fringe . 5)
				  (internal-border-width . 1)
				  (background-color . nil)
		 		  (alpha . (95 . 95))
				  ))
  (ivy-posframe-mode))

(use-package which-key-posframe
  :after exwm
  :config
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-center
	which-key-posframe-parameters '((parent-frame . nil)
					  (internal-border-width . 1)
					  (left-fringe . 8)
					  (right-fringe . 5)))
  (which-key-posframe-mode))


(use-package pdf-tools
  :config
  (pdf-tools-install)
  :hook
  (pdf-view-mode . (lambda ()
		     (pdf-view-midnight-minor-mode)
		     (local-set-key (kbd "C-v") 'pdf-view-scroll-up-or-next-page)
		     (local-set-key (kbd "M-v") 'pdf-view-scroll-down-or-previous-page))))

;; (use-package equake
;;   :ensure t
;;   :config
;;   (setq equake-default-shell 'vterm
;; 	equake-use-frame-hide t))

;; for projectile-ag
(use-package ag
  :ensure t)

(defvar my/toggle-eterm--cmd "st -e fish")

(defun my/toggle-eterm--prop (index)
  (intern (format "eterm-toggle-%d" index)))

(defun my/toggle-eterm--title (index)
  (format "eterm %d : %s" index default-directory))

(defun my/toggle-eterm--set-eterm (index)
;  (theme-magic-from-emacs)
  (setq toggle-eterm--index index)
  (exwm-workspace-rename-buffer (my/toggle-eterm--title index))
  (set-frame-parameter nil (my/toggle-eterm--prop index)
		       (window-buffer (selected-window))))

(defvar my/toggle-eterm-use-vterm t)

(defun my/toggle-eterm--create-new (index)
  (if my/toggle-eterm-use-vterm
      (vterm)
    (run-external-command my/toggle-eterm--cmd))
  (run-at-time 0.2 nil 'my/toggle-eterm--set-eterm index))


(defun my/toggle-eterm--do-toggle (index)
  (let* ((eterm (frame-parameter nil (my/toggle-eterm--prop index)))
	(eterm-window (get-buffer-window eterm)))
    (if eterm
	(if (equal (selected-window) eterm-window)
	    (previous-buffer)
	  (if eterm-window
	      (select-window eterm-window)
	    (condition-case nil
	      (exwm-workspace-switch-to-buffer eterm)
	     (error (my/toggle-eterm--create-new index)))))
      (my/toggle-eterm--create-new index))))

;; (add-hook 'exwm-update-title-hook
;;   	  (lambda ()
;;   	    (let ((eterm-title (buffer-local-value 'toggle-eterm--title (current-buffer))))
;;  	      (if eterm-title
;;  		  (exwm-workspace-rename-buffer eterm-title)))))
 	    ;;   (if eterm-title
 	    ;; 	  eterm-title))))

(defun my/toggle-eterm-0 () (interactive) (my/toggle-eterm--do-toggle 0))
(defun my/toggle-eterm-1 () (interactive) (my/toggle-eterm--do-toggle 1))
(defun my/toggle-eterm-2 () (interactive) (my/toggle-eterm--do-toggle 2))
(defun my/toggle-eterm-3 () (interactive) (my/toggle-eterm--do-toggle 3))
(defun my/toggle-eterm-4 () (interactive) (my/toggle-eterm--do-toggle 4))
(defun my/toggle-eterm-5 () (interactive) (my/toggle-eterm--do-toggle 5))
(defun my/toggle-eterm-6 () (interactive) (my/toggle-eterm--do-toggle 6))
(defun my/toggle-eterm-7 () (interactive) (my/toggle-eterm--do-toggle 7))
(defun my/toggle-eterm-8 () (interactive) (my/toggle-eterm--do-toggle 8))
(defun my/toggle-eterm-9 () (interactive) (my/toggle-eterm--do-toggle 9))

(use-package tab-bar
  :ensure t
  :config
  (setq tab-bar-show nil)
  :bind (("C-x t n" . 'tab-bar-switch-to-next-tab)
	 ("C-x t p" . 'tab-bar-switch-to-prev-tab)
	 ("C-x t t" . 'tab-bar-new-tab)
	 ("C-x t d" . 'tab-bar-close-tab)))
