;; An (evil) emacs init  cobbled together from the far corners of the world.
;; Feel free to copy and paste, fork, clone, or anything you like. 


;; ---------------------------------- ;;
;; PACKAGES :: INITIALIZATION
;; ---------------------------------- ;;

;; initialize melpa and gnu package repos
;; https://melpa.org/#/getting-started
(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; automagically install/bootstrap packages
;; https://blog.aaronbieber.com/2015/05/24/from-vim-to-emacs-in-fourteen-days.html
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


;; ---------------------------------- ;;
;; PACKAGES :: MISC
;; ---------------------------------- ;;

(use-package chicken-scheme
  :ensure t
  :config
    (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package dracula-theme
  :ensure t
  :config
    (load-theme 'dracula t))

(use-package geiser
  :ensure t
  :config
    (setq geiser-default-implementation 'guile)
    (setq geiser-repl-use-other-window nil))

(use-package haskell-mode
    :ensure t)
    
;; https://github.com/abo-abo/swiper
(use-package ivy
  :ensure t
  :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t))

;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :ensure t
  :config
    (ivy-rich-mode 1)
    (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t)
    (setq ivy-rich-path-style 'abbrev))

;; https://www.emacswiki.org/emacs/RainbowDelimiters
(use-package rainbow-delimiters
  :ensure t
  :config
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rotate
  :ensure t)
  
;; http://pragmaticemacs.com/emacs/pop-up-a-quick-shell-with-shell-pop/
(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package slime
  :ensure t
  :config
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (require 'slime-autoloads)
    (slime-setup '(slime-fancy)))

;; https://github.com/nonsequitur/smex/
(use-package smex
  :ensure t
  :config
    (smex-initialize)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(use-package which-key
  :ensure t
  :config
    (which-key-mode))


;; ---------------------------------- ;;
;; PACKAGES :: ORG MODE
;; ---------------------------------- ;;

;; http://ergoemacs.org/emacs/emacs_org_babel_literate_programing.html
(use-package org
  :ensure t
  :config
    (setq org-agenda-files '("~/Documents/personal/org-mode/"))
    
    ;; https://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (setq org-log-done t)

    ;; Tell the latex export to use the minted package for source
    ;; code coloration.
    (setq org-latex-listings 'minted)
    ;; Let the exporter use the -shell-escape option to let latex
    ;; execute external programs.
    ;; This obviously and can be dangerous to activate!
    (setq org-latex-pdf-process
'("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

    ;; make org mode allow eval of some langs
    (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
    (C . t)
    (haskell . t)
    (js . t)
    (python . t)
    (scheme . t)
    (shell . t)
    (sql . t)
    (ruby . t)))

    (setq org-confirm-babel-evaluate nil)
    (setq org-src-fontify-natively t)

    (setq htmlize-output-type 'css)
    (setq org-html-htmlize-output-type 'css)
    (setq org-html-validation-link 'nil)

    (eval-after-load 'autoinsert
      '(define-auto-insert
         '("\.org\'" . "org skeleton")
         '("#+TITLE:" \n
            "#+SETUPFILE: ~/main.org" \n\n)))



(setq org-src-preserve-indentation 't)
    
(setq org-latex-minted-options '(("breaklines" "true")
                                 ("breakanywhere" "true")))

;; Let the exporter use the -shell-escape option to let latex
;; execute external programs.
;; This obviously and can be dangerous to activate!
(setq org-latex-pdf-process
    '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; https://www.emacswiki.org/emacs/AutoInsertMode
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.org\\'" . "org skeleton")
     '(
       (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
       "#+TITLE: " str \n
       "#+AUTHOR: liam beckman" \n
       "#+SETUPFILE: ~/main.org" > \n \n
       )))


;; ---------------------------------- ;;
;; PACKAGES :: MAGIT
;; ---------------------------------- ;;

(use-package magit
  :ensure t)

;; https://github.com/emacs-evil/evil-magit
;; optional: this is the evil state that evil-magit will use
;; (setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
;; (setq evil-magit-use-y-for-yank nil)
(use-package evil-magit
  :ensure t)


;; ---------------------------------- ;;
;; PACKAGES :: EVIL
;; ---------------------------------- ;;

(use-package evil
  :ensure t
  :config
    (evil-mode 1)

    ;; https://emacs.stackexchange.com/questions/17673/no-org-babel-execute-function-for-c-and-no-org-babel-execute-function-for-c
    (global-set-key (kbd "C-S-c") #'evil-copy)
    (global-set-key (kbd "C-S-v") #'evil-paste-before))
    (setq evil-want-C-i-jump nil)

;; https://github.com/milkypostman/powerline
(use-package powerline-evil
    :ensure t
    :config 
    (powerline-evil-vim-color-theme))
    ;; i like square borders in powerline
    ;(setq powerline-evil-default-separator 'nil))

;; evil keybindings
(use-package general
    :ensure t
    :config (general-define-key
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    "f"  'find-file
    "gs" 'magit-status
    "w+" 'rotate-layout
    "l"  'ivy-switch-buffer
    "'"  'shell-pop
    "bd" 'evil-delete-buffer
))


;; ---------------------------------- ;;
;; KEYBINDINGS
;; ---------------------------------- ;;

;; emacs keybindings
;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/Documents/personal.org")))


;; ---------------------------------- ;;
;; SETTINGS
;; ---------------------------------- ;;

;; scrolling
;; https://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4)))

;; tabs
(setq c-default-style "linux")
(setq c-basic-offset 4)

(setq indent-tabs-mode nil)
(global-set-key (kbd "TAB") 'self-insert-command);
;;(setq indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq tab-width 4)
(setq org-src-tab-acts-natively t)

;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks 
    (lambda () (if (not indent-tabs-mode)
                    (untabify (point-min) (point-max)))
                nil ))

;; flycheck
(set-face-attribute 'flyspell-incorrect nil :underline '(:color "deep pink" :style line))

;; Tramp
;; https://www.emacswiki.org/emacs/TrampMode
(setq tramp-default-method "ssh")
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;;  https://www.reddit.com/r/emacs/comments/7v6fll/whats_in_your_initialscratchmessage/
(setq initial-scratch-message
    ";; - 'Tis but a scratch!\n;; - A scratch? Your arm's off!\n;; - No, it isn't!\n\n")
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; https://stackoverflow.com/questions/10545437/how-to-disable-the-beep-in-emacs-on-windows
(setq visible-bell 1)
(global-linum-mode 1)
(global-visual-line-mode t)

;; https://www.emacswiki.org/emacs/ToolBar
;; https://www.emacswiki.org/emacs/MenuBar#toc7
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(show-paren-mode  1)
(scroll-bar-mode -1)
(tooltip-mode    -1)

;; https://stackoverflow.com/questions/1229142/how-can-i-save-my-mini-buffer-history-in-emacs
(savehist-mode 1)

;; spell checker
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US")

;; change all prompts to y or n
;; http://pragmaticemacs.com/emacs/make-all-prompts-y-or-n/
(fset 'yes-or-no-p 'y-or-n-p)

;; https://www.emacswiki.org/emacs/BackupDirectory#toc2
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backups/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; https://stackoverflow.com/questions/2985050/is-there-any-way-to-have-emacs-save-your-undo-history-between-sessions
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; https://www.emacswiki.org/emacs/TransparentEmacs
 (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
 (add-to-list 'default-frame-alist '(alpha . (85 . 50)))
 (defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(85 . 75) '(100 . 100)))))
 (global-set-key (kbd "C-c t") 'toggle-transparency)

;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
;; has to be client (emacsclient -c -a "" -nw), not (emacs -nw)
(defun on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

(add-hook 'after-make-frame-functions 'on-frame-open)


;; ---------------------------------- ;;
;; CUSTOM-SET VARIABLES
;; ---------------------------------- ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   [("#383a62" "#383a62")
    ("#7aa5ff" "#383a62")
    ("#ae81ff" "#383a62")
    ("#a0a0c5" "#a0a0c5")
    ("#2de0a7" "#2de0a7")
    ("#8eaee0" "#8eaee0")
    ("#6dfedf" "#6dfedf")
    ("#ccccff" "#ccccff")])
 '(custom-safe-themes
   (quote
    ("3b5ce826b9c9f455b7c4c8bff22c020779383a12f2f57bf2eb25139244bb7290" "3cb2d5a795e1c93d1fbc8360d6ea41f0173aa1366d334b16e1b83b996b8d9ce6" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "4e4d9f6e1f5b50805478c5630be80cce40bee4e640077e1a6a7c78490765b03f" default)))
 '(evil-want-C-i-jump nil)
 '(indent-tabs-mode nil)
 '(org-cycle-emulate-tab t)
 '(org-export-backends (quote (ascii html icalendar latex md odt org)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (ivy-rich gnuplot-mode gnuplot sicp haskell-mode geiser chicken-scheme chess github-theme htmlize which-key use-package smex slime shell-pop rotate rebecca-theme rainbow-delimiters powerline-evil paredit multiple-cursors ivy general flycheck evil-magit evil-leader dracula-theme dashboard)))
 '(tab-width 4)
 '(tls-checktrust (quote ask)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
