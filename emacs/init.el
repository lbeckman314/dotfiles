;; An (evil) emacs init cobbled together from the far corners of the world.
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

;; https://github.com/jwiegley/emacs-async
(use-package async
  :ensure t
  :config
  (async-bytecomp-package-mode 1))

;; http://pragmaticemacs.com/emacs/super-efficient-movement-using-avy/
(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  :bind ("M-s" . avy-goto-char))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package chicken-scheme
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package desktop
  :ensure t
  :config 
  ;; save sessions
  ;; https://www.emacswiki.org/emacs?action=browse;oldid=DeskTop;id=Desktop
  (setq desktop-path '("~/.emacs.d/"))
  (setq desktop-dirname "~/.emacs.d/")
  (desktop-save-mode 1)
  (defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
  (add-hook 'auto-save-hook 'my-desktop-save))


(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))
;; https://draculatheme.com/emacs/
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
                                        ;(load-theme 'blacula t)

(use-package elmacro
  :ensure t)

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))

(use-package flycheck
  :ensure t)

(use-package geiser
  :ensure t
  :config
  (setq geiser-default-implementation 'guile)
  (setq geiser-repl-use-other-window nil))

(use-package haskell-mode
  :ensure t)

(use-package htmlize
  :ensure t)

;; https://github.com/abo-abo/swiper
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)

  ;; recent buffers
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

;; https://old.reddit.com/r/emacs/comments/8x4xtt/tip_how_i_use_ledger_to_track_my_money/
(use-package ledger-mode
  :ensure t
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :custom (ledger-clear-whole-transactions t)
  :config
  (use-package flycheck-ledger :ensure t :after ledger-mode))

(use-package pdf-tools
  :ensure t)

(use-package projectile
  :ensure t)

;; http://cachestocaches.com/2015/8/getting-started-use-package/
(use-package tex
  :ensure auctex
  :config
  ;; https://tex.stackexchange.com/questions/207889/how-to-set-up-forward-inverse-searches-with-auctex-and-zathura
  ;;(add-to-list 'TeX-view-program-selection
  ;;             '(output-pdf "Zathura"))

  ;; https://www.emacswiki.org/emacs/AUCTeX#toc5
  (setq TeX-PDF-mode t)

  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

  ;; https://emacs.stackexchange.com/questions/19472/how-to-let-auctex-open-pdf-with-pdf-tools
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (setq TeX-source-correlate-method (quote synctex))
  (setq TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-mode t))

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
  (setq shell-pop-window-position "right")
  (setq shell-pop-window-size 50)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (require 'slime-autoloads)
  (slime-setup '(slime-fancy)))

(use-package smartparens
  :ensure t)

;; https://www.emacswiki.org/emacs/SmartTabs
(use-package smart-tabs-mode
  :ensure t
  :config
  (smart-tabs-insinuate 'c 'javascript))

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

(use-package ws-butler
  :ensure t)


;; ---------------------------------- ;;
;; PACKAGES :: ORG MODE
;; ---------------------------------- ;;

;; http://ergoemacs.org/emacs/emacs_org_babel_literate_programing.html
(use-package org
  :ensure t
  :config
  (setq org-agenda-files '("~/Documents/personal/org-mode/"))
  (add-hook 'org-src-mode-hook 'display-line-numbers-mode)
  
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
   (latex . t)
   (python . t)
   (scheme . t)
   (shell . t)
   (sql . t)
   (ruby . t)))

(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate t)
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


;; https://old.reddit.com/r/orgmode/comments/8kzbii/tip_how_i_use_orgjournal_to_improve_my/
(setq org-capture-templates
      '(("j" "Journal" entry (file+olp+datetree "~/Documents/code/osu/2018fall/cs361-software/journal.org") "\n* %?\n")))

;; ---------------------------------- ;;
;; PACKAGESu :: MAGIT
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

  (setq evil-want-C-i-jump nil)

  ;; https://old.reddit.com/r/emacs/comments/50j2po/has_anyone_gotten_evil_and_smarttabsmode_playing/
  (setq evil-indent-convert-tabs nil)

  ;; https://emacs.stackexchange.com/questions/17673/no-org-babel-execute-function-for-c-and-no-org-babel-execute-function-for-c
  (global-set-key (kbd "C-S-c") #'evil-copy)
  (global-set-key (kbd "C-S-v") #'evil-paste-before))

;; (use-package powerline
;;     :ensure t)

;; ;; https://github.com/milkypostman/powerline
;; (use-package powerline-evil
;;     :ensure t
;;     :config 
;;     (powerline-evil-vim-color-theme))

(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-flat
        telephone-line-primary-right-separator 'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-flat)

  ;; https://old.reddit.com/r/emacs/comments/7e7xzg/telephoneline_theming_question/
  (set-face-attribute 'telephone-line-evil-normal
                      nil
                      :background
                      "#BD93F9")
  (set-face-attribute 'telephone-line-evil-insert
                      nil
                      :background
                      "#50FA7B")
  (set-face-attribute 'telephone-line-evil-visual
                      nil
                      :background
                      "#F1FA8C")
  (telephone-line-mode 1))


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
;; PACKAGES :: MU4E
;; ---------------------------------- ;;

;; http://www.djcbsoftware.nl/code/mu/mu4e/Gmail-configuration.html#Gmail-configuration
(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;;location of my maildir
(setq mu4e-maildir (expand-file-name "~/Maildir"))

;;command used to get mail
;; use this for testing
;;(setq mu4e-get-mail-command "true")
;; use this to sync with mbsync
(setq mu4e-get-mail-command "mbsync gmail")
(setq mu4e-update-interval 300)             ;; update every 5 minutes



;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/queue to set up first
(setq smtpmail-queue-mail nil  ;; start in normal mode
      smtpmail-queue-dir   "~/Maildir/queue/cur")


(setq mu4e-drafts-folder "/[Gmail]/Drafts")
(setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
(setq mu4e-trash-folder  "/[Gmail]/Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/[Gmail]/Sent Mail"   . ?s)
         ("/[Gmail]/Trash"       . ?t)
         ("/[Gmail]/All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
;;(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
 user-mail-address "lbeckman314@gmail.com"
 user-full-name  "liam beckman")
                                        ;mu4e-compose-signature
                                        ; (concat
                                        ;   "liam  beckman\n"
                                        ;   "http://www.liambeckman.com\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "lbeckman314@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
;;(setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; https://www.djcbsoftware.nl/code/mu/mu4e/Retrieval-and-indexing.html
(setq
 mu4e-index-cleanup nil      ;; don't do a full cleanup check
 mu4e-index-lazy-check t)    ;; don't consider up-to-date dirs

;;(setq
;; mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
;; mu4e-update-interval 300)             ;; update every 5 minutes
;;                                        ; ;(run-at-time nil 300 'mu4e-update-index)	 

;; https://groups.google.com/forum/#!msg/mu-discuss/4WyTcvKzkAY/bUC5w_941esJ
;; (add-hook 'mu4e-index-updated-hook 'mu4e-headers-do-auto-update)


;; https://www.djcbsoftware.nl/code/mu/mu4e/Displaying-rich_002dtext-messages.html
(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

(setq shr-color-visible-luminance-min 80)

;; https://www.djcbsoftware.nl/code/mu/mu4e/Org_002dmode-links.html
(require 'org-mu4e)

;; https://kitchingroup.cheme.cmu.edu/blog/2016/10/29/Sending-html-emails-from-org-mode-with-org-mime/#orgheadline2
(defun mu4e-compose-org-mail ()
  (interactive)
  (mu4e-compose-new)
  (org-mu4e-compose-org-mode))

(defun htmlize-and-send ()
  "When in an org-mu4e-compose-org-mode message, htmlize and send it."
  (interactive)
  (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
    (load-theme 'github t)
    (org-mime-htmlize) 
    (disable-theme 'github)
    (message-send-and-exit)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)

(setq global-mu4e-conversation-mode t)


;; ---------------------------------- ;;
;; KEYBINDINGS
;; ---------------------------------- ;;

;; emacs keybindings
;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/Documents/personal.org")))

(global-set-key (kbd "C-c l")
                (lambda () (interactive) (find-file "~/Documents/personal/finances/ledger/personal.dat")))


;; ---------------------------------- ;;
;; SETTINGS AND FUNCTIONS
;; ---------------------------------- ;;

(defun startup ()
  (osu)
  (mail)
  (init)
  (vcs)
  ;;(ledger)
  (scratch)
  )

(defun scratch()
  (eyebrowse-switch-to-window-config-1)
  (eyebrowse-rename-window-config 1 "scratch"))

(defun osu()
  (eyebrowse-switch-to-window-config-2)
  (eyebrowse-rename-window-config 2 "osu")
  (dired "/home/liam/Documents/code/osu/2019winter/"))

(defun mail ()
  (eyebrowse-switch-to-window-config-3)
  (eyebrowse-rename-window-config 3 "mail")
  (mu4e))

(defun init ()
  (eyebrowse-switch-to-window-config-4)
  (eyebrowse-rename-window-config 4 "init")
  (find-file "/home/liam/Documents/code/dotfiles/emacs/init.el"))

(defun vcs ()
  (eyebrowse-switch-to-window-config-5)
  (eyebrowse-rename-window-config 5 "magit")
  (magit "/home/liam/Documents/code/osu/")
  (delete-other-windows))

(defun ledger ()
  (eyebrowse-switch-to-window-config-6)
  (eyebrowse-rename-window-config 6 "ledger")
  (find-file "/home/liam/Documents/personal/finances/ledger/personal.dat"))
  

;;(startup)
(add-hook 'after-init-hook #'startup)


;; https://www.emacswiki.org/emacs/ZoneMode
(defun zone-pgm-md5 ()
  "MD5 the buffer, then recursively checksum each hash."
  (let ((prev-md5 (buffer-substring-no-properties ;; Initialize.
                   (point-min) (point-max))))
    ;; Whitespace-fill the window.
    (zone-fill-out-screen (window-width) (window-height))
    (random t)
    (goto-char (point-min))
    (while (not (input-pending-p))
      (when (eobp)
        (goto-char (point-min)))
      (while (not (eobp))
        (delete-region (point) (line-end-position))
        (let ((next-md5 (md5 prev-md5)))
          (insert next-md5)
          (setq prev-md5 next-md5))
        (forward-line 1)
        (zone-park/sit-for (point-min) 0.1)))))
(eval-after-load "zone"
  '(unless (memq 'zone-pgm-md5 (append zone-programs nil))
     (setq zone-programs
           (vconcat zone-programs [zone-pgm-md5]))))

(defun zone-choose (pgm)
  "Choose a PGM to run for `zone'."
  (interactive
   (list
    (completing-read
     "Program: "
     (mapcar 'symbol-name zone-programs))))
  (let ((zone-programs (list (intern pgm))))
    (zone)))
(toggle-truncate-lines 't)

(defun remove-prelude ()
  (interactive)
  (goto-char 1)
  (while (search-forward-regexp "Prelude> " nil t) 
    (replace-match "" t nil)))

;; line number height
;; https://unix.stackexchange.com/questions/29786/font-size-issues-with-emacs-in-linum-mode/30087#30087
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 110))


;; https://www.emacswiki.org/emacs/RecentFiles
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; xdg-open (e.g. org export to html and open
;; https://emacs.stackexchange.com/questions/19344/why-does-xdg-open-not-work-in-eshell
(setq process-connection-type nil)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(require 'em-glob)
(defun directory-files-glob (path)
  (directory-files (file-name-directory path) 
                   nil 
                   (eshell-glob-regexp (file-name-nondirectory path))))

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
;; (add-hook 'write-file-hooks 
;;           (lambda () (if (not indent-tabs-mode)
;;                          (untabify (point-min) (point-max)))
;;             nil ))

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
(setq visual-linum-mode t)
(setq global-visual-line-mode t)
(setq global-linum-mode t)
(setq linum-mode t)

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
(setq ispell-program-name "aspell")
(setq ispell-dictionary "en")

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
(setq undo-tree-enable-undo-in-region nil)
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


;; https://www.emacswiki.org/emacs/KillingBuffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))


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
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "5eda93d7e92808a69e771b02bf65b21ea6f2e94309bdc5135495e195bd7913e1" "f20795b6b18a6487168643337dbd3aa6b930b86b9d16c2407e2bd6d0d91d4ca4" "0556e4e9b305bc00f1a6e2c7a395ff981798d6ca6f22aa59062117a69ee642e2" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "5057614f7e14de98bbc02200e2fe827ad897696bfd222d1bcab42ad8ff313e20" "233bb646e100bda00c0af26afe7ab563ef118b9d685f1ac3ca5387856674285d" "72a097f48e588eaa08b17027ac20304dd3b3ea8ceaca4ca553fb2577b64f4d09" "3b5ce826b9c9f455b7c4c8bff22c020779383a12f2f57bf2eb25139244bb7290" "3cb2d5a795e1c93d1fbc8360d6ea41f0173aa1366d334b16e1b83b996b8d9ce6" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "4e4d9f6e1f5b50805478c5630be80cce40bee4e640077e1a6a7c78490765b03f" default)))
 '(doc-view-continuous t)
 '(evil-want-C-i-jump nil)
 '(fci-rule-color "#969896")
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(ledger-clear-whole-transactions t)
 '(ledger-reports
   (quote
    (("/home/liam/Documents/personal/finances/ledger/personal.dat" "ledger ")
     (#("bal" 0 1
        (idx 1))
      "%(binary) -f %(ledger-file) bal")
     (#("reg" 0 1
        (idx 3))
      "%(binary) -f %(ledger-file) reg")
     (#("payee" 0 1
        (idx 2))
      "%(binary) -f %(ledger-file) reg @%(payee)")
     (#("account" 0 1
        (idx 0))
      "%(binary) -f %(ledger-file) reg %(account)"))))
 '(nrepl-message-colors
   (quote
    ("#183691" "#969896" "#a71d5d" "#969896" "#0086b3" "#795da3" "#a71d5d" "#969896")))
 '(org-cycle-emulate-tab t)
 '(org-export-backends (quote (ascii html icalendar latex md odt org)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (sane-term flycheck-ledger ledger-mode doom-modeline mu4e-conversation telephone-line session ob-tmux eyebrowse format-all rainbow-mode zone-sl zone-rainbow zone-nyan perspective golden-ratio android-mode elmacro rmsbolt swiper ace-jump-mode powerline-evil powerline esup auctex org-ref-pubmed org-ref-scopus org-ref-wos org-id org-ref org-mime pdf-tools weechat aggressive-indent smart-tabs-mode smart-tabs smooth-scrolling evil-mu4e mu4e highlight-indentation company-mode company ws-butler 0blayout anki-editor auto-complete hydra-ivy ivy-hydra smart-parens hydra projectile smartparens ob-sql-mode org-babel-eval-in-repl ivy-rich gnuplot-mode gnuplot sicp haskell-mode geiser chicken-scheme chess github-theme htmlize which-key use-package smex slime shell-pop rotate rebecca-theme rainbow-delimiters paredit multiple-cursors ivy general flycheck evil-magit evil-leader dracula-theme dashboard)))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(projectile-mode nil nil (projectile))
 '(send-mail-function (quote mailclient-send-it))
 '(tab-width 4)
 '(tls-checktrust (quote ask))
 '(vc-annotate-background "#b0cde7")
 '(vc-annotate-color-map
   (quote
    ((20 . "#969896")
     (40 . "#183691")
     (60 . "#969896")
     (80 . "#969896")
     (100 . "#969896")
     (120 . "#a71d5d")
     (140 . "#969896")
     (160 . "#969896")
     (180 . "#969896")
     (200 . "#969896")
     (220 . "#63a35c")
     (240 . "#0086b3")
     (260 . "#795da3")
     (280 . "#969896")
     (300 . "#0086b3")
     (320 . "#969896")
     (340 . "#a71d5d")
     (360 . "#969896"))))
 '(vc-annotate-very-old-color "#969896"))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
