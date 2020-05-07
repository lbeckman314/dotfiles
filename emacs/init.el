; An (evil) emacs init cobbled together from the far corners of the world.
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

;; initialize straight.el
;; https://github.com/raxod502/straight.el
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

;; integrate straight and use-package
(straight-use-package 'use-package)

;; ---------------------------------- ;;
;; PACKAGES :: MISC
;; ---------------------------------- ;;

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)



(use-package all-the-icons
  :ensure t) 

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup)) 

;; https://github.com/jwiegley/emacs-async
(use-package 
    async 
  :ensure t 
  :config (async-bytecomp-package-mode 1))

;; http://pragmaticemacs.com/emacs/super-efficient-movement-using-avy/
(use-package 
    avy 
  :ensure t 
  :config (avy-setup-default)
  ;; https://oremacs.com/2019/05/11/avy-0.5.0/
  (setq avy-style 'words) 
  :bind ("M-z" . avy-goto-char)) 

;; https://github.com/kwrooijen/cargo.el
(use-package
    cargo
  :ensure t
  :config (add-hook 'rust-mode-hook 'cargo-minor-mode)) 

(use-package 
    company 
  :ensure t 
  :config (add-hook 'after-init-hook 'global-company-mode))

;; (use-package chicken-scheme
;;   :ensure t)
(setq inferior-lisp-program "/usr/bin/sbcl")

(use-package 
    dired-sidebar 
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar)) 
  :ensure t 
  :commands (dired-sidebar-toggle-sidebar) 
  :init (add-hook 'dired-sidebar-mode-hook (lambda () 
                                             (unless (file-remote-p default-directory) 
                                               (auto-revert-mode)))) 
  :config (push 'toggle-window-split dired-sidebar-toggle-hidden-commands) 
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands) 
  (setq dired-sidebar-subtree-line-prefix "__") 
  (setq dired-sidebar-theme 'ascii) 
  (setq dired-sidebar-use-term-integration t) 
  (setq dired-sidebar-use-custom-font t))

(use-package 
    counsel 
  :ensure t)

(use-package 
    dracula-theme 
  :ensure t)

;; VOID
;; https://draculatheme.com/emacs/
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'darcula t) 

(use-package 
    elmacro 
  :ensure t)

(use-package 
    elisp-format 
  :ensure t)

(use-package 
    eyebrowse 
  :ensure t 
  :config (eyebrowse-mode t))

(use-package eval-in-repl
  :ensure t
  :straight t
  :config
  ;;; SLIME support (for Common Lisp)
  ;; (require 'slime) ; if not done elsewhere
  (add-hook 'lisp-mode-hook
          '(lambda ()
              (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))) 

(use-package 
    flycheck 
  :ensure t)

(use-package 
    geiser 
  :ensure t 
  :config (setq geiser-default-implementation 'guile) 
  (setq geiser-repl-use-other-window nil))

(use-package 
    haskell-mode 
  :ensure t)

(use-package 
    htmlize 
  :ensure t)

;; https://github.com/abo-abo/swiper
(use-package 
    ivy 
  :ensure t 
  :config (ivy-mode 1)

  ;; recent buffers
  (setq ivy-use-virtual-buffers t) 
  (setq enable-recursive-minibuffers t)

  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper) 
  (global-set-key (kbd "C-c C-r") 'ivy-resume) 
  (global-set-key (kbd "<f6>") 'ivy-resume) 
  (global-set-key (kbd "M-x") 'counsel-M-x) 
  (global-set-key (kbd "C-x C-f") 'counsel-find-file) 
  (global-set-key (kbd "<f1> f") 'counsel-describe-function) 
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable) 
  (global-set-key (kbd "<f1> l") 'counsel-find-library) 
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol) 
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char) 
  (global-set-key (kbd "C-c g") 'counsel-git) 
  (global-set-key (kbd "C-c j") 'counsel-git-grep) 
  (global-set-key (kbd "C-c k") 'counsel-ag) 
  (global-set-key (kbd "C-x l") 'counsel-locate) 
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox) 
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; https://github.com/Yevgnen/ivy-rich
(use-package 
    ivy-rich 
  :ensure t 
  :config
  ;;(ivy-rich-mode 1)
  (setq ivy-virtual-abbreviate 'full ivy-rich-switch-buffer-align-virtual-buffer t) 
  (setq ivy-rich-path-style 'abbrev))

;; https://krsoninikhil.github.io/2018/12/15/easy-moving-from-vscode-to-emacs/
(use-package 
    helm 
  :ensure t 
  :config (global-set-key (kbd "C-c h") 'helm-command-prefix) 
  (global-unset-key (kbd "C-x c")) 
  (helm-autoresize-mode 1) 
  (global-set-key (kbd "M-x") 'helm-M-x) 
  (setq helm-M-x-fuzzy-match t)
  (global-set-key (kbd "C-x p") 'helm-projectile) 
  (helm-mode 1))


;; https://old.reddit.com/r/emacs/comments/8x4xtt/tip_how_i_use_ledger_to_track_my_money/
(use-package 
    ledger-mode 
  :ensure t 
  :mode ("\\.dat\\'" "\\.ledger\\'") 
  :custom (ledger-clear-whole-transactions t) 
  :config (use-package 
              flycheck-ledger 
            :ensure t 
            :after ledger-mode))

(require 'cc-mode)

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(use-package projectile :ensure t)
(use-package yasnippet :ensure t)
(use-package lsp-mode :ensure t)
(use-package hydra :ensure t)
(use-package company-lsp :ensure t)

(use-package 
    pdf-tools 
  :ensure t)

(use-package 
    projectile 
  :ensure t 
  :config (projectile-mode +1) 
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map) 
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; http://cachestocaches.com/2015/8/getting-started-use-package/
(use-package 
    tex 
  :ensure auctex 
  :config
  ;; https://tex.stackexchange.com/questions/207889/how-to-set-up-forward-inverse-searches-with-auctex-and-zathura
  ;;(add-to-list 'TeX-view-program-selection
  ;;             '(output-pdf "Zathura"))

  ;; https://www.emacswiki.org/emacs/AUCTeX#toc5
  (setq TeX-PDF-mode t)

  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")) TeX-view-program-list '(("PDF Tools"
                                                                                        TeX-pdf-tools-sync-view)))

  ;; https://emacs.stackexchange.com/questions/19472/how-to-let-auctex-open-pdf-with-pdf-tools
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) 
  (setq TeX-source-correlate-method (quote synctex)) 
  (setq TeX-source-correlate-start-server t) 
  (setq TeX-source-correlate-mode t))

;; https://www.emacswiki.org/emacs/RainbowDelimiters
(use-package 
    rainbow-delimiters 
  :ensure t 
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package 
    rotate 
  :ensure t)

;; http://pragmaticemacs.com/emacs/pop-up-a-quick-shell-with-shell-pop/
(use-package 
    shell-pop 
  :bind (("C-t" . shell-pop)) 
  :config (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell
                                                                              shell-pop-term-shell))))) 
  (setq shell-pop-term-shell "/bin/zsh") 
  (setq shell-pop-window-position "right") 
  (setq shell-pop-window-size 50)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;; (defun myshell ()
;;   (interactive)
;;   (split-window-horizontally)
;;   (rotate-window)
;;   (eshell))

;; VOID
(use-package 
    slime 
  :ensure t 
  :config (setq inferior-lisp-program "/usr/local/bin/sbcl") 
  (require 'slime-autoloads) 
  (slime-setup '(slime-fancy))) 

(use-package 
    smartparens 
  :ensure t)

;; https://www.emacswiki.org/emacs/SmartTabs
(use-package 
    smart-tabs-mode 
  :ensure t 
  :config (smart-tabs-insinuate 'c 'javascript))

;; https://github.com/nonsequitur/smex/
(use-package 
    smex 
  :ensure t 
  :config (smex-initialize))
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands))

;; (use-package
;;     undohist
;;   :ensure t
;;   :config (undohist-initialize)) 

(use-package 
    which-key 
  :ensure t 
  :config (which-key-mode))

(use-package 
    ws-butler 
  :ensure t)


;; ---------------------------------- ;;
;; PACKAGES :: ORG MODE
;; ---------------------------------- ;;

;; VOID
;; http://ergoemacs.org/emacs/emacs_org_babel_literate_programing.html
(use-package 
    org 
  :ensure t 
  :config (setq org-agenda-/switrfiles '("~/Documents/personal/org-mode/")) 
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
  (setq org-latex-pdf-process '("xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f")))

;; make org mode allow eval of some langs
(org-babel-do-load-languages 'org-babel-load-languages '((C . t) 
                                                         (emacs-lisp . t) 
                                                         (haskell . t) 
                                                         (js . t) 
                                                         (latex . t) 
                                                         (python . t) 
                                                         ;;(racket . t) 
                                                         (ruby . t) 
                                                         (scheme . t) 
                                                         (shell . t) 
                                                         (sql . t))) 

(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate t)
(setq org-src-fontify-natively t)

(setq htmlize-output-type 'css)
(setq org-html-htmlize-output-type 'css)
(setq org-html-validation-link 'nil)

(eval-after-load 'autoinsert '(define-auto-insert '("\.org\'" . "org skeleton") 
                                '("#+TITLE:" \n "#+SETUPFILE: ~/main.org" \n\n)))



(setq org-src-preserve-indentation 't)

(setq org-latex-minted-options '(("breaklines" "true") 
                                 ("breakanywhere" "true")))


;; Let the exporter use the -shell-escape option to let latex
;; execute external programs.
;; This obviously and can be dangerous to activate!
(setq org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; https://www.emacswiki.org/emacs/AutoInsertMode
(eval-after-load 'autoinsert '(define-auto-insert '("\\.org\\'" . "org skeleton") 
                                '((file-name-sans-extension (file-name-nondirectory
                                                             (buffer-file-name))) "#+TITLE: " str \n
                                                             "#+AUTHOR: liam beckman" \n
                                                             "#+SETUPFILE: ~/main.org" > \n \n)))


;; VOID
;; https://old.reddit.com/r/orgmode/comments/8kzbii/tip_how_i_use_orgjournal_to_improve_my/
(setq org-capture-templates '(("j" "Journal" entry (file+olp+datetree
                                                    "~/Documents/code/osu/2018fall/cs361-software/journal.org") "\n* %?\n")))

;; VOID
;;(setq org-html-mathjax-options '(""))
(setq org-file-apps '((auto-mode . emacs) 
                      ("\\.odt\\'" . "/usr/bin/libreoffice %s") 
                      ("\\.x?html?\\'" . "open %s") 
                      ("\\.pdf\\'" . "/usr/bin/zathura %s"))) 


;(setcdr (assq 'system org-file-apps-defaults-gnu ) 
;        '(call-process "xdg-open" nil 0 nil file))

;; ---------------------------------- ;;
;; PACKAGES :: MAGIT
;; ---------------------------------- ;;

(use-package 
    magit 
  :ensure t)

;; setup a key binding
;; (define-key magit-status-mode-map (kbd "C-c s d") 'jorbi-magit/delete-hunk-trailing-whitespace)

;; https://github.com/emacs-evil/evil-magit
;; optional: this is the evil state that evil-magit will use
;; (setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
;; (setq evil-magit-use-y-for-yank nil)

(use-package 
    forge 
  :ensure t)
;;:after magit)


;; ---------------------------------- ;;
;; PACKAGES :: EVIL
;; ---------------------------------- ;;

(use-package 
    evil 
  :ensure t 
  :config (evil-mode 1) 
  (setq evil-want-C-i-jump nil)

  ;; https://old.reddit.com/r/emacs/comments/50j2po/has_anyone_gotten_evil_and_smarttabsmode_playing/
  (setq evil-indent-convert-tabs nil)

  ;; https://emacs.stackexchange.com/questions/17673/no-org-babel-execute-function-for-c-and-no-org-babel-execute-function-for-c
  (global-set-key (kbd "C-S-c") #'evil-copy) 
  (global-set-key (kbd "C-S-v") #'evil-paste-before))

;; (use-package powerline
;;     :straight t
;; :ensure t)

;; ;; https://github.com/milkypostman/powerline
;; (use-package powerline-evil
;;     :straight t
;; :ensure t
;;     :config
;;     (powerline-evil-vim-color-theme))

(use-package 
    telephone-line 
  :ensure t 
  :config (setq telephone-line-primary-left-separator 'telephone-line-flat
                telephone-line-secondary-left-separator 'telephone-line-flat
                telephone-line-primary-right-separator 'telephone-line-flat
                telephone-line-secondary-right-separator 'telephone-line-flat)

  ;; https://old.reddit.com/r/emacs/comments/7e7xzg/telephoneline_theming_question/
  (set-face-attribute 'telephone-line-evil-normal nil 
                      :background "#BD93F9") 
  (set-face-attribute 'telephone-line-evil-insert nil 
                      :background "#50FA7B") 
  (set-face-attribute 'telephone-line-evil-visual nil 
                      :background "#F1FA8C") 
  (telephone-line-mode 1))


;; evil keybindings
(use-package 
    general 
  :ensure t 
  :config (general-define-key :states '(normal visual insert emacs) 
                              :prefix "SPC" 
                              :non-normal-prefix "M-SPC" 
                              "f"  'find-file "gs" 'magit-status "w+" 'rotate-layout "l"
                              'ivy-switch-buffer "'"  'shell-pop "bd" 'evil-delete-buffer))


;; ---------------------------------- ;;
;; PACKAGES :: MU4E
;; ---------------------------------- ;;

;; ;; VOID
;; ;; http://www.djcbsoftware.nl/code/mu/mu4e/Gmail-configuration.html#Gmail-configuration
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e") 
(use-package 
    mu4e) 

;; (use-package
;;     mu4e-alert
;;   :ensure t
;;   :config (;(mu4e-alert-set-default-style 'libnotify)
;;            (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))) 

;; ;; https://www.djcbsoftware.nl/code/mu/mu4e/Org_002dmode-links.html
(use-package 
    org-mu4e) 
(require 'org-mu4e) 

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;;location of my maildir
(setq mu4e-maildir (expand-file-name "~/.mail/ohsu")) 

;;command used to get mail
;; use this for testing
;;(setq mu4e-get-mail-command "true")
;; use this to sync with mbsync
(setq mu4e-get-mail-command "mbsync ohsu") 
(setq mu4e-update-interval 300) ;; update every 5 minutes

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/queue to set up first
(setq smtpmail-queue-mail nil ;; start in normal mode
      smtpmail-queue-dir   "~/.mail/queue/cur")

;; (setq mu4e-drafts-folder "/gmail/[Gmail]/Drafts")
;; (setq mu4e-sent-folder   "/gmail/[Gmail]/Sent Mail")
;; (setq mu4e-trash-folder  "/gmail/[Gmail]/Trash")

;; ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;; (setq mu4e-sent-messages-behavior 'delete)

;; ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; ;; additional non-Gmail addresses and want assign them different
;; ;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts '( ("/ohsu/Inbox"  . ?i) 
                                ("/ohsu/Sent"   . ?s) 
                                ("/ohsu/Trash"  . ?t)))

;; allow for updating mail using 'U' in the main view:
;;(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq user-mail-address "beckmanl@ohsu.edu" user-full-name  "liam beckman")
;;                                         ;mu4e-compose-signature
;;                                         ; (concat
;;                                         ;   "liam  beckman\n"
;;                                         ;   "http://www.liambeckman.com\n"))

;; ;; sending mail -- replace USERNAME with your gmail username
;; ;; also, make sure the gnutls command line utils are installed
;; ;; package 'gnutls-bin' in Debian/Ubuntu

;; (use-package 
;;     smtpmail)
;; (setq message-send-mail-function 'smtpmail-send-it starttls-use-gnutls t
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)) smtpmail-auth-credentials '(("smtp.gmail.com"
;;                                                                                                    587
;;                                                                                                    "lbeckman314@gmail.com"
;;                                                                                                    nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com" smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
(require 'smtpmail) 
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("localhost" 1025 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "localhost"
      smtpmail-smtp-server "localhost"
      smtpmail-smtp-service 1025
      smtpmail-debug-info t) 

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; https://www.djcbsoftware.nl/code/mu/mu4e/Retrieval-and-indexing.html
(setq mu4e-index-cleanup nil   ;; don't do a full cleanup check
      mu4e-index-lazy-check t) ;; don't consider up-to-date dirs

;;(setq
;; mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
;; mu4e-update-interval 300)             ;; update every 5 minutes
;;                                        ; ;(run-at-time nil 300 'mu4e-update-index)

;; https://groups.google.com/forum/#!msg/mu-discuss/4WyTcvKzkAY/bUC5w_941esJ
;; (add-hook 'mu4e-index-updated-hook 'mu4e-headers-do-auto-update)


;; https://www.djcbsoftware.nl/code/mu/mu4e/Displaying-rich_002dtext-messages.html
(add-hook 'mu4e-view-mode-hook (lambda()
                                 ;; try to emulate some of the eww key-bindings
                                 (local-set-key (kbd "<tab>") 'shr-next-link) 
                                 (local-set-key (kbd "<backtab>") 'shr-previous-link)))

(setq shr-color-visible-luminance-min 80)

;; https://kitchingroup.cheme.cmu.edu/blog/2016/10/29/Sending-html-emails-from-org-mode-with-org-mime/#orgheadline2
(defun mu4e-compose-org-mail () 
  (interactive) 
  (mu4e-compose-new) 
  (org-mu4e-compose-org-mode))

(defun htmlize-and-send () 
  "When in an org-mu4e-compose-org-mode message, htmlize and send it." 
  (interactive) 
  (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
                                        ;(load-theme 'dracula t)
    (org-mime-htmlize)
                                        ;(disable-theme 'dracula)
    (message-send)))

(setq browse-url-browser-function (quote browse-url-generic))

;; ;; VOID
;; (setq browse-url-generic-program "firefox")

;; (defvar my-org-html-export-theme 'darcula)

;; (defun my-with-theme (orig-fun &rest args) 
;;   (load-theme my-org-html-export-theme) 
;;   (unwind-protect (apply orig-fun args) 
;;     (disable-theme my-org-html-export-theme)))

;; (with-eval-after-load "ox-html" (advice-add 'org-export-to-buffer 
;;                                             :around 'dracula))

(add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t) 

(setq global-mu4e-conversation-mode t)

;; ;; VOID
;; (setq mu4e-contexts `( ,(make-mu4e-context :name "personal" 
;;                                            :enter-func (lambda () 
;;                                                          (mu4e-message "Entering personal context")) 
;;                                            :leave-func (lambda () 
;;                                                          (mu4e-message "Leaving personal context"))
;;                                            ;; we match based on the contact-fields of the message
;;                                            :match-func (lambda (msg) 
;;                                                          (when msg
;;                                                            (mu4e-message-contact-field-matches msg 
;;                                                                                                :to
;;                                                                                                "lbeckman314@gmail.com"))) 
;;                                            :vars '( ( user-mail-address      .
;;                                                                              "lbeckman314@gmail.com") 
;;                                                     ( user-full-name         . "liam beckman" ) 
;;                                                     )) 
;;                        ,(make-mu4e-context :name "osu" 
;;                                            :enter-func (lambda () 
;;                                                          (mu4e-message "Switch to osu context"))
;;                                            ;; no leave-func
;;                                            ;; we match based on the maildir of the message
;;                                            ;; this matches maildir /Arkham and its sub-directories
;;                                            :match-func (lambda (msg) 
;;                                                          (when msg (string-match-p "^/osu"
;;                                                                                    (mu4e-message-field
;;                                                                                     msg 
;;                                                                                     :maildir)))) 
;;                                            :vars '( ( user-mail-address       .
;;                                                                               "beckmanl@oregonstate.edu" ) 
;;                                                     ( user-full-name          . "liam beckman" ) 
;;                                                     ( mu4e-compose-signature  . (concat
;;                                                                                  "Prof. Alice Derleth\n"
;;                                                                                  "Miskatonic University, Dept. of Occult Sciences\n"))))))


;; ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; ;; guess or ask the correct context, e.g.

;; ;; start with the first (default) context;
;; ;; default is to ask-if-none (ask when there's no context yet, and none match)
;; (setq mu4e-context-policy 'pick-first)

;; ;; compose with the current context is no context matches;
;; ;; default is to ask
;; ;; (setq mu4e-compose-context-policy nil)

;; ---------------------------------- ;;
;; KEYBINDINGS
;; ---------------------------------- ;;

;; VOID
;; emacs keybindings
;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(global-set-key (kbd "C-c o") 
                (lambda () 
                  (interactive) 
                  (find-file "~/Documents/personal/personal.org")))

;; VOID
(global-set-key (kbd "C-c l") 
                (lambda () 
                  (interactive) 
                  (find-file "~/Documents/personal/finances/ledger/ledger.journal")))  

;; VOID
(add-to-list 'load-path "/home/liam/dev/emacs-libvterm")
;;(require 'vterm)

;; ---------------------------------- ;;
;; SETTINGS AND FUNCTIONS
;; ---------------------------------- ;;

;; https://emacs.stackexchange.com/questions/14438/remove-hooks-for-specific-modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun ivy-rich-switch-buffer-icon (candidate) 
  (with-current-buffer (get-buffer candidate) 
    (let ((icon (all-the-icons-icon-for-mode major-mode))) 
      (if (symbolp icon) 
          (all-the-icons-icon-for-mode 'fundamental-mode) icon))))

(setq ivy-rich--display-transformers-list '(ivy-switch-buffer 
                                            (:columns ((ivy-rich-switch-buffer-icon :width 2) 
                                                       (ivy-rich-candidate 
                                                        (:width 30)) 
                                                       (ivy-rich-switch-buffer-size 
                                                        (:width 7)) 
                                                       (ivy-rich-switch-buffer-indicators 
                                                        (:width 4 
                                                                :face error 
                                                                :align right)) 
                                                       (ivy-rich-switch-buffer-major-mode 
                                                        (:width 12 
                                                                :face warning)) 
                                                       (ivy-rich-switch-buffer-project 
                                                        (:width 15 
                                                                :face success)) 
                                                       (ivy-rich-switch-buffer-path 
                                                        (:width (lambda (x) 
                                                                  (ivy-rich-switch-buffer-shorten-path
                                                                   x (ivy-rich-minibuffer-width
                                                                      0.3)))))) 
                                                      :predicate (lambda (cand) 
                                                                   (get-buffer cand)))))

(setq browse-url-browser-function 'browse-url-firefox) 
(setq browse-url-firefox-program "open") 

;; VOID
(add-to-list 'load-path "~/.emacs.d/pkgs/")
;;(require 'dired+)
;;require 'color-dired)

(setq speedbar-use-images nil)

(defun imalison:org-get-raw-value (item) 
  (when (listp item) 
    (let* ((property-list (cadr item))) 
      (when property-list (plist-get property-list 
                                     :raw-value)))))

(defun imalison:sanitize-name (name) 
  (replace-regexp-in-string "[^[:alpha:]]" "" (s-downcase name)))

(defun imalison:generate-name (datum cache) 
  (let ((raw-value (imalison:org-get-raw-value datum))) 
    (if raw-value (imalison:sanitize-name raw-value)
      ;; This is the default implementation from org
      (let ((type (org-element-type datum))) 
        (format "org%s%d" (if type (replace-regexp-in-string "-" "" (symbol-name type))
                            "secondarystring") 
                (incf (gethash type cache 0)))))))
(use-package 
    ox 
  :defer t 
  :config (defun org-export-get-reference (datum info) 
            "Return a unique reference for DATUM, as a string.
DATUM is either an element or an object.  INFO is the current
export state, as a plist.  Returned reference consists of
alphanumeric characters only."
            (let ((type (org-element-type datum)) 
                  (cache (or (plist-get info 
                                        :internal-references) 
                             (let ((h (make-hash-table :test #'eq))) 
                               (plist-put info 
                                          :internal-references h) h))) 
                  (reverse-cache (or (plist-get info 
                                                :taken-internal-references) 
                                     (let ((h (make-hash-table :test 'equal))) 
                                       (plist-put info 
                                                  :taken-internal-references h) h)))) 
              (or (gethash datum cache) 
                  (let* ((name (imalison:generate-name datum cache)) 
                         (number (+ 1 (gethash name reverse-cache -1))) 
                         (new-name (format "%s%s" name (if (< 0 number) number "")))) 
                    (puthash name number reverse-cache) 
                    (puthash datum new-name cache) new-name)))))

(use-package 
    ox-html 
  :commands (org-html-export-as-html org-html-export-as-html) 
  :preface (progn 
             (defvar imalison:link-svg-html 
               "<svg aria-hidden=\"true\" class=\"octicon octicon-link\" height=\"16\" version=\"1.1\" viewBox=\"0 0 16 16\" width=\"16\"><path fill-rule=\"evenodd\" d=\"M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z\"></path></svg>") 
             (defvar imalison:current-html-headline) 
             (defun imalison:set-current-html-headline (headline &rest args) 
               (setq imalison:current-html-headline headline)) 
             (defun imalison:clear-current-html-headline 
                 (&rest 
                  args) 
               (setq imalison:current-html-headline nil)) 
             (defun imalison:org-html-format-heading-function (todo todo-type priority text tags
                                                                    info) 
               (let* ((reference (when imalison:current-html-headline (org-export-get-reference
                                                                       imalison:current-html-headline
                                                                       info)))
                      ;; Don't do anything special if the current headline is not set
                      (new-text (if reference (format "%s <a href=\"#%s\">%s</a>" text reference
                                                      imalison:link-svg-html) text))) 
                 (org-html-format-headline-default-function todo todo-type priority new-text tags
                                                            info)))) 
  :config (progn
            ;; This is set before and cleared afterwards, so that we know when we are
            ;; generating the text for the headline itself and when we are not.
            (advice-add 'org-html-headline 
                        :before 'imalison:set-current-html-headline) 
            (advice-add 'org-html-headline 
                        :after 'imalison:clear-current-html-headline) 
            (setq org-html-format-headline-function 'imalison:org-html-format-heading-function)))

(setq highlight-indent-guides-method 'character)

(defun startup () 
  (interactive) 
  (org)
  (scratch)
  (init) 
  (vcs)) 

;; VOID
(defun org () 
  (interactive)
  (eyebrowse-switch-to-window-config-1) 
  (eyebrowse-rename-window-config 1 "org")
  (find-file "~/Nextcloud/professional.org"))

(defun vcs () 
  (interactive)
  (eyebrowse-switch-to-window-config-2) 
  (eyebrowse-rename-window-config 2 "magit") 
  (magit "~/git/CuratorTool/") 
  (delete-other-windows))  

(defun scratch () 
  (interactive)
  (eyebrowse-switch-to-window-config-3) 
  (eyebrowse-rename-window-config 3 "scratch") 
  (switch-to-buffer "*scratch*")) 

(defun init () 
  (interactive)
  (eyebrowse-switch-to-window-config-4) 
  (eyebrowse-rename-window-config 4 "init") 
  (find-file "~/Documents/code/dotfiles/emacs/init.el")) 

;;(startup)
;;(add-hook 'after-init-hook 'startup)

(defun create-scratch-buffer nil 
  "create a scratch buffer" 
  (interactive) 
  (switch-to-buffer (get-buffer-create "*scratch*")) 
  (insert initial-scratch-message) 
  (lisp-interaction-mode))

;; https://stackoverflow.com/questions/2284703/emacs-how-to-disable-file-changed-on-disk-checking
(setq revert-without-query '(".*"))

;; https://www.emacswiki.org/emacs/ZoneMode
(defun zone-pgm-md5 () 
  "MD5 the buffer, then recursively checksum each hash."
  (let ((prev-md5 
         (buffer-substring-no-properties ;; Initialize.
          (point-min) 
          (point-max))))
    ;; Whitespace-fill the window.
    (zone-fill-out-screen (window-width) 
                          (window-height)) 
    (random t) 
    (goto-char (point-min)) 
    (while (not (input-pending-p)) 
      (when (eobp) 
        (goto-char (point-min))) 
      (while (not (eobp)) 
        (delete-region (point) 
                       (line-end-position)) 
        (let ((next-md5 (md5 prev-md5))) 
          (insert next-md5) 
          (setq prev-md5 next-md5)) 
        (forward-line 1) 
        (zone-park/sit-for (point-min) 0.1)))))
(eval-after-load "zone" '(unless (memq 'zone-pgm-md5 (append zone-programs nil)) 
                           (setq zone-programs (vconcat zone-programs [zone-pgm-md5]))))

(defun zone-choose (pgm) 
  "Choose a PGM to run for `zone'." 
  (interactive (list (completing-read "Program: " (mapcar 'symbol-name zone-programs)))) 
  (let ((zone-programs (list (intern pgm)))) 
    (zone)))
(toggle-truncate-lines 't) 

(defun remove-prelude () 
  (interactive) 
  (goto-char 1) 
  (while (search-forward-regexp "Prelude> " nil t) 
    (replace-match "" t nil)))

;; used for SICP stuff.
(defun lispy ()
  (interactive)
  (find-file (read-file-name "Enter file name:"))
  (geiser-mode-switch-to-repl-and-enter)
  (previous-buffer)
  (split-window-right)
  (other-window 1) 
  (split-window-below) 
  (switch-to-buffer "*Geiser dbg*")
  (other-window 1)
  (switch-to-buffer "* Racket REPL *")
  (other-window 1)) 

;; line number height
;; https://unix.stackexchange.com/questions/29786/font-size-issues-with-emacs-in-linum-mode/30087#30087
(eval-after-load "linum" '(set-face-attribute 'linum nil 
                                              :height 110))

;; https://www.emacswiki.org/emacs/RecentFiles
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; xdg-open (e.g. org export to html and open
;; https://emacs.stackexchange.com/questions/19344/why-does-xdg-open-not-work-in-eshell
(setq process-connection-type nil)

(setq ido-enable-flex-matching t)
;;(setq ido-everywhere t)
(ido-mode 1)

(use-package 
    em-glob)
(defun directory-files-glob (path) 
  (directory-files (file-name-directory path) nil (eshell-glob-regexp (file-name-nondirectory
                                                                       path))))

;; scrolling
;; https://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4)))

;; tabs
(setq c-default-style "linux")
(setq c-basic-offset 4)

;;(setq indent-tabs-mode nil)
;;(setq indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq tab-width 4)
(setq org-src-tab-acts-natively t)
(global-set-key (kbd "TAB") 'self-insert-command)

;; if indent-tabs-mode is off, untabify before saving
;; (add-hook 'write-file-hooks
;;           (lambda () (if (not indent-tabs-mode)
;;                          (untabify (point-min) (point-max)))
;;             nil ))

;; flyspell/flycheck
(set-face-attribute 'flyspell-incorrect nil 
                    :underline '(:color "deep pink" 
                                        :style line))
(set-face-attribute 'flycheck-error nil 
                    :underline '(:color "deep pink" 
                                        :style line))
(set-face-attribute 'flycheck-warning nil 
                    :underline '(:color "purple" 
                                        :style line))
(set-face-attribute 'flycheck-info nil 
                    :underline '(:color "green" 
                                        :style line))

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
;;(setq global-linum-mode t)
;;(setq linum-mode t)

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

;; VOID
;; https://www.emacswiki.org/emacs/BackupDirectory#toc2
(setq backup-by-copying t               ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backups/")) ; don't litter my fs tree
      delete-old-versions t kept-new-versions 6 kept-old-versions 2 version-control t) ; use versioned backups

;; https://stackoverflow.com/questions/2985050/is-there-any-way-to-have-emacs-save-your-undo-history-between-sessions
;;(global-undo-tree-mode)
(setq undo-tree-enable-undo-in-region nil)
;;(setq undo-tree-auto-save-history t)
;;(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; https://emacs.stackexchange.com/questions/47341/fine-grained-undo/47349#47349
(when (timerp undo-auto-current-boundary-timer) 
  (cancel-timer undo-auto-current-boundary-timer))

(fset 'undo-auto--undoable-change (lambda () 
                                    (add-to-list 'undo-auto--undoably-changed-buffers
                                                 (current-buffer))))

(fset 'undo-auto-amalgamate 'ignore)

;; https://www.emacswiki.org/emacs/TransparentEmacs
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95))) 
(defun toggle-transparency () 
  (interactive) 
  (let ((alpha (frame-parameter nil 'alpha))) 
    (set-frame-parameter nil 'alpha (if (eql (cond ((numberp alpha) alpha) 
                                                   ((numberp (cdr alpha)) 
                                                    (cdr alpha))
                                                   ;; Also handle undocumented (<active> <inactive>) form.
                                                   ((numberp (cadr alpha)) 
                                                    (cadr alpha))) 100) 
                                        '(85 . 75) 
                                      '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
;; has to be client (emacsclient -c -a "" -nw), not (emacs -nw)
(defun on-frame-open 
    (&optional 
     frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame) 
    (set-face-background 'default "unspecified-bg" frame)))

(add-hook 'after-make-frame-functions 'on-frame-open)


;; https://www.emacswiki.org/emacs/KillingBuffers
(defun kill-other-buffers () 
  "Kill all other buffers." 
  (interactive) 
  (mapc 'kill-buffer (delq (current-buffer) 
                           (buffer-list))))

(defvar killed-file-list nil 
  "List of recently killed files.")

(defun add-file-to-killed-file-list () 
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun reopen-killed-file-fancy () 
  "Pick a file to revisit from a list of files killed during this
Emacs session." 
  (interactive) 
  (if killed-file-list (let ((file (completing-read "Reopen killed file: " killed-file-list nil nil
                                                    nil nil (car killed-file-list)))) 
                         (when file 
                           (setq killed-file-list (cl-delete file killed-file-list 
                                                             :test #'equal)) 
                           (find-file file))) 
    (error 
     "No recently-killed files to reopen")))

(defun reopen-killed-file () 
  "Reopen the most recently killed file, if one exists." 
  (interactive) 
  (when killed-file-list (find-file (pop killed-file-list))))

(setq org-html-htmlize-output-type 'css) 
(setq neo-theme (if (display-graphic-p) 'icons 'arrow)) 
(global-set-key [f8] 'neotree-toggle) 
(setq mu4e-hide-index-messages t) 
;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map) 
(define-key global-map [?\s-f] 'projectile-find-file)

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
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   (quote
    ("9594f82ed131d551c2a793028770cfe410ca8336407be3c84338bd63d673abfe" "f5e432ac29648b18acebda1058183de0ee797aa78f40552e88a3c143275ae30c" "cef4ac05a85b6e640ec0a4ac3cec95047e7164a824f15c6465684d9d7566b576" "35b0b0e531731e270708ddb342dc2e576a31fb298dcbc56a206596a43afac54f" "b0f0e2e4cc5d8e5e93dc9dabdb998ce8f7a4b63a68ce3cbf5e8e0525ed628e71" "0301a26dedfda81ca220ad6169588b5408884e7b4a5363f3e6a0e98d5c65a257" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "847575d12a9f396e050ddd45a350b8fc52c3f8fe914c2ea9e1fa8f06a7cfb4d9" "7ae3f88d0caa9db14f2a757755a47e572de7a6ba41780503e9a1f08cbb0802f0" "bf6940873299cc17e4339c96d7aac5a25855498379a4a11a6bc0dba47902ec35" "109d2e420f10339b151e22e452e7af5550118e941ac6d839e875a07c85c1003a" "99d1911fbea7d603989f7521a6c6e17b550c8d9ac37d5ee9b660941e37825c81" "7985ab0eaf8ed692055a9a3671b902afa09d26e6f384cfff5a5c3bb5b3d64cca" "14391f8e9773ce511b98b151d0655d73953068798fcb843cd67ef26e60c9f00f" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "5eda93d7e92808a69e771b02bf65b21ea6f2e94309bdc5135495e195bd7913e1" "f20795b6b18a6487168643337dbd3aa6b930b86b9d16c2407e2bd6d0d91d4ca4" "0556e4e9b305bc00f1a6e2c7a395ff981798d6ca6f22aa59062117a69ee642e2" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "5057614f7e14de98bbc02200e2fe827ad897696bfd222d1bcab42ad8ff313e20" "233bb646e100bda00c0af26afe7ab563ef118b9d685f1ac3ca5387856674285d" "72a097f48e588eaa08b17027ac20304dd3b3ea8ceaca4ca553fb2577b64f4d09" "3b5ce826b9c9f455b7c4c8bff22c020779383a12f2f57bf2eb25139244bb7290" "3cb2d5a795e1c93d1fbc8360d6ea41f0173aa1366d334b16e1b83b996b8d9ce6" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "4e4d9f6e1f5b50805478c5630be80cce40bee4e640077e1a6a7c78490765b03f" default)))
 '(debug-on-error nil)
 '(doc-view-continuous t)
 '(dumb-jump-mode t)
 '(evil-want-C-i-jump nil)
 '(fci-rule-color "#969896")
 '(global-company-mode t)
 '(global-display-line-numbers-mode t)
 '(global-linum-mode nil)
 '(global-visual-line-mode t)
 '(helm-mode t)
 '(indent-tabs-mode nil)
 '(ivy-mode t)
 '(ivy-rich-mode t)
 '(ledger-clear-whole-transactions t t)
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
 '(lisp-mode-hook
   (quote
    (#[nil "\300\301\302\303\211$\207"
           [add-hook font-lock-extend-region-functions sly-extend-region-for-font-lock t]
           5]
     common-lisp-lisp-mode-hook
     #[nil "\300\301\302\303\211$\207"
           [add-hook font-lock-extend-region-functions slime-extend-region-for-font-lock t]
           5]
     (lambda nil
       (local-set-key
        (kbd "<C-return>")
        (quote eir-eval-in-slime))))))
 '(nrepl-message-colors
   (quote
    ("#183691" "#969896" "#a71d5d" "#969896" "#0086b3" "#795da3" "#a71d5d" "#969896")))
 '(org-cycle-emulate-tab t)
 '(org-export-backends (quote (ascii html icalendar latex md odt org)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-org-htmlized-css-url "css")
 '(package-selected-packages
   (quote
    (hledger-mode racket-mode ob-rust all-the-icons elisp-format flycheck-rust java-imports pandoc-mode flymd dired-sidebar lsp-rust lsp-mode rust-mode deadgrep dired-ranger ranger el-get indent-guide magit equake guix jupyter git-time-metric transient helm-system-packages image+ multi-term treemacs nimbus-theme yasnippet undo-propose dumb-jump thread-dump counsel chip8 quelpa-use-package quelpa sr-speedbar rtags toc-org highlight-indent-guides git-gutter diff-hl prettier-js reformatter s "s" abyss-theme sane-term flycheck-ledger ledger-mode doom-modeline mu4e-conversation telephone-line session ob-tmux eyebrowse format-all rainbow-mode zone-sl zone-rainbow zone-nyan perspective golden-ratio android-mode elmacro rmsbolt swiper ace-jump-mode powerline-evil powerline esup auctex org-ref-pubmed org-ref-scopus org-ref-wos org-id org-ref org-mime pdf-tools weechat aggressive-indent smart-tabs-mode smart-tabs smooth-scrolling evil-mu4e mu4e highlight-indentation company-mode company ws-butler 0blayout anki-editor auto-complete hydra-ivy ivy-hydra smart-parens hydra projectile ob-sql-mode org-babel-eval-in-repl ivy-rich gnuplot-mode gnuplot sicp haskell-mode geiser chess github-theme htmlize which-key use-package smex slime shell-pop rotate rebecca-theme rainbow-delimiters paredit multiple-cursors general flycheck evil-leader dashboard)))
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
(put 'downcase-region 'disabled nil)
