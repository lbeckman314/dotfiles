;; ivy magit evil-magit rebecca slime rotate
(setq htmlize-output-type 'css)
(global-set-key (kbd "C-S-c") #'evil-copy)
(global-set-key (kbd "C-S-c") #'evil-copy)
(global-set-key (kbd "C-S-c") #'copy-to-clipboard)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq evil-want-C-i-jump nil)
;; https://blog.aaronbieber.com/2015/05/24/from-vim-to-emacs-in-fourteen-days.html
;; https://www.emacswiki.org/emacs/Evil#toc1
;;(require 'package)

;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;;(setq package-enable-at-startup nil)
;;(package-initialize)

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

;; https://github.com/jwiegley/use-package
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))


(global-evil-leader-mode 1)

  (require 'evil)
  (evil-mode 1)




;; https://www.emacswiki.org/emacs/TransparentEmacs

 ;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
 ;;(set-frame-parameter (selected-frame) 'alpha <both>)
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



;; https://www.emacswiki.org/emacs/ToolBar
;; https://www.emacswiki.org/emacs/MenuBar#toc7
(tool-bar-mode -1)
(menu-bar-mode -1)


;; https://github.com/vic/rebecca-theme
   (load-theme #'dracula t)



;; https://github.com/abo-abo/swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)




;; https://github.com/emacs-helm/helm/wiki#from-melpa
;; (require 'helm-config)
;; (helm-mode 1)


(global-linum-mode 1)

;; https://github.com/emacs-evil/evil-magit
;; optional: this is the evil state that evil-magit will use
;; (setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
;; (setq evil-magit-use-y-for-yank nil)
(require 'evil-magit)


(show-paren-mode 1)

(scroll-bar-mode -1)


(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
 "f" 'find-file
 "gs" 'magit-status
 "w+" 'rotate-layout
 "l" 'ivy-switch-buffer
 "!" 'eshell-here
 "'" 'shell-pop
 "bd" 'evil-delete-buffer
 )
 ; Bindings under the leader key defined here...
;; bind a key globally in normal state; keymaps must be quoted
;; (setq general-default-keymaps 'evil-normal-state-map 'evil-motion-state-map 'dired-mode-map)
;; (setq general-default-keymaps 'evil-motion-state-map)

;; named prefix key
;; (setq my-leader1 "SPC")
;; (general-define-key :prefix my-leader1
                    ;; "f" 'find-file)
;; (general-define-key :prefix my-leader1
                    ;; "gs" 'magit-status)
;; (general-define-key :prefix my-leader1
                    ;; "w+" 'rotate-layout)
;; (general-define-key :prefix my-leader1
                    ;; "l" 'ivy-switch-buffer)

;; https://stackoverflow.com/questions/1229142/how-can-i-save-my-mini-buffer-history-in-emacs
(savehist-mode 1)

(require 'which-key)
(which-key-mode)

(require 'shell-pop)


;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
(defun on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

(add-hook 'after-make-frame-functions 'on-frame-open)

;; https://stackoverflow.com/questions/10545437/how-to-disable-the-beep-in-emacs-on-windows
(setq visible-bell 1)

;; https://github.com/rakanalh/emacs-dashboard
;;(require 'dashboard)
;;(dashboard-setup-startup-hook)
;;
;;(use-package dashboard
;;    :ensure t
;;    :diminish dashboard-mode
;;    :config
;;    (setq dashboard-banner-logo-title "hello! :)")
;;    ;; (setq dashboard-startup-banner "/home/liam/Pictures/ChickenJoeSmaller.png")
;;    (setq dashboard-startup-banner "/home/liam/Pictures/emacs.png")
;;    (setq dashboard-items '((recents  . 10)
;;                            (bookmarks . 10)))
;;    (dashboard-setup-startup-hook))


;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

;; http://pragmaticemacs.com/emacs/pop-up-a-quick-shell-with-shell-pop/
(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;; http://pragmaticemacs.com/emacs/make-all-prompts-y-or-n/
;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; https://github.com/milkypostman/powerline
;; (require 'powerline-evil)
;; (powerline-evil-vim-color-theme)

;; i like square borders in powerline
;; (setq powerline-evil-default-separator 'nil)

;; https://github.com/syl20bnr/spacemacs/issues/5140
;; (setq split-height-threshold 'nil)
;; (setq split-width-threshold 0)


(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US")


;; https://github.com/nonsequitur/smex/
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
        ; when Smex is auto-initialized on its first run.



(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(setq evil-want-C-i-jump nil)


(setq tab-stop-list (number-sequence 4 200 4))

;; http://www.flycheck.org/en/latest/#try-out
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("MELPA Stable" . "http://stable.melpa.org/packages/") t)
;; (package-initialize)
;; (package-refresh-contents)
;;
;; (package-install 'flycheck)
;;
;; (global-flycheck-mode)


;; https://www.reddit.com/r/emacs/comments/7v6fll/whats_in_your_initialscratchmessage/
(setq initial-scratch-message
    ";; - 'Tis but a scratch!\n;; - A scratch? Your arm's off!\n;; - No, it isn't!\n\n")

;; https://superuser.com/questions/602510/how-to-insert-tab-character-in-text-mode
(global-set-key (kbd "TAB") 'self-insert-command);

;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/Documents/personal.org")))

;; https://emacs.stackexchange.com/questions/17673/no-org-babel-execute-function-for-c-and-no-org-babel-execute-function-for-c
(org-babel-do-load-languages
 'org-babel-load-languages '(
                             (C . t)
                             (sh . t)
                             (js . t)
                             ))

;; http://ergoemacs.org/emacs/emacs_org_babel_literate_programing.html
(require 'org)
(require 'ob)

;; make org mode allow eval of some langs
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (python . t)
   (ruby . t)))

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)


(global-set-key (kbd "C-S-c") #'evil-copy)
(global-set-key (kbd "C-S-v") #'evil-paste-before)

(setq htmlize-output-type 'css)
(setq org-html-htmlize-output-type 'css)

;; Include the latex-exporter
(require 'ox-latex)
;; Add minted to the defaults packages to include when exporting.
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; Tell the latex export to use the minted package for source
;; code coloration.
(setq org-latex-listings 'minted)
;; Let the exporter use the -shell-escape option to let latex
;; execute external programs.
;; This obviously and can be dangerous to activate!
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


(setq org-html-validation-link 'nil)
(global-visual-line-mode t)

;; https://www.emacswiki.org/emacs/BackupDirectory#toc2
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "$HOME/.emacs.d/backupdir/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

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
 '(org-export-backends (quote (ascii html icalendar latex md odt org)))
 '(org-modules
        (quote
         (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages
        (quote
         (github-theme htmlize which-key use-package smex slime shell-pop rotate rebecca-theme rainbow-delimiters powerline-evil paredit multiple-cursors ivy general flycheck evil-magit evil-leader dracula-theme dashboard)))
 '(tab-width 4)
 '(tls-checktrust (quote ask)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



