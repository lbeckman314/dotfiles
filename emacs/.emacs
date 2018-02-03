;; ivy magit evil-magit rebecca rotate general


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
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


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
   (load-theme #'rebecca t)



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


;; https://github.com/noctuid/general.el
(require 'general)
;; bind a key globally in normal state; keymaps must be quoted
;; (setq general-default-keymaps 'evil-normal-state-map)

;; named prefix key
;; (setq my-leader1 "SPC")
;; (general-define-key :prefix my-leader1
;;                    "f" 'find-file)

;; a default prefix sequence
;; (setq general-default-prefix "SPC")
;;    (general-define-key "f" 'find-file)
;;    (general-define-key "gs" 'magit-status)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "4e4d9f6e1f5b50805478c5630be80cce40bee4e640077e1a6a7c78490765b03f" default)))
 '(package-selected-packages
   (quote
    (rotate rainbow-delimiters slime evil-magit helm ivy magit dracula-theme rebecca-theme evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )