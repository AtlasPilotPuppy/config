(setq-default indent-tabs-mode nil)
(add-to-list 'load-path "/home/anant/.emacs.d")
(require 'python-mode)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(require 'ipython)
(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
(require 'anything) (require 'anything-ipython)
  (when (require 'anything-show-completion nil t)
     (use-anything-show-completion 'anything-ipython-complete
                                   '(length initial-pattern)))

;;autocomplete stuff
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(global-auto-complete-mode t)


;;autopair

;; (autoload 'autopair-global-mode "autopair" nil t)
;;   (autopair-global-mode)
;;   (add-hook 'lisp-mode-hook #'(lambda () (setq autopair-dont-activate t)))
;; (add-hook 'python-mode-hook
;;           #'(lambda () (push '(?' . ?')
;;                               (getf autopair-extra-pairs :code))
;;  (setq autopair-handle-action-fns
;;       (list #'autopair-default-handle-action
;;             #'autopair-python-triple-quote-action))))



;;white space mode
(require 'whitespace)
(setq whitespace-line-column 80
      whitespace-style '(face tabs trailing lines-tail))
;; display only tails of lines longer than 80 columns, tabs and
;; trailing whitespaces
(setq whitespace-line-column 80
      whitespace-style '(tabs trailing lines-tail))
;; face for long lines' tails
(set-face-attribute 'whitespace-line nil
                    :background "red1"
                    :foreground "yellow"
                    :weight 'bold)
;; face for Tabs
(set-face-attribute 'whitespace-tab nil
                    :background "red1"
                    :foreground "yellow"
                    :weight 'bold)

;; activate minor whitespace mode when in python mode
(add-hook 'python-mode-hook 'whitespace-mode)

(setq whitespace-line-column 80
      whitespace-style '(face tabs trailing lines-tail))

;;pylookup

(setq pylookup-dir "~/.emacs.d")
(add-to-list 'load-path pylookup-dir)
(eval-when-compile (require 'pylookup))
(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")
(setq pylookup-program "~/.emacs.d/pylookup.py")
(setq pylookup-db-file "~/.emacs.d/pylookup.db")
(global-set-key "\C-ch" 'pylookup-lookup)

;;ack-grep
    (require 'ack-and-a-half)
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;;git-emacs
(load "~/.emacs.d/git-emacs/git.el")
(load "~/.emacs.d/git-emacs/git-blame.el")
(load "~/.emacs.d/git-emacs/vc-git.el")
(add-to-list 'vc-handled-backends 'GIT)

;; ;;cedet

;; (load-file "~/.emacs.d/cedet-1.0/common/cedet.el")
;; (global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;; (global-srecode-minor-mode 1)            ; Enable template insertion menu

;; ;;ecb

;; (add-to-list 'load-path
;; 	     "~/.emacs.d/ecb-2.40")

;; (require 'ecb)
;; (require 'ecb-autoloads)


;; package manager 
(require 'package)
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
;;elpy

(package-initialize)
(elpy-enable)

;;iy-go-to-char

    (require 'iy-go-to-char)
;;
;; Then you can bind functions like:
;;
    (global-set-key (kbd "C-c f") 'iy-go-to-char)
    (global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
    (global-set-key (kbd "C-c ;") 'iy-go-to-char-continue)
    (global-set-key (kbd "C-c ,") 'iy-go-to-char-continue-backward)


;;pgsql 
(require 'pg)

;;MELPA

(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; previous window
(global-set-key (kbd "C-x O") 'previous-multiframe-window)
;;rebind tags
(global-set-key (kbd "M-.") 'find-tag)
;;django-mode
(require 'python-django)

;; trampmode setup

(setq tramp-default-method "ssh")


;; go mode

(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((test-case-name . twisted\.protocols\.test\.test_basic)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
