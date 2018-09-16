;; Load the emacs themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/cyberpunk-theme.el")

;; Elpy Python package.
(package-initialize)
(elpy-enable)
;; (package-initialize) should already exist at the top of the init
;; file on Debian-derived systems, thus (elpy-enable) should be all
;; that is required.

;; Making it easier to navigate the emacs windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Have auto-complete by default
(ac-config-default)

;; Setup for Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Turn off the menu-bar
(menu-bar-mode -1)

;; Turn off the tool-bar
(tool-bar-mode -1)

;; Turn off scroll-bar
(scroll-bar-mode -1)

;; Change the tab-width
(setq-default c-basic-offset 4)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; Have highlighting of parantheses
(show-paren-mode 1)

;; Put in a line limit
;; (require 'whitespace)
;; (setq whitespace-line-column 73) ;; limit line length
;; (setq whitespace-style '(face lines-tail))

;; (add-hook 'prog-mode-hook 'whitespace-mode)
;; Make the lines better.
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-modes
   (quote
    (emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode nim-mode c-mode cc-mode c++-mode objc-mode swift-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js-jsx-mode js2-mode js2-jsx-mode coffee-mode php-mode css-mode scss-mode less-css-mode elixir-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode apples-mode)))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
    ("7e3890e2f54212c6929eb6bab6024a9afd6414324e45ad48b6a6ec9fb2029306" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b70a313dd20e65976564b4e19b91db94e8b8482b799bcbc55176c938f9eeb54" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(package-selected-packages (quote (magit elpy markdown-mode auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Enables MELPA packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Have auto-complete by default
;; (ac-config-default)

