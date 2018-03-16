;;; Make Emacs less annoying

;; Disable backup files
(setq backup-inhibited t)

;; Disable auto save
(setq auto-save-default nil)

;; Change yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Display column numbers by default
(setq column-number-mode t)

;; Turn off the menu bar
(menu-bar-mode 0)

;; Scrolling behaviour
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 5)

;; Stop the screen from flashing
(setq ring-bell-function 'ignore)


;; Enable package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Auto install packages
(defvar packages '(better-defaults
                   idle-highlight-mode
                   ido-ubiquitous
                   magit
                   smex
                   multiple-cursors
                   ggtags
                   monokai-theme))
(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; All themes are stupid
(load-theme 'monokai t)

;; Winner mode
;;(when (fboundp 'winner-mode)
;;  (winner-mode 1)
;;  (windmove-default-keybindings))

;; Whitespace mode
(global-whitespace-mode 1)
(setq whitespace-line-column 80)
(setq whitespace-style '(face trailing lines-tail empty))

;; All kinds of ido
(require 'ido-ubiquitous)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'idle-highlight-mode)
(idle-highlight-mode 1)
(add-hook 'prog-mode-hook #'idle-highlight-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

;; Speedbar
;;(require 'sr-speedbar)
;;(setq sr-speedbar-right-side nil)
;;(setq sr-speedbar-width-console 80)
;;(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
;;(setq speedbar-show-unknown-files t)

;; C++ stuff
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-default-style "linux" c-basic-offset 4)

;; Switch between hpp and cpp
(global-set-key (kbd "C-c TAB") 'ff-find-other-file)

;; Compile hotkey
(global-set-key (kbd "<f10>") 'recompile)

;; Pretty-print a screwed up JSON file
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e "python -mjson.tool" (current-buffer) t)))

;; Handle coloured output in compile buffer
;; (Mainly for Catch errors)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Split window vertically or horizontally
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(global-set-key (kbd "C-x |") 'toggle-window-split)

;; ggtags
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; Spellcheck in markdown mode
(add-hook 'markdown-mode-hook 'flyspell-mode)
