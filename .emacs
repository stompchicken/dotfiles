; Disable backup files
(setq backup-inhibited t)

; Disable auto save
(setq auto-save-default nil)

; Change yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

; Package management
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

; Scrolling behaviour
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 5)

;; Turn off the menu bar
(menu-bar-mode 0)

;; Whitespace mode
(setq whitespace-line-column 120)
(global-whitespace-mode 1)

;; C++ stuff
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-default-style "linux" c-basic-offset 4)
;; Switch between hpp and cpp
(global-set-key (kbd "C-c TAB") 'ff-find-other-file)
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
