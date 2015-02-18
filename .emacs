; Disable backup files
(setq backup-inhibited t)

; Disable auto save
(setq auto-save-default nil)

; Change yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

; Package management
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Scrolling behaviour
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 5)

;; Display column numbers by default                                                                                                                         
(setq column-number-mode t)

;; Turn off the menu bar
(menu-bar-mode 0)

;; Winner mode                                                                                                                                              
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (windmove-default-keybindings))

;; Whitespace mode
(setq whitespace-line-column 120)
(global-whitespace-mode 1)

;; Speedbar
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width-console 80)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
(setq speedbar-show-unknown-files t)

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

