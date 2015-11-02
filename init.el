;; PACKAGE INSTALLATION

;; make sure that we have the packages I want
(setq package-list '(helm helm-core markdown-mode monokai-theme))

;; I'm not super sure exactly what this is doing, but
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; STARTUP

;; uses the desktop library to save the state of Emacs from one session to another
(desktop-save-mode 1)

;; Start Emacs fullscreen
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; gets rid of unpleasant looking welcome screen
(setq inhibit-startup-message t)

;; MARKDOWN FUNCTIONALITY

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; OTHER MODIFICATIONS

;; Display Visited File's Path in the Frame Title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; trying out helm mode, like I had at Jane Street
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

;; visit recent files using the ido mode
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

;; turns alarms off entirely 
(setq ring-bell-function 'ignore)

;; Bind control-z to undo; we almost never need to suspend
(global-set-key (kbd "C-z") 'undo)

;; More Janestreety stuff
(global-set-key (kbd "C-x C-c") 'compile)

;; Bind control-/ to commenting and uncommenting since it is no longer
;; required for undo
 (defun comment-or-uncomment-current-line-or-region ()
   "Comments or uncomments current current line or whole lines in region."
   (interactive)
   (save-excursion
     (let (min max)
       (if (region-active-p)
           (setq min (region-beginning) max (region-end))
         (setq min (point) max (point)))
       (comment-or-uncomment-region
        (progn (goto-char min) (line-beginning-position))
        (progn (goto-char max) (line-end-position))))))

 (global-set-key (kbd "C-/") 'comment-or-uncomment-current-line-or-region)

;; Show matching parenthesis
(show-paren-mode)

;; sets the default theme to monokai
(load-theme 'monokai t)

;; use windmove to allow switching to window next to the currently active one
;; uses the shift (not the super) button
(windmove-default-keybindings)
