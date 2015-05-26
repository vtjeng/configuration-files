;;;; PACKAGE MANAGEMENT

;;; Automatically Install Packages
;; source: http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
;; list the packages we want
;; NOTE THAT YOU NEED TO MANUALLY UPDATE THIS
(setq package-list '(cl-lib merlin monokai-theme rainbow-delimiters solarized-theme dash tuareg caml))

;; package manager, available in emacs version 24 onwards
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  ;; list the repositories containing these packages
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; activate all packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;; IDO MODE
;; activate ido mode
(require 'ido)
(ido-mode t)
;; this enables fuzzy matching. FUZZY MATCHING IS AMAZING.
(setq ido-enable-flex-matching t) 

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

;;;; START-UP
;; Makes window splitting default result in side-by-side buffers
;; source: http://stackoverflow.com/questions/6697514/when-opening-2-files-in-emacs-how-can-i-have-them-appear-side-by-side
(setq split-height-threshold nil) 
(setq split-width-threshold 0) 

;; gets rid of unpleasant looking welcome screen
(setq inhibit-startup-message t)


;;;; MISCELLANEOUS
;; Bind control-z to undo; we almost never need to suspend
(global-set-key (kbd "C-z") 'undo)

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

;; Display Visited File’s Path in the Frame Title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Show matching parenthesis
(show-paren-mode)

;; Rainbow-delimiters mode.
(add-hook 'tuareg-mode-hook 'rainbow-delimiters-mode)

;; Auto-indentation. The below example is from OCaml, but anything else is possible
(add-hook 'tuareg-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;; define function to help with toggling window splitting
;; source: http://www.emacswiki.org/emacs/ToggleWindowSplit
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


;;;; OCaml MAGIC 
;; gets Tuareg mode, for OCaml goodness
(require 'tuareg)

;; Setup environment variables using opam
(dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

;; Update the emacs path
(setq exec-path (append (parse-colon-path (getenv "PATH"))
                        (list exec-directory)))

;; Update the emacs load path
(add-to-list 'load-path (expand-file-name "../../share/emacs/site-lisp"
                                          (getenv "OCAML_TOPLEVEL_PATH")))

;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)

;;; Merlin setup
;; source: https://github.com/the-lambda-church/merlin
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'merlin)
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)
;; enables auto-complete in merlin buffers, adding merlin source to default sources
(setq merlin-use-auto-complete-mode 'easy)





;;;; THEMES and SKINS
;; sets the default theme to monokai
(load-theme 'monokai t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" default))))
