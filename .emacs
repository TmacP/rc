;;; -*- lexical-binding: t; -*-

;; ------------------------------
;; Custom Variables
;; ------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 '(desktop-save-mode t)
 '(package-selected-packages '(magit))
 '(save-place-mode t))

(setq visible-bell t)
(global-hl-line-mode t)

;; Minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'leuven t)

;; ------------------------------
;; ANSI color in compile buffers
;; ------------------------------
(require 'ansi-color)
(defun my/colorize-compilation-buffer ()
  "Colorize ANSI escape sequences in compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)

;; ------------------------------
;; Compilation key: M-m
;; ------------------------------

;; Make this your default compile command
(setq compile-command "make")
(setq compilation-ask-about-save nil)
(setq compilation-scroll-output t)

(defun my/compile-here ()
  "Run `make` in current buffer's directory."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory "Makefile")
			       default-directory)))
    (compile compile-command)))

(global-set-key (kbd "M-m") #'my/compile-here)

;; ------------------------------
;; Show compilation buffer on build, hide on success
;; ------------------------------
(defvar my/build-start-time nil)

(add-hook 'compilation-start-hook
	  (lambda (proc)
	    (setq my/build-start-time (float-time))
	    (setq next-error-last-buffer (process-buffer proc))
	    (display-buffer (process-buffer proc))))

(add-hook 'compilation-finish-functions
	  (lambda (buf msg)
	    (let ((secs (when my/build-start-time
			  (- (float-time) my/build-start-time))))
	      (when secs
		(message "Build: %s (%.2fs)" (string-trim msg) secs)))
	    (if (string-match-p "\\`finished" msg)
		(run-at-time 0.15 nil
			     (lambda (b)
			       (when (buffer-live-p b)
				 (when-let ((win (get-buffer-window b))
					    )
				   (quit-window nil win))))
			     buf)
	      (display-buffer buf))))

;; Make *compilation* buffer show at bottom
(add-to-list 'display-buffer-alist
	     '("\\*compilation\\*"
	       (display-buffer-reuse-window display-buffer-at-bottom)
	       (window-height . 0.25)))

;; ------------------------------
;; Git helpers (unchanged)
;; ------------------------------
(defun my/run-shell-on-file (cmd)
  "Run shell CMD with % replaced by current file, show output."
  (interactive)
  (if buffer-file-name
      (let* ((file (shell-quote-argument buffer-file-name))
	     (expanded (replace-regexp-in-string "%" file cmd))
	     (shell (executable-find "bash"))  ;; use bash if available
	     (buf (get-buffer-create "*Shell Output*")))
	(with-current-buffer buf (erase-buffer))
	(call-process shell nil buf nil "-c" expanded)
	(with-current-buffer buf
	  (ansi-color-apply-on-region (point-min) (point-max)))
	(display-buffer buf))
    (message "No file associated with buffer.")))

(defun my/git-diff ()     (interactive) (my/run-shell-on-file "git diff %"))
(defun my/git-status ()   (interactive) (my/run-shell-on-file "git status -sb"))
(defun my/git-add ()      (interactive) (my/run-shell-on-file "git add %"))
(defun my/git-commit ()
  (interactive)
  (let ((msg (read-string "Commit message: ")))
    (my/run-shell-on-file (format "git commit -m \"%s\"" msg))))
(defun my/git-pushpull () (interactive) (my/run-shell-on-file "git pull && git push"))
(defun my/git-log ()      (interactive) (my/run-shell-on-file "git log %"))

(global-unset-key (kbd "M-g"))
(define-prefix-command 'my/meta-g-map)
(global-set-key (kbd "M-g") 'my/meta-g-map)
(define-key my/meta-g-map (kbd "d") #'my/git-diff)
(define-key my/meta-g-map (kbd "s") #'my/git-status)
(define-key my/meta-g-map (kbd "a") #'my/git-add)
(define-key my/meta-g-map (kbd "c") #'my/git-commit)
(define-key my/meta-g-map (kbd "p") #'my/git-pushpull)
(define-key my/meta-g-map (kbd "l") #'my/git-log)

;; ------------------------------
;; Handmade Hero-style indentation
;; ------------------------------
(require 'cc-mode)

(defun casey-big-fun-c-hook ()
  (c-set-style "bsd")
  (setq tab-width 4
	indent-tabs-mode nil
	c-basic-offset 4)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open 0)
  (c-set-offset 'statement-case-intro '++)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'member-init-intro '++)
  (c-toggle-auto-hungry-state -1)
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop))))

(add-hook 'c-mode-common-hook 'casey-big-fun-c-hook)
(define-key c-mode-base-map (kbd "TAB") 'dabbrev-expand)

;; ------------------------------
;; next-error M-n / M-p
;; ------------------------------
(add-hook 'compilation-start-hook
	  (lambda (proc)
	    (setq next-error-last-buffer (process-buffer proc))))
(global-set-key (kbd "M-n") #'next-error)
(global-set-key (kbd "M-p") #'previous-error)
(setq compilation-skip-threshold 0)
(setq next-error-recenter 0)
(setq compilation-parse-errors-filename-function
      (lambda (f) (and f (subst-char-in-string ?\\ ?/ f))))
(with-eval-after-load 'compile
  (dolist (sym '(msvc gnu))
    (add-to-list 'compilation-error-regexp-alist sym)))

;; ------------------------------
;; Narrow / Widen helpers
;; ------------------------------
(put 'narrow-to-region 'disabled nil)

(defun my/toggle-narrow ()
  (interactive)
  (cond
   ((buffer-narrowed-p) (widen) (message "Widened"))
   ((use-region-p) (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark) (message "Narrowed to region"))
   (t (narrow-to-defun) (message "Narrowed to defun"))))

(defvar my/narrow-map (make-sparse-keymap))
(define-key my/narrow-map (kbd "t") #'my/toggle-narrow)
(define-key my/narrow-map (kbd "n") #'narrow-to-region)
(define-key my/narrow-map (kbd "d") #'narrow-to-defun)
(define-key my/narrow-map (kbd "p") #'narrow-to-page)
(define-key my/narrow-map (kbd "w") #'widen)
(global-set-key (kbd "C-c n") my/narrow-map)
(global-set-key (kbd "M-RET") #'my/toggle-narrow)

;; ------------------------------
;; Faces
;; ------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 )


;; ------------------------------
;; Fill column indicator at 80
;; ------------------------------
(setq-default fill-column 80)

;; enable globally
(global-display-fill-column-indicator-mode t)
 
