;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tlm))
 '(custom-safe-themes
   '("35d0fc3040202a3797a65fb1b2c6775287120023b4ee968001d7762f592265a8"
     "68d998fb631ce32263e8957f2dc160487e99dee33c691dee0e0797936cc7451a"
     "f87c86fa3d38be32dc557ba3d4cedaaea7bc3d97ce816c0e518dfe9633250e34"
     "c51a5fc0a64d75e040836ddc17229ec48d46fd5c24785f1d5c1af10417404bc3"
     "4fb39d013dc68bd8ba63be34bad47c272f07f7e2244fa1855d51461d17205aae"
     "26b3cce73c2507647c2e4d1ad8f808ed8cc8e1f64643b13769f7fa343501f438"
     default))
 '(desktop-save-mode t)
 '(save-place-mode t)
 '(tool-bar-mode nil))

(setq visible-bell t)
(global-hl-line-mode t)

;;; -*- lexical-binding: t -*-




;; ------------------------------
;; Minimal UI
;; ------------------------------
;;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; ------------------------------
;; Git command helper
;; ------------------------------
(require 'ansi-color)

(defun my/run-shell-on-file (cmd)
  "Run shell CMD with % replaced by current file, using Git Bash, and show output."
  (interactive)
  (if buffer-file-name
      (let* ((file (shell-quote-argument buffer-file-name))
             (expanded (replace-regexp-in-string "%" file cmd))
             (bash-path "C:/Program Files/Git/bin/bash.exe")
             (buf (get-buffer-create "*Shell Output*")))
        (with-current-buffer buf
          (erase-buffer))
        ;; Run in Git Bash login shell
        (call-process bash-path nil buf nil "-lc" expanded)
        ;; Colorize ANSI escape codes from git
        (with-current-buffer buf
          (ansi-color-apply-on-region (point-min) (point-max)))
        ;; Show buffer
        (display-buffer buf))
    (message "No file associated with buffer.")))

;; ------------------------------
;; Git macros
;; ------------------------------
(defun my/git-diff ()     (interactive) (my/run-shell-on-file "git diff %"))
(defun my/git-status ()   (interactive) (my/run-shell-on-file "git status -sb"))
(defun my/git-add ()      (interactive) (my/run-shell-on-file "git add %"))
(defun my/git-commit ()
  "Prompt for git commit message."
  (interactive)
  (let ((msg (read-string "Commit message: ")))
    (my/run-shell-on-file (format "git commit -m \"%s\"" msg))))
(defun my/git-pushpull () (interactive) (my/run-shell-on-file "git pull && git push"))
(defun my/git-log ()      (interactive) (my/run-shell-on-file "git log %"))

;; ------------------------------
;; Bind macros to M-g keymap
;; ------------------------------
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
;; Optional: Show startup time
;; ------------------------------
(add-hook 'emacs-startup-hook
          (lambda () (message "Emacs ready in %s" (emacs-init-time))))


;;
;; BUILD two
;;
;;; --- Import MSVC env once into Emacs ---

(defvar my/msvc-env-imported nil)

(defconst my/vcvars
  "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Auxiliary/Build/vcvars64.bat")

(defun my/msvc-import-env ()
  "Call vcvars64.bat and copy its environment into Emacs (once)."
  (interactive)
  (unless my/msvc-env-imported
    (let* ((cmd (format "cmd.exe /c \"call \"%s\" >nul & set\"" my/vcvars))
           (out (with-temp-buffer
                  (call-process-shell-command cmd nil (current-buffer))
                  (buffer-string))))
      (dolist (line (split-string out "[\r\n]+" t))
        (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" line)
          (setenv (match-string 1 line) (match-string 2 line))))
      ;; Keep exec-path in sync with PATH
      (setq exec-path
            (append (split-string (getenv "PATH") path-separator t)
                    (list exec-directory)))
      (setq my/msvc-env-imported t)
      (message "MSVC environment imported."))))

;;; --- Minibuffer-driven compile that uses the imported env ---

(defconst my/project-dir "C:/Users/trm00/Documents/handmaiden/code")

(setq compile-command "build.bat")            ;; what C-x p c / M-m will run by default
(setq compilation-ask-about-save nil)         ;; save without asking
(setq compilation-scroll-output t)            ;; follow output

(defun my/compile-here ()
  "Import MSVC env once, set project dir, then run `compile`."
  (interactive)
  (my/msvc-import-env)
  (let ((default-directory my/project-dir))
    (compile compile-command)))

;; Keep focus in your code buffer; put *compilation* at bottom and don’t steal point
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.25)))

(global-set-key (kbd "M-m") #'my/compile-here)  ;; your M-m key
;; or: (global-set-key (kbd "M-m") #'project-compile) if you prefer project.el


;; ------------------------------
;; Handmade Hero style indentation
;; ------------------------------
(require 'cc-mode)

(defun casey-big-fun-c-hook ()
  "Automatic Casey-style indentation for C/C++."
  (c-set-style "bsd")
  (setq tab-width 4
        indent-tabs-mode nil
        c-basic-offset 4)

  ;; Casey-style switch/case
  (c-set-offset 'case-label '+)            ;; indent case one level in from {
  (c-set-offset 'statement-case-open 0)    ;; { sits under case
  (c-set-offset 'statement-case-intro '++) ;; code inside case indents another level

  ;; Function call args
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-close 0)

  ;; Member init style
  (c-set-offset 'member-init-intro '++)

  ;; Disable hungry delete
  (c-toggle-auto-hungry-state -1)

  ;; Bind Enter to newline + indent in C++
  (define-key c++-mode-map "\C-m" 'newline-and-indent)

  ;; Prevent hanging commas from affecting formatting
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop))))

(add-hook 'c-mode-common-hook 'casey-big-fun-c-hook)


;; Bind Tab to dynamic expansion in C/C++ modes
(define-key c-mode-base-map (kbd "TAB") 'dabbrev-expand)


;;
;; NEXT ERROR
;;
;;; Fix/expand Windows error parsing so clicks & jumps go straight to files
;; ---------- NEXT ERROR (minimal with /FC) ----------
;; MSVC prints absolute paths with /FC, so Emacs can open them directly.

;; Make M-n / M-p work from *any* buffer by remembering the newest compilation buf
(add-hook 'compilation-start-hook
          (lambda (proc)
            (setq next-error-last-buffer (process-buffer proc))))

;; Jump through hits
(global-set-key (kbd "M-n") #'next-error)
(global-set-key (kbd "M-p") #'previous-error)

;; Optional: control what counts as a “hit”
;; 0=visit all, 1=skip notes, 2=skip warnings+notes
(setq compilation-skip-threshold 0)

;; Optional: avoid recentering when jumping
(setq next-error-recenter 0)

;; Optional: normalize backslashes (harmless with /FC but nice on Windows)
(setq compilation-parse-errors-filename-function
      (lambda (f) (and f (subst-char-in-string ?\\ ?/ f))))

;; Ensure built-in MSVC/gnu regexps are active (usually already are)
(with-eval-after-load 'compile
  (dolist (sym '(msvc gnu))
    (add-to-list 'compilation-error-regexp-alist sym)))



;;
;; AUTO HIDE BUILD 
;;
;;; Show *compilation* at start, hide on success, keep visible on errors

(defvar my/build-start-time nil)

;; Always show the compilation window when a build starts (don’t steal focus)
(add-hook 'compilation-start-hook
          (lambda (proc)
            (setq my/build-start-time (float-time))
            (setq next-error-last-buffer (process-buffer proc))
            (display-buffer (process-buffer proc))))

(add-hook 'compilation-finish-functions
          (lambda (buf msg)
            ;; timing message
            (let ((secs (when my/build-start-time
                          (- (float-time) my/build-start-time))))
              (when secs
                (message "Build: %s (%.2fs)" (string-trim msg) secs)))
            (if (string-match-p "\\`finished" msg)
                ;; success: briefly show status, then hide the window
                (run-at-time 0.15 nil
                             (lambda (b)
                               (when (buffer-live-p b)
                                 (when-let ((win (get-buffer-window b)))
                                   (quit-window nil win))))
                             buf)
              ;; failure (nonzero exit): ensure the window is visible and stays
              (display-buffer buf))))


;;
;; widen narrow
;;

;; --- Narrow / Widen helpers ---
;; Allow narrowing without the safety prompt
(put 'narrow-to-region 'disabled nil)

(defun my/toggle-narrow ()
  "Smart narrow/widen:
   - If already narrowed -> widen
   - If region active    -> narrow to region
   - Else                -> narrow to defun"
  (interactive)
  (cond
   ((buffer-narrowed-p)
    (widen) (message "Widened"))
   ((use-region-p)
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark)
    (message "Narrowed to region"))
   (t
    (narrow-to-defun)
    (message "Narrowed to defun"))))

;; Prefix map on C-c n
(defvar my/narrow-map (make-sparse-keymap) "Keymap for narrowing commands.")
(define-key my/narrow-map (kbd "t") #'my/toggle-narrow)  ;; C-c n t -> smart toggle
(define-key my/narrow-map (kbd "n") #'narrow-to-region)  ;; C-c n n
(define-key my/narrow-map (kbd "d") #'narrow-to-defun)   ;; C-c n d
(define-key my/narrow-map (kbd "p") #'narrow-to-page)    ;; C-c n p
(define-key my/narrow-map (kbd "w") #'widen)             ;; C-c n w
(global-set-key (kbd "C-c n") my/narrow-map)

;; Also bind a single Meta key in programming buffers
(global-set-key (kbd "M-RET") #'my/toggle-narrow)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
