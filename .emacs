;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("f87c86fa3d38be32dc557ba3d4cedaaea7bc3d97ce816c0e518dfe9633250e34"
     default))
 '(desktop-save-mode t)
 '(save-place-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Arial" :foundry "outline" :slant normal :weight regular :height 120 :width normal)))))




;;; -*- lexical-binding: t -*-

(custom-set-variables
 '(custom-enabled-themes '(zenburn)))
(custom-set-faces)

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
;; Build build.bat
;;
;; ---------- Build: persistent CMD with MSVC env ----------
(require 'comint)

(defvar my/build-shell-buffer "*Build*")
(defvar my/build-shell-initialized nil)

(defconst my/project-dir "C:/Users/trm00/Documents/handmaiden/code")
(defconst my/vcvars
  "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Auxiliary/Build/vcvars64.bat")

(defun my/run-build ()
  "Run build.bat in a persistent cmd shell with MSVC env set once.
Shows *Build* if it didn't exist; otherwise stays in the current code buffer.
Always clears old output before starting."
  (interactive)
  (let* ((here (current-buffer))
         (created nil)
         (buf (get-buffer my/build-shell-buffer)))

    ;; Create the build shell if missing
    (unless (and buf (comint-check-proc buf))
      (setq buf (shell my/build-shell-buffer))
      (setq created t)
      (with-current-buffer buf
        ;; Ensure the shell starts in the project dir
        (setq default-directory my/project-dir)))

    ;; Send commands
    (with-current-buffer buf
      (read-only-mode -1)
      (let ((proc (get-buffer-process buf)))
        ;; start fresh output
        (erase-buffer)
        ;; Initialize MSVC env once per Emacs session
        (unless my/build-shell-initialized
          (comint-send-string proc (format "call \"%s\"\r\n" my/vcvars))
          (setq my/build-shell-initialized t))
        ;; Always cd to project before building
        (comint-send-string proc (format "cd /d \"%s\"\r\n" my/project-dir))
        ;; Kick the build
        (comint-send-string proc "build.bat\r\n")))

    ;; Show the buffer only the first time; otherwise leave user in place
    (if created
        (display-buffer buf)
      (message "Build started in %s" my/build-shell-buffer))

    ;; stay in the code buffer if it already existed
    (switch-to-buffer here)))

(global-set-key (kbd "M-m") #'my/run-build)


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
