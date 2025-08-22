;;; tlm-theme.el --- GBC 4-color theme, simple & clean -*- lexical-binding: t; -*-

(deftheme tlm
  "A gbc theme using a 4-color palette.")

(let* (
       ;; === Color Palette ===
       (purple "#84739C")
       (font   "#AC9FAC")
       (peach  "#725050")
       (dark   "#343030")

       ;; === Semantic Face Colors ===
       (bg-default dark)
       (fg-default font)
       (bg-hl-line peach)
       (bg-region font)
       (fg-region dark)  ;; text visible on selection
       (fg-comments purple)
       (fg-keywords font)
       (fg-strings  font)
       (fg-constants font)
       (fg-func     font)
       (bg-modeline dark)
       (fg-modeline font))

  (custom-theme-set-faces
   'tlm

   ;; ---------- Base / UI ----------
   `(default ((t (:background ,bg-default :foreground ,fg-default :family "Arial" :height 120))))
   `(cursor  ((t (:background ,peach))))
   `(region  ((t (:extend t :background ,bg-region :foreground ,fg-region))))
   `(highlight ((t (:inherit region))))
   `(hl-line ((t (:extend t :background ,bg-hl-line))))
   `(fringe  ((t (:background ,bg-default))))
   `(vertical-border ((t (:foreground ,peach))))
   `(minibuffer-prompt ((t (:foreground ,peach :weight bold))))
   `(mode-line ((t (:background ,bg-modeline :foreground ,fg-modeline))))
   `(mode-line-inactive ((t (:background ,bg-default :foreground ,fg-default))))
   `(mode-line-buffer-id ((t (:weight bold :foreground ,fg-modeline))))
   `(mode-line-emphasis  ((t (:weight bold :foreground ,fg-modeline))))
   `(header-line ((t (:background ,bg-modeline :foreground ,fg-modeline))))

   ;; ---------- Syntax ----------
   `(font-lock-builtin-face         ((t (:foreground ,fg-keywords))))
   `(font-lock-comment-face         ((t (:foreground ,fg-comments :slant italic))))
   `(font-lock-constant-face        ((t (:foreground ,fg-constants))))
   `(font-lock-function-name-face   ((t (:foreground ,fg-func :weight bold))))
   `(font-lock-keyword-face         ((t (:foreground ,fg-keywords :weight bold))))
   `(font-lock-string-face          ((t (:foreground ,fg-strings))))
   `(font-lock-type-face            ((t (:foreground ,fg-default))))
   `(font-lock-variable-name-face   ((t (:foreground ,fg-default))))
   `(font-lock-warning-face         ((t (:foreground ,fg-keywords :weight bold))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg-comments :slant italic))))
   `(font-lock-doc-face             ((t (:foreground ,fg-comments :slant italic))))
   `(font-lock-preprocessor-face    ((t (:foreground ,fg-keywords))))
   `(font-lock-negation-char-face   ((t (:foreground ,fg-keywords))))
   `(font-lock-regexp-grouping-backslash  ((t (:foreground ,fg-keywords))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,fg-keywords))))

   ;; ---------- Search ----------
   `(isearch        ((t (:background ,fg-keywords :foreground ,fg-region :weight bold))))
   `(lazy-highlight ((t (:background ,purple :foreground ,fg-region))))
   `(match          ((t (:background ,purple :foreground ,fg-region))))

   ;; ---------- Parens / Links ----------
   `(link ((t (:foreground ,peach :underline t))))
   `(show-paren-match    ((t (:background ,peach :foreground ,dark :weight bold))))
   `(show-paren-mismatch ((t (:background ,purple :foreground ,fg-region :weight bold))))

   ;; ---------- Line numbers ----------
   `(line-number ((t (:foreground ,purple :background ,bg-default))))
   `(line-number-current-line ((t (:foreground ,fg-default :background ,bg-hl-line :weight bold))))

   ;; ---------- Misc QoL ----------
   `(secondary-selection ((t (:extend t :background ,purple :foreground ,fg-region))))
   `(trailing-whitespace ((t (:background ,peach))))

   ;; ---------- Org-mode ----------
   `(org-level-1 ((t (:foreground ,fg-keywords :weight bold))))
   `(org-level-2 ((t (:foreground ,fg-default :weight bold))))
   `(org-level-3 ((t (:foreground ,fg-default))))
   `(org-level-4 ((t (:foreground ,fg-comments))))
   `(org-document-title ((t (:foreground ,fg-func :weight bold :height 1.3))))
   `(org-link ((t (:foreground ,peach :underline t))))
   `(org-code ((t (:foreground ,fg-constants))))
   `(org-date ((t (:foreground ,fg-constants :underline t))))
   `(org-special-keyword ((t (:foreground ,fg-comments :slant italic))))
   ;; Readable blocks: light bg with dark fg
   `(org-block ((t (:extend t :background ,bg-region :foreground ,fg-region))))
   `(org-block-begin-line ((t (:extend t :foreground ,fg-comments :slant italic :background ,bg-region))))
   `(org-block-end-line   ((t (:extend t :foreground ,fg-comments :slant italic :background ,bg-region))))
   `(org-verbatim ((t (:foreground ,fg-constants))))
   `(org-meta-line ((t (:foreground ,fg-comments :slant italic))))
   `(org-checkbox ((t (:foreground ,fg-keywords :weight bold))))
   `(org-checkbox-statistics-todo ((t (:foreground ,peach :weight bold))))
   `(org-checkbox-statistics-done ((t (:foreground ,fg-comments :weight bold))))
   `(org-footnote ((t (:foreground ,fg-comments :underline t))))
   `(org-formula ((t (:foreground ,fg-comments))))
   `(org-headline-done ((t (:foreground ,fg-comments :strike-through t))))
   `(org-ellipsis ((t (:foreground ,fg-comments))))
   `(org-warning ((t (:foreground ,peach :weight bold))))
   `(org-tag ((t (:foreground ,fg-comments :weight bold))))
   ))

(provide-theme 'tlm)
;;; tlm-theme.el ends here
