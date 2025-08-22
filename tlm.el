
(deftheme tlm
  "A gbc theme using a 4-color palette.")

(let* (
       ;; === Color Palette ===
       (purple "#84739C")
       (font "#AC9FAC")
       (peach "#725050")
       (dark "#343030")

       ;; === Semantic Face Colors ===
       (bg-default dark)
       (fg-default font)
       (bg-hl-line peach)
       (bg-region font)
       (fg-region dark)  ;; changed to dark so text visible on selection
       (fg-comments purple)
       (fg-keywords font)
       (fg-strings font)
       (fg-constants font)
       (fg-func font)
       (bg-modeline dark)
       (fg-modeline font))

  (custom-theme-set-faces
   'tlm

   ;; Base UI
   `(default ((t (:background ,bg-default :foreground ,fg-default :family "Arial" :height 120))))
   `(cursor ((t (:background ,peach))))
   `(region ((t (:background ,bg-region :foreground ,fg-region))))
   `(highlight ((t (:background ,bg-region :foreground ,fg-region))))
   `(hl-line ((t (:background ,bg-hl-line))))
   `(fringe ((t (:background ,bg-default))))
   `(vertical-border ((t (:foreground ,peach))))
   `(minibuffer-prompt ((t (:foreground ,peach :weight bold))))
   `(mode-line ((t (:background ,bg-modeline :foreground ,fg-modeline))))
   `(mode-line-inactive ((t (:background ,bg-default :foreground ,fg-default))))

   ;; Syntax faces all locked to your palette colors
   `(font-lock-builtin-face ((t (:foreground ,fg-keywords))))
   `(font-lock-comment-face ((t (:foreground ,fg-comments :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,fg-constants))))
   `(font-lock-function-name-face ((t (:foreground ,fg-func :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,fg-keywords :weight bold))))
   `(font-lock-string-face ((t (:foreground ,fg-strings))))
   `(font-lock-type-face ((t (:foreground ,fg-default))))
   `(font-lock-variable-name-face ((t (:foreground ,fg-default))))
   `(font-lock-warning-face ((t (:foreground ,fg-keywords :weight bold))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg-comments :slant italic))))

   ;; Added some often missed faces and forced them to your palette:
   `(font-lock-doc-face ((t (:foreground ,fg-comments :slant italic))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg-keywords))))
   `(font-lock-negation-char-face ((t (:foreground ,fg-keywords))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,fg-keywords))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,fg-keywords))))

   ;; Org-mode faces locked to palette
   `(org-level-1 ((t (:foreground ,fg-keywords :weight bold))))
   `(org-level-2 ((t (:foreground ,fg-default :weight bold))))
   `(org-level-3 ((t (:foreground ,fg-default))))
   `(org-level-4 ((t (:foreground ,fg-comments))))
   `(org-document-title ((t (:foreground ,fg-func :weight bold :height 1.3))))
   `(org-link ((t (:foreground ,peach :underline t))))
   `(org-code ((t (:foreground ,fg-constants))))
   `(org-date ((t (:foreground ,fg-constants :underline t))))
   `(org-special-keyword ((t (:foreground ,fg-comments :slant italic))))
   `(org-block ((t (:background ,bg-region))))
   `(org-block-begin-line ((t (:foreground ,fg-comments :slant italic :background ,bg-region))))
   `(org-block-end-line ((t (:foreground ,fg-comments :slant italic :background ,bg-region))))
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

   
   ;; Line numbers
   `(line-number ((t (:foreground ,dark :background ,bg-default))))
   `(line-number-current-line ((t (:foreground ,fg-default :background ,bg-hl-line))))

   ;; Extras
   `(link ((t (:foreground ,peach :underline t))))
   `(show-paren-match ((t (:background ,peach :foreground ,dark :weight bold))))
   ))

(provide-theme 'tlm)
