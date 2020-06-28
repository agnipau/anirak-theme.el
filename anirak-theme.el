;;; anirak-theme.el --- Dark theme based on the Anirak VSCode theme

;; Copyright (C) 2020 Matteo Guarda

;; Author: Matteo Guarda <matteoguarda@tutanota.com>
;; Keywords: faces themes
;; Homepage: http://github.com/agnipau/anirak-theme.el
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package include a dark theme based on the Anirak VSCode theme.
;; This theme focuses on finding a middle ground between a colorful
;; typical theme and a monochromatic one.
;; Usually themes have too many attention grabbers but at the same
;; time a monochromatic theme makes no difference between elements
;; such as comments, literal values and code.
;; This theme tries to be a middle ground between the two.

;; Usage:

;; If your Emacs has the `load-theme' command, you can use it to
;; activate one of these themes programatically, or use
;; `customize-themes' to select a theme interactively.

;;; Credit:

;; Anirak theme for VSCode by @barjoco:
;; URL `https://github.com/barjoco/anirak'
;; URL `https://marketplace.visualstudio.com/items?itemName=barjo.anirak'
;;
;; Color conundrum by Akshay Oppiliappan:
;; URL `https://peppe.rs/posts/color_conundrum/'

;;; Code:

(defconst anirak-colors
  '((cyan . "#9ccdff")
    (white . "#f0f8ff")
    (blue . "#599aff")
    (gray . "#757c88")
    (gray-darker . "#40444b")
    (gray-darkest . "#21232b")
    (black . "#0c0d10")
    (black-cursor-line . "#111217")
    (blue-cursor . "#303eff")
    (green . "#17ff93")
    (blue-selection . "#15194c")
    (blue-fold . "#10132f")
    (highlight-inactive . "#233551")
    (highlight-active . "#426498")
    (lint-error-bg . "#241517")
    (lint-error-fg . "#ff6464")
    (lint-hint-bg . "#0d2023")
    (lint-hint-fg . "#2faf64")
    (lint-info-bg . "#0a232c")
    (lint-info-fg . "#00b7e4")
    (lint-warning-bg . "#271c13")
    (lint-warning-fg . "#fa973a")
    (lint-yellow-underline . "#ffe89e")
    (menu-active-bg . "#3a3d46")
    (menu-inactive-bg . "#21232b")
    (menu-sthumb . "#3c3e46")
    (debugging . "#ff0000")
    (prova . "#4a55c1")
    (term-0 . "#1e2227")
    (term-1 . "#ff4766")
    (term-2 . "#17ff93")
    (term-3 . "#ffdc69")
    (term-4 . "#3743ee")
    (term-5 . "#4a8ffd")
    (term-6 . "#75abff")
    (term-7 . "#f5faff")
    (term-8 . "#3a434d")
    (term-9 . "#ff8398")
    (term-10 . "#71faba")
    (term-11 . "#ffe388")
    (term-12 . "#6485f1")
    (term-13 . "#7aadff")
    (term-14 . "#bad3ff")
    (term-15 . "#ffffff")))

(deftheme anirak
  "Dark theme based on the Anirak VSCode theme.")

(defmacro eval-with-colors (body)
  "Evaluate every sexpr in BODY while having colors from `anirak-colors' in scope."
  `(let ,(mapcar (lambda (x) `(,(car x) ,(cdr x))) anirak-colors)
     ,@body))

(defmacro set-theme (body)
  "Macro that simplifies the declaration of the theme.

Every color in every sexpr in BODY gets expanded to the actual color string."
  `(eval (cons 'custom-theme-set-faces (cons ''anirak
                (mapcar (lambda (x) `(quote (,(car x) ((t ,(car (cdr x)))))))
                        (let ,(mapcar (lambda (x) `(,(car x) ,(cdr x))) anirak-colors)
                          ,body))))))

;; theme
(set-theme
 `(
   ;; Standard font lock faces
   (default (:foreground ,white :background ,black))
   (bold (:weight bold))
   (bold-italic (:slant italic :weight bold))
   (underline (:underline t))
   (italic (:slant italic))
   ;; :foreground
   (font-lock-builtin-face (:foreground ,gray))
   ;; Comment prefix
   (font-lock-comment-delimiter-face (:foreground ,gray-darker))
   ;; Text of a comment
   (font-lock-comment-face (:foreground ,gray-darker))
   ;; \`...\'
   (font-lock-constant-face (:foreground ,white))
   ;; Doc comment of a function: (defun ... () "...")
   (font-lock-doc-face (:foreground ,blue))
   (font-lock-doc-string-face (:foreground ,debugging))
   ;; Function name
   (font-lock-function-name-face (:foreground ,white :weight bold))
   ;; Keyword
   (font-lock-keyword-face (:foreground ,cyan))
   (font-lock-negation-char-face (:foreground ,debugging))
   (font-lock-preprocessor-face (:foreground ,white :weight bold))
   (font-lock-preprocessor-char-face (:foreground ,debugging))
   (font-lock-regexp-grouping-backslash (:foreground ,debugging))
   (font-lock-regexp-grouping-construct (:foreground ,debugging))
   ;; String
   (font-lock-string-face (:foreground ,blue))
   ;; TODO: Not sure
   (font-lock-type-face (:foreground ,white :weight bold))
   ;; Variable name
   (font-lock-variable-name-face (:foreground ,white))
   ;; Warning (like `error')
   (font-lock-warning-face (:weight bold :foreground ,lint-warning-fg :background ,lint-warning-bg))
   (shadow (:foreground ,debugging))
   (success (:foreground ,debugging))
   (error (:foreground ,debugging))
   (warning (:foreground ,debugging))
   (tooltip (:foreground ,debugging :background ,black :inverse-video t))
   
   ;; Emacs interface
   (cursor (:background ,blue-cursor))
   ;; TODO: Long line break arrow fringe
   (fringe (:background ,black :foreground ,gray-darker))
   ;; Line numbers
   (linum (:background ,black :foreground ,gray-darker))
   (line-number (:background ,black :foreground ,gray-darker))
   (line-number-current-line (:inherit line-number :foreground ,white))
   (fill-column-indicator (:foreground ,debugging :weight normal :slant normal
                                       :underline nil :overline nil :strike-through nil
                                       :box nil :inverse-video nil :stipple nil))
   ;; Vertical border that separates windows
   (vertical-border (:foreground ,gray-darkest))
   (border (:background ,debugging :foreground ,debugging))
   (highlight (:inverse-video nil :background ,debugging))
   ;; Active mode line
   (mode-line ((t (:foreground ,white :background ,gray-darkest :weight normal
                               :box (:line-width 1 :color ,debugging)))))
   (mode-line-buffer-id (:foreground ,debugging :background nil))
   ;; Inactive mode line
   (mode-line-inactive (:inherit mode-line
                                 :foreground ,gray-darker
                                 :background ,black-cursor-line))
   (mode-line-emphasis (:foreground ,debugging :slant italic))
   ;; TODO: Not sure
   (mode-line-highlight (:background ,blue-selection :box nil))
   (minibuffer-prompt (:foreground ,gray))
   ;; Visual selection
   (region (:background ,blue-selection :foreground nil :extend t))
   (secondary-selection (:background ,debugging :foreground nil :extend t))
   ;; Header line
   (header-line (:inherit mode-line-inactive :foreground ,white :background nil))
   
   ;; search
   (match (:foreground ,debugging :background ,debugging :inverse-video t))
   ;; Active search result
   (isearch (:background ,highlight-active))
   ;; Inactive search results
   (lazy-highlight (:background ,highlight-inactive))
   ;; Failed search
   (isearch-fail (:background ,lint-error-bg :foreground ,lint-error-fg :weight bold))
   ;; Links like in Help mode
   (link (:foreground ,cyan :underline t))
   (widget-button (:foreground ,debugging :background ,debugging :underline t))
   (widget-field (:background ,debugging :box (:line-width 1 :color ,debugging)))
   
   ;; ansi-term (built-in)
   ;; Color 0
   (term (:foreground nil :background nil :inherit default))
   ;; Color 1
   (term-color-black (:foreground ,term-0 :background ,term-0))
   ;; Color 2
   (term-color-red (:foreground ,term-1 :background ,term-1))
   ;; Color 3
   (term-color-green (:foreground ,term-2 :background ,term-2))
   ;; Color 4
   (term-color-yellow (:foreground ,term-3 :background ,term-3))
   ;; Color 5
   (term-color-blue (:foreground ,term-4 :background ,term-4))
   ;; Color 6
   (term-color-magenta (:foreground ,term-5 :background ,term-5))
   ;; Color 6
   (term-color-cyan (:foreground ,term-6 :background ,term-6))
   ;; Color 7
   (term-color-white (:foreground ,term-7 :background ,term-7))
   
   ;; vterm
   (vterm-color-default (:foreground nil :background nil :inherit default))
   (vterm-color-black (:background ,term-0 :foreground ,term-0))
   (vterm-color-red (:background ,term-1 :foreground ,term-1))
   (vterm-color-green (:background ,term-2 :foreground ,term-2))
   (vterm-color-yellow (:background ,term-3 :foreground ,term-3))
   (vterm-color-blue (:background ,term-4 :foreground ,term-4))
   (vterm-color-magenta (:background ,term-5 :foreground ,term-5))
   (vterm-color-cyan (:background ,term-6 :foreground ,term-6))
   (vterm-color-white (:background ,term-7 :foreground ,term-7))
   
   ;; eshell (built-in)
   (eshell-prompt (:foreground ,gray :weight bold))
   ;; .zip files for example
   (eshell-ls-archive (:foreground ,blue))
   ;; .bak files for example
   (eshell-ls-backup (:foreground ,gray))
   (eshell-ls-clutter (:foreground ,debugging :weight bold))
   (eshell-ls-directory (:foreground ,blue :weight bold))
   (eshell-ls-executable (:foreground ,green :weight bold))
   (eshell-ls-missing (:foreground ,debugging :weight bold))
   (eshell-ls-product (:foreground ,debugging))
   (eshell-ls-readonly (:foreground ,debugging))
   (eshell-ls-special (:foreground ,debugging :weight bold))
   (eshell-ls-symlink (:foreground ,cyan :weight bold))
   ;; TODO: Not sure
   (eshell-ls-unreadable (:foreground ,gray-darker))
   
   ;; ElDoc (built-in)
   ;; Bottom line function ARGS keyword
   (eldoc-highlight-function-argument (:foreground ,green :weight bold))
   
   ;; Flycheck (built-in)
   (flycheck-error (:background ,lint-error-bg :foreground ,lint-error-fg))
   (flycheck-info (:background ,lint-info-bg :foreground ,lint-info-fg))
   (flycheck-warning (:background ,lint-warning-bg :foreground ,lint-warning-fg))
   (flycheck-fringe-error (:background ,lint-error-bg :foreground ,lint-error-fg))
   (flycheck-fringe-info (:background ,lint-info-bg :foreground ,lint-info-fg))
   (flycheck-fringe-warning (:background ,lint-warning-bg, :foreground ,lint-warning-fg))
   (flycheck-color-mode-line-error-face (:foreground ,debugging))
   (flycheck-color-mode-line-warning-face (:foreground ,debugging))
   (flycheck-color-mode-line-info-face (:foreground ,debugging))
   (flycheck-color-mode-line-running-face (:foreground ,debugging))
   (flycheck-color-mode-line-success-face (:foreground ,debugging))
   
   ;; Parenthesis matching (built-in)
   (show-paren-match (:background ,gray-darkest))
   (show-paren-mismatch (:background nil :foreground nil))
   
   ;; whitespace (built-in)
   (whitespace-big-indent (:background ,debugging :foreground ,debugging))
   (whitespace-empty (:background ,debugging :foreground ,debugging))
   (whitespace-hspace (:background ,debugging :foreground ,debugging))
   (whitespace-indentation (:background ,debugging :foreground ,debugging))
   ;; Too long line
   (whitespace-line (:underline (:style wave :color ,lint-error-fg)))
   (whitespace-newline (:foreground ,gray-darker))
   (whitespace-space (:foreground ,gray-darker))
   (whitespace-space-after-tab (:foreground ,gray-darker))
   (whitespace-space-before-tab (:foreground ,gray-darker))
   (whitespace-tab (:foreground ,gray-darker))
   (whitespace-trailing (:underline (:style wave :color ,lint-error-fg)))
   (trailing-whitespace (:inherit whitespace-trailing))
   
   ;; window-divider (built-in)
   (window-divider (:foreground ,debugging))
   (window-divider-first-pixel (:foreground ,debugging))
   (window-divider-last-pixel (:foreground ,debugging))
   
   ;; company
   (company-preview (:foreground ,debugging :background ,debugging))
   ;; Last suggestion that gets previewed in the line
   (company-preview-common (:background ,lint-info-bg :foreground ,lint-info-fg))
   (company-preview-search (:inherit company-preview :foreground ,debugging))
   ;; Company completion menu inactive rows
   (company-tooltip (:background ,menu-inactive-bg :foreground ,gray))
   ;; Company completion menu active row
   (company-tooltip-selection (:background ,menu-active-bg :foreground ,white))
   ;; Company completion menu active text in all rows
   (company-tooltip-common (:foreground ,white :weight bold))
   ;; Company completion menu active text in active row
   (company-tooltip-common-selection (:inherit company-tooltip-common))
   (company-tooltip-search (:inherit company-tooltip :foreground ,debugging))
   (company-tooltip-annotation (:inherit company-tooltip :foreground ,gray))
   (company-tooltip-annotation-selection (:inherit company-tooltip-selection :foreground ,gray))
   ;; Scrollbar thumb background
   (company-scrollbar-bg (:background ,menu-inactive-bg))
   ;; Scrollbar background
   (company-scrollbar-fg (:background ,menu-active-bg))
   (company-echo-common (:inherit company-echo :foreground ,debugging))
   
   ;; Ivy
   ;; TODO: Mouse hover
   (ivy-action (:foreground ,debugging))
   (ivy-confirm-face (:foreground ,debugging))
   (ivy-current-match (:background ,gray-darkest :foreground ,cyan :weight bold))
   (ivy-cursor (:background ,debugging))
   (ivy-match-required-face (:foreground ,debugging :background ,green))
   (ivy-remote (:foreground ,debugging))
   (ivy-subdir (:foreground ,cyan))
   (ivy-virtual (:foreground ,debugging))
   (ivy-minibuffer-match-face-1 (:foreground ,white :weight bold))
   (ivy-minibuffer-match-face-2 (:foreground ,white :weight bold))
   (ivy-minibuffer-match-face-3 (:foreground ,white :weight bold))
   (ivy-minibuffer-match-face-4 (:foreground ,white :weight bold))
   (ivy-highlight-face (:foreground ,debugging :weight bold))
   (ivy-minibuffer-match-highlight (:foreground ,debugging))
   (ivy-modified-buffer (:foreground ,gray))
   (ivy-modified-outside-buffer (:foreground ,gray))
   (ivy-prompt-match (:foreground ,debugging))
   (ivy-separator (:foreground ,debugging))
   (ivy-grep-info (:foreground ,debugging))
   (ivy-grep-line-number (:foreground ,debugging))
   (ivy-completions-annotations (:foreground ,debugging))
   (ivy-yanked-word (:foreground ,debugging))
   
   ;; markdown
   (markdown-url-face (:foreground ,white :underline t))
   (markdown-link-face (:foreground ,blue))
   ;; TODO: Search the face for *Helpful* mouse hover
   (markdown-highlight-face (:background ,blue-selection :foreground ,white))
   (markdown-markup-face (:foreground ,gray))
   ;; TODO: Define fixed-pitch font
   (markdown-code-face (:inherit fixed-pitch))
   ;; TODO: Define fixed-pitch font
   (markdown-inline-code-face (:inherit markdown-code-face))
   
   ;; lsp
   (lsp-face-highlight-textual (:background ,highlight-active))
   (lsp-face-highlight-read (:background ,highlight-inactive))
   (lsp-face-highlight-write (:foreground ,debugging :background ,debugging))
   (lsp-ui-doc-background (:background ,menu-inactive-bg))
   (lsp-ui-peek-filename (:foreground ,debugging :background ,debugging))
   (lsp-ui-peek-header (:foreground ,debugging :background ,debugging))
   (lsp-ui-peek-selection (:foreground ,debugging :background ,debugging))
   (lsp-ui-peek-list (:foreground ,debugging :background ,debugging))
   (lsp-ui-peek-peek (:foreground ,debugging :background ,debugging))
   (lsp-ui-peek-highlight (:foreground ,debugging :background ,debugging))
   (lsp-ui-peek-line-number (:foreground ,debugging :background ,debugging))
   (lsp-ui-sideline-code-action (:foreground ,debugging :background ,debugging))
   (lsp-ui-sideline-current-symbol (:foreground ,debugging :background ,debugging))
   (lsp-ui-sideline-symbol-info (:foreground ,debugging :background ,debugging))
   ))

(eval-with-colors
 (
  ;; rustic
  (setq rustic-ansi-faces `[,term-0 ,term-1 ,term-2 ,term-3 ,term-4 ,term-5 ,term-6 ,term-7])
  ;; hl-todo
  (setq hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" . ,green)
          ;; For problems that will become bigger problems later if
          ;; not fixed ASAP.
          ("FIXME" . ,green)
          ;; For tidbits that are unconventional and not intended uses
          ;; of the constituent parts, and may break in a future
          ;; update.
          ("HACK" . ,green)
          ;; For things that were done hastily and/or hasn't been
          ;; thoroughly tested. It may not even be necessary!
          ("REVIEW" . ,green)
          ;; For especially important gotchas with a given
          ;; implementation, directed at another user other than the
          ;; author.
          ("NOTE" . ,green)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" . ,green)))))

;; (font-lock-add-keywords
;;  'rustic-mode
;;  '(("{" 1 'font-lock-type-face)
;;    ("}" 1 'font-lock-type-face))
;;  1)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'anirak)

;;; anirak-theme.el ends here
