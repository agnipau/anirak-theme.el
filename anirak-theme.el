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

(defconst anirak/colors
  '((light . (;; TODO: Not sure
              (builtin . "#0066cf")
              (foreground . "#000000")
              ;; TODO: Not sure
              (string-literal . "#00378c")
              (dimmed . "#6b7a88")
              ;; A 5% darker version of `dimmed-darker` for things that are
              ;; important (like comments).
              (dimmed-darker-important . "#afbcc9")
              (dimmed-darker . "#bec9d3")
              (dimmed-darkest . "#ebf0f5")
              (background . "#ffffff")
              (background-cursor-line . "#f2f7ff")
              (cursor . "#004bff")
              ;; TODO: Not sure
              (accent . "#00b05e")
              (selection . "#b2d0ff")
              (highlight-inactive . "#ffefb2")
              (highlight-active . "#ffdf66")
              (lint-error-bg . "#fce9e9")
              (lint-error-fg . "#e45454")
              (lint-hint-bg . "#e1f3f3")
              (lint-hint-fg . "#2faf64")
              (lint-info-bg . "#def6fc")
              (lint-info-fg . "#00b7e4")
              (lint-warning-bg . "#fff1e4")
              (lint-warning-fg . "#ff942f")
              ;; TODO: Yet to be used
              (lint-underline . "#ffe89e")
              (menu-active-bg . "#d0e0fc")
              (menu-inactive-bg . "#f4f6fc")
              (menu-sthumb . "#ebeef9")
              (debugging . "#ff0000")
              ;; TODO: Term palette
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
    (dark . ((builtin . "#9ccdff")
             (foreground . "#f0f8ff")
             (string-literal . "#599aff")
             (dimmed . "#757c88")
             ;; A 5% lighter version of `dimmed-darker` for things that are
             ;; important (like comments).
             (dimmed-darker-important . "#5c636d")
             (dimmed-darker . "#585d67")
             (dimmed-darkest . "#292c37")
             ;; (background . "#0c0d10")
             (background . "#22252e")
             ;; (background-cursor-line . "#13141a")
             (background-cursor-line . "#292c37")
             (cursor . "#303eff")
             (accent . "#17ff93")
             (selection . "#1b2060")
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
             ;; TODO: Yet to be used
             (lint-underline . "#ffe89e")
             (menu-active-bg . "#3a3d46")
             (menu-inactive-bg . "#21232b")
             (menu-sthumb . "#3c3e46")
             (debugging . "#ff0000")
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
             (term-15 . "#ffffff")))))

(deftheme anirak
  "Dark theme based on the Anirak VSCode theme.")

(defmacro anirak/eval-with-colors (variant body)
  "Evaluate every sexpr in BODY while having colors of variant VARIANT from `anirak/colors' in scope."
  `(let ,(mapcar (lambda (x) `(,(car x) ,(cdr x)))
                 (cdr (assoc variant anirak/colors)))
     ,@body))

;; TODO: Add :attribute nil to every attribute that is not specified.
;; For example if :background is not specified add :background nil.
;; This ensures that when the user switches theme, a color of the
;; previous theme isn't used.
(defmacro anirak/create-theme (variant body)
  "Macro that simplifies the declaration of the theme.

Every color in every sexpr in BODY gets expanded to the actual color
string of variant VARIANT from `anirak/colors'."
  `(eval (cons 'custom-theme-set-faces
               (cons ,(if (eq variant 'light) '''anirak-light
                        (if (eq variant 'dark) '''anirak
                          (error "Invalid variant `%s'" variant)))
                     (mapcar (lambda (x) `(quote (,(car x) ((t ,(cdr x))))))
                             (let ,(mapcar (lambda (x) `(,(car x) ,(cdr x)))
                                           (cdr (assoc variant anirak/colors)))
                               ,body))))))

(defgroup anirak/font-lock-faces nil
  "Extra font lock faces offered by the Anirak theme."
  :prefix "anirak/font-lock-"
  :group 'faces)

(defvar anirak/font-lock-noise 'anirak/font-lock-noise
  "Face name to use for noise symbols like braces, brackets, parenthesis acc.")
(defface anirak/font-lock-noise
  '()
  "Face name to use for noise symbols like braces, brackets, parenthesis acc."
  :group 'anirak/font-lock-faces)

(defvar anirak/font-lock-number-literal 'anirak/font-lock-number-literal
  "Face name to use for number literals.")
(defface anirak/font-lock-number-literal
  '()
  "Face name to use for number literals."
  :group 'anirak/font-lock-faces)

(defvar anirak/font-lock-boolean-literal 'anirak/font-lock-boolean-literal
  "Face name to use for boolean literals.")
(defface anirak/font-lock-boolean-literal
  '()
  "Face name to use for boolean literals."
  :group 'anirak/font-lock-faces)

(defvar anirak/font-lock-operator 'anirak/font-lock-operator
  "Face name to use for operators.")
(defface anirak/font-lock-operator
  '()
  "Face name to use for operators."
  :group 'anirak/font-lock-faces)

;; (defvar anirak/font-lock-important 'anirak/font-lock-important
;;   "Face name to use for important elements.")
;; (defface anirak/font-lock-important
;;   '()
;;   "Face name to use for important elements."
;;   :group 'anirak/font-lock-faces)

;; theme
(defmacro anirak/set-theme (variant)
  `(progn
     (anirak/create-theme
      ,variant
      `(
        ;; Standard font lock faces
        (default :foreground ,foreground :background ,background)
        (bold :weight bold)
        (bold-italic :slant italic :weight bold)
        (underline :underline t)
        (italic :slant italic)
        ;; :foreground
        (font-lock-builtin-face :foreground ,dimmed :background nil)
        ;; Comment prefix
        (font-lock-comment-delimiter-face :foreground ,dimmed-darker-important :background nil)
        ;; Text of a comment
        (font-lock-comment-face :foreground ,dimmed-darker-important :background nil)
        ;; \`...\'
        (font-lock-constant-face :foreground ,foreground :weight bold :background nil)
        ;; Doc comment of a function: (defun ... () "...")
        (font-lock-doc-face :foreground ,string-literal :background nil)
        (font-lock-doc-string-face :foreground ,debugging :background nil)
        ;; Function name
        (font-lock-function-name-face :foreground ,foreground :weight bold :background nil)
        ;; Noise symbols -> { } , ( ) [ ] :
        (anirak/font-lock-noise :foreground ,dimmed :background nil)
        ;; Number literals
        (anirak/font-lock-number-literal :foreground ,accent :background nil)
        ;; Boolean literals
        (anirak/font-lock-boolean-literal :foreground ,accent :background nil)
        ;; Operators
        (anirak/font-lock-operator :foreground ,dimmed :background nil)
        ;; Keyword
        (font-lock-keyword-face :foreground ,builtin :background nil)
        ;; In regexp string, the caret -> [^z-a]
        (font-lock-negation-char-face :foreground ,dimmed :background nil)
        (font-lock-preprocessor-face :foreground ,foreground :weight bold :background nil)
        (font-lock-preprocessor-char-face :foreground ,debugging :background nil)
        (font-lock-regexp-grouping-backslash :foreground ,dimmed-darker :background nil)
        (font-lock-regexp-grouping-construct :foreground ,dimmed :background nil)
        ;; String
        (font-lock-string-face :foreground ,string-literal :background nil)
        ;; TODO: Not sure
        (font-lock-type-face :foreground ,foreground :weight bold :background nil)
        ;; Variable name
        (font-lock-variable-name-face :foreground ,foreground :background nil)
        ;; Warning (like `error')
        (font-lock-warning-face :weight bold :foreground ,lint-warning-fg :background ,lint-warning-bg)
        (shadow :foreground ,dimmed :background nil)
        ;; TODO: Not sure
        (success :foreground ,lint-hint-fg :background ,lint-hint-bg)
        ;; TODO: Not sure
        (error :foreground ,lint-error-fg :background ,lint-error-bg)
        ;; TODO: Not sure
        (warning :foreground ,lint-warning-fg :background ,lint-warning-bg)
        (tooltip :foreground ,debugging :background ,accent :inverse-video t)

        ;; Emacs interface
        ;; NOTE: Emacs ignores foreground for cursor.
        (cursor :background ,cursor :foreground ,foreground)
        ;; TODO: Long line break arrow fringe
        (fringe :background ,background :foreground ,dimmed-darker)
        ;; Line numbers
        (linum :background ,background :foreground ,dimmed-darker)
        (line-number :background ,background :foreground ,dimmed-darker)
        (line-number-current-line :inherit line-number :foreground ,foreground :background nil)
        (fill-column-indicator :foreground ,debugging :weight normal :slant normal
                               :underline nil :overline nil :strike-through nil
                               :box nil :inverse-video nil :stipple nil
                               :background nil)
        ;; Vertical border that separates windows
        (vertical-border :foreground ,dimmed-darkest :background nil)
        (border :background ,debugging :foreground ,debugging)
        ;; Visual selection
        (highlight :background ,selection :foreground nil)
        ;; Highlight current line
        (hl-line :background ,background-cursor-line :foreground nil)
        ;; Active mode line
        (mode-line :foreground ,foreground :background ,dimmed-darkest :weight normal
                   :box (:line-width 1 :color ,dimmed-darkest))
        ;; Like *Messages*
        (mode-line-buffer-id :foreground nil, :background nil)
        ;; Inactive mode line
        (mode-line-inactive :inherit mode-line
                            :foreground ,dimmed-darker
                            :background ,background)
        (mode-line-emphasis :foreground ,debugging :slant italic :background nil)
        ;; TODO: Not sure
        (mode-line-highlight :background ,selection :box nil :foreground nil)
        (minibuffer-prompt :foreground ,dimmed :background nil)
        ;; Visual selection
        (region :background ,selection :foreground nil :extend t)
        (secondary-selection :background ,debugging :foreground nil :extend t)
        ;; Header line
        (header-line :inherit mode-line-inactive :foreground ,foreground :background nil)

        ;; search
        (match :foreground ,lint-info-fg :background ,lint-info-bg)
        ;; Active search result
        (isearch :background ,highlight-active :foreground nil)
        ;; Inactive search results
        (lazy-highlight :background ,highlight-inactive :foreground nil)
        ;; Failed search
        (isearch-fail :background ,lint-error-bg :foreground ,lint-error-fg :weight bold)
        ;; Links like in Help mode
        (link :foreground ,builtin :underline t :background nil)
        (widget-button :foreground ,debugging :background ,debugging :underline t)
        ;; Like a search bar background.
        (widget-field :background ,menu-inactive-bg :foreground nil
                      :box (:line-width 1 :color ,dimmed-darker))
        ;; Buttons.
        (custom-button :background ,dimmed-darker :foreground ,foreground)
        ;; Buttons on mouse hover.
        ;; TODO: On mouse down.
        (custom-button-mouse :background ,dimmed-darkest :foreground nil)
        ;; Like the message "NO CUSTOMIZATION DATA; not intended to be
        ;; customized" in custom-mode.
        (custom-state :background ,lint-hint-bg :foreground ,lint-hint-fg)
        ;; Like the face tag in custom-mode.
        (custom-face-tag :foreground ,foreground :weight bold :background nil)

        ;; diff-mode (built-in)
        ;; Used also by evil for search and replace (:%s) preview.
        (diff-removed :background ,lint-error-bg :foreground ,lint-error-fg)
        (diff-added :background ,lint-hint-bg :foreground ,lint-hint-fg)
        ;; TODO: Not sure
        (diff-changed :background ,lint-warning-bg :foreground ,lint-warning-fg)
        ;; TODO: Not sure
        (diff-indicator-added :inherit diff-added)
        ;; TODO: Not sure
        (diff-indicator-changed :inherit diff-changed)
        ;; TODO: Not sure
        (diff-indicator-removed :inherit diff-removed)
        (diff-context :background ,debugging :foreground ,accent)
        (diff-file-header :background ,debugging :foreground ,accent)
        (diff-function :background ,debugging :foreground ,accent)
        (diff-header :background ,debugging :foreground ,accent)
        (diff-hunk-header :background ,debugging :foreground ,accent)
        (diff-index :background ,debugging :foreground ,accent)
        (diff-nonexistent :background ,debugging :foreground ,accent)
        (diff-refined-added :background ,debugging :foreground ,accent)
        (diff-refined-changed :background ,debugging :foreground ,accent)
        (diff-refined-removed :background ,debugging :foreground ,accent)

        ;; ansi-term (built-in)
        ;; Color 0
        (term :foreground nil :background nil :inherit default)
        ;; Color 1
        (term-color-black :foreground ,term-0 :background ,term-0)
        ;; Color 2
        (term-color-red :foreground ,term-1 :background ,term-1)
        ;; Color 3
        (term-color-green :foreground ,term-2 :background ,term-2)
        ;; Color 4
        (term-color-yellow :foreground ,term-3 :background ,term-3)
        ;; Color 5
        (term-color-blue :foreground ,term-4 :background ,term-4)
        ;; Color 6
        (term-color-magenta :foreground ,term-5 :background ,term-5)
        ;; Color 6
        (term-color-cyan :foreground ,term-6 :background ,term-6)
        ;; Color 7
        (term-color-white :foreground ,term-7 :background ,term-7)

        ;; vterm
        (vterm-color-default :foreground nil :background nil :inherit default)
        (vterm-color-black :background ,term-0 :foreground ,term-0)
        (vterm-color-red :background ,term-1 :foreground ,term-1)
        (vterm-color-green :background ,term-2 :foreground ,term-2)
        (vterm-color-yellow :background ,term-3 :foreground ,term-3)
        (vterm-color-blue :background ,term-4 :foreground ,term-4)
        (vterm-color-magenta :background ,term-5 :foreground ,term-5)
        (vterm-color-cyan :background ,term-6 :foreground ,term-6)
        (vterm-color-white :background ,term-7 :foreground ,term-7)

        ;; eshell (built-in)
        (eshell-prompt :foreground ,dimmed :weight bold :background nil)
        ;; .zip files for example
        (eshell-ls-archive :foreground ,string-literal :background nil)
        ;; .bak files for example
        (eshell-ls-backup :foreground ,dimmed :background nil)
        (eshell-ls-clutter :foreground ,debugging :weight bold :background nil)
        (eshell-ls-directory :foreground ,string-literal :weight bold :background nil)
        (eshell-ls-executable :foreground ,accent :weight bold :background nil)
        (eshell-ls-missing :foreground ,debugging :weight bold :background nil)
        (eshell-ls-product :foreground ,debugging :background nil)
        (eshell-ls-readonly :foreground ,debugging :background nil)
        (eshell-ls-special :foreground ,debugging :weight bold :background nil)
        (eshell-ls-symlink :foreground ,builtin :weight bold :background nil)
        ;; TODO: Not sure
        (eshell-ls-unreadable :foreground ,dimmed-darker :background nil)

        ;; ElDoc (built-in)
        ;; Bottom line function ARGS keyword
        (eldoc-highlight-function-argument :foreground ,accent :weight bold :background nil)

        ;; Flycheck (built-in)
        (flycheck-error :underline (:style wave :color ,lint-error-fg))
        (flycheck-info :underline (:style wave :color ,lint-info-fg))
        (flycheck-warning :underline (:style wave :color ,lint-warning-fg))
        (flycheck-fringe-error :background ,lint-error-fg :foreground ,lint-error-fg)
        (flycheck-fringe-info :background ,lint-info-fg :foreground ,lint-info-fg)
        (flycheck-fringe-warning :background ,lint-warning-fg, :foreground ,lint-warning-fg)
        (flycheck-color-mode-line-error-face :foreground ,debugging :background nil)
        (flycheck-color-mode-line-warning-face :foreground ,debugging :background nil)
        (flycheck-color-mode-line-info-face :foreground ,debugging :background nil)
        (flycheck-color-mode-line-running-face :foreground ,debugging :background nil)
        (flycheck-color-mode-line-success-face :foreground ,debugging :background nil)

        ;; Parenthesis matching (built-in)
        (show-paren-match :background ,dimmed-darkest :foreground nil)
        (show-paren-mismatch :background nil :foreground nil)

        ;; whitespace (built-in)
        (whitespace-big-indent :background ,debugging :foreground ,debugging)
        (whitespace-empty :background ,debugging :foreground ,debugging)
        (whitespace-hspace :background ,debugging :foreground ,debugging)
        (whitespace-indentation :background ,debugging :foreground ,debugging)
        ;; Too long line
        (whitespace-line :underline (:style wave :color ,lint-error-fg))
        (whitespace-newline :foreground ,dimmed-darker :background nil)
        (whitespace-space :foreground ,dimmed-darker :background nil)
        (whitespace-space-after-tab :foreground ,dimmed-darker :background nil)
        (whitespace-space-before-tab :foreground ,dimmed-darker :background nil)
        (whitespace-tab :foreground ,dimmed-darker :background nil)
        (whitespace-trailing :underline (:style wave :color ,lint-error-fg) :background nil)
        (trailing-whitespace :inherit whitespace-trailing :background nil)

        ;; window-divider (built-in)
        (window-divider :foreground ,debugging :background nil)
        (window-divider-first-pixel :foreground ,debugging :background nil)
        (window-divider-last-pixel :foreground ,debugging :background nil)

        ;; company
        (company-preview :foreground ,debugging :background ,debugging)
        ;; Last suggestion that gets previewed in the line
        (company-preview-common :background ,lint-info-bg :foreground ,lint-info-fg)
        (company-preview-search :inherit company-preview :foreground ,debugging :background nil)
        ;; Company completion menu inactive rows
        (company-tooltip :background ,menu-inactive-bg :foreground ,dimmed)
        ;; Company completion menu active row
        (company-tooltip-selection :background ,menu-active-bg :foreground ,foreground)
        ;; Company completion menu active text in all rows
        (company-tooltip-common :foreground ,foreground :weight bold :background nil)
        ;; Company completion menu active text in active row
        (company-tooltip-common-selection :inherit company-tooltip-common)
        (company-tooltip-search :inherit company-tooltip :foreground ,debugging :background nil)
        (company-tooltip-annotation :inherit company-tooltip :foreground ,dimmed :background nil)
        (company-tooltip-annotation-selection :inherit company-tooltip-selection :foreground ,dimmed :background nil)
        ;; Scrollbar thumb background
        (company-scrollbar-bg :background ,menu-inactive-bg :foreground nil)
        ;; Scrollbar background
        (company-scrollbar-fg :background ,menu-active-bg :foreground nil)
        (company-echo-common :inherit company-echo :foreground ,debugging :background nil)

        ;; Ivy
        ;; TODO: Mouse hover
        (ivy-action :foreground ,debugging :background nil)
        (ivy-confirm-face :foreground ,debugging :background nil)
        (ivy-current-match :background ,dimmed-darkest :foreground ,builtin :weight bold)
        (ivy-cursor :background ,cursor :foreground nil)
        ;; Like the (match-required) message in swiper
        (ivy-match-required-face :foreground ,lint-error-fg :background ,lint-error-bg)
        (ivy-remote :foreground ,debugging :background nil)
        (ivy-subdir :foreground ,builtin :background nil)
        (ivy-virtual :foreground ,dimmed :background nil)
        (ivy-minibuffer-match-face-1 :foreground ,foreground :weight bold :background nil)
        (ivy-minibuffer-match-face-2 :foreground ,foreground :weight bold :background nil)
        (ivy-minibuffer-match-face-3 :foreground ,foreground :weight bold :background nil)
        (ivy-minibuffer-match-face-4 :foreground ,foreground :weight bold :background nil)
        (ivy-highlight-face :foreground ,debugging :weight bold :background nil)
        (ivy-minibuffer-match-highlight :foreground ,debugging :background nil)
        (ivy-modified-buffer :foreground ,foreground :background nil)
        (ivy-modified-outside-buffer :foreground ,foreground :background nil)
        (ivy-prompt-match :foreground ,debugging :background nil)
        (ivy-separator :foreground ,debugging :background nil)
        (ivy-grep-info :foreground ,debugging :background nil)
        (ivy-grep-line-number :foreground ,debugging :background nil)
        (ivy-completions-annotations :foreground ,debugging :background nil)
        (ivy-yanked-word :foreground ,debugging :background nil)

        ;; markdown
        (markdown-url-face :foreground ,foreground :underline t :background nil)
        (markdown-link-face :foreground ,string-literal :background nil)
        ;; TODO: Search the face for *Helpful* mouse hover
        (markdown-highlight-face :background ,selection :foreground ,foreground)
        (markdown-markup-face :foreground ,dimmed :background nil)
        ;; TODO: Define fixed-pitch font
        (markdown-code-face :inherit fixed-pitch)
        ;; TODO: Define fixed-pitch font
        (markdown-inline-code-face :inherit markdown-code-face)

        ;; lsp
        ;; TODO: Find face for the white sideline text.
        (lsp-face-highlight-textual :background ,highlight-active :foreground nil)
        (lsp-face-highlight-read :background ,highlight-inactive :foreground nil)
        (lsp-face-highlight-write :background ,highlight-active :foreground nil)
        (lsp-ui-doc-background :background ,menu-inactive-bg :foreground nil)
        (lsp-ui-peek-filename :foreground ,debugging :background ,debugging)
        (lsp-ui-peek-header :foreground ,debugging :background ,debugging)
        (lsp-ui-peek-selection :foreground ,debugging :background ,debugging)
        (lsp-ui-peek-list :foreground ,debugging :background ,debugging)
        (lsp-ui-peek-peek :foreground ,debugging :background ,debugging)
        (lsp-ui-peek-highlight :foreground ,debugging :background ,debugging)
        (lsp-ui-peek-line-number :foreground ,debugging :background ,debugging)
        ;; Action text on the right side.
        (lsp-ui-sideline-code-action :foreground ,lint-hint-fg :background ,lint-hint-bg)
        (lsp-ui-sideline-current-symbol :foreground ,debugging :background ,debugging)
        (lsp-ui-sideline-symbol-info :foreground ,debugging :background ,debugging)

        ;; evil-mode
        (evil-ex-commands :background ,accent :foreground ,debugging)
        ;; Warnings and errors on the evil command line
        (evil-ex-info :background ,lint-warning-bg :foreground ,lint-warning-fg)
        (evil-ex-lazy-highlight :background ,accent :foreground ,debugging)
        (evil-ex-search :background ,accent :foreground ,debugging)
        (evil-ex-substitute-matches :background ,lint-error-bg :foreground ,lint-error-fg)
        (evil-ex-substitute-replacement :background ,lint-hint-bg :foreground ,lint-hint-fg)

        ;; hydra
        (hydra-face-red :background ,lint-error-bg :foreground ,lint-error-fg)
        (hydra-face-teal :background ,accent :foreground ,debugging)
        (hydra-face-pink :background ,accent :foreground ,debugging)
        (hydra-face-blue :background ,accent :foreground ,debugging)
        (hydra-face-amaranth :background ,accent :foreground ,debugging)

        ;; evil-mc
        (evil-mc-cursor-bar-face :foreground ,debugging :background ,accent)
        ;; Non active cursors.
        (evil-mc-cursor-default-face :background ,highlight-active :foreground nil)
        (evil-mc-cursor-hbar-face :foreground ,debugging :background ,accent)
        ;; Non active cursors selections.
        (evil-mc-region-face :background ,highlight-inactive :foreground nil)

        ;; evil-snipe
        ;; (evil-snipe-first-match-face (:foreground ,debugging :background ,accent))
        ;; (evil-snipe-matches-face (:foreground ,debugging :background ,accent))
        ))

     (anirak/eval-with-colors
      ,variant
      (
       ;; fill-column-indicator
       (setq fci-rule-width 1
             fci-rule-color dimmed-darkest)

       ;; lsp-ui
       (setq lsp-ui-doc-border dimmed-darker)

       ;; rustic
       (setq rustic-ansi-faces `[,term-0 ,term-1 ,term-2 ,term-3
                                         ,term-4 ,term-5 ,term-6 ,term-7])
       ;; hl-todo
       (setq hl-todo-keyword-faces
             `(;; For things that need to be done, just not today.
               ("TODO" . ,accent)
               ;; For problems that will become bigger problems later if
               ;; not fixed ASAP.
               ("FIXME" . ,accent)
               ;; For tidbits that are unconventional and not intended uses
               ;; of the constituent parts, and may break in a future
               ;; update.
               ("HACK" . ,accent)
               ;; For things that were done hastily and/or hasn't been
               ;; thoroughly tested. It may not even be necessary!
               ("REVIEW" . ,accent)
               ;; For especially important gotchas with a given
               ;; implementation, directed at another user other than the
               ;; author.
               ("NOTE" . ,accent)
               ;; For things that just gotta go and will soon be gone.
               ("DEPRECATED" . ,accent)))))
     ))

(anirak/set-theme dark)

(defmacro anirak/multi-font-lock-add-keywords (modes alist)
  "Similiar to `font-lock-add-keywords' but let's you apply ALIST to every mode in MODES."
  `(mapc
    (lambda (x) (font-lock-add-keywords x ,alist))
    ',modes))

(anirak/multi-font-lock-add-keywords
 (lisp-mode scheme-mode)
 `(
   ;; Number literals
   ;; TODO: Improve
   (,(rx (and (or (and
                   symbol-start
                   (or
                    (and
                     (? (any "-+"))
                     (+ digit)
                     (? (or (and (any "eE")
                                 (? (any "-+"))
                                 (+ digit))
                            (and "."
                                 (? (and (+ digit)
                                         (? (and
                                             (any "eE")
                                             (? (any "-+"))
                                             (+ digit))))))
                            (and "/"
                                 (+ digit)))))
                    (and
                     "."
                     (+ digit)
                     (? (and
                         (any "eE")
                         (? (any "-+"))
                         (+ digit))))))
                  (and "#"
                       symbol-start
                       (or (and (any "bB")
                                (? (any "-+"))
                                (+ (any "01")))
                           (and (any "oO")
                                (? (any "-+"))
                                (+ (any "0-7")))
                           (and (any "xX")
                                (? (any "-+"))
                                (+ hex-digit)))))
              symbol-end))
    . 'anirak/font-lock-number-literal)
   ))

(anirak/multi-font-lock-add-keywords
 (clojure-mode)
 `(
   ;; Number literals
   ;; TODO: Improve
   (,(rx (and symbol-start
              (? "-")
              digit
              (*? any)
              symbol-end))
    . 'anirak/font-lock-number-literal)
   ))

(anirak/multi-font-lock-add-keywords
 (julia-mode ess-julia-mode)
 `(
   ;; Number literals
   ;; TODO: Improve
   (,(rx (and symbol-start
              (or (and (+ digit)
                       (? (and "." (* digit)))
                       (? (and (any "eE")
                               (? (any "-+"))
                               (+ digit))))
                  (and "0"
                       (any "xX")
                       (+ hex-digit)))))
    . 'anirak/font-lock-number-literal)
   ))

(anirak/multi-font-lock-add-keywords
 (css-mode)
 `(
   ;; Number literals
   (,(rx (and symbol-start
              (group (and
                      (? (any "+-"))
                      (+ (any "0-9"))
                      (? (and "." (* (any "0-9"))))))
              (? (or "cm"
                     "mm"
                     "in"
                     "px"
                     "pt"
                     "pc"
                     "em"
                     "ex"
                     "ch"
                     "rem"
                     "vw"
                     "vh"
                     "vmin"
                     "vmax"
                     "%"))
              symbol-end))
    . 'anirak/font-lock-number-literal)
   (,(rx "%") . 'anirak/font-lock-number-literal)

   ;; Noise
   (,(rx (any ":,;{}[]()>.")) . 'anirak/font-lock-noise)
   ))

(anirak/multi-font-lock-add-keywords
 (nxml-mode)
 `(
   ;; Noise
   (,(rx (any "=</>?")) . 'anirak/font-lock-noise)
   ))

(anirak/multi-font-lock-add-keywords
 (mhtml-mode)
 `(
   ;; Noise
   (,(rx (any "=</>")) . 'anirak/font-lock-noise)
   ))

(anirak/multi-font-lock-add-keywords
 (fish-mode)
 `(
   ;; Noise
   (,(rx (any "();")) . 'anirak/font-lock-noise)
   ))

(anirak/multi-font-lock-add-keywords
 (dart-mode)
 `(
   ;; Boolean literals
   (,(rx (and symbol-start
              (or "true"
                  "false"
                  "null")
              symbol-end))
    . 'anirak/font-lock-boolean-literal)

   ;; Number literals
   (,(rx (and symbol-start
              (or (and (+ (any "0-9"))
                       (? (or (and (any "eE")
                                   (? (any "-+"))
                                   (+ (any "0-9")))
                              (and "."
                                   (+ (any "0-9"))
                                   (? (and (any "eE")
                                           (? (any "-+"))
                                           (+ (any "0-9"))))))))
                  (and "0"
                       (any "xX")
                       (+ hex-digit))
                  (and "0"
                       (any "bB")
                       (+ (any "01")))
                  (and "."
                       (+ (any "0-9"))
                       (? (and (any "eE")
                               (? (any "-+"))
                               (+ (any "0-9"))))))
              symbol-end))
    . 'anirak/font-lock-number-literal)

   ;; Noise
   (,(rx (any "{}[]();,.")) . 'anirak/font-lock-noise)
   (,(rx (group "r")
         (or (and "\"" (* (not "\"")) "\"")
             (and "'" (* (not "'")) "'")))
    1 'anirak/font-lock-noise)

   ;; Keywords that dart-mode doesn't cover
   (,(rx (and symbol-start
              (or "extension" "on" "hide" "show" "is!")
              symbol-end))
    . 'font-lock-keyword-face)

   ;; Operators
   (,(rx (or "+"
             "*"
             "/"
             "~/"
             "%"
             "++"
             "--"
             ">"
             "<"
             ">="
             "<="
             "=="
             "!="
             "&"
             "|"
             "^"
             "~"
             "<<"
             ">>"
             "="
             "??="
             "+="
             "-="
             "*="
             "/="
             "&&"
             "||"
             "!"
             "?"
             ":"
             "??"
             "-"))
    . 'anirak/font-lock-operator)
   ))

(anirak/multi-font-lock-add-keywords
 (csharp-mode)
 `(
   ;; Boolean literals
   (,(rx (and symbol-start
              (or "true"
                  "false"
                  "null")
              symbol-end))
    . 'anirak/font-lock-boolean-literal)

   ;; Number literals
   (,(rx (and symbol-start
              (or (and "0"
                       (any "xX")
                       (* (or hex-digit "_"))
                       hex-digit)
                  (and "0"
                       (any "bB")
                       (* (any "_01"))
                       (any "01"))
                  (and (any "0-9")
                       (* (any "_0-9"))
                       (? (or (and (any "eE")
                                   (? (any "-+"))
                                   (+ (any "0-9")))
                              (and "."
                                   (+ (any "0-9"))
                                   (? (and (any "eE")
                                           (? (any "-+"))
                                           (+ (any "0-9"))))))))
                  (and "."
                       (+ (any "0-9"))
                       (? (and (any "eE")
                               (? (any "-+"))
                               (+ (any "0-9"))))))
              symbol-end))
    . 'anirak/font-lock-number-literal)

   ;; Noise
   (,(rx (any "{}[]();,.")) . 'anirak/font-lock-noise)
   (,(rx (group (any "$@"))
         (and "\"" (* (not "\"")) "\""))
    1 'anirak/font-lock-noise)

   ;; Keywords that csharp-mode doesn't cover
   (,(rx (and symbol-start
              (or "dechecked" "nameof" "stackalloc")
              symbol-end))
    . 'font-lock-keyword-face)

   ;; Operators
   (,(rx (or "+"
             "?."
             "?"
             ":"
             "?["
             "++"
             "--"
             "!"
             "->"
             "~"
             "^"
             "&"
             ".."
             "*"
             "/"
             "%"
             "<"
             ">"
             "<<"
             ">>"
             ">="
             "<="
             "=="
             "!="
             "|"
             "&&"
             "||"
             "??"
             "="
             "+="
             "-="
             "*="
             "/="
             "%="
             "&="
             "|="
             "^="
             "<<="
             ">>="
             "?="
             "=>"
             "-"))
    . 'anirak/font-lock-operator)
   ))

(anirak/multi-font-lock-add-keywords
 (python-mode)
 `(
   ;; Boolean literals
   (,(rx (and symbol-start
              (or "True"
                  "False"
                  "None")
              symbol-end))
    . 'anirak/font-lock-boolean-literal)

   ;; Number literals
   (,(rx (and symbol-start
              (or (and "."
                       (any "0-9")
                       (* (or (any "0-9") (and "_" (any "0-9"))))
                       (? (and (any "eE") (? (any "+-")) (+ (any "0-9")))))
                  (and "0"
                       (any "xX")
                       hex-digit
                       (* (or hex-digit (and "_" hex-digit))))
                  (and "0"
                       (any "oO")
                       (any "0-7")
                       (* (or (any "0-7") (and "_" (any "0-7")))))
                  (and "0"
                       (any "bB")
                       (any "01")
                       (* (or (any "01") (and "_" (any "01")))))
                  (and (any "0-9")
                       (* (or (any "0-9") (and "_" (any "0-9"))))
                       (? (and "."
                               (? (any "0-9")
                                  (* (or (any "0-9") (and "_" (any "0-9")))))))
                       (? (and (any "eE") (? (any "+-")) (+ (any "0-9"))))))
              symbol-end))
    . 'anirak/font-lock-number-literal)

   ;; Noise
   (,(rx (any "{}[]();:,.")) . 'anirak/font-lock-noise)
   (,(rx (group (any "fFrR"))
         (or (and "\"" (* (not "\"")) "\"")
             (and "'" (* (not "'")) "'")))
    1 'anirak/font-lock-noise)

   ;; Operators
   (,(rx (or "+"
             "*"
             "/"
             "%"
             "**"
             "//"
             "="
             "+="
             "-="
             "*="
             "/="
             "%="
             "//="
             "**="
             "&="
             "|="
             "^="
             ">>="
             "<<="
             "=="
             "!="
             ">"
             "<"
             ">="
             "<="
             "&"
             "|"
             "^"
             "~"
             "<<"
             ">>"
             "-"))
    . 'anirak/font-lock-operator)
   ))

(anirak/multi-font-lock-add-keywords
 (typescript-mode js-mode)
 `(
   ;; Boolean literals
   (,(rx (and symbol-start
              (or "true" "false" "null" "undefined")
              symbol-end))
    . 'anirak/font-lock-boolean-literal)

   ;; Number literals
   (,(rx (and symbol-start
              (or (and "."
                       (any "0-9")
                       (* (or (any "0-9") (and "_" (any "0-9"))))
                       (? (and (any "eE") (? (any "+-")) (+ (any "0-9")))))
                  (and "0"
                       (any "xX")
                       hex-digit
                       (* (or hex-digit (and "_" hex-digit))))
                  (and "0"
                       (any "oO")
                       (any "0-7")
                       (* (or (any "0-7") (and "_" (any "0-7")))))
                  (and "0"
                       (any "bB")
                       (any "01")
                       (* (or (any "01") (and "_" (any "01")))))
                  (and (any "0-9")
                       (* (or (any "0-9") (and "_" (any "0-9"))))
                       (? (and "."
                               (? (any "0-9")
                                  (* (or (any "0-9") (and "_" (any "0-9")))))))
                       (? (and (any "eE") (? (any "+-")) (+ (any "0-9"))))))
              symbol-end))
    . 'anirak/font-lock-number-literal)

   ;; Noise
   (,(rx (any "{}[]();,.")) . 'anirak/font-lock-noise)

   ;; Operators
   (,(rx (or "+"
             "*"
             "/"
             "%"
             "++"
             "--"
             ">"
             "<"
             ">="
             "<="
             "=="
             "!="
             "&&"
             "||"
             "!"
             "&"
             "|"
             "^"
             "~"
             "<<"
             ">>"
             ">>>"
             "="
             "+="
             "-="
             "*="
             "/="
             "<<="
             ">>="
             ">>>="
             "&="
             "|="
             "^="
             "~="
             "?"
             ":"
             "typeof"
             "instanceof"
             "-"))
    . 'anirak/font-lock-operator)
   ))

(anirak/multi-font-lock-add-keywords
 (c-mode)
 `(
   ;; Operators
   (,(rx (or "+"
             "*"
             "/"
             "%"
             "++"
             "--"
             "=="
             "!="
             ">"
             "<"
             ">="
             "<="
             "&&"
             "||"
             "!"
             "&"
             "|"
             "^"
             "~"
             "<<"
             ">>"
             "="
             "+="
             "-="
             "*="
             "/="
             "%="
             "<<="
             ">>="
             "&="
             "^="
             "|="
             "sizeof"
             "?"
             ":"
             "->"
             "-"))
    . 'anirak/font-lock-operator)

   ;; Number literals
   (,(rx (and
          symbol-start
          (or (and (+ digit)
                   (? (and "." (* digit)))
                   (? (and (any "eE")
                           (? (any "-+"))
                           (+ digit))))
              (and "0"
                   (any "xX")
                   (+ hex-digit)))
          (? (or "f" "F"
                 "u" "U"
                 "l" "L"
                 "ll" "lL" "Ll" "LL"
                 "ul" "uL" "Ul" "UL"
                 "lu" "lU" "Lu" "LU"
                 "ull" "ulL" "uLl" "uLL" "Ull" "UlL" "ULl" "ULL"
                 "llu" "llU" "lLu" "lLU" "Llu" "LlU" "LLu" "LLU"))
          symbol-end))
    . 'anirak/font-lock-number-literal)

   ;; Noise
   (,(rx (any "{}[]();,.")) . 'anirak/font-lock-noise)
   ))

(anirak/multi-font-lock-add-keywords
 (c++-mode)
 `(
   ;; Operators
   (,(rx (or "+"
             "="
             "*"
             "/"
             "%"
             "++"
             "--"
             "=="
             "!="
             ">"
             "<"
             ">="
             "<="
             "<=>"
             "!"
             "&&"
             "||"
             "~"
             "&"
             "|"
             "^"
             ">>"
             "<<"
             "+="
             "-="
             "*="
             "/="
             "%="
             "&="
             "|="
             "^="
             "<<="
             ">>="
             "->"
             "?"
             ":"
             "::"
             "..."
             "-"))
    . 'anirak/font-lock-operator)

   ;; Number literals
   (,(rx (and
          symbol-start
          (or (and (+ digit)
                   (? (and "." (* digit)))
                   (? (and (any "eE")
                           (? (any "-+"))
                           (+ digit))))
              (and "0"
                   (any "xX")
                   (+ hex-digit)))
          (? (and (any "_" "A-Z" "a-z")
                  (* (any "_" "A-Z" "a-z" "0-9"))))
          symbol-end))
    . 'anirak/font-lock-number-literal)

   ;; Noise
   (,(rx (any "{}[]();,.")) . 'anirak/font-lock-noise)
   ))

(anirak/multi-font-lock-add-keywords
 (emacs-lisp-mode lisp-interaction-mode)
 `(
   ;; Boolean literals
   (,(rx (and symbol-start
              (or "t" "nil")
              symbol-end))
    . 'anirak/font-lock-boolean-literal)

   ;; Number literals
   (,(rx (and
          (or (and
               symbol-start
               (or
                (and
                 (? (any "-+"))
                 (+ digit)
                 (? (or (and (any "eE")
                             (? (any "-+"))
                             (or (+ digit)
                                 "INF"
                                 "NaN"))
                        (and "."
                             (? (and (+ digit)
                                     (? (and
                                         (any "eE")
                                         (? (any "-+"))
                                         (or (+ digit)
                                             "INF"
                                             "NaN")))))))))
                (and
                 "."
                 (+ digit)
                 (? (and
                     (any "eE")
                     (? (any "-+"))
                     (or (+ digit)
                         "INF"
                         "NaN"))))))
              (and "#"
                   symbol-start
                   (or (and (any "bB")
                            (? (any "-+"))
                            (+ (any "01")))
                       (and (any "oO")
                            (? (any "-+"))
                            (+ (any "0-7")))
                       (and (any "xX")
                            (? (any "-+"))
                            (+ hex-digit))
                       (and (or (and (any "1-2") (any "0-9"))
                                (and "3" (any "0-6"))
                                (any "2-9"))
                            (any "rR")
                            (? (any "-+"))
                            (+ (any "0-9a-zA-Z"))))))
          symbol-end))
    . 'anirak/font-lock-number-literal)

   ;; Noise
   (,(rx (any "().[],`'#@")) . 'anirak/font-lock-noise)
   ))

;; TODO: Make [] and () not highlighted in attributes.
(anirak/multi-font-lock-add-keywords
 (rustic-mode)
 `(;; Noise
   (,(rx (any "{}[]()'")) . 'anirak/font-lock-noise)

   ;; Boolean literals
   (,(rx (and symbol-start
              (or "false" "true")
              symbol-end))
    . 'anirak/font-lock-boolean-literal)

   ;; Number literals
   (,(rx (and symbol-start
              (or (and "0x"
                       (* "_")
                       hex-digit
                       (* (any hex-digit "_")))
                  (and "0o"
                       (* "_")
                       (any "0-7")
                       (* (any "0-7" "_")))
                  (and "0b"
                       (* "_")
                       (any "01")
                       (* (any "01" "_")))
                  (and (any "0-9")
                       (* (any "0-9" "_"))
                       (? (and "."
                               (any "0-9")
                               (* (any "0-9" "_")))))))
         symbol-end)
    . 'anirak/font-lock-number-literal)

   ;; Operators
   ;; URL `https://doc.rust-lang.org/book/appendix-02-operators.html'
   (,(rx (or "!"
             "!="
             "%"
             "%="
             "&"
             "&="
             "&&"
             "*"
             "*="
             "+"
             "+="
             ","
             "-"
             "-="
             "->"
             ".."
             "..="
             "..."
             "/"
             "/="
             ":"
             ";"
             "<<"
             "<<="
             "<"
             "<="
             "="
             "=="
             "=>"
             ">"
             ">="
             ">>"
             ">>="
             "@"
             "^"
             "^="
             "|"
             "|="
             "||"
             "?"
             "."))
    . 'anirak/font-lock-operator)
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'anirak)

(provide 'anirak)

;;; anirak-theme.el ends here
