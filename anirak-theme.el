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
  '((cyan . "#9ccdff")
    (white . "#f0f8ff")
    (blue . "#599aff")
    (gray . "#757c88")
    ;; A 5% lighter version of `gray-darker` for things that are
    ;; important (like comments).
    (gray-darker-important . "#4c5059")
    (gray-darker . "#40444b")
    (gray-darkest . "#21232b")
    (black . "#0c0d10")
    (black-cursor-line . "#13141a")
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

(defmacro anirak/eval-with-colors (body)
  "Evaluate every sexpr in BODY while having colors from `anirak/colors' in scope."
  `(let ,(mapcar (lambda (x) `(,(car x) ,(cdr x))) anirak/colors)
     ,@body))

(defmacro anirak/set-theme (body)
  "Macro that simplifies the declaration of the theme.

Every color in every sexpr in BODY gets expanded to the actual color string."
  `(eval (cons 'custom-theme-set-faces (cons ''anirak
                (mapcar (lambda (x) `(quote (,(car x) ((t ,(cdr x))))))
                        (let ,(mapcar (lambda (x) `(,(car x) ,(cdr x))) anirak/colors)
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
;; TODO: Improve macro so that it isn't necessary to specify the
;; arguments in a list.
(anirak/set-theme
 `(
   ;; Standard font lock faces
   (default :foreground ,white :background ,black)
   (bold :weight bold)
   (bold-italic :slant italic :weight bold)
   (underline :underline t)
   (italic :slant italic)
   ;; :foreground
   (font-lock-builtin-face :foreground ,gray)
   ;; Comment prefix
   (font-lock-comment-delimiter-face :foreground ,gray-darker-important)
   ;; Text of a comment
   (font-lock-comment-face :foreground ,gray-darker-important)
   ;; \`...\'
   (font-lock-constant-face :foreground ,white :weight bold)
   ;; Doc comment of a function: (defun ... () "...")
   (font-lock-doc-face :foreground ,blue)
   (font-lock-doc-string-face :foreground ,debugging)
   ;; Function name
   (font-lock-function-name-face :foreground ,white :weight bold)
   ;; Noise symbols -> { } , ( ) [ ] :
   (anirak/font-lock-noise :foreground ,gray)
   ;; Number literals
   (anirak/font-lock-number-literal :foreground ,green)
   ;; Boolean literals
   (anirak/font-lock-boolean-literal :foreground ,green)
   ;; Operators
   (anirak/font-lock-operator :foreground ,gray)
   ;; Keyword
   (font-lock-keyword-face :foreground ,cyan)
   ;; In regexp string, the caret -> [^z-a]
   (font-lock-negation-char-face :foreground ,gray)
   (font-lock-preprocessor-face :foreground ,white :weight bold)
   (font-lock-preprocessor-char-face :foreground ,debugging)
   (font-lock-regexp-grouping-backslash :foreground ,gray-darker)
   (font-lock-regexp-grouping-construct :foreground ,gray)
   ;; String
   (font-lock-string-face :foreground ,blue)
   ;; TODO: Not sure
   (font-lock-type-face :foreground ,white :weight bold)
   ;; Variable name
   (font-lock-variable-name-face :foreground ,white)
   ;; Warning (like `error')
   (font-lock-warning-face :weight bold :foreground ,lint-warning-fg :background ,lint-warning-bg)
   (shadow :foreground ,gray)
   ;; TODO: Not sure
   (success :foreground ,lint-hint-fg :background ,lint-hint-bg)
   ;; TODO: Not sure
   (error :foreground ,lint-error-fg :background ,lint-error-bg)
   ;; TODO: Not sure
   (warning :foreground ,lint-warning-fg :background ,lint-warning-bg)
   (tooltip :foreground ,debugging :background ,green :inverse-video t)

   ;; Emacs interface
   ;; NOTE: Emacs ignores foreground for cursor.
   (cursor :background ,blue-cursor :foreground ,white)
   ;; TODO: Long line break arrow fringe
   (fringe :background ,black :foreground ,gray-darker)
   ;; Line numbers
   (linum :background ,black :foreground ,gray-darker)
   (line-number :background ,black :foreground ,gray-darker)
   (line-number-current-line :inherit line-number :foreground ,white)
   (fill-column-indicator :foreground ,debugging :weight normal :slant normal
                          :underline nil :overline nil :strike-through nil
                          :box nil :inverse-video nil :stipple nil)
   ;; Vertical border that separates windows
   (vertical-border :foreground ,gray-darkest)
   (border :background ,debugging :foreground ,debugging)
   ;; Visual selection
   (highlight :background ,blue-selection :foreground nil)
   ;; Highlight current line
   (hl-line :background ,black-cursor-line)
   ;; Active mode line
   (mode-line :foreground ,white :background ,gray-darkest :weight normal
              :box (:line-width 1 :color ,gray-darkest))
   ;; Like *Messages*
   (mode-line-buffer-id :foreground nil, :background nil)
   ;; Inactive mode line
   (mode-line-inactive :inherit mode-line
                       :foreground ,gray-darker
                       :background ,black)
   (mode-line-emphasis :foreground ,debugging :slant italic)
   ;; TODO: Not sure
   (mode-line-highlight :background ,blue-selection :box nil)
   (minibuffer-prompt :foreground ,gray)
   ;; Visual selection
   (region :background ,blue-selection :foreground nil :extend t)
   (secondary-selection :background ,debugging :foreground nil :extend t)
   ;; Header line
   (header-line :inherit mode-line-inactive :foreground ,white :background nil)

   ;; search
   (match :foreground ,lint-info-fg :background ,lint-info-bg)
   ;; Active search result
   (isearch :background ,highlight-active)
   ;; Inactive search results
   (lazy-highlight :background ,highlight-inactive)
   ;; Failed search
   (isearch-fail :background ,lint-error-bg :foreground ,lint-error-fg :weight bold)
   ;; Links like in Help mode
   (link :foreground ,cyan :underline t)
   (widget-button :foreground ,debugging :background ,debugging :underline t)
   ;; Like a search bar background.
   (widget-field :background ,menu-inactive-bg :box (:line-width 1 :color ,gray-darker))
   ;; Buttons.
   (custom-button :background ,gray-darker :foreground ,white)
   ;; Buttons on mouse hover.
   ;; TODO: On mouse down.
   (custom-button-mouse :background ,gray-darkest :foreground nil)
   ;; Like the message "NO CUSTOMIZATION DATA; not intended to be
   ;; customized" in custom-mode.
   (custom-state :background ,lint-hint-bg :foreground ,lint-hint-fg)
   ;; Like the face tag in custom-mode.
   (custom-face-tag :foreground ,white :weight bold)

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
   (diff-context :background ,debugging :foreground ,green)
   (diff-file-header :background ,debugging :foreground ,green)
   (diff-function :background ,debugging :foreground ,green)
   (diff-header :background ,debugging :foreground ,green)
   (diff-hunk-header :background ,debugging :foreground ,green)
   (diff-index :background ,debugging :foreground ,green)
   (diff-nonexistent :background ,debugging :foreground ,green)
   (diff-refined-added :background ,debugging :foreground ,green)
   (diff-refined-changed :background ,debugging :foreground ,green)
   (diff-refined-removed :background ,debugging :foreground ,green)

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
   (eshell-prompt :foreground ,gray :weight bold)
   ;; .zip files for example
   (eshell-ls-archive :foreground ,blue)
   ;; .bak files for example
   (eshell-ls-backup :foreground ,gray)
   (eshell-ls-clutter :foreground ,debugging :weight bold)
   (eshell-ls-directory :foreground ,blue :weight bold)
   (eshell-ls-executable :foreground ,green :weight bold)
   (eshell-ls-missing :foreground ,debugging :weight bold)
   (eshell-ls-product :foreground ,debugging)
   (eshell-ls-readonly :foreground ,debugging)
   (eshell-ls-special :foreground ,debugging :weight bold)
   (eshell-ls-symlink :foreground ,cyan :weight bold)
   ;; TODO: Not sure
   (eshell-ls-unreadable :foreground ,gray-darker)

   ;; ElDoc (built-in)
   ;; Bottom line function ARGS keyword
   (eldoc-highlight-function-argument :foreground ,green :weight bold)

   ;; Flycheck (built-in)
   (flycheck-error :underline (:style wave :color ,lint-error-fg))
   (flycheck-info :underline (:style wave :color ,lint-info-fg))
   (flycheck-warning :underline (:style wave :color ,lint-warning-fg))
   (flycheck-fringe-error :background ,lint-error-fg :foreground ,lint-error-fg)
   (flycheck-fringe-info :background ,lint-info-fg :foreground ,lint-info-fg)
   (flycheck-fringe-warning :background ,lint-warning-fg, :foreground ,lint-warning-fg)
   (flycheck-color-mode-line-error-face :foreground ,debugging)
   (flycheck-color-mode-line-warning-face :foreground ,debugging)
   (flycheck-color-mode-line-info-face :foreground ,debugging)
   (flycheck-color-mode-line-running-face :foreground ,debugging)
   (flycheck-color-mode-line-success-face :foreground ,debugging)

   ;; Parenthesis matching (built-in)
   (show-paren-match :background ,gray-darkest)
   (show-paren-mismatch :background nil :foreground nil)

   ;; whitespace (built-in)
   (whitespace-big-indent :background ,debugging :foreground ,debugging)
   (whitespace-empty :background ,debugging :foreground ,debugging)
   (whitespace-hspace :background ,debugging :foreground ,debugging)
   (whitespace-indentation :background ,debugging :foreground ,debugging)
   ;; Too long line
   (whitespace-line :underline (:style wave :color ,lint-error-fg))
   (whitespace-newline :foreground ,gray-darker)
   (whitespace-space :foreground ,gray-darker)
   (whitespace-space-after-tab :foreground ,gray-darker)
   (whitespace-space-before-tab :foreground ,gray-darker)
   (whitespace-tab :foreground ,gray-darker)
   (whitespace-trailing :underline (:style wave :color ,lint-error-fg))
   (trailing-whitespace :inherit whitespace-trailing)

   ;; window-divider (built-in)
   (window-divider :foreground ,debugging)
   (window-divider-first-pixel :foreground ,debugging)
   (window-divider-last-pixel :foreground ,debugging)

   ;; company
   (company-preview :foreground ,debugging :background ,debugging)
   ;; Last suggestion that gets previewed in the line
   (company-preview-common :background ,lint-info-bg :foreground ,lint-info-fg)
   (company-preview-search :inherit company-preview :foreground ,debugging)
   ;; Company completion menu inactive rows
   (company-tooltip :background ,menu-inactive-bg :foreground ,gray)
   ;; Company completion menu active row
   (company-tooltip-selection :background ,menu-active-bg :foreground ,white)
   ;; Company completion menu active text in all rows
   (company-tooltip-common :foreground ,white :weight bold)
   ;; Company completion menu active text in active row
   (company-tooltip-common-selection :inherit company-tooltip-common)
   (company-tooltip-search :inherit company-tooltip :foreground ,debugging)
   (company-tooltip-annotation :inherit company-tooltip :foreground ,gray)
   (company-tooltip-annotation-selection :inherit company-tooltip-selection :foreground ,gray)
   ;; Scrollbar thumb background
   (company-scrollbar-bg :background ,menu-inactive-bg)
   ;; Scrollbar background
   (company-scrollbar-fg :background ,menu-active-bg)
   (company-echo-common :inherit company-echo :foreground ,debugging)

   ;; Ivy
   ;; TODO: Mouse hover
   (ivy-action :foreground ,debugging)
   (ivy-confirm-face :foreground ,debugging)
   (ivy-current-match :background ,gray-darkest :foreground ,cyan :weight bold)
   (ivy-cursor :background ,blue-cursor)
   ;; Like the (match-required) message in swiper
   (ivy-match-required-face :foreground ,lint-error-fg :background ,lint-error-bg)
   (ivy-remote :foreground ,debugging)
   (ivy-subdir :foreground ,cyan)
   (ivy-virtual :foreground ,gray)
   (ivy-minibuffer-match-face-1 :foreground ,white :weight bold)
   (ivy-minibuffer-match-face-2 :foreground ,white :weight bold)
   (ivy-minibuffer-match-face-3 :foreground ,white :weight bold)
   (ivy-minibuffer-match-face-4 :foreground ,white :weight bold)
   (ivy-highlight-face :foreground ,debugging :weight bold)
   (ivy-minibuffer-match-highlight :foreground ,debugging)
   (ivy-modified-buffer :foreground ,white)
   (ivy-modified-outside-buffer :foreground ,white)
   (ivy-prompt-match :foreground ,debugging)
   (ivy-separator :foreground ,debugging)
   (ivy-grep-info :foreground ,debugging)
   (ivy-grep-line-number :foreground ,debugging)
   (ivy-completions-annotations :foreground ,debugging)
   (ivy-yanked-word :foreground ,debugging)

   ;; markdown
   (markdown-url-face :foreground ,white :underline t)
   (markdown-link-face :foreground ,blue)
   ;; TODO: Search the face for *Helpful* mouse hover
   (markdown-highlight-face :background ,blue-selection :foreground ,white)
   (markdown-markup-face :foreground ,gray)
   ;; TODO: Define fixed-pitch font
   (markdown-code-face :inherit fixed-pitch)
   ;; TODO: Define fixed-pitch font
   (markdown-inline-code-face :inherit markdown-code-face)

   ;; lsp
   ;; TODO: Find face for the white sideline text.
   (lsp-face-highlight-textual :background ,highlight-active)
   (lsp-face-highlight-read :background ,highlight-inactive)
   (lsp-face-highlight-write :background ,highlight-active)
   (lsp-ui-doc-background :background ,menu-inactive-bg)
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
   (evil-ex-commands :background ,green :foreground ,debugging)
   ;; Warnings and errors on the evil command line
   (evil-ex-info :background ,lint-warning-bg :foreground ,lint-warning-fg)
   (evil-ex-lazy-highlight :background ,green :foreground ,debugging)
   (evil-ex-search :background ,green :foreground ,debugging)
   (evil-ex-substitute-matches :background ,debugging :foreground ,green)
   (evil-ex-substitute-replacement :background ,debugging :foreground ,green)

   ;; hydra
   (hydra-face-red :background ,lint-error-bg :foreground ,lint-error-fg)
   (hydra-face-teal :background ,green :foreground ,debugging)
   (hydra-face-pink :background ,green :foreground ,debugging)
   (hydra-face-blue :background ,green :foreground ,debugging)
   (hydra-face-amaranth :background ,green :foreground ,debugging)

   ;; evil-mc
   (evil-mc-cursor-bar-face :foreground ,debugging :background ,green)
   ;; Non active cursors.
   (evil-mc-cursor-default-face :background ,highlight-active)
   (evil-mc-cursor-hbar-face :foreground ,debugging :background ,green)
   ;; Non active cursors selections.
   (evil-mc-region-face :background ,highlight-inactive)

   ;; evil-snipe
   ;; (evil-snipe-first-match-face (:foreground ,debugging :background ,green))
   ;; (evil-snipe-matches-face (:foreground ,debugging :background ,green))
   ))

(anirak/eval-with-colors
 (
  ;; fill-column-indicator
  (setq fci-rule-width 1
        fci-rule-color gray-darkest)

  ;; lsp-ui
  (setq lsp-ui-doc-border gray-darker)

  ;; rustic
  (setq rustic-ansi-faces `[,term-0 ,term-1 ,term-2 ,term-3
                                    ,term-4 ,term-5 ,term-6 ,term-7])
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

(defmacro anirak/multi-font-lock-add-keywords (modes alist)
  "Similiar to `font-lock-add-keywords' but let's you apply ALIST to every mode in MODES."
  `(mapc
    (lambda (x) (font-lock-add-keywords x ,alist))
    ',modes))

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

;;; anirak-theme.el ends here
