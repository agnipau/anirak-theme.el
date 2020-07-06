;;; anirak-light-theme.el --- Light theme based on the Anirak VSCode theme

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

;; This package include a light theme based on the Anirak VSCode theme.
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

(require 'anirak)

(deftheme anirak-light
  "Light theme based on the Anirak VSCode theme.")

(anirak/set-theme light)

(provide-theme 'anirak-light)

;;; anirak-light-theme.el ends here
