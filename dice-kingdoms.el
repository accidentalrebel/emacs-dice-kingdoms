;;; dice-kingdoms.el --- Dice Kingdoms Game          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Juan Karlo Licudine

;; Author: Juan Karlo Licudine <accidentalrebel@gmail.com>
;; Keywords: games
;; Version: 0.1

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

;; 

;;; Code:

(defvar dice-kingdoms--play-area-width 100)
(defvar dice-kingdoms--play-area-height 20)
(defvar dice-kingdoms--buffer-name "*dice-kingdoms*")

(defun dice-kingdoms ()
  ""
  (interactive)
  (devenv-smart-open-elisp-output-window dice-kingdoms--buffer-name)
  (erase-buffer)
  (coordinate-initialize-view-area dice-kingdoms--play-area-width dice-kingdoms--play-area-height "-")
  (dice-kingdoms--initialize-world)
  )

(defun dice-kingdoms--initialize-world ()
  ""
  (let ((partition-width (/ dice-kingdoms--play-area-width 5))
	(partition-height 10))
    (dotimes (x-index 5)
      (dice-kingdoms--create-territory
       (+ (* x-index partition-width) (random partition-width))
       10)
      ))
  )

(defun dice-kingdoms--create-territory (col row)
  ""
  (message "Placing at %s, %s" col row)
  (coordinate-place-char-at col row "x")
  )

(provide 'dice-kingdoms)
;;; dice-kingdoms.el ends here
