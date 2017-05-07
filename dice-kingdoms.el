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

(defvar dice-kingdoms--game-area-width 100)
(defvar dice-kingdoms--game-area-height 25)
(defvar dice-kingdoms--play-area-dimensions '(2 2 96 20))
(defvar dice-kingdoms--buffer-name "*dice-kingdoms*")

(defun dice-kingdoms ()
  ""
  (interactive)
  (devenv-smart-open-elisp-output-window dice-kingdoms--buffer-name)
  (erase-buffer)
  (coordinate-initialize-view-area dice-kingdoms--game-area-width dice-kingdoms--game-area-height ".")
  (dice-kingdoms--setup-play-area)
  (dice-kingdoms--initialize-world)
  )

;; SETUP
;;
(defun dice-kingdoms--initialize-world ()
  ""
  (let* ((play-area-padding 4)
	 (territory-padding 2)
	 (num-of-territories-horizontal 8)
	 (partition-width (/ (- (nth 2 dice-kingdoms--play-area-dimensions) (* play-area-padding)) num-of-territories-horizontal))
	 (partition-height 10)
	 (territory-width (- partition-width (* 2 territory-padding)))
	 rolled)
    (dotimes (x-index num-of-territories-horizontal)
      (dice-kingdoms--create-territory (+ (* x-index partition-width) (random territory-width) play-area-padding)
				       15)
      ))
  )

(defun dice-kingdoms--create-territory (col row)
  ""
  (let ((play-area-x (car dice-kingdoms--play-area-dimensions))
	(play-area-y (car dice-kingdoms--play-area-dimensions)))
    (message "Placing at %s, %s" col row)
    (coordinate-place-char-at (+ play-area-x col) row "x"))
  )

(defun dice-kingdoms--setup-play-area ()
  ""
  (coordinate-place-char-at-area (car dice-kingdoms--play-area-dimensions) (nth 1 dice-kingdoms--play-area-dimensions)
		      (nth 2 dice-kingdoms--play-area-dimensions) (nth 3 dice-kingdoms--play-area-dimensions) "~")
  )

(provide 'dice-kingdoms)
;;; dice-kingdoms.el ends here
