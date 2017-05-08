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

(defvar dice-kingdoms--game-area-width 80 "The width of the game area.")
(defvar dice-kingdoms--game-area-height 35 "The height of the game area.")
(defvar dice-kingdoms--play-area-dimensions '(2 2 76 31) "The dimensions of the area.  The data are as follows: (START_COL START_ROW WIDTH HEIGHT).")
(defvar dice-kingdoms--territory-list '() "The list containing the territories.")
(defvar dice-kingdoms--buffer-name "*dice-kingdoms*" "The name of the buffer for this game.")

(defun dice-kingdoms ()
  ""
  (interactive)
  (when (not (string= (buffer-name) dice-kingdoms--buffer-name))
    (devenv-smart-open-elisp-output-window dice-kingdoms--buffer-name))

  (setq dice-kingdoms--territory-list '())
  
  (erase-buffer)
  (coordinate-initialize-view-area dice-kingdoms--game-area-width dice-kingdoms--game-area-height ".")
  (dice-kingdoms--setup-play-area)
  ;;  (dice-kingdoms--initialize-world)
  (dice-kingdoms--generate-map)
  )

;; SETUP
;;
(defun dice-kingdoms--initialize-world ()
  ""
  (let* ((play-area-padding 4)
	 (territory-padding 2)
	 (num-of-territories-horizontal 8)
	 (num-of-territories-vertical 3)
	 (partition-width (/ (- (nth 2 dice-kingdoms--play-area-dimensions) (* play-area-padding)) num-of-territories-horizontal))
	 (partition-height (/ (- (nth 3 dice-kingdoms--play-area-dimensions) (* play-area-padding)) num-of-territories-vertical))
	 (territory-width (- partition-width (* 2 territory-padding)))
	 (territory-height (- partition-height (* 2 territory-padding)))
	 current-territory)
    (dotimes (y-index num-of-territories-vertical)
      (dotimes (x-index num-of-territories-horizontal)
	(setq current-territory (dice-kingdoms--create-territory (+ (* x-index partition-width) (random territory-width) play-area-padding)
								 (+ (* y-index partition-height) (random territory-height) play-area-padding)))
	(dice-kingdoms--display-territory current-territory)
	)
      ))
  )

(defun dice-kingdoms--generate-map ()
  ""
  (let ((play-area-padding 4)
	(num-of-territories 30)
	(play-area-width (nth 2 dice-kingdoms--play-area-dimensions))
	(play-area-height (nth 3 dice-kingdoms--play-area-dimensions))
	rolled-col
	rolled-row
	current-territory)
    (dotimes (count num-of-territories)
      (setq rolled-col (+ (random (- play-area-width play-area-padding)) play-area-padding))
      (setq rolled-row (+ (random (- play-area-height play-area-padding)) play-area-padding))
      (when (not (dice-kingdoms--get-overlapping-owned-coordinates rolled-col rolled-row))
	(setq current-territory (dice-kingdoms--create-territory rolled-col rolled-row))
	(dice-kingdoms--display-territory current-territory))
      )
    )
  )

(defun dice-kingdoms--get-overlapping-owned-coordinates (col row)
  "Check if the given COL and ROW coordinate is already occupied.
Considers all owned coordinates of each territory when checking."
  (let ((equal-list '()))
    (dolist (territory dice-kingdoms--territory-list)
      (dolist (territory-coordinate (dice-kingdoms--get-initial-territory-coordinates (plist-get territory ':col) (plist-get territory ':row)))
	(dolist (to-check-coordinate (dice-kingdoms--get-initial-territory-coordinates col row))
	  (when (and (eq (car territory-coordinate) (car to-check-coordinate))
		     (eq (cadr territory-coordinate) (cadr to-check-coordinate)))
	    (push territory-coordinate equal-list)
	    )
	  )
	)
      )
    equal-list))

(defun dice-kingdoms--get-initial-territory-coordinates (col row)
  "Gets the surronding coordinates for the given COL and ROW."
  `((,col ,row)
    (,(- col 1) ,(- row 1))
    (,col ,(- row 1))
    (,(+ col 1) ,(- row 1))
    (,(- col 1) ,row)
    (,(+ col 1) ,row)
    (,(- col 1) ,(+ row 1))
    (,col ,(+ row 1))
    (,(+ col 1) ,(+ row 1)))
  )

(defun dice-kingdoms--create-territory (col row)
  ""
  (let* ((owned (dice-kingdoms--get-initial-territory-coordinates col row))
	 (territory-plist `(:col ,col :row ,row :owned ,owned)))
    (message "Added: %s" territory-plist)
    (push territory-plist dice-kingdoms--territory-list)
    territory-plist
    )
  )

(defun dice-kingdoms--display-territory (territory)
  ""
  (let ((play-area-x (car dice-kingdoms--play-area-dimensions))
	(play-area-y (nth 1 dice-kingdoms--play-area-dimensions))
	(current-col (plist-get territory ':col))
	(current-row (plist-get territory ':row)))
    (message "Placing at %s, %s" current-col current-row)
    (coordinate-place-char-at (+ play-area-x current-col) (+ play-area-y current-row) "x")
    )
  )

(defun dice-kingdoms--setup-play-area ()
  ""
  (coordinate-place-char-at-area (car dice-kingdoms--play-area-dimensions) (nth 1 dice-kingdoms--play-area-dimensions)
		      (nth 2 dice-kingdoms--play-area-dimensions) (nth 3 dice-kingdoms--play-area-dimensions) "~")
  )

(provide 'dice-kingdoms)
;;; dice-kingdoms.el ends here
