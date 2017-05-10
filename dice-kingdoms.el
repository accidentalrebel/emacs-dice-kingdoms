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
  (dice-kingdoms--generate-map)
  )

;; SETUP
;;
(defun dice-kingdoms--generate-map ()
  ""
  (let ((play-area-padding 3)
	(num-of-territories 30)
	(play-area-width (nth 2 dice-kingdoms--play-area-dimensions))
	(play-area-height (nth 3 dice-kingdoms--play-area-dimensions))
	rolled-col
	rolled-row
	current-territory
	roll-successful)
    (dotimes (count num-of-territories)
      (setq roll-successful nil)
      (while (not roll-successful)
	(setq rolled-col (+ (random (- play-area-width (* 2 play-area-padding))) play-area-padding))
	(setq rolled-row (+ (random (- play-area-height (* 2 play-area-padding))) play-area-padding))
	(setq current-territory (dice-kingdoms--create-territory rolled-col rolled-row))
	(when (not (dice-kingdoms--get-overlapping-owned-coordinates dice-kingdoms--territory-list current-territory))
	  (push current-territory dice-kingdoms--territory-list)
	  (dice-kingdoms--display-territory current-territory)
	  (setq roll-successful t)
	  ))
      )
    (dice-kingdoms--expand-territory (car dice-kingdoms--territory-list))
    )
  )

(defun dice-kingdoms--expand-territory (territory)
  "Expands the TERRITORY until it reaches a neighbor."
  (let (coordinate-to-expand-from
	coordinate-to-expand-to)
    (dotimes (index 3)
      (setq coordinate-to-expand-from (dice-kingdoms--roll-random-owned-coordinate territory))
      (setq coordinate-to-expand-to (dice-kingdoms--get-coordinate-at-direction coordinate-to-expand-from))
      (message "Start %s" coordinate-to-expand-from)
      (if (equal (dice-kingdoms--get-owner-of-coordinate coordinate-to-expand-to) nil)
	  (message "CAN EXPAND! %s" coordinate-to-expand-to)
	(message "CANT EXPAND! %s" coordinate-to-expand-to))
      )))

(defun dice-kingdoms--roll-random-owned-coordinate (territory)
  "Gets a random owned coordinate from a given TERRITORY."
  (let ((owned (plist-get territory ':owned)))
    (nth (random (length owned)) owned)))

(defun dice-kingdoms--get-coordinate-at-direction (start-coordinate)
  "Gets the coordinate from START-COORDINATE and a given DIRECTION."
  (let ((col (car start-coordinate))
	(row (cadr start-coordinate)))
    `(,(+ col 1) ,row)))

(defun dice-kingdoms--get-owner-of-coordinate (to-check-coordinate)
  "Gets the owner at the given TO-CHECK-COORDINATE."
  (catch 'found-owner
    (dolist (territory dice-kingdoms--territory-list ())
      (dolist (coordinate (plist-get territory ':owned))
	(when (and (eq (car coordinate) (car to-check-coordinate))
		   (eq (cadr coordinate) (cadr to-check-coordinate)))
	  (throw 'found-owner territory)
	  )
	)
      )
    ))

(defun dice-kingdoms--get-overlapping-owned-coordinates (territory-list to-check-territory)
  "Get the overlapping between all from TERRITORY-LIST and TO-CHECK-TERRITORY.
Considers all owned coordinates of each territory when checking."
  (let ((equal-list '()))
    (dolist (territory territory-list)
      (dolist (territory-coordinate (plist-get territory ':owned))
	(dolist (to-check-coordinate (plist-get to-check-territory ':owned))
	  ;;(message "%s ?= %s" territory-coordinate to-check-coordinate)
	  (when (and (eq (car territory-coordinate) (car to-check-coordinate))
		     (eq (cadr territory-coordinate) (cadr to-check-coordinate)))
	    ;;(message "EQUALLL!!!")
	    (push territory-coordinate equal-list)
	    )
	  )
	)
      )
    equal-list))

(defun dice-kingdoms--get-initial-territory-coordinates (col row)
  "Gets the surronding coordinates for the given COL and ROW."
  `((,(- col 1) ,(- row 1))
    (,col ,(- row 1))
    (,(+ col 1) ,(- row 1))
    (,(- col 1) ,row)
    (,col ,row)
    (,(+ col 1) ,row)
    (,(- col 1) ,(+ row 1))
    (,col ,(+ row 1))
    (,(+ col 1) ,(+ row 1)))
  )

(defun dice-kingdoms--create-territory (col row)
  ""
  (let* ((owned (dice-kingdoms--get-initial-territory-coordinates col row))
	 (territory-plist `(:col ,col :row ,row :owned ,owned)))
    ;;(message "Added: %s" territory-plist)
    territory-plist
    )
  )

(defun dice-kingdoms--display-territory (territory)
  "Print the TERRITORY and its owned coordiates onto the map."
  (let ((play-area-x (car dice-kingdoms--play-area-dimensions))
	(play-area-y (nth 1 dice-kingdoms--play-area-dimensions))
	(current-col (plist-get territory ':col))
	(current-row (plist-get territory ':row)))
    ;;(message "Placing at %s, %s" current-col current-row)
    (dolist (coordinate (plist-get territory ':owned))
      (coordinate-place-char-at (+ play-area-x (car coordinate)) (+ play-area-y (cadr coordinate)) "x")
      )
    (coordinate-place-char-at (+ play-area-x current-col) (+ play-area-y current-row) "X")
    )
  )

(defun dice-kingdoms--setup-play-area ()
  "Setup the play area.
This is diferent from the game area.  The play area is the place where territories are placed."
  (coordinate-place-char-at-area (car dice-kingdoms--play-area-dimensions) (nth 1 dice-kingdoms--play-area-dimensions)
		      (nth 2 dice-kingdoms--play-area-dimensions) (nth 3 dice-kingdoms--play-area-dimensions) "~")
  )

(provide 'dice-kingdoms)
;;; dice-kingdoms.el ends here
