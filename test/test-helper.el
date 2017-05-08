;;; test-helper --- Test helper for dice-kingdoms

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar dice-kingdoms-test-path
  (f-dirname (f-this-file)))

(defvar dice-kingdoms-root-path
  (f-parent dice-kingdoms-test-path))

(defvar dice-kingdoms-sandbox-path
  (f-expand "sandbox" dice-kingdoms-test-path))

(when (f-exists? dice-kingdoms-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" dice-kingdoms-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory dice-kingdoms-sandbox-path))
     (when (f-exists? dice-kingdoms-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir dice-kingdoms-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
  (require 'cl))
(require 'dice-kingdoms)

;;; test-helper.el ends here
