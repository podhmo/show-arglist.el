;;; show-arglist.el --- showing arglist of function at point

;; Copyright (C) 2010 Kentarou Shimatani

;; Author: Kentaro Shimatani <kentarou.shimatani@gmail.com>
;; Created: 1 Oct 2010
;; Keywords: function

;;; Commentary:
;; Put this into your ~/.emacs.d/init.d or ~/.emacs:

;;  (require 'show-arglist)
;;  (show-arglist-mode t)

;; It will display arglist of function at point.

;;; Code:
(eval-when-compile (require 'cl))
(require 'help-fns)

(defgroup arglist-showing nil
  "Showing arglist of function at point."
  :prefix "show-arglist-"
  :group 'paren-matching)

(defcustom show-arglist-delay
  (if (featurep 'lisp-float-type) (/ (float 1) (float 8)) 1)
  "*Time in seconds to delay before showing a arglist of function."
  :type '(number :tag "seconds")
  :group 'arglist-showing)

(defcustom show-arglist-left-padding-string "  "
  "left padding string for show-arglist."
  :type 'string
  :group 'arglist-showing)

(defvar show-arglist-idle-timer nil)

(defun show-arglist-buffer-exist-p ()
  (loop for b in (buffer-list)
        when (with-current-buffer b
               show-arglist-mode)
        return t
        finally return nil))

;;;###autoload
(define-minor-mode show-arglist-mode
  "Toggle Show Arglist mode.
With prefix ARG, turn Show Arglist mode on if and only if ARG is positive.
Returns the new status of Show Arglist mode (non-nil means on).

When Show Arglist mode is enabled, any arglist of function is shown on header line
after `show-arglist-delay' seconds of Emacs idle time."
  :global t :group 'arglist-showing :lighter " Show-Arg"

  ;; Enable or disable the mechanism.
  ;; First get rid of the old idle timer.
  (when show-arglist-idle-timer
    (cancel-timer show-arglist-idle-timer))

  ;; If show-arglist-mode is enabled in some buffer now,
  ;; set up a new timer.
  (when (show-arglist-buffer-exist-p)
    (setq show-arglist-idle-timer (run-with-idle-timer
                                   show-arglist-delay t
                                   'show-arglist-function))))

(defun show-arglist-1 (function)
  "引数リストを取り出して，`header-line-format'にほり込む"
  (let* ((def (if (symbolp function)
                  (symbol-function function)
                  function))
         (arglist (help-function-arglist def)))

      ;; If definition is a keymap, skip arglist note.
      (unless (keymapp def)
        (let* ((usage (show-arglist-get-usage function def arglist))
               (usage (concat show-arglist-left-padding-string usage))
               (highlighted (help-highlight-arguments usage "")))
          (setq header-line-format (car highlighted))
          ;; for obsolete symbols
          (let ((obsolete (and
                           ;; function might be a lambda construct.
                           (symbolp function)
                           (get function 'byte-obsolete-info))))
            (when obsolete
              (show-arglist-edit-line-format-for-obsolete obsolete)))))))

(defun show-arglist-get-usage (function def arglist)
  (cond ((listp arglist)
         (format "%S" (help-make-usage function arglist)))
        ((stringp arglist) arglist)
        ;; Maybe the arglist is in the docstring of the alias.
        ((let ((fun function))
           (while (and (symbolp fun)
                       (setq fun (symbol-function fun))
                       (not (setq usage (help-split-fundoc
                                         (documentation fun)
                                         function)))))
           usage)
         (car usage))
        ((or (stringp def)
             (vectorp def))
         (format "\nMacro: %s" (format-kbd-macro def)))
        (t "[Missing arglist.  Please make a bug report.]")))

(defun show-arglist-edit-line-format-for-obsolete (obsolete)
  (setq header-line-format
        (format "%s(obsolete%s; %s)"
                show-arglist-left-padding-string
                (let ((message (nth 2 obsolete)))
                  (if message (format " since %s" message) ""))
                (let ((newer (car obsolete)))
                  (if (stringp newer) newer (format "use `%s' instead."))))))

(defun show-arglist-function ()
  "Display the full documentation of FUNCTION (a symbol)."
  (cond (show-arglist-mode
         (let ((function (function-called-at-point)))
           (unless (null function)
             (show-arglist-1 function))))
        (t (setq header-line-format nil)
           (message "show done"))))

;; if mode is off then clean header line
(defvar show-arglist-mode-off-hook nil)
(defun show-arglist-mode-clean-header-line ()
  (setq header-line-format nil))
(add-hook 'show-arglist-mode-off-hook 
          'show-arglist-mode-clean-header-line)

(provide 'show-arglist)

;;; show-arglist.el ends here
