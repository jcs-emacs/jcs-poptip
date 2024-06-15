;;; jcs-poptip.el --- Generic popup tip  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-emacs/jcs-poptip
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (company "0.8.12") (lsp-ui "8.0.1") (preview-it "1.1.0") (define-it "0.2.5") (msgu "0.1.0" ) (elenv "0.1.0" ))
;; Keywords: help

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Generic popup tip
;;

;;; Code:

(require 'elenv)
(require 'msgu)

(require 'company)
(require 'lsp-mode)
(require 'lsp-ui)
(require 'preview-it)
(require 'define-it)  ; this includes `popup', `pos-tip', and `posframe'

(defgroup jcs-poptip nil
  "Generic popup tip."
  :group 'convenience)

(defcustom jcs-poptip-text-scale-level 0
  "Text scale amount for doc buffer."
  :type 'integer
  :group 'jcs-poptip)

(defcustom jcs-poptip-background-color "#2A2D38"
  "Background color string."
  :type 'string
  :group 'jcs-poptip)

(defcustom jcs-poptip-foreground-color "#F1F1F1"
  "Foreground color string."
  :type 'string
  :group 'jcs-poptip)

(defconst jcs-poptip--buffer-name "*jcs-poptip*"
  "Buffer name for posframe tooltip.")

(defvar jcs-poptip-frame nil
  "Hold the frame.")

;;
;;; Externals

(defvar company-fuzzy-mode)
(defvar company-fuzzy--backends)
(defvar company-backends)

(declare-function company-dict--relevant-dicts "ext:company-dic.el")
(declare-function company-dict--quickhelp-string "ext:company-dic.el")

(declare-function dap-tooltip-mouse-motion "ext:dap-mouse.el")
(declare-function lsp-ui-doc--tooltip-mouse-motion "ext:lsp-ui-doc.el")

;;
;;; Modes

(define-minor-mode jcs-poptip-frame-mode
  "Marker mode to add additional key bind for poptip frame."
  :init-value nil
  :lighter ""
  :group jcs-poptip
  :keymap `(([?q] . jcs-poptip-unfocus-frame))
  (when jcs-poptip-frame-mode
    (buffer-disable-undo)
    (let ((text-scale-mode-step 1.1))
      (text-scale-set jcs-poptip-text-scale-level))))

(defun jcs-poptip-focus-frame ()
  "Focus doc frame."
  (interactive)
  (when (and jcs-poptip-frame
             (frame-visible-p jcs-poptip-frame))
    (select-frame-set-input-focus jcs-poptip-frame)))

(defun jcs-poptip-unfocus-frame ()
  "Unfocus doc frame."
  (interactive)
  (when (and jcs-poptip-frame
             (frame-visible-p jcs-poptip-frame))
    (select-frame-set-input-focus (frame-parent jcs-poptip-frame))))

;;
;;; Util

(defun jcs-poptip-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

(defun jcs-poptip--posframe-window ()
  "Return the posframe window displaying `jcs-poptip--buffer-name'."
  (frame-selected-window
   (buffer-local-value 'posframe--frame
                       (get-buffer jcs-poptip--buffer-name))))

;;
;;; Core

(defun jcs-poptip--next-post ()
  "Hide tooltip after first post command."
  (posframe-hide jcs-poptip--buffer-name)
  (remove-hook 'post-command-hook #'jcs-poptip--next-post))

(defun jcs-poptip--pre ()
  "Register for next pre command."
  (when-let* (((not (minibufferp)))
              ((not (memq this-command '( jcs-poptip-focus jcs-poptip-focus-frame
                                          handle-switch-frame))))
              (buffer (window-buffer (jcs-poptip--posframe-window)))
              ((not (equal (current-buffer) buffer))))
    (add-hook 'post-command-hook #'jcs-poptip--next-post)
    (remove-hook 'pre-command-hook #'jcs-poptip--pre)))

(cl-defun jcs-poptip-create (string &key point (timeout 300) (height 30))
  "Pop up an tooltip depends on the graphic used.

STRING is the content of the toolip.  The location POINT.  TIMEOUT for not
forever delay.  HEIGHT of the tooltip that will display."
  (let ((bg jcs-poptip-background-color)
        (fg jcs-poptip-foreground-color)
        (fringe-width 10))
    (cond (elenv-graphic-p
           (with-current-buffer (get-buffer-create jcs-poptip--buffer-name)
             (jcs-poptip-frame-mode 1)
             (setq-local buffer-read-only nil))
           (setq jcs-poptip-frame
                 (posframe-show jcs-poptip--buffer-name
                                :string string :position point
                                :timeout timeout
                                :background-color bg :foreground-color fg
                                :internal-border-width 1
                                :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                                :left-fringe fringe-width :right-fringe fringe-width
                                :override-parameters
                                `((left                     . -1)
                                  (no-focus-on-map          . t)
                                  (min-width                . 0)
                                  (width                    . 0)
                                  (min-height               . 0)
                                  (height                   . 0)
                                  (internal-border-width    . 1)
                                  (menu-bar-lines           . 0)
                                  (tool-bar-lines           . 0)
                                  (tab-bar-lines            . 0)
                                  (tab-bar-lines-keep-state . 0)
                                  (line-spacing             . 0)
                                  (unsplittable             . t)
                                  (undecorated              . t)
                                  (top                      . -1)
                                  (visibility               . nil)
                                  (mouse-wheel-frame        . nil)
                                  (no-other-frame           . t)
                                  (inhibit-double-buffering . t)
                                  (drag-internal-border     . t)
                                  (no-special-glyphs        . t)
                                  (desktop-dont-save        . t)
                                  (vertical-scroll-bars     . t)
                                  (default-minibuffer-frame . ,(selected-frame))
                                  (minibuffer               . ,(minibuffer-window)))
                                :accept-focus t))
           (with-current-buffer (get-buffer-create jcs-poptip--buffer-name)
             (setq-local buffer-read-only t
                         cursor-type 'hbar))
           (select-frame-set-input-focus (frame-parent jcs-poptip-frame))
           (add-hook 'pre-command-hook #'jcs-poptip--pre))
          (t
           (popup-tip string :point point :around t :height height :scroll-bar t :margin t)))
    t))

(defun jcs-poptip--describe-symbol-string ()
  "Return the describe symbol string."
  (let ((thing (symbol-at-point)))
    (save-window-excursion
      (with-current-buffer (help-buffer)
        (let (buffer-read-only) (erase-buffer))
        (msgu-silent (describe-symbol thing))
        (buffer-string)))))

(defun jcs-poptip--fill-text (text &optional column)
  "Fill TEXT with COLUMN size."
  (with-temp-buffer
    (let ((fill-column (or column (frame-width))))
      (insert text)
      (fill-region (point-min) (point-max))
      (buffer-string))))

(defun jcs-poptip--describe-it ()
  "Describe symbol at point."
  (when-let* (((derived-mode-p 'lisp-data-mode))
              (desc (jcs-poptip--describe-symbol-string))
              (desc (jcs-poptip--fill-text desc)))
    (if (or (string-empty-p desc)
            (string= (string-trim desc) "[back]"))
        (error "[ERROR] No description at point")
      (jcs-poptip-create desc :point (point)))))

(defun jcs-poptip--company-backends ()
  "Return a list of company backends."
  (if (bound-and-true-p company-fuzzy-mode)
      company-fuzzy--backends
    company-backends))

(defun jcs-poptip--company-doc-buffer (backend thing)
  "Return command `doc-buffer' from BACKEND and THING."
  (when-let ((buf (ignore-errors (funcall backend 'doc-buffer thing))))
    (with-current-buffer buf
      (let ((trimmed (string-trim (buffer-string))))
        (if (or (string-empty-p trimmed)
                (string= trimmed "nil"))
            nil
          (buffer-string))))))

(defun jcs-poptip--company-doc ()
  "Return company documentation."
  (let ((thing (jcs-poptip-2str (symbol-at-point)))
        (desc))
    (msgu-silent
      (cl-some (lambda (backend)
                 (ignore-errors (funcall backend 'candidates thing))  ; refresh
                 (setq desc
                       (or (jcs-poptip--company-doc-buffer backend thing)
                           (ignore-errors (funcall backend 'quickhelp-string thing))
                           (ignore-errors (funcall backend 'meta thing))))
                 desc)
               (jcs-poptip--company-backends)))
    (jcs-poptip-create (string-trim desc) :point (point))))

(defun jcs-poptip--company-dict ()
  "Describe symbol at point."
  (when (featurep 'company-dict)
    (let* ((thing (jcs-poptip-2str (symbol-at-point)))  ; this has no use
           (dicts (company-dict--relevant-dicts))
           (mem (member thing dicts))                   ; it stores in text property
           (desc (company-dict--quickhelp-string (car mem))))
      (jcs-poptip-create desc :point (point)))))

;;
;;; LSP

(defun jcs-poptip--lsp-pre (&rest _)
  "Post command for LSP doc."
  (unless (memq this-command '( handle-switch-frame
                                lsp-ui-doc--tooltip-mouse-motion
                                dap-tooltip-mouse-motion))
    (unless (memq this-command '( jcs-poptip-focus jcs-poptip-focus-frame))
      (lsp-ui-doc--hide-frame))
    (remove-hook 'pre-command-hook #'jcs-poptip--lsp-pre)))

;;
;;; API

;;;###autoload
(defun jcs-poptip-focus ()
  "Unfocus poptip."
  (interactive)
  (ignore-errors (jcs-poptip-focus-frame))
  (ignore-errors (lsp-ui-doc-focus-frame)))

;;;###autoload
(defun jcs-poptip-unfocus ()
  "Unfocus poptip."
  (interactive)
  (ignore-errors (jcs-poptip-unfocus-frame))
  (ignore-errors (lsp-ui-doc-unfocus-frame))
  (add-hook 'pre-command-hook #'jcs-poptip--lsp-pre))

;;;###autoload
(defun jcs-poptip ()
  "Show current symbol info."
  (interactive)
  (company-abort)
  (if (bound-and-true-p lsp-managed-mode)  ; if lsp is connected
      (or (ignore-errors (call-interactively #'lsp-ui-doc-glance))
          (ignore-errors (call-interactively #'lsp-ui-doc-show)))
    (cond ((ignore-errors (jcs-poptip--describe-it)))
          ((or (ignore-errors (jcs-poptip--company-dict))
               (ignore-errors (jcs-poptip--company-doc))))
          ((ignore-errors (preview-it)))
          (t (define-it-at-point)))
    ;; In case we are using region, cancel the select region.
    (deactivate-mark)))

(provide 'jcs-poptip)
;;; jcs-poptip.el ends here
