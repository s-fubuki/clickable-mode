;;; clickable-mode.el --- Clickable minor mode.
;; Copyright (C) 2024 fubuki

;; Author: fubuki at frill.org
;; Version: @(#)$Revision: 1.8 $
;; Keywords: multimedia

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Make the URL string clickable.

;;; Install:

;; (require 'clickable-mode)
;;
;; (easy-menu-add-item
;;  global-map '(menu-bar)
;;  '("Private" ["Clickable" clickable-mode
;;               :style toggle :selected clickable-mode]))

;;; Code:

(defvar clickable-mode-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]   #'clickable-open)
    ;; (define-key map [double-mouse-1] #'clickable-open)
    ;; C-mouse-1 にしたいときは以下ふたつをセットしないと
    ;; デフォルトのポップアップが出たりアンデファインドと出たりする
    (define-key map [C-mouse-1] #'clickable-open)
    (define-key map [C-down-mouse-1] #'clickable-open)
    (define-key map [C-return]  #'clickable-open)
    map))

(defvar clickable-mode-map
  (let ((map (make-sparse-keymap))
        (menu (make-sparse-keymap "clickable")))
    (define-key map [tab]   #'clickable-next)
    (define-key map [S-tab] #'clickable-previous)
    (define-key map "\C-c@c" #'clickable-change)
    (define-key map [menu-bar clickable] (cons "Clickable" menu))
    (define-key menu [clickable-change] '("Regexp Change" . clickable-change))
    (define-key menu [clickable-mode] '(menu-item "Clickable mode" clickable-mode
                                                  :button (:toggle . clickable-mode)))
    map))

(defvar-local clickable-ov      nil)
(defvar-local clickable-url-pos nil)

(defcustom clickable-url
  '("https?://[^\"> \n]+"
    "https?://[^\"> ?\n]+"
    ("\"\\(?1://[^\" ?]+\\)" (format "https:%s" (match-string 1))))
  "Contents of the list are REGEXP or \(list REGEXP ARG).
Matches REGEXP is taken as the argument.
If list, ARG is taken as the argument.
ARG is a lisp form."
  :type '(repeat (choice regexp (list regexp sexp)))
  :group 'text)

(defun clickable-change ()
  (interactive)
  (when (consp clickable-url)
    (setq clickable-url-pos (cdr clickable-url-pos))
    (clickable-make (car clickable-url-pos))))

(defun clickable-uri (exp)
  (cond
   ((null exp)
    (match-string 0))
   ((functionp exp)
    (funcall exp))
   ((listp exp)
    (eval exp))
   (t
    exp)))

(defun clickable-make (url)
  (let (exp ov pos)
    (if (consp url) (setq exp (nth 1 url) url (car url)))
    (and clickable-ov (clickable-clear))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward url nil t)
        (setq pos (if (match-string 1) 1 0))
        (push (make-overlay (match-beginning pos) (match-end pos)) ov)
        (overlay-put (car ov) 'category 'clickable)
        (overlay-put (car ov) 'face 'link)
        (overlay-put (car ov) 'mouse-face 'highlight)
        (overlay-put (car ov) 'uri (clickable-uri exp))
        (overlay-put (car ov) 'help-echo (clickable-uri exp))
        (overlay-put (car ov) 'keymap clickable-mode-mouse-map)))
    (setq  clickable-ov ov)))

(defun clickable-clear ()
  (setq clickable-ov nil)
  (remove-overlays (point-min) (point-max) 'category 'clickable))

(defun clickable-open ()
  (interactive)
  (let ((pos (posn-point (event-start last-command-event)))
        ov)
    (and pos (goto-char pos))
    (setq ov (overlays-at (point)))
    (dolist (o ov)
      (if (eq (overlay-get o 'category) 'clickable)
          (w32-shell-execute "open" (overlay-get o 'uri))))))

(defun clickable-next (arg)
  (interactive "p")
  (dotimes (i arg) (clickable--next)))

(defun clickable-previous (arg)
  (interactive "p")
  (dotimes (i arg) (clickable--previous)))

(defun clickable--next ()
  (let ((pos (next-single-char-property-change (point) 'category)))
    (if (eq pos (point-max))
        (ding)
      (goto-char pos)
      (or (overlays-at pos) (clickable--next)))))

(defun clickable--previous ()
  (let ((pos (previous-single-char-property-change (point) 'category)))
    (if (eq pos (point-min))
        (ding)
      (goto-char pos)
      (or (overlays-at pos) (clickable--previous)))))

;;;###autoload
(define-minor-mode clickable-mode
  "Mouse click url open mode."
  :init-value nil
  :lighter " Clickable"
  (let ((prefix current-prefix-arg))
    (if clickable-mode
        (clickable-make
         (if (consp clickable-url)
             (progn
                   (setq clickable-url-pos clickable-url)
                   (setcdr (last clickable-url-pos) clickable-url-pos)
                   (if prefix (nth 1 clickable-url-pos) (car clickable-url-pos)))
           clickable-url))
      (clickable-clear))))

;;;###autoload
(define-derived-mode clickable-text-mode text-mode  "CText"
  "Mouse click url open text mode."
  (clickable-mode 1))

(provide 'clickable-mode)
;;; clickable-mode.el ends here
