;;; tarps.el --- color themes for emacs -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2020 neeasade

;; Author: neeasade <neeasade@gmail.com>
;; URL: https://github.com/neeasade/tarps
;; Package-Requires: (color-tools base16-theme ht s)
;; Version: 0.1

;;; Commentary:
;; <void>

;;; Code:

(defcustom tarp-org-level-resizing t
  "Set to non-nil for `org-level-*' faces to be different larger
  than the default font height."
  :type 'boolean
  :group 'tarps)

;; idea for another defcustom: a hook for tarp/edit before it's mapped and set

(defun tarp/edit (transform-function)
  "edit the hashtable tarp/theme with a function taking KEY VALUE that returns VALUE"
  (-->
    (lambda (k v)
      (list k (funcall transform-function k v)))
    (ht-map it tarp/theme)
    (eval `(ht ,@ it))
    (setq tarp/theme it)))

(defun tarp/nth (index coll)
  "a version of nth that counts from the end if the input is negative"
  ;; this just makes it easier to sort through color collections in real time.
  (if (< index 0)
    (car (seq-subseq coll index
           (if (= 0 (+ index 1)) nil (+ index 1))))
    (nth index coll)))

(defmacro ht-with-context (table &rest content)
  (-tree-map
    (lambda (tree)
      (-tree-map-nodes (lambda (node) t)
        (lambda (node)
          (if (and
                (s-starts-with-p ":" (prin1-to-string node))
                ;; if the table doesn't exist, don't sanity check the key
                (if (boundp table)
                  (-contains-p (ht-keys (eval table)) node)
                  t))
            (list 'ht-get table node)
            node))
        tree))
    (cons 'progn content)))

(defun tarp/show-contrasts ()
  "message the contrast ratios of colors in the loaded theme against it's background"
  (-map
    (fn
      (message
        (format "%s: %s"
          <>
          (ct/contrast-ratio
            (ht-get tarp/theme <>)
            (ht-get tarp/theme :background)
            ))))
    (ht-keys tarp/theme)))

(and load-file-name
  (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory
      (file-name-directory load-file-name))))

(provide 'tarps)
