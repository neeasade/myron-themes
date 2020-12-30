;;; tarps.el --- color themes for emacs -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2020 neeasade

;; Author: neeasade <neeasade@gmail.com>
;; URL: https://github.com/neeasade/tarps
;; Package-Requires: (color-tools base16-theme ht)
;; Version: 0.1

;;; Commentary:
;; <void>

;;; Code:

(defcustom tarp-org-level-resizing t
  "Set to non-nil for `org-level-*' faces to be different larger
  than the default font height."
  :type 'boolean
  :group 'tarps)

(defun ht-transform-kv (table transform-function)
  "Apply some transformation to all keys + values in a hashtable"
  (eval `(ht ,@(-map (fn (list <>
                               (funcall transform-function
					<>
					(ht-get table <>))))
                     (ht-keys table)))))

(defun ht-transform-v (table transform-function)
  "Apply some transformation to all values in a hashtable"
  (eval `(ht ,@(-map (fn (list <>
                               (funcall transform-function
					(ht-get table <>))))
                     (ht-keys table)))))

(defun tarp/nth (index coll)
  "a version of nth that counts from the end if the input is negative"
  ;; this just makes it easier to sort through color collections in real time.
  (if (< index 0)
      (car (seq-subseq coll index
		       (if (= 0 (+ index 1)) nil (+ index 1))))
    (nth index coll)))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'tarps)
