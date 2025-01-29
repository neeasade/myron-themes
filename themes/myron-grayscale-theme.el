;; -*- lexical-binding: t; -*-
;; inspo: base16-grayscale

(require 'myron-themes)

(defun myron-grayscale-colors (background)
  "Get the grayscale foreground colors against a specific BACKGROUND."
  (->> '(:background 0
          :foreground 6
          :faded 3
          :primary 4
          :assumed 5
          :alt 3
          :strings 4)
    (-partition 2)
    (-mapcat (-lambda ((label contrast))
               (list label (ct-contrast-min background background contrast))))
    (ht<-plist)))

(defun myron-grayscale-create ()
  "Create the colors for the grayscale theme."
  (-let*
    ((background
       ;; "#e9e9e9"
       "#f3f3f3"
       ;; "#f7f7f7"
       ;; "#e0e0e0"
       ))
    (->> '(:focused 3
            :weak 4
            :strong 7)
      (-partition 2)
      (-map (-lambda ((label distance))
              (list label (ct-change background distance 'ct-edit-lab-l-dec))))
      (append `((:normal ,background)))
      (-mapcat (-lambda ((label bg)) (list label (myron-grayscale-colors bg))))
      (ht<-plist))))

(deftheme myron-grayscale)
(myron-themes--define 'myron-grayscale)
(myron-themes-evil-cursor-color (myron-themes-get :alt))

(provide-theme 'myron-grayscale)
(provide 'myron-grayscale-theme)
;;; myron-grayscale-theme.el ends here
