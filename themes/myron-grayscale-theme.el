;; -*- lexical-binding: t; -*-

(require 'myron-themes)

(defun myron-grayscale-colors (background)
  "get the foreground colors against a specific background"
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
  (-let*
    ((background
       "#e9e9e9"
       ;; "#f7f7f7"
       ;; "#e0e0e0"
       ))
    (->> '(:focused 3
            :weak 4
            :strong 7)
      (-partition 2)
      (-map (-lambda ((label distance))
              (list label
                (ct-iterate background 'ct-edit-lab-l-dec
                  (fn (> (ct-distance <> background) distance))))))
      (append `((:normal ,background)))
      (-mapcat (-lambda ((label bg)) (list label (myron-grayscale-colors bg))))
      (ht<-plist))))

(deftheme myron-grayscale)
(myron-theme-define 'myron-grayscale)
(myron-evil-cursor-color (myron-get :alt))

(provide-theme 'myron-grayscale)
(provide 'myron-grayscale-theme)
;;; myron-grayscale-theme.el ends here
