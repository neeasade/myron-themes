;; -*- lexical-binding: t; -*-
;; named for dogman of the first law series
;; the main colors of this theme came so naturally, late at night, in an escape
;; the backgrounds (weak/strong/focused) took more time

(require 'myron-themes)

(defun myron-dogman-colors (background)
  "get the foreground colors against a specific background"
  (let* ((colors (--> (ct-make-hsl 240 80 43.596)
                   (ct-rotation-hsv it 72)
                   (--map (ct-contrast-clamp it background 4.1) it))))
    (ht<-plist
      `(:background ,background
         :foreground ,(ct-contrast-min background background 6.0)
         :assumed ,(-> (nth 1 colors)
                     (ct-edit-lch-c 90)
                     (ct-contrast-max background 2.6))

         ,@(-interleave
             '(:primary :faded :alt :strings)
             ;; (-select-by-indices '(0 4 3 2) colors)
             ;; what if strings and types were the same color
             (-select-by-indices '(0 4 3 3) colors))))))

(defun myron-dogman-create ()
  (-let*
    ((background (ct-make-hsluv 180 100 94))
      ;; (background "#fdf6e3")
      ;; (background (ct-make-hsluv 180 100 90))
      (normal-parts (myron-dogman-colors background))
      ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

      (background> (myron-cdist background 4
                     (-compose
                       'ct-edit-hsv-v-dec
                       'ct-edit-hsv-v-dec
                       'ct-edit-lab-a-inc)))

      (background>> (myron-cdist background> 6
                      (-compose 'ct-edit-lab-a-inc 'ct-edit-lab-b-inc)))

      (background+
        (-> primary
          (ct-edit-lch-c 80)
          (ct-steal 'hsluv-l background>))))

    (ht<-plist
      (list
        :focused (myron-dogman-colors background+)
        :normal normal-parts
        :weak (myron-dogman-colors background>)
        :strong (myron-dogman-colors background>>)))))

(deftheme myron-dogman)
(myron-theme-define 'myron-dogman)

(myron-evil-cursor-color (myron-get :assumed))

(provide-theme 'myron-dogman)
(provide 'myron-dogman-theme)
;;; myron-dogman-theme.el ends here
