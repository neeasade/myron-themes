;; -*- lexical-binding: t; -*-
;; template the foobar theme

(require 'myron-themes)

(defun myron-foobar-colors (background)
  "Get the foobar foreground colors against a specific BACKGROUND."
  (let* ((colors (->> (ct-make-hsl 240 80 43.596)
                   (ct-rotation-hsv 5)
                   (--map (ct-contrast-clamp it background 4.1)))))
    (ht<-plist
      `(:background ,background
         :foreground ,(ct-contrast-min background background 6.0)
         :assumed ,(-> (nth 1 colors)
                     (ct-edit-lch-c 90)
                     (ct-contrast-max background 2.6))

         ,@(-interleave
             '(:primary :faded :alt :strings)
             (-select-by-indices '(0 4 3 3) colors))))))

(defun myron-foobar-create ()
  "Create the colors for the foobar theme."
  (-let* ((background (ct-make-hsluv 180 100 94))
           (background+ (-> (myron-foobar-colors background)
                          (ht-get :primary)
                          (ct-edit-lch-c 80)
                          (ct-steal 'hsluv-l background>))))

    (ht<-plist
      (list
        :focused (myron-foobar-colors background+)
        :normal  (myron-foobar-colors background)
        :weak    (myron-foobar-colors background>)
        :strong  (myron-foobar-colors background>>)))))

(deftheme myron-foobar)
(myron-themes--define 'myron-foobar)

(myron-themes-evil-cursor-color (myron-themes-get :assumed))

(provide-theme 'myron-foobar)
(provide 'myron-foobar-theme)
;;; myron-foobar-theme.el ends here
