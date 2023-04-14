;; -*- lexical-binding: t; -*-
;; placeholder for themes being developed

(require 'myron)

(defun myron-test-colors (background)
  "get the foreground colors against a specific background"
  (let* ((colors (-->
                   (ct-make-hsl 240 80 43.596)

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

(-let*
  (
    (background (ct-make-hsluv 180 100 94))
    ;; (background (ct-make-hsluv 180 100 90))
    (normal-parts (myron-test-colors background))
    ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

    (background>
      (-> background
        (ct-iterate
          (-compose 'ct-edit-hsluv-l-dec 'ct-edit-lab-a-inc 'ct-edit-lab-b-inc)
          ;; 'ct-edit-lab-l-dec
          (fn (> (ct-name-distance <> background) 3)))
        ;; (ct-edit-hsluv-h (ct-get-hsluv-h assumed))
        ))

    (background>>
      (-> background
        (ct-iterate
          (-compose 'ct-edit-hsluv-l-dec 'ct-edit-lab-a-inc 'ct-edit-lab-b-inc)
          ;; 'ct-edit-lab-l-dec
          (fn (> (ct-name-distance <> background) 2)))
        ;; (ct-edit-hsluv-h (ct-get-hsluv-h assumed))
        ))

    (background+
      (-> primary
        (ct-edit-lch-c 20)
        (ct-edit-hsluv-l
          (ct-get-hsluv-l background>>)))))

  (setq myron-theme*
    (ht<-plist
      (list
        :focused (myron-test-colors background+)
        :normal normal-parts
        :weak (myron-test-colors background>)
        :strong (myron-test-colors background>>)))))

(deftheme myron-test)
(myron-theme-define 'myron-test (lambda () myron-theme*) myron-theme*)

;; (myron-evil-cursor-color (myron-get :assumed))

(provide-theme 'myron-test)
(provide 'myron-test-theme)
;;; myron-test-theme.el ends here
