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
      ;; (background (ct-make-hsluv 180 100 90))
      (normal-parts (myron-dogman-colors background))
      ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

      (background>
        (let ((base background))
          (-> base
            (ct-iterate
              (-compose
                'ct-edit-hsv-v-dec
                'ct-edit-hsv-v-dec
                'ct-edit-lab-a-inc)
              (fn (> (ct-distance <> base) 4))))))

      (background>>
        (let ((base background>))
          (-> base
            (ct-iterate
              (-compose 'ct-edit-lab-a-inc 'ct-edit-lab-b-inc)
              (fn (> (ct-distance <> base) 6))))))

      (background+
        (-> primary
          (ct-edit-lch-c 80)
          (ct-edit-hsluv-l (ct-get-hsluv-l background>)))))

    (ht<-plist
      (list
        :focused (myron-dogman-colors background+)
        :normal normal-parts
        :weak (myron-dogman-colors background>)
        :strong (myron-dogman-colors background>>)))))

(deftheme myron-dogman)
(myron-theme-define
  'myron-dogman
  'myron-dogman-create
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#026d00" :alt "#026d00" :faded "#006b47" :primary "#6d34e3" :assumed "#d144a7" :foreground "#064d51" :background "#92d9dd")) :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#0e7900" :alt "#0e7900" :faded "#007753" :primary "#7940ef" :assumed "#e14db9" :foreground "#005953" :background "#92ede7")) :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#1a8500" :alt "#1a8500" :faded "#00825e" :primary "#854cfb" :assumed "#f256c9" :foreground "#036458" :background "#9efff3")) :focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#0e7900" :alt "#0e7900" :faded "#007753" :primary "#7940ef" :assumed "#e14db9" :foreground "#524b6a" :background "#e0d9f8")))))

(myron-evil-cursor-color (myron-get :assumed))

(provide-theme 'myron-dogman)
(provide 'myron-dogman-theme)
;;; myron-dogman-theme.el ends here