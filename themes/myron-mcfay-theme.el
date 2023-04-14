;; -*- lexical-binding: t; -*-
;; Namesake: Jamie McFay, from the James Clavell novel Gai-Jin

(require 'myron)

(defun myron-mcfay-colors (background)
  "get the foreground colors against a specific background"
  (let*
    ((colors (-->
               (ct-make-hsluv 270 75 43.596)
               (ct-rotation-lch it -45)
               (-select-by-indices '(7 1 2 4) it)
               (-map (fn (ct-edit-hsluv-l <> 43.596)) it)
               (-map (fn (ct-tint-ratio <> background 4.3)) it))))

    (ht
      (:background background)

      (:foreground (ct-tint-ratio background background 7.7))
      (:faded (ct-tint-ratio background background 4.3))

      (:primary (nth 0 colors))
      (:assumed (nth 1 colors))
      (:alt (nth 2 colors))
      (:strings (ct-edit-lch-c (nth 3 colors) 100)))))

(defun myron-mcfay-create ()
  (-let*
    (
      ;; /slightly/ cool
      (background (ct-make-lab 93 -0.5 -1))
      (normal-parts (myron-mcfay-colors background))
      ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

      (background>
        (-> background
          (ct-iterate 'ct-edit-lab-l-dec
            (fn (> (ct-name-distance <> background) 4)))
          (ct-edit-hsluv-h (ct-get-hsluv-h alt))))

      (background>>
        (-> background
          (ct-iterate 'ct-edit-lab-l-dec
            (fn (> (ct-name-distance <> background) 7)))
          (ct-edit-hsluv-h (ct-get-hsluv-h primary))))

      (background+
        (-> alt
          (ct-edit-lch-c 20)
          (ct-edit-hsluv-l
            (ct-get-hsluv-l background>>)))))

    (ht
      (:focused (myron-mcfay-colors background+))
      (:normal normal-parts)
      (:weak (myron-mcfay-colors background>))
      (:strong (myron-mcfay-colors background>>)))))

(deftheme myron-mcfay)
(myron-theme-define 'myron-mcfay 'myron-mcfay-create'
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#a6d0ed" :foreground "#0c3653" :faded "#335d7a" :primary "#ad0066" :assumed "#0055b8" :alt "#005f87" :strings "#006c00")) :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#e8ebec" :foreground "#444748" :faded "#6a6d6e" :primary "#c6007f" :assumed "#0065c8" :alt "#006e96" :strings "#007c00")) :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#d6d7d8" :foreground "#3a3b3c" :faded "#606162" :primary "#bb0074" :assumed "#005ec1" :alt "#006890" :strings "#007500")) :strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#cdcacb" :foreground "#353233" :faded "#5b5859" :primary "#ae0067" :assumed "#0055b8" :alt "#005f87" :strings "#006d00")))))
(myron-evil-cursor-color (myron-get :alt))

(provide-theme 'myron-mcfay)
(provide 'myron-mcfay-theme)
;;; myron-mcfay-theme.el ends here
