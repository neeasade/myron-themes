;; -*- lexical-binding: t; -*-
;; It's a shadowy dusk here, and I am merely mortal.
;; a theme wherein we play with the hct colorspace

(require 'myron-themes)

(defun myron-mortal-colors (background &optional print?)
  "Get the mortal foreground colors against a specific BACKGROUND."
  (-let* ((og-hue (ct-get-hct-h background))
           ;; c is complement, a is original
           ;; ((a b c d) (-iota 4 hue 90))

           ;; ｔｅｔｒａｄｉｃ
           (o 60)
           ((a b c d)
             ;; pairs: ac and bd
             (list
               og-hue
               (+ og-hue o)
               (+ og-hue 180)
               (+ og-hue o 180)))
           (contrast-boost 0.7))

    (when print?
      ;; visualize
      (->> (list a b c d)
        (-map (lambda (hue)
                (message
                  (-> background
                    (ct-edit-hct-t-dec 10)
                    (ct-edit-hct-h hue)
                    (ct-edit-hct-c 50)))))))

    ;; "#ffc6b6"
    ;; "#dfd758"
    ;; "#57e4ff"
    ;; "#d3cdff"
    (->> (list
           ;; contrast hue chroma
           :foreground  4.0  a 7
           :assumed     4.0  c 40
           :alt         3.5  b 20
           :primary     3.0  a 100
           :faded       2.5  b 25       ; this was initially 'a', wanted to spread abcd further
           :strings     3.0  d 50
           )
      (-partition 4)
      (-mapcat (-lambda ((label contrast hue chroma))
                 (list label
                   (-> background
                     ;; important to contrast first and then apply chroma, else we nuke it
                     (ct-contrast-min background (+ contrast-boost contrast) 'hct-t)
                     (ct-edit-hct (lambda (_ _ tone) (list hue chroma tone)))))))
      (-concat (list :background background))
      (ht<-plist))))

(defun myron-mortal-create ()
  "Create the colors for the mortal theme."
  (-let* (
           ;; (ct-rotation-hct 12 "#fbe6e1")
           (seed "#fbe6e1")

           ;; the seed origin. uncomment to randomize, but results vary (often bad)
           ;; (seed (ns/random-list (ct-rotation-hct 12 "#fbe6e1")))

           ;; (b seed)
           ;; (b (ct-edit-hct-t-inc seed 2))

           (b (ct-edit-hct-t-inc seed 1.6))
           (b> (ct-aedit-hct seed  (list h (* 1.5 c) (- tt 5))))
           (b>> (ct-aedit-hct seed (list h (* 2 c)   (- tt 8))))
           (b+ (-> b>
                 (ct-complement-hct)
                 (ct-edit-hct-c 25)
                 ;; this clamp allows tampering with b value for future me
                 (ct-contrast-min b 1.1863))))
    (ht<-plist
      (list
        :focused (myron-mortal-colors b+)
        :normal  (myron-mortal-colors b)
        :weak    (myron-mortal-colors b>)
        :strong  (myron-mortal-colors b>>)))))

(deftheme myron-mortal)

(myron-themes--define 'myron-mortal
  ;; '((font-lock-comment-face :slant italic))
  `(
     ;; todo
     ;; we've been complecting "highlighting" and "types"
     ;; maybe "types" can be "highlighting" with less chroma
     ;; we need a meta -> highlight color for character matching, can't get away with it here
     ((orderless-match-face-0 orderless-match-face-1 orderless-match-face-2 orderless-match-face-3)
       :foreground
       ;; heavy - but can't use myron-get-color here because myron-themes-define is what sets it
       ,(ct-edit-hct-c (ht-get* (myron-mortal-create) :normal :strings) 55))))

(provide-theme 'myron-mortal)

(provide 'myron-mortal-theme)
;;; myron-mortal-theme.el ends here
