(defcustom myron--cache
  '(myron-dogman #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
			               (:strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
						                      (:strings "#026d00" :alt "#026d00" :faded "#006b47" :primary "#6d34e3" :assumed "#d144a7" :foreground "#064d51" :background "#92d9dd"))
				               :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
							                   (:strings "#0e7900" :alt "#0e7900" :faded "#007753" :primary "#7940ef" :assumed "#e14db9" :foreground "#005953" :background "#92ede7"))
				               :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
							                     (:strings "#1a8500" :alt "#1a8500" :faded "#00825e" :primary "#854cfb" :assumed "#f256c9" :foreground "#036458" :background "#9efff3"))
				               :focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
							                      (:strings "#0e7900" :alt "#0e7900" :faded "#007753" :primary "#7940ef" :assumed "#e14db9" :foreground "#524b6a" :background "#e0d9f8"))
				               :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
							                   (:interactive-background-highlight "#c6cec6" :interactive-background "#d6ded6" :diff-remove-highlight "#f2c7ce" :diff-add-highlight "#9ef57d" :diff-remove "#f5d6db" :diff-add "#c7f9b7"))))
	   myron-grayscale #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
					               (:strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                      (:strings "#656565" :alt "#787878" :assumed "#565656" :primary "#656565" :faded "#787878" :foreground "#4b4b4b" :background "#d6d6d6"))
						               :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
									                   (:strings "#6e6e6e" :alt "#828282" :assumed "#5f5f5f" :primary "#6e6e6e" :faded "#828282" :foreground "#535353" :background "#e4e4e4"))
						               :focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
									                      (:strings "#707070" :alt "#858585" :assumed "#616161" :primary "#707070" :faded "#858585" :foreground "#555555" :background "#e8e8e8"))
						               :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
									                     (:strings "#7a7a7a" :alt "#8f8f8f" :assumed "#6a6a6a" :primary "#7a7a7a" :faded "#8f8f8f" :foreground "#5e5e5e" :background "#f7f7f7"))
						               :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
									                   (:interactive-background-highlight "#d1d7d7" :interactive-background "#dee5e5" :diff-remove-highlight "#f3d0d7" :diff-add-highlight "#bcf6a6" :diff-remove "#f7e1e5" :diff-add "#e5fbdd"))))
	   myron-kobo #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
				            (:focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
							                    (:primary "#555f00" :faded "#5f5a57" :alt "#697400" :strings "#697400" :assumed "#4b4643" :foreground "#4b4643" :background "#ccc7c4"))
						          :strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                  (:primary "#545e00" :faded "#5e5956" :alt "#677200" :strings "#677200" :assumed "#4a4542" :foreground "#4a4542" :background "#cac5c2"))
						          :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                (:primary "#616c00" :faded "#6b6663" :alt "#748100" :strings "#748100" :assumed "#57524f" :foreground "#57524f" :background "#ded9d6"))
						          :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                  (:primary "#5d6800" :faded "#67625f" :alt "#6f7c00" :strings "#6f7c00" :assumed "#524d4a" :foreground "#524d4a" :background "#d7d2cf"))
						          :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                (:interactive-background-highlight "#c5c5c0" :interactive-background "#d8dad3" :diff-remove-highlight "#efbac3" :diff-add-highlight "#95ea74" :diff-remove "#eeb4be" :diff-add "#92e572"))))
	   myron-mcfay #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
					           (:focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                   (:background "#a6d0ed" :foreground "#0c3653" :faded "#335d7a" :primary "#ad0066" :assumed "#0055b8" :alt "#005f87" :strings "#006c00"))
						           :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
									                 (:background "#e8ebec" :foreground "#444748" :faded "#6a6d6e" :primary "#c6007f" :assumed "#0065c8" :alt "#006e96" :strings "#007c00"))
						           :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                 (:background "#d6d7d8" :foreground "#3a3b3c" :faded "#606162" :primary "#bb0074" :assumed "#005ec1" :alt "#006890" :strings "#007500"))
						           :strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
									                 (:background "#cdcacb" :foreground "#353233" :faded "#5b5859" :primary "#ae0067" :assumed "#0055b8" :alt "#005f87" :strings "#006d00"))
						           :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                 (:interactive-background-highlight "#c4ccc4" :interactive-background "#d0d8d0" :diff-remove-highlight "#f0c0c8" :diff-add-highlight "#9bef7b" :diff-remove "#f4d2d7" :diff-add "#bbf8a7"))))
	   myron-room #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
				            (:strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
							                   (:strings "#a66300" :alt "#0094ab" :assumed "#525252" :primary "#007e97" :faded "#747474" :foreground "#474747" :background "#d0d0d0"))
						          :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                (:strings "#b56800" :alt "#009eb5" :assumed "#5a5a5a" :primary "#00879f" :faded "#7d7d7d" :foreground "#4f4f4f" :background "#dddddd"))
						          :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                  (:strings "#cb7000" :alt "#00acc3" :assumed "#666666" :primary "#0095ac" :faded "#8a8a8a" :foreground "#5a5a5a" :background "#f0f0f0"))
						          :focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                   (:strings "#626d1e" :alt "#9454ff" :assumed "#7a4f44" :primary "#803bea" :faded "#9e7368" :foreground "#6e4338" :background "#fed3c8"))
						          :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                (:interactive-background-highlight "#cad1ca" :interactive-background "#d6ded6" :diff-remove-highlight "#f1c8ce" :diff-add-highlight "#a3f483" :diff-remove "#f5d9dd" :diff-add "#cff9c2"))))
	   myron-storm #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
					           (:normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
							                    (:background "#e3f0ed" :foreground "#697673" :faded "#7e8b88" :primary "#a356a4" :assumed "#108082" :strings "#0e8618" :alt "#a95e5b"))
						           :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                 (:background "#ccd9d6" :foreground "#5b6865" :faded "#6e7b78" :primary "#924894" :assumed "#067074" :strings "#05760f" :alt "#984f49"))
						           :strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                   (:background "#c3d0cd" :foreground "#55625f" :faded "#687572" :primary "#8b418d" :assumed "#026a6f" :strings "#006f0a" :alt "#924942"))
						           :focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
									                  (:background "#ecccec" :foreground "#7a5a7a" :faded "#8e6e8e" :primary "#a74908" :assumed "#a74446" :strings "#a138a1" :alt "#00716d"))
						           :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                 (:interactive-background-highlight "#c7cec7" :interactive-background "#cfd7cf" :diff-remove-highlight "#efc0c8" :diff-add-highlight "#99f078" :diff-remove "#f4d5da" :diff-add "#c5f9b5"))))
	   myron-struan #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
					            (:focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                    (:strings "#667400" :alt "#916156" :assumed "#2a7783" :primary "#8f5d7f" :faded "#a35a29" :foreground "#544b45" :background "#e1c5c0"))
						            :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
									                  (:strings "#667400" :alt "#916156" :assumed "#2a7783" :primary "#8f5d7f" :faded "#a35a29" :foreground "#544b45" :background "#f2e9e3"))
						            :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                  (:strings "#5b6600" :alt "#845449" :assumed "#1d6a76" :primary "#825072" :faded "#a93b2f" :foreground "#483f3e" :background "#ded5d4"))
						            :strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
									                  (:strings "#545f00" :alt "#7d4d42" :assumed "#15626e" :primary "#7b496b" :faded "#a1276b" :foreground "#40373c" :background "#d2c9ce"))
						            :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
								                  (:interactive-background-highlight "#cacbc5" :interactive-background "#d5d7d0" :diff-remove-highlight "#f0c0c8" :diff-add-highlight "#9aef7b" :diff-remove "#f4d2d7" :diff-add "#bbf8a9")))))
  "Cache value for the themes. Internal use only."
  :type 'sexp
  :group 'myron)

(provide 'myron-cache)
