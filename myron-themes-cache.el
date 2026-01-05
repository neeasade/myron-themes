;; -*- lexical-binding: t; -*-
(defvar myron-themes-cache
  '(myron-dogman
    #s(hash-table test equal data
       (:strong
	#s(hash-table test equal data
		      (:strings "#026d00" :alt "#026d00" :faded "#006b47" :primary "#6d34e3"
				:assumed "#d144a7" :foreground "#064d51" :background "#92d9dd"))
	:weak
	#s(hash-table test equal data
		      (:strings "#0e7900" :alt "#0e7900" :faded "#007753" :primary "#7940ef"
				:assumed "#e14db9" :foreground "#005953" :background "#92ede7"))
	:normal
	#s(hash-table test equal data
		      (:strings "#1a8500" :alt "#1a8500" :faded "#00825e" :primary "#854cfb"
				:assumed "#f256c9" :foreground "#036458" :background "#9efff3"))
	:focused
	#s(hash-table test equal data
		      (:strings "#0e7900" :alt "#0e7900" :faded "#007753" :primary "#7940ef"
				:assumed "#e14db9" :foreground "#524b6a" :background "#e0d9f8"))
	:meta
	#s(hash-table test equal data
		      (:subtle "#95f6ea" :diff-remove-highlight "#f2c7ce" :diff-add-highlight
			       "#9ef57d" :diff-remove "#f5d6db" :diff-add "#c7f9b7"))))
    myron-grayscale
    #s(hash-table test equal data
       (:strong
	#s(hash-table test equal data
		      (:strings "#636363" :alt "#767676" :assumed "#545454" :primary "#636363"
				:faded "#767676" :foreground "#494949" :background "#d3d3d3"))
	:weak
	#s(hash-table test equal data
		      (:strings "#6b6b6b" :alt "#7f7f7f" :assumed "#5c5c5c" :primary "#6b6b6b"
				:faded "#7f7f7f" :foreground "#515151" :background "#e0e0e0"))
	:focused
	#s(hash-table test equal data
		      (:strings "#6e6e6e" :alt "#828282" :assumed "#5f5f5f" :primary "#6e6e6e"
				:faded "#828282" :foreground "#535353" :background "#e4e4e4"))
	:normal
	#s(hash-table test equal data
		      (:strings "#777777" :alt "#8c8c8c" :assumed "#686868" :primary "#777777"
				:faded "#8c8c8c" :foreground "#5c5c5c" :background "#f3f3f3"))
	:meta
	#s(hash-table test equal data
		      (:subtle "#eaeaea" :diff-remove-highlight "#f2cbd1" :diff-add-highlight
			       "#abf68d" :diff-remove "#f6dce0" :diff-add "#d8face"))))
    myron-kobo
    #s(hash-table test equal data
       (:focused
	#s(hash-table test equal data
		      (:primary "#5a5f00" :faded "#5f5a57" :alt "#6e7400" :strings "#6e7400"
				:assumed "#4b4643" :foreground "#4b4643" :background "#ccc7c4"))
	:strong
	#s(hash-table test equal data
		      (:primary "#595e00" :faded "#5e5956" :alt "#6c7100" :strings "#6c7100"
				:assumed "#4a4542" :foreground "#4a4542" :background "#cac5c2"))
	:weak
	#s(hash-table test equal data
		      (:primary "#666b00" :faded "#6b6663" :alt "#7a8000" :strings "#7a8000"
				:assumed "#57524f" :foreground "#57524f" :background "#ded9d6"))
	:normal
	#s(hash-table test equal data
		      (:primary "#626700" :faded "#67625f" :alt "#757b00" :strings "#757b00"
				:assumed "#524d4a" :foreground "#524d4a" :background "#d7d2cf"))
	:meta
	#s(hash-table test equal data
		      (:subtle "#cec9c6" :diff-remove-highlight "#efbac3" :diff-add-highlight
			       "#95ea74" :diff-remove "#eeb4be" :diff-add "#92e572"))))
    myron-mcfay
    #s(hash-table test equal data
       (:focused
	#s(hash-table test equal data
		      (:background "#a6d0ed" :foreground "#0c3653" :faded "#335d7a" :primary
				   "#ad0066" :assumed "#0055b8" :alt "#005f87" :strings "#006c00"))
	:normal
	#s(hash-table test equal data
		      (:background "#e8ebec" :foreground "#444748" :faded "#6a6d6e" :primary
				   "#c6007f" :assumed "#0065c8" :alt "#006e96" :strings "#007c00"))
	:weak
	#s(hash-table test equal data
		      (:background "#d6d7d8" :foreground "#3a3b3c" :faded "#606162" :primary
				   "#bb0074" :assumed "#005ec1" :alt "#006890" :strings "#007500"))
	:strong
	#s(hash-table test equal data
		      (:background "#cdcacb" :foreground "#353233" :faded "#5b5859" :primary
				   "#ae0067" :assumed "#0055b8" :alt "#005f87" :strings "#006d00"))
	:meta
	#s(hash-table test equal data
		      (:subtle "#dfe2e3" :diff-remove-highlight "#f0c0c8" :diff-add-highlight
			       "#9bef7b" :diff-remove "#f4d2d7" :diff-add "#bbf8a7"))))
    myron-room
    #s(hash-table test equal data
       (:strong
	#s(hash-table test equal data
		      (:strings "#a66300" :alt "#0094ab" :assumed "#474747" :primary "#007e97"
				:faded "#747474" :foreground "#474747" :background "#d0d0d0"))
	:weak
	#s(hash-table test equal data
		      (:strings "#b56800" :alt "#009eb5" :assumed "#4f4f4f" :primary "#00879f"
				:faded "#7d7d7d" :foreground "#4f4f4f" :background "#dddddd"))
	:normal
	#s(hash-table test equal data
		      (:strings "#cb7000" :alt "#00acc3" :assumed "#5a5a5a" :primary "#0095ac"
				:faded "#8a8a8a" :foreground "#5a5a5a" :background "#f0f0f0"))
	:focused
	#s(hash-table test equal data
		      (:strings "#626d1e" :alt "#9454ff" :assumed "#6e4338" :primary "#803bea"
				:faded "#9e7368" :foreground "#6e4338" :background "#fed3c8"))
	:meta
	#s(hash-table test equal data
		      (:subtle "#e7e7e7" :diff-remove-highlight "#f1c8ce" :diff-add-highlight
			       "#a3f483" :diff-remove "#f5d9dd" :diff-add "#cff9c2"))))
    myron-storm
    #s(hash-table test equal data
       (:normal
	#s(hash-table test equal data
		      (:background "#e3f0ed" :foreground "#697673" :faded "#7e8b88" :primary
				   "#a356a4" :assumed "#108082" :strings "#0e8618" :alt "#a95e5b"))
	:weak
	#s(hash-table test equal data
		      (:background "#ccd9d6" :foreground "#5b6865" :faded "#6e7b78" :primary
				   "#924894" :assumed "#067074" :strings "#05760f" :alt "#984f49"))
	:strong
	#s(hash-table test equal data
		      (:background "#c3d0cd" :foreground "#55625f" :faded "#687572" :primary
				   "#8b418d" :assumed "#026a6f" :strings "#006f0a" :alt "#924942"))
	:focused
	#s(hash-table test equal data
		      (:background "#ecccec" :foreground "#7a5a7a" :faded "#8e6e8e" :primary
				   "#a74908" :assumed "#a74446" :strings "#a138a1" :alt "#00716d"))
	:meta
	#s(hash-table test equal data
		      (:subtle "#dae7e4" :diff-remove-highlight "#efc0c8" :diff-add-highlight
			       "#99f078" :diff-remove "#f4d5da" :diff-add "#c5f9b5"))))
    myron-struan
    #s(hash-table test equal data
       (:normal
	#s(hash-table test equal data
		      (:strings "#677400" :alt "#916156" :assumed "#2a7783" :primary "#8f5d7f"
				:faded "#a35a29" :foreground "#544b45" :background "#f2e9e3"))
	:weak
	#s(hash-table test equal data
		      (:strings "#5b6700" :alt "#845449" :assumed "#1d6a76" :primary "#825072"
				:faded "#a93b2f" :foreground "#483f3e" :background "#ded5d4"))
	:strong
	#s(hash-table test equal data
		      (:strings "#535f00" :alt "#7d4d42" :assumed "#15626e" :primary "#7b496b"
				:faded "#a1276b" :foreground "#40373c" :background "#d2c9ce"))
	:focused
	#s(hash-table test equal data
		      (:strings "#677400" :alt "#916156" :assumed "#2a7783" :primary "#8f5d7f"
				:faded "#a35a29" :foreground "#544b45" :background "#e1c5c0"))
	:meta
	#s(hash-table test equal data
		      (:subtle "#e9e0da" :diff-remove-highlight "#f0c0c8" :diff-add-highlight
			       "#9aef7b" :diff-remove "#f4d2d7" :diff-add "#bbf8a9")))))
  "Cache value for the themes. Internal use only.")
(provide 'myron-themes-cache)