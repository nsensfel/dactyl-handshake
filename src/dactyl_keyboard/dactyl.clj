(ns dactyl-keyboard.dactyl
	(:refer-clojure :exclude [use import])
	(:require
		[clojure.core.matrix :refer [array matrix mmul]]
		[scad-clj.scad :refer :all]
		[scad-clj.model :refer :all]
		[unicode-math.core :refer :all]
	)
)

;; Consider origin (the {0, 0} point) as being on the bottom-left. This is where
;; standard keyboards have their left CTRL key.
;; {column, row} -> column x, row y.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FORWARD DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare rows-last-index)
(declare columns-last-index)
(declare thumb-cluster-rows-last-index)
(declare thumb-cluster-columns-last-index)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Height of the square hole in which you put a switch.
(def key-sockets-inner-height 14.15)

;; Width of the square hole in which you put a switch.
(def key-sockets-inner-width 14.15)

; If you use Cherry MX or Gateron switches, this can be turned on.
; If you use other switches such as Kailh, you should set this as false
(def key-sockets-have-side-nubs? true)

;; TODO: what do these actually do?
(def wall-z-offset -8) ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5) ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)

;; How thick should the walls be?
;; FIXME: is it really? Difference with shell-thickness need explaining.
(def wall-thickness 2)

;; Margin between two rows (vertical space between keys).
(defn key-inter-row-margin [column row] 4.0)

;; Margin between two columns (horizontal space between keys).
(defn key-inter-column-margin [column row] 2.0)

(def shell-thickness 4.5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Keyboard Settings (excludes the thumb cluster) ;;;;;;;;;;;;;;;;;;;;;;;;;

;; Total number of rows for the keyboard.
;; Rows are horizontal lines. Higher numbers increase the number of lines.
(def rows-count 5)

;; Total number of columns for the keyboard.
;; Columns are vertical lines. Higher numbers increase the length of lines.
(def columns-count 5)

;; Index (0 based) of the row to be considered as the middle one.
(def rows-middle-index (- rows-count 3))

;; Index (0 based) of the column to be considered as the middle one.
(def columns-middle-index 4)

;; TODO: not sure this is actually that.
(def keyboard-center-height 180)

;; FIXME: That's probably rads anyway...
(def keyboard-tenting-angle 100)

;; Base curvature of the columns (rads).
(def columns-base-curvature (/ pi (- 12 6)))

;; Base curvature of the rows (rads).
(def rows-base-curvature (/ pi (+ 36 0)))

;; Lets you specify conditions for a key to use the 1-5u format.
;; The default example has none.
(defn key-type [column row]
	(cond
		(= column 0) :s1-5u-horizontal
		:else :s1u
	)
)

;; Lets you specify conditions for a key to exist.
;; The default example removes some keys to give room to the thumb cluster.
;; We might want different levels for this:
;; - It's a socket. :socket
;; - It's filled in. :filled
;; - It's void. :void
;; - It's a void but without walls. :void-no-walls
(defn key-status [column row]
	(cond
		;; Sanity checks:
		(> column columns-last-index) :void-no-walls
		(> row rows-last-index) :void-no-walls
		(< column 0) :void-no-walls
		(< row 0) :void-no-walls

		;; Actual configuration:
		(and
			(== column columns-last-index)
			(or
				(== row 0)
				(== row 1)
				(== row 2)
			)
		)
			:void-no-walls
		:else :socket
	)
)

;; Lets you specify specific key offset.
;; The default example has none.
(defn key-offset [column row]
	(cond
		(and (== row rows-count) (== column columns-count)) true ;; impossible cond.
		:else [0 0 0]
	)
)

;; Lets you specify specific key column curvature
;; The default example has none.
(defn key-column-curvature [column row]
	(cond
		(and (== row rows-count) (== column columns-count)) pi ;; impossible cond.
		:else columns-base-curvature
	)
)

;; Lets you specify specific key row curvature
;; The default example has none.
(defn key-row-curvature [column row]
	(cond
		(and (== row rows-count) (== column columns-count)) pi ;; impossible cond.
		:else rows-base-curvature
	)
)

;; Padding around the keyboard's outermost switches to avoid any clipping with
;; the walls.
(def bezel-width 4)
(def bezel-depth 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thumb Cluster Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def thumb-cluster-rows-count 2)
(def thumb-cluster-columns-count 3)
(def thumb-cluster-rows-middle-index 0)
(def thumb-cluster-columns-middle-index (dec thumb-cluster-columns-count))
(def thumb-cluster-columns-base-curvature 0.1)
(def thumb-cluster-rows-base-curvature -0.1)

;; Margin between two rows (vertical space between keys).
(defn thumb-cluster-key-inter-row-margin [column row] 2)

;; Margin between two columns (horizontal space between keys).
(defn thumb-cluster-key-inter-column-margin [column row] 2)

(defn thumb-cluster-key-type [column row]
	(if
		(and
			(== row 0)
			(or
				(== column 0)
				(== column thumb-cluster-columns-last-index)
			)
		)
		:s1-5u-vertical
		:s1u
	)
)

(defn thumb-cluster-key-status [column row]
	(cond
		(> 0 column) :void
		(> 0 row) :void
		(< thumb-cluster-columns-last-index column) :void
		(< thumb-cluster-rows-last-index row) :void
		:else :socket
	)
)

(defn thumb-cluster-key-offset [column row]
	(cond
		(and
			(== row thumb-cluster-rows-last-index)
			(or
				(== column 0)
				(== column thumb-cluster-columns-last-index)
			)
		)
			[0 0 4]
		:else [0 0 0]
	)
)

(defn thumb-cluster-key-column-curvature [column row]
	thumb-cluster-columns-base-curvature
)

(defn thumb-cluster-key-row-curvature [column row]
	thumb-cluster-rows-base-curvature
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GENERAL UTILITY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotate-around-x [angle position]
	(mmul
		[
			[1 0 0]
			[0 (Math/cos angle) (- (Math/sin angle))]
			[0 (Math/sin angle)    (Math/cos angle)]
		]
		position
	)
)

(defn rotate-around-y [angle position]
	(mmul
		[
			[(Math/cos angle)     0 (Math/sin angle)]
			[0                    1 0]
			[(- (Math/sin angle)) 0 (Math/cos angle)]
		]
		position
	)
)

(defn hull-triangle-mesh [& shapes]
	(apply union
		(map
			(partial apply hull)
			(partition 3 1 shapes)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMPUTED PARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def rows-last-index (dec rows-count))
(def columns-last-index (dec columns-count))

(def rows-index-list (range 0 rows-count))
(def columns-index-list (range 0 columns-count))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;
(def sa-profile-key-height 12.7)

;; Not the same as shell-thickness, perhaps to ensure the socket frames are
;; kept separate from the rest of the shell?
(def key-sockets-thickness 4)
(def key-sockets-side-nub-thickness 4)
(def key-sockets-retention-tab-thickness 1.5)
(def key-sockets-retention-tab-hole-thickness
	(- (+ key-sockets-thickness 0.5) key-sockets-retention-tab-thickness)
)

;; Why different sizes?
(def key-sockets-1u-outer-width (+ key-sockets-inner-width 3.2))
(def key-sockets-1u-outer-height key-sockets-1u-outer-width)

(def key-sockets-1-5u-horizontal-outer-width (* 1.5 key-sockets-1u-outer-width))
(def key-sockets-1-5u-horizontal-outer-height key-sockets-1u-outer-height)

(def key-sockets-1-5u-vertical-outer-width key-sockets-1u-outer-width)
(def key-sockets-1-5u-vertical-outer-height
	(* 1.5 key-sockets-1u-outer-height)
)

(def key-socket-1u-filled-shape
	(cube
		key-sockets-1u-outer-width
		key-sockets-1u-outer-height
		(+ key-sockets-thickness 0.5)
	)
)

(defn key-sockets-shape [outer-height outer-width]
	(let*
		[
			height-thickness
				(/ (- outer-height key-sockets-inner-height) 2)

			width-thickness
				(/ (- outer-width key-sockets-inner-width) 2)

			top-wall
				(->>
					(cube
						outer-width
						height-thickness
						(+ key-sockets-thickness 0.5)
					)
					(translate
						[
							0
							(+ (/ height-thickness 2) (/ key-sockets-inner-height 2))
							(- (/ key-sockets-thickness 2) 0.25)
						]
					)
				)

			left-wall
				(->>
					(cube
						width-thickness
						outer-height
						(+ key-sockets-thickness 0.5)
					)
					(translate
						[
							(+ (/ width-thickness 2) (/ key-sockets-inner-width 2))
							0
							(- (/ key-sockets-thickness 2) 0.25)
						]
					)
				)

			side-nub
				(->>
					(binding [*fn* 30] (cylinder 1 2.75))
					(rotate (/ π 2) [1 0 0])
					(translate [(+ (/ key-sockets-inner-width 2)) 0 1])
					(hull
						(->>
							(cube 1.5 2.75 key-sockets-side-nub-thickness)
							(translate
								[
									(+ (/ 1.5 2) (/ key-sockets-inner-width 2))
									0
									(/ key-sockets-side-nub-thickness 2)
								]
							)
						)
					)
					(translate
						[
							0
							0
							(- key-sockets-thickness key-sockets-side-nub-thickness)
						]
					)
				)

			plate-half
				(union
					top-wall
					left-wall
					(when key-sockets-have-side-nubs? (with-fn 100 side-nub))
				)

			top-nub
				(->>
					(cube 5 5 key-sockets-retention-tab-hole-thickness)
					(translate
						[
							(+ (/ key-sockets-inner-width 2.5))
							0
							(- (/ key-sockets-retention-tab-hole-thickness 2) 0.5)
						]
					)
				)

			top-nub-pair
				(union
					top-nub
					(->>
						top-nub
						(mirror [1 0 0])
						(mirror [0 1 0])
					)
				)
		]
		(difference
			(union
				plate-half
				(->>
					plate-half
					(mirror [1 0 0])
					(mirror [0 1 0])
				)
			)
			(->>
				top-nub-pair
				(rotate (/ pi 2) [0 0 1])
			)
		)
	)
)

(def key-sockets-1u-shape
	(key-sockets-shape
		key-sockets-1u-outer-height
		key-sockets-1u-outer-width
	)
)

(def key-sockets-1-5u-horizontal-shape
	(key-sockets-shape
		key-sockets-1-5u-horizontal-outer-height
		key-sockets-1-5u-horizontal-outer-width
	)
)

(def key-sockets-1-5u-vertical-shape
	(key-sockets-shape
		key-sockets-1-5u-vertical-outer-height
		key-sockets-1-5u-vertical-outer-width
	)
)

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 key-sockets-thickness)])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 sa-length
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 key-sockets-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 27.94 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 key-sockets-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;; Fill the keyholes instead of placing a a keycap over them
(def keyhole-fill
	(->>
		(cube
			key-sockets-inner-height
			key-sockets-inner-width
			key-sockets-thickness
		)
		(translate [0 0 (/ key-sockets-thickness 2)])
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(def cap-top-height (+ key-sockets-thickness sa-profile-key-height))

(defn key-outer-height [column row]
	(case (key-type column row)
		:s1-5u-horizontal key-sockets-1-5u-horizontal-outer-height
		:s1-5u-vertical key-sockets-1-5u-vertical-outer-height
		:s1u key-sockets-1u-outer-height
	)
)

(defn key-outer-width [column row]
	(case (key-type column row)
		:s1-5u-horizontal key-sockets-1-5u-horizontal-outer-width
		:s1-5u-vertical key-sockets-1-5u-vertical-outer-width
		:s1u key-sockets-1u-outer-width
	)
)

(defn key-apply-geometry
	[
		translate-fn
		rotate-x-fn
		rotate-y-fn
		column
		row
		shape
	]
	(let*
		[
			column-range-step (if (< columns-middle-index column) 1 -1)
			column-indices (range columns-middle-index column column-range-step)
			row-range-step (if (< rows-middle-index row) 1 -1)
			row-indices (range rows-middle-index row row-range-step)

			adjusted-shape
				(translate-fn
					(key-offset column row)
					shape
				)

			placed-in-column
				(if (== rows-middle-index row)
					adjusted-shape
					(reduce
						(fn [shape-step row-step]
							(translate-fn
								[
									0
									(*
										(+
											(/ (key-outer-height column row-step) 2)
											(/
												(key-outer-height
													column
													(+ row-step row-range-step)
												)
												2
											)
											(key-inter-row-margin column row-step)
										)
										row-range-step
									)
									key-sockets-thickness
								]
								(rotate-x-fn
									(*
										(key-column-curvature column row-step)
										row-range-step
									)
									shape-step
								)
							)
						)
						adjusted-shape
						row-indices
					)
				)

			placed-in-column-and-row
				(if (== columns-middle-index column)
					placed-in-column
					(reduce
						(fn [shape-step column-step]
							(translate-fn
								[
									(*
										(+
											(/ (key-outer-width column-step row) 2)
											(/
												(key-outer-width
													(+ column-step column-range-step)
													row
												)
												2
											)
											(key-inter-column-margin column-step row)
										)
										column-range-step
									)
									0
									0
								]
								(rotate-y-fn
									(*
										(key-row-curvature column-step row)
										column-range-step
									)
									shape-step
								)
							)
						)
						placed-in-column
						column-indices
					)
				)
		]

		;; Lift to the keyboard's center height.
		(translate-fn
			[0 0 keyboard-center-height]
			;; Apply the keyboard's tenting angle.
			(rotate-y-fn keyboard-tenting-angle placed-in-column-and-row)
		)
	)
)

;; Places a shape at the location of the given key.
(defn shape-place-at-key [column row shape]
	(key-apply-geometry
		translate
		(fn [angle obj] (rotate angle [1 0 0] obj))
		(fn [angle obj] (rotate angle [0 1 0] obj))
		column
		row
		shape
	)
)

;; Computes the absolute position of a position relative to a key
(defn key-get-position [column row position]
	(key-apply-geometry
		(partial map +)
		rotate-around-x
		rotate-around-y
		column
		row
		position
	)
)

(def key-sockets-all-shapes
	(apply union
		(for
			[
				column columns-index-list
				row rows-index-list
				:when (= (key-status column row) :socket)
			]
			(->>
				(case (key-type column row)
					:s1-5u-horizontal key-sockets-1-5u-horizontal-shape
					:s1-5u-vertical key-sockets-1-5u-vertical-shape
					:s1u key-sockets-1u-shape
				)

				(shape-place-at-key column row)
			)
		)
		(for
			[
				column columns-index-list
				row rows-index-list
				:when (= (key-status column row) :filled)
			]
			(->>
				key-socket-1u-filled-shape
				(shape-place-at-key column row)
			)
		)
	)
)

(def key-caps-all-shapes
	(apply union
		(conj
			(for
				[
					column columns-index-list
					row rows-index-list
					:when (= (key-status column row) :socket)
				]
				(->>
					(sa-cap
						(case (key-type column row)
							:s1-5u-horizontal 1.5
							:s1-5u-vertical 1.5
							:s1u 1
						)
					)
					(shape-place-at-key column row)
				)
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SHELL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This was called "web" in the previous codebase. I get that it's a web of
;; shapes tying all the keys together, but "web" sounds too close to the Web.
;; It seems like the shell of the keyboard to me.

(def location-dot-size 0.1)
(def location-dot-half-size (/ location-dot-size 2))

(def key-socket-location-dot
	(->>
		(cube location-dot-size location-dot-size shell-thickness)
		(translate [0 0 (+ (/ shell-thickness -2) key-sockets-thickness)])
	)
)

(defn key-sockets-top-right-corner-relative-dot [height width]
	(translate
		[
			(- (/ width 2) location-dot-half-size)
			(- (/ height 2) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

(defn key-sockets-top-left-corner-relative-dot [height width]
	(translate
		[
			(+ (/ width -2) location-dot-half-size)
			(- (/ height 2) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

(defn key-sockets-bottom-left-corner-relative-dot [height width]
	(translate
		[
			(+ (/ width -2) location-dot-half-size)
			(+ (/ height -2) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

(defn key-sockets-bottom-right-corner-relative-dot [height width]
	(translate
		[
			(- (/ width 2) location-dot-half-size)
			(+ (/ height -2) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

(def key-1u-socket-top-right-corner-relative-dot
	(key-sockets-top-right-corner-relative-dot
		key-sockets-1u-outer-height
		key-sockets-1u-outer-width
	)
)

(def key-1u-socket-top-left-corner-relative-dot
	(key-sockets-top-left-corner-relative-dot
		key-sockets-1u-outer-height
		key-sockets-1u-outer-width
	)
)

(def key-1u-socket-bottom-left-corner-relative-dot
	(key-sockets-bottom-left-corner-relative-dot
		key-sockets-1u-outer-height
		key-sockets-1u-outer-width
	)
)

(def key-1u-socket-bottom-right-corner-relative-dot
	(key-sockets-bottom-right-corner-relative-dot
		key-sockets-1u-outer-height
		key-sockets-1u-outer-width
	)
)

(def key-1-5u-horizontal-socket-top-right-corner-relative-dot
	(key-sockets-top-right-corner-relative-dot
		key-sockets-1-5u-horizontal-outer-height
		key-sockets-1-5u-horizontal-outer-width
	)
)

(def key-1-5u-horizontal-socket-top-left-corner-relative-dot
	(key-sockets-top-left-corner-relative-dot
		key-sockets-1-5u-horizontal-outer-height
		key-sockets-1-5u-horizontal-outer-width
	)
)

(def key-1-5u-horizontal-socket-bottom-left-corner-relative-dot
	(key-sockets-bottom-left-corner-relative-dot
		key-sockets-1-5u-horizontal-outer-height
		key-sockets-1-5u-horizontal-outer-width
	)
)

(def key-1-5u-horizontal-socket-bottom-right-corner-relative-dot
	(key-sockets-bottom-right-corner-relative-dot
		key-sockets-1-5u-horizontal-outer-height
		key-sockets-1-5u-horizontal-outer-width
	)
)

(def key-1-5u-vertical-socket-top-right-corner-relative-dot
	(key-sockets-top-right-corner-relative-dot
		key-sockets-1-5u-vertical-outer-height
		key-sockets-1-5u-vertical-outer-width
	)
)

(def key-1-5u-vertical-socket-top-left-corner-relative-dot
	(key-sockets-top-left-corner-relative-dot
		key-sockets-1-5u-vertical-outer-height
		key-sockets-1-5u-vertical-outer-width
	)
)

(def key-1-5u-vertical-socket-bottom-left-corner-relative-dot
	(key-sockets-bottom-left-corner-relative-dot
		key-sockets-1-5u-vertical-outer-height
		key-sockets-1-5u-vertical-outer-width
	)
)

(def key-1-5u-vertical-socket-bottom-right-corner-relative-dot
	(key-sockets-bottom-right-corner-relative-dot
		key-sockets-1-5u-vertical-outer-height
		key-sockets-1-5u-vertical-outer-width
	)
)

(defn key-socket-bottom-right-corner-relative-dot [column row]
	(case (key-type column row)
		:s1-5u-horizontal
			key-1-5u-horizontal-socket-bottom-right-corner-relative-dot

		:s1-5u-vertical
			key-1-5u-vertical-socket-bottom-right-corner-relative-dot

		:s1u key-1u-socket-bottom-right-corner-relative-dot
	)
)

(defn key-socket-bottom-right-corner-absolute-dot [column row]
	(shape-place-at-key
		column
		row
		(key-socket-bottom-right-corner-relative-dot column row)
	)
)

(defn key-socket-top-right-corner-relative-dot [column row]
	(case (key-type column row)
		:s1-5u-horizontal
			key-1-5u-horizontal-socket-top-right-corner-relative-dot

		:s1-5u-vertical
			key-1-5u-vertical-socket-top-right-corner-relative-dot

		:s1u key-1u-socket-top-right-corner-relative-dot
	)
)

(defn key-socket-top-right-corner-absolute-dot [column row]
	(shape-place-at-key
		column
		row
		(key-socket-top-right-corner-relative-dot column row)
	)
)

(defn key-socket-bottom-left-corner-relative-dot [column row]
	(case (key-type column row)
		:s1-5u-horizontal
			key-1-5u-horizontal-socket-bottom-left-corner-relative-dot

		:s1-5u-vertical
			key-1-5u-vertical-socket-bottom-left-corner-relative-dot

		:s1u key-1u-socket-bottom-left-corner-relative-dot
	)
)

(defn key-socket-bottom-left-corner-absolute-dot [column row]
	(shape-place-at-key
		column
		row
		(key-socket-bottom-left-corner-relative-dot column row)
	)
)

(defn key-socket-top-left-corner-relative-dot [column row]
	(case (key-type column row)
		:s1-5u-horizontal
			key-1-5u-horizontal-socket-top-left-corner-relative-dot

		:s1-5u-vertical
			key-1-5u-vertical-socket-top-left-corner-relative-dot

		:s1u key-1u-socket-top-left-corner-relative-dot
	)
)

(defn key-socket-top-left-corner-absolute-dot [column row]
	(shape-place-at-key
		column
		row
		(key-socket-top-left-corner-relative-dot column row)
	)
)

(defn key-is-not-void? [column row]
	(let
		[
			this-key-status (key-status column row)
		]
		(case this-key-status
			:void false
			:void-no-walls false
			true
		)
	)
)

(def key-sockets-interconnecting-mesh-shape
	(apply
		union
		(concat
			;; Interconnections within a row.
			(for
				[
					column columns-index-list
					row rows-index-list
					:when
						(and
							(key-is-not-void? (dec column) row)
							(key-is-not-void? column row)
						)
				]
				(hull-triangle-mesh
					(key-socket-top-right-corner-absolute-dot (dec column) row)
					(key-socket-top-left-corner-absolute-dot column row)
					(key-socket-bottom-right-corner-absolute-dot (dec column) row)
					(key-socket-bottom-left-corner-absolute-dot column row)
				)
			)
			;; Interconnections within a column.
			(for
				[
					column columns-index-list
					row rows-index-list
					:when
						(and
							(key-is-not-void? column (dec row))
							(key-is-not-void? column row)
						)
				]
				(hull-triangle-mesh
					(key-socket-top-left-corner-absolute-dot column (dec row))
					(key-socket-top-right-corner-absolute-dot column (dec row))
					(key-socket-bottom-left-corner-absolute-dot column row)
					(key-socket-bottom-right-corner-absolute-dot column row)
				)
			)
			;; Diagonal interconnections (little bit not covered by horizontal and
			;; vertical connections).
			(for
				[
					column columns-index-list
					row rows-index-list
					:when
						(and
							(key-is-not-void? (dec column) (dec row))
							(key-is-not-void? (dec column) row)
							(key-is-not-void? column (dec row))
							(key-is-not-void? column row)
						)
				]
				(hull-triangle-mesh
					(key-socket-top-right-corner-absolute-dot (dec column) (dec row))
					(key-socket-top-left-corner-absolute-dot column (dec row))
					(key-socket-bottom-right-corner-absolute-dot (dec column) row)
					(key-socket-bottom-left-corner-absolute-dot column row)
				)
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THUMB CLUSTER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def thumb-cluster-rows-last-index (dec thumb-cluster-rows-count))
(def thumb-cluster-columns-last-index (dec thumb-cluster-columns-count))

(def thumb-cluster-rows-index-list (range 0 thumb-cluster-rows-count))
(def thumb-cluster-columns-index-list (range 0 thumb-cluster-columns-count))

(defn thumb-cluster-key-outer-height [column row]
	(case (thumb-cluster-key-type column row)
		:s1-5u-horizontal key-sockets-1-5u-horizontal-outer-height
		:s1-5u-vertical key-sockets-1-5u-vertical-outer-height
		:s1u key-sockets-1u-outer-height
	)
)

(defn thumb-cluster-key-outer-width [column row]
	(case (thumb-cluster-key-type column row)
		:s1-5u-horizontal key-sockets-1-5u-horizontal-outer-width
		:s1-5u-vertical key-sockets-1-5u-vertical-outer-width
		:s1u key-sockets-1u-outer-width
	)
)

(defn thumb-cluster-key-apply-base-geometry
	[
		translate-fn
		rotate-x-fn
		rotate-y-fn
		column
		row
		shape
	]
	(let*
		[
			column-range-step
				(if (< thumb-cluster-columns-middle-index column) 1 -1)

			column-indices
				(range thumb-cluster-columns-middle-index column column-range-step)

			row-range-step
				(if (< thumb-cluster-rows-middle-index row) 1 -1)

			row-indices
				(range thumb-cluster-rows-middle-index row row-range-step)

			adjusted-shape
				(translate-fn
					(thumb-cluster-key-offset column row)
					shape
				)

			placed-in-column
				(if (== thumb-cluster-rows-middle-index row)
					adjusted-shape
					(reduce
						(fn [shape-step row-step]
							(translate-fn
								[
									0
									(*
										(+
											(/
												(thumb-cluster-key-outer-height
													column
													row-step
												)
												2
											)
											(/
												(thumb-cluster-key-outer-height
													column
													(+ row-step row-range-step)
												)
												2
											)
											(thumb-cluster-key-inter-row-margin
												column
												row-step
											)
										)
										row-range-step
									)
									0
								]
								(rotate-x-fn
									(*
										(thumb-cluster-key-column-curvature
											column
											row-step
										)
										row-range-step
									)
									shape-step
								)
							)
						)
						adjusted-shape
						row-indices
					)
				)

			placed-in-column-and-row
				(if (== thumb-cluster-columns-middle-index column)
					placed-in-column
					(reduce
						(fn [shape-step column-step]
							(translate-fn
								[
									(*
										(+
											(/
												(thumb-cluster-key-outer-width
													column-step
													row
												)
												2
											)
											(/
												(thumb-cluster-key-outer-width
													(+ column-step column-range-step)
													row
												)
												2
											)
											(thumb-cluster-key-inter-column-margin
												column-step
												row
											)
										)
										column-range-step
									)
									0
									0
								]
								(rotate-y-fn
									(*
										(thumb-cluster-key-row-curvature column-step row)
										column-range-step
									)
									shape-step
								)
							)
						)
						placed-in-column
						column-indices
					)
				)
		]
		placed-in-column-and-row
	)
)

;; Computes the absolute position of a position relative to a key
(defn thumb-cluster-key-get-base-position [column row position]
	(thumb-cluster-key-apply-base-geometry
		(partial map +)
		rotate-around-x
		rotate-around-y
		column
		row
		position
	)
)

(defn thumb-cluster-key-apply-geometry
	[
		translate-fn
		rotate-x-fn
		rotate-y-fn
		column
		row
		shape
	]
	(let
		[
			[top-x top-y top-z]
				(thumb-cluster-key-get-base-position
					thumb-cluster-columns-last-index
					0
					[0 0 0]
				)
		]
		(translate-fn
			(key-get-position columns-last-index 0 [0 0 0])
			(rotate-y-fn
				keyboard-tenting-angle
				(rotate-y-fn
					(/ pi 2)
					(translate-fn
						;; Trying to set it at the top of the base keyboard key,
						;; but that's not quite right. This may be related to
						;; the computation being centered around a row and column.
						[
							(+
								(/ (key-outer-height columns-last-index 0) -2)
								(/
									(+
										(thumb-cluster-key-outer-width
											thumb-cluster-columns-last-index
											0
										)
										top-x
									)
									-2
								)
							)
							(/ (key-outer-width columns-last-index 0) 2)
							;; Guessing...
							(+ wall-xy-offset key-sockets-thickness)
						]
						(thumb-cluster-key-apply-base-geometry
							translate-fn
							rotate-x-fn
							rotate-y-fn
							column
							row
							shape
						)
					)
				)
			)
		)
	)
)

(defn shape-place-at-thumb-cluster-key [column row shape]
	(thumb-cluster-key-apply-geometry
		translate
		(fn [angle obj] (rotate angle [1 0 0] obj))
		(fn [angle obj] (rotate angle [0 1 0] obj))
		column
		row
		shape
	)
)

;; Computes the absolute position of a position relative to a key
(defn thumb-cluster-key-get-position [column row position]
	(thumb-cluster-key-apply-geometry
		(partial map +)
		rotate-around-x
		rotate-around-y
		column
		row
		position
	)
)

(def thumb-cluster-key-sockets-all-shapes
	(apply union
		(for
			[
				column thumb-cluster-columns-index-list
				row thumb-cluster-rows-index-list
				:when (= (thumb-cluster-key-status column row) :socket)
			]
			(->>
				(case (thumb-cluster-key-type column row)
					:s1-5u-horizontal key-sockets-1-5u-horizontal-shape
					:s1-5u-vertical key-sockets-1-5u-vertical-shape
					:s1u key-sockets-1u-shape
				)
				(shape-place-at-thumb-cluster-key column row)
			)
		)
	)
)

(def thumb-cluster-key-caps-all-shapes
	(apply union
		(conj
			(for
				[
					column thumb-cluster-columns-index-list
					row thumb-cluster-rows-index-list
					:when (= (thumb-cluster-key-status column row) :socket)
				]
				(->>
					(sa-cap
						(case (thumb-cluster-key-type column row)
							:s1-5u-horizontal 1.5
							:s1-5u-vertical 1.5
							:s1u 1
						)
					)
					(shape-place-at-thumb-cluster-key column row)
				)
			)
		)
	)
)

(defn thumb-cluster-key-socket-bottom-right-corner-relative-dot [column row]
	(case (thumb-cluster-key-type column row)
		:s1-5u-horizontal
			key-1-5u-horizontal-socket-bottom-right-corner-relative-dot

		:s1-5u-vertical
			key-1-5u-vertical-socket-bottom-right-corner-relative-dot

		:s1u key-1u-socket-bottom-right-corner-relative-dot
	)
)

(defn thumb-cluster-key-socket-bottom-right-corner-absolute-dot [column row]
	(shape-place-at-thumb-cluster-key
		column
		row
		(thumb-cluster-key-socket-bottom-right-corner-relative-dot column row)
	)
)

(defn thumb-cluster-key-socket-top-right-corner-relative-dot [column row]
	(case (thumb-cluster-key-type column row)
		:s1-5u-horizontal
			key-1-5u-horizontal-socket-top-right-corner-relative-dot

		:s1-5u-vertical
			key-1-5u-vertical-socket-top-right-corner-relative-dot

		:s1u key-1u-socket-top-right-corner-relative-dot
	)
)

(defn thumb-cluster-key-socket-top-right-corner-absolute-dot [column row]
	(shape-place-at-thumb-cluster-key
		column
		row
		(thumb-cluster-key-socket-top-right-corner-relative-dot column row)
	)
)

(defn thumb-cluster-key-socket-bottom-left-corner-relative-dot [column row]
	(case (thumb-cluster-key-type column row)
		:s1-5u-horizontal
			key-1-5u-horizontal-socket-bottom-left-corner-relative-dot

		:s1-5u-vertical
			key-1-5u-vertical-socket-bottom-left-corner-relative-dot

		:s1u key-1u-socket-bottom-left-corner-relative-dot
	)
)

(defn thumb-cluster-key-socket-bottom-left-corner-absolute-dot [column row]
	(shape-place-at-thumb-cluster-key
		column
		row
		(thumb-cluster-key-socket-bottom-left-corner-relative-dot column row)
	)
)

(defn thumb-cluster-key-socket-top-left-corner-relative-dot [column row]
	(case (thumb-cluster-key-type column row)
		:s1-5u-horizontal
			key-1-5u-horizontal-socket-top-left-corner-relative-dot

		:s1-5u-vertical
			key-1-5u-vertical-socket-top-left-corner-relative-dot

		:s1u key-1u-socket-top-left-corner-relative-dot
	)
)

(defn thumb-cluster-key-socket-top-left-corner-absolute-dot [column row]
	(shape-place-at-thumb-cluster-key
		column
		row
		(thumb-cluster-key-socket-top-left-corner-relative-dot column row)
	)
)

(def thumb-cluster-key-sockets-interconnecting-mesh-shape
	(apply
		union
		(concat
			;; Interconnections within a row.
			(for
				[
					column thumb-cluster-columns-index-list
					row thumb-cluster-rows-index-list
					:when
						(not
							(or
								(= (thumb-cluster-key-status (dec column) row) :void)
								(= (thumb-cluster-key-status column row) :void)
							)
						)
				]
				(hull-triangle-mesh
					(thumb-cluster-key-socket-top-right-corner-absolute-dot
						(dec column)
						row
					)
					(thumb-cluster-key-socket-top-left-corner-absolute-dot
						column
						row
					)
					(thumb-cluster-key-socket-bottom-right-corner-absolute-dot
						(dec column)
						row
					)
					(thumb-cluster-key-socket-bottom-left-corner-absolute-dot
						column
						row
					)
				)
			)
			;; Interconnections within a column.
			(for
				[
					column thumb-cluster-columns-index-list
					row thumb-cluster-rows-index-list
					:when
						(not
							(or
								(= (thumb-cluster-key-status column (dec row)) :void)
								(= (thumb-cluster-key-status column row) :void)
							)
						)
				]
				(hull-triangle-mesh
					(thumb-cluster-key-socket-top-left-corner-absolute-dot
						column
						(dec row)
					)
					(thumb-cluster-key-socket-top-right-corner-absolute-dot
						column
						(dec row)
					)
					(thumb-cluster-key-socket-bottom-left-corner-absolute-dot
						column
						row
					)
					(thumb-cluster-key-socket-bottom-right-corner-absolute-dot
						column
						row
					)
				)
			)
			;; Diagonal interconnections (little bit not covered by horizontal and
			;; vertical connections).
			(for
				[
					column thumb-cluster-columns-index-list
					row thumb-cluster-rows-index-list
					:when
						(not
							(or
								(=
									(thumb-cluster-key-status (dec column) (dec row))
									:void
								)
								(= (thumb-cluster-key-status (dec column) row) :void)
								(= (thumb-cluster-key-status column (dec row)) :void)
								(= (thumb-cluster-key-status column row) :void)
							)
						)
				]
				(hull-triangle-mesh
					(thumb-cluster-key-socket-top-right-corner-absolute-dot
						(dec column)
						(dec row)
					)
					(thumb-cluster-key-socket-top-left-corner-absolute-dot
						column
						(dec row)
					)
					(thumb-cluster-key-socket-bottom-right-corner-absolute-dot
						(dec column)
						row
					)
					(thumb-cluster-key-socket-bottom-left-corner-absolute-dot
						column
						row
					)
				)
			)
		)
	)
)

(def thumb-cluster-to-keyboard-connecting-shape
	(apply
		union
		(concat
			;; Have the inter column pads extend to the empty cells' rows:
			(for
				[
					row rows-index-list
					:when (not (key-is-not-void? columns-last-index row))
				]
				(hull-triangle-mesh
					(key-socket-top-right-corner-absolute-dot
						(dec columns-last-index)
						row
					)
					(key-socket-top-left-corner-absolute-dot
						columns-last-index
						row
					)
					(key-socket-bottom-right-corner-absolute-dot
						(dec columns-last-index)
						row
					)
					(key-socket-bottom-left-corner-absolute-dot
						columns-last-index
						row
					)
				)
			)
			;; Have the inter row pads extend to the empty cells column.
			[
				(hull-triangle-mesh
					(key-socket-top-left-corner-absolute-dot
						columns-last-index
						2
					)
					(key-socket-top-right-corner-absolute-dot
						columns-last-index
						2
					)
					(key-socket-bottom-left-corner-absolute-dot
						columns-last-index
						3
					)
					(key-socket-bottom-right-corner-absolute-dot
						columns-last-index
						3
					)
				)
			]
			;; Diagonal interconnections (little bit not covered by horizontal and
			;; vertical connections).
			(for
				[
					row rows-index-list
					:when (not (key-is-not-void? columns-last-index row))
				]
				(hull-triangle-mesh
					(key-socket-bottom-right-corner-absolute-dot
						(dec columns-last-index)
						(inc row)
					)
					(key-socket-bottom-left-corner-absolute-dot
						columns-last-index
						(inc row)
					)
					(key-socket-top-right-corner-absolute-dot
						(dec columns-last-index)
						row
					)
					(key-socket-top-left-corner-absolute-dot
						columns-last-index
						row
					)
				)
			)
		)
	)
)

(def larger-plate
	(let
		[
			plate-height (/ (- sa-double-length key-sockets-1u-outer-height) 3)
			top-plate
				(->>
					(cube key-sockets-1u-outer-width plate-height shell-thickness)
					(translate
						[
							0
							(/ (+ plate-height key-sockets-1u-outer-height) 2)
							(- key-sockets-thickness (/ shell-thickness 2))
						]
					)
				)
		]
		(union top-plate (mirror [0 1 0] top-plate))
	)
)

(def larger-plate-half
	(let
		[
			plate-height (/ (- sa-double-length key-sockets-1u-outer-height) 3)
			top-plate
				(->>
					(cube key-sockets-1u-outer-width plate-height shell-thickness)
					(translate
						[
							0
							(/ (+ plate-height key-sockets-1u-outer-height) 2)
							(- key-sockets-thickness (/ shell-thickness 2))
						]
					)
				)
		]
		(union top-plate (mirror [0 0 0] top-plate))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WALL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn shape-project-on-ground [height p]
	(->>
		(project p)
		(extrude-linear {:height height :twist 0 :convexity 0})
		(translate [0 0 (- (/ height 2) 10)])
	)
)

(defn hull-to-ground [& p]
	(hull p (shape-project-on-ground 0.001 p))
)

(defn direction-to-vector [direction scale]
	(case direction
		:north [0 scale]
		:south [0 (- scale)]
		:east [scale 0]
		:west [(- scale) 0]
	)
)

;; The wall may have a bezel. This finds a position for that bezel for a given
;; key. I am not seeing that being used much, though. It was previously named
;; left-... so it may not be meant to be used everywhere.
(defn key-get-bezel-position [column row direction]
	(let
		[
			;; 0.5 because of centering. It reaches edges.
			[key-x-factor key-y-factor] (direction-to-vector direction 0.5)
			[bezel-x-factor bezel-y-factor] (direction-to-vector direction 1)
		]
		(map
			+
			(key-get-position
				column
				row
				[
					(*
						key-x-factor
						(case (key-type column row)
							:s1-5u-horizontal key-sockets-1-5u-horizontal-outer-width
							:s1-5u-vertical key-sockets-1-5u-vertical-outer-width
							:s1u key-sockets-1u-outer-width
						)
					)
					(*
						key-y-factor
						(case (key-type column row)
							:s1-5u-horizontal key-sockets-1-5u-horizontal-outer-height
							:s1-5u-vertical key-sockets-1-5u-vertical-outer-height
							:s1u key-sockets-1u-outer-height
						)
					)
					0
				]
			)
			[
				(* bezel-x-factor bezel-width)
				(* bezel-y-factor bezel-width)
				bezel-depth
			]
		)
	)
)

(defn shape-place-at-key-bezel [column row direction shape]
	(translate (key-get-bezel-position column row direction) shape)
)

;; This is the little lip all around the case, in three pieces.
(defn case-upper-lip-inner-offset [direction]
	(let
		[
			[x y] (direction-to-vector direction wall-thickness)
		]
		[x y -1]
	)
)

(defn case-upper-lip-outer-offset [direction]
	(let
		[
			[x y] (direction-to-vector direction wall-xy-offset)
		]
		[x y wall-z-offset]
	)
)

(defn case-upper-lip-middle-offset [direction]
	(let
		[
			[x y] (direction-to-vector direction (+ wall-xy-offset wall-thickness))
		]
		[x y wall-z-offset]
	)
)

(defn case-lip-invert-z [location]
	(let [[x y z] location] [x y (- z)])
)

(defn case-upper-lip-shapes
	[
		place-function-1
		direction-1
		corner-location-1
		place-function-2
		direction-2
		corner-location-2
	]
	(hull
		(place-function-1 corner-location-1)
		(place-function-1
			(translate
				(case-upper-lip-inner-offset direction-1)
				corner-location-1
			)
		)
		(place-function-1
			(translate
				(case-upper-lip-outer-offset direction-1)
				corner-location-1
			)
		)
		(place-function-1
			(translate
				(case-upper-lip-middle-offset direction-1)
				corner-location-1
			)
		)
		(place-function-2 corner-location-2)
		(place-function-2
			(translate
				(case-upper-lip-inner-offset direction-2)
				corner-location-2
			)
		)
		(place-function-2
			(translate
				(case-upper-lip-outer-offset direction-2)
				corner-location-2
			)
		)
		(place-function-2
			(translate
				(case-upper-lip-middle-offset direction-2)
				corner-location-2
			)
		)
	)
)

(defn case-lower-lip-shapes
	[
		place-function-1
		direction-1
		corner-location-1
		place-function-2
		direction-2
		corner-location-2
	]
	(hull
		(place-function-1 corner-location-1)
		(place-function-1
			(translate
				(case-lip-invert-z (case-upper-lip-inner-offset direction-1))
				corner-location-1
			)
		)
		(place-function-1
			(translate
				(case-lip-invert-z (case-upper-lip-outer-offset direction-1))
				corner-location-1
			)
		)
		(place-function-1
			(translate
				(case-lip-invert-z (case-upper-lip-middle-offset direction-1))
				corner-location-1
			)
		)
		(place-function-2 corner-location-2)
		(place-function-2
			(translate
				(case-lip-invert-z (case-upper-lip-inner-offset direction-2))
				corner-location-2
			)
		)
		(place-function-2
			(translate
				(case-lip-invert-z (case-upper-lip-outer-offset direction-2))
				corner-location-2
			)
		)
		(place-function-2
			(translate
				(case-lip-invert-z (case-upper-lip-middle-offset direction-2))
				corner-location-2
			)
		)
	)
)

(defn case-enclosed-lip-shapes
	[
		place-function-1
		direction-1
		corner-location-1
		place-function-2
		direction-2
		corner-location-2
	]
	(case-upper-lip-shapes
		(fn [shape]
			(place-function-1
				(translate
					(let
						[
							[x y z] (case-upper-lip-outer-offset direction-1)
						]
						[0 0 (* z 2)]
					)
					(mirror [0 0 -1] shape)
				)
			)
		)
		direction-1
		corner-location-1
		(fn [shape]
			(place-function-2
				(translate
					(let
						[
							[x y z] (case-upper-lip-outer-offset direction-2)
						]
						[0 0 (* z 2)]
					)
					(mirror [0 0 -1] shape)
				)
			)
		)
		direction-2
		corner-location-2
	)
)

(defn case-upper-and-enclosed-lip-shapes
	[
		place-function-1
		direction-1
		corner-location-1
		place-function-2
		direction-2
		corner-location-2
	]
	(union
		(case-upper-lip-shapes
			place-function-1
			direction-1
			corner-location-1
			place-function-2
			direction-2
			corner-location-2
		)
		(case-enclosed-lip-shapes
			place-function-1
			direction-1
			corner-location-1
			place-function-2
			direction-2
			corner-location-2
		)
	)
)

(defn wall-and-case-upper-lip-shapes
	[
		place-function-1
		direction-1
		corner-location-1
		place-function-2
		direction-2
		corner-location-2
	]
	(union
		(case-upper-lip-shapes
			place-function-1
			direction-1
			corner-location-1
			place-function-2
			direction-2
			corner-location-2
		)
		(hull-to-ground
			(place-function-1
				(translate
					(case-upper-lip-outer-offset direction-1)
					corner-location-1
				)
			)
			(place-function-1
				(translate
					(case-upper-lip-middle-offset direction-1)
					corner-location-1
				)
			)
			(place-function-2
				(translate
					(case-upper-lip-outer-offset direction-2)
					corner-location-2
				)
			)
			(place-function-2
				(translate
					(case-upper-lip-middle-offset direction-2)
					corner-location-2
				)
			)
		)
	)
)

(defn wall-and-case-upper-lip-shapes-between-keys
	[
		column-1
		row-1
		direction-1
		corner-location-1
		column-2
		row-2
		direction-2
		corner-location-2
	]
	(wall-and-case-upper-lip-shapes
		(partial shape-place-at-key column-1 row-1)
		direction-1
		corner-location-1
		(partial shape-place-at-key column-2 row-2)
		direction-2
		corner-location-2
	)
)

(def case-walls
	(union
		;; North and South Wall
		(for
			[x columns-index-list]
			;; TODO: Account for (get-key-status ...)
			(union
				(when (not (= (key-status x 0) :void-no-walls))
					(wall-and-case-upper-lip-shapes-between-keys
						x
						0
						:south
						(key-socket-bottom-left-corner-relative-dot x 0)

						x
						0
						:south
						(key-socket-bottom-right-corner-relative-dot x 0)
					)
				)
				(when (not (= (key-status x rows-last-index) :void-no-walls))
					(wall-and-case-upper-lip-shapes-between-keys
						x
						rows-last-index
						:north
						(key-socket-top-left-corner-relative-dot x rows-last-index)

						x
						rows-last-index
						:north
						(key-socket-top-right-corner-relative-dot x rows-last-index)
					)
				)
			)
		)
		(for
			[x (range 1 columns-count)]
			;; TODO: Account for (get-key-status ...)
			(union
				(wall-and-case-upper-lip-shapes-between-keys
					x
					0
					:south
					(key-socket-bottom-left-corner-relative-dot x 0)

					(dec x)
					0
					:south
					(key-socket-bottom-right-corner-relative-dot (dec x) 0)
				)
				(wall-and-case-upper-lip-shapes-between-keys
					x
					rows-last-index
					:north
					(key-socket-top-left-corner-relative-dot x rows-last-index)

					(dec x)
					rows-last-index
					:north
					(key-socket-top-right-corner-relative-dot
						(dec x)
						rows-last-index
					)
				)
			)
		)
		;; West and East Wall
		(for
			[y rows-index-list]
			;; TODO: Account for (get-key-status ...)
			(union
				(wall-and-case-upper-lip-shapes-between-keys
					0
					y
					:west
					(key-socket-top-left-corner-relative-dot 0 y)

					0
					y
					:west
					(key-socket-bottom-left-corner-relative-dot 0 y)
				)
				(wall-and-case-upper-lip-shapes-between-keys
					columns-last-index
					y
					:east
					(key-socket-top-right-corner-relative-dot columns-last-index y)

					columns-last-index
					y
					:east
					(key-socket-bottom-right-corner-relative-dot
						columns-last-index
						y
					)
				)
			)
		)
		(for
			[y (range 1 rows-count)]
			;; TODO: Account for (get-key-status ...)
			(union
				(wall-and-case-upper-lip-shapes-between-keys
					0
					y
					:west
					(key-socket-bottom-left-corner-relative-dot 0 y)

					0
					(dec y)
					:west
					(key-socket-top-left-corner-relative-dot 0 (dec y))
				)
				(wall-and-case-upper-lip-shapes-between-keys
					columns-last-index
					y
					:east
					(key-socket-bottom-right-corner-relative-dot columns-last-index y)

					columns-last-index
					(dec y)
					:east
					(key-socket-top-right-corner-relative-dot
						columns-last-index
						(dec y)
					)
				)
			)
		)
		;; Not quite corners. This hacky approximation makes it look like they
		;; are here, but even though I coded it, I can't follow. The fact that
		;; they rely on out-of-bounds indices doesn't help. Offsets applied to
		;; border keys aren't reflected as a result.
		;; FIXME: need a proper version of this.
		(wall-and-case-upper-lip-shapes-between-keys
			columns-last-index
			rows-last-index
			:north
			(key-socket-top-right-corner-relative-dot
				columns-last-index
				rows-last-index
			)

			columns-last-index
			rows-last-index
			:east
			(key-socket-top-right-corner-relative-dot
				columns-last-index
				rows-last-index
			)
		)
		(wall-and-case-upper-lip-shapes-between-keys
			columns-last-index
			0
			:south
			(key-socket-bottom-right-corner-relative-dot
				columns-last-index
				0
			)

			columns-last-index
			0
			:east
			(key-socket-bottom-right-corner-relative-dot
				columns-last-index
				0
			)
		)
		(wall-and-case-upper-lip-shapes-between-keys
			0
			rows-last-index
			:north
			(key-socket-top-left-corner-relative-dot
				0
				rows-last-index
			)

			0
			rows-last-index
			:west
			(key-socket-top-left-corner-relative-dot
				0
				rows-last-index
			)
		)
		(wall-and-case-upper-lip-shapes-between-keys
			0
			0
			:south
			(key-socket-bottom-left-corner-relative-dot
				0
				0
			)

			0
			0
			:west
			(key-socket-bottom-left-corner-relative-dot
				0
				0
			)
		)
	)
)

(def thumb-cluster-case-walls
	(union
		(for
			[x thumb-cluster-columns-index-list]
			;; TODO: Account for (get-key-status ...)
			(union
				;; south lip
				(case-upper-and-enclosed-lip-shapes
					(partial shape-place-at-thumb-cluster-key x 0)
					:south
					(thumb-cluster-key-socket-bottom-left-corner-relative-dot x 0)

					(partial shape-place-at-thumb-cluster-key x 0)
					:south
					(thumb-cluster-key-socket-bottom-right-corner-relative-dot x 0)
				)
				(when (not (= (thumb-cluster-key-status (dec x) 0) :void))
					(case-upper-and-enclosed-lip-shapes
						(partial shape-place-at-thumb-cluster-key (dec x) 0)
						:south
						(thumb-cluster-key-socket-bottom-right-corner-relative-dot
							(dec x)
							0
						)

						(partial shape-place-at-thumb-cluster-key x 0)
						:south
						(thumb-cluster-key-socket-bottom-left-corner-relative-dot x 0)
					)
				)
				;; north lip
				(case-upper-and-enclosed-lip-shapes
					(partial
						shape-place-at-thumb-cluster-key
						x
						thumb-cluster-rows-last-index
					)
					:north
					(thumb-cluster-key-socket-top-left-corner-relative-dot
						x
						thumb-cluster-rows-last-index
					)

					(partial
						shape-place-at-thumb-cluster-key
						x
						thumb-cluster-rows-last-index
					)
					:north
					(thumb-cluster-key-socket-top-right-corner-relative-dot
						x
						thumb-cluster-rows-last-index
					)
				)
				(when
					(not
						(=
							(thumb-cluster-key-status
								(dec x)
								thumb-cluster-rows-last-index
							)
							:void
						)
					)
					(case-upper-and-enclosed-lip-shapes
						(partial
							shape-place-at-thumb-cluster-key
							(dec x)
							thumb-cluster-rows-last-index
						)
						:north
						(thumb-cluster-key-socket-top-right-corner-relative-dot
							(dec x)
							thumb-cluster-rows-last-index
						)

						(partial
							shape-place-at-thumb-cluster-key
							x
							thumb-cluster-rows-last-index
						)
						:north
						(thumb-cluster-key-socket-top-left-corner-relative-dot
							x
							thumb-cluster-rows-last-index
						)
					)
				)
			)
		)
		(for
			[y thumb-cluster-rows-index-list]
			;; TODO: Account for (get-key-status ...)
			(union
				;; lip on "left" side
				(case-upper-and-enclosed-lip-shapes
					(partial shape-place-at-thumb-cluster-key 0 y)
					:west
					(thumb-cluster-key-socket-top-left-corner-relative-dot 0 y)

					(partial shape-place-at-thumb-cluster-key 0 y)
					:west
					(thumb-cluster-key-socket-bottom-left-corner-relative-dot 0 y)
				)
				(when (not (= (thumb-cluster-key-status 0 (dec y)) :void))
					(case-upper-and-enclosed-lip-shapes
						(partial shape-place-at-thumb-cluster-key 0 (dec y))
						:west
						(thumb-cluster-key-socket-top-left-corner-relative-dot
							0
							(dec y)
						)

						(partial shape-place-at-thumb-cluster-key 0 y)
						:west
						(thumb-cluster-key-socket-bottom-left-corner-relative-dot 0 y)
					)
				)
			)
		)
		(case-upper-lip-shapes
			(partial shape-place-at-thumb-cluster-key 0 0)
			:west
			(thumb-cluster-key-socket-bottom-left-corner-relative-dot 0 0)

			(partial shape-place-at-thumb-cluster-key 0 0)
			:south
			(thumb-cluster-key-socket-bottom-left-corner-relative-dot 0 0)
		)
		(case-enclosed-lip-shapes
			(partial shape-place-at-thumb-cluster-key 0 0)
			:west
			(thumb-cluster-key-socket-bottom-left-corner-relative-dot 0 0)

			(partial shape-place-at-thumb-cluster-key 0 0)
			:south
			(thumb-cluster-key-socket-bottom-left-corner-relative-dot 0 0)
		)
		(case-upper-lip-shapes
			(partial
				shape-place-at-thumb-cluster-key
				0
				thumb-cluster-rows-last-index
			)
			:west
			(thumb-cluster-key-socket-top-left-corner-relative-dot
				0
				thumb-cluster-rows-last-index
			)

			(partial
				shape-place-at-thumb-cluster-key
				0
				thumb-cluster-rows-last-index
			)
			:north
			(thumb-cluster-key-socket-top-left-corner-relative-dot
				0
				thumb-cluster-rows-last-index
			)
		)
		(case-enclosed-lip-shapes
			(partial
				shape-place-at-thumb-cluster-key
				0
				thumb-cluster-rows-last-index
			)
			:west
			(thumb-cluster-key-socket-top-left-corner-relative-dot
				0
				thumb-cluster-rows-last-index
			)

			(partial
				shape-place-at-thumb-cluster-key
				0
				thumb-cluster-rows-last-index
			)
			:north
			(thumb-cluster-key-socket-top-left-corner-relative-dot
				0
				thumb-cluster-rows-last-index
			)
		)
	)
)

(def thumb-cluster-link-to-keyboard
)

; Offsets for the controller/trrs holder cutout
(def holder-offset
  (case rows-count
    4 -3.5
    5 (if true 0 -6.5)
    6 (if true 3.2 2.2)))

(def notch-offset
  (case rows-count
    4 3.35
    5 0.15
    6 -5.07))

; Cutout for MCU holder
(def usb-holder-ref
	(key-get-position
		0
		0
		(map -
			(case-upper-lip-outer-offset :north)
			[0 (/ key-sockets-1u-outer-height 2) 0]
		)
	)
)

(def usb-holder-position
	(map +
		[(+ 18.8 holder-offset) 18.7 1.3]
		[(first usb-holder-ref) (second usb-holder-ref) 1.8]
	)
)

(def usb-holder-space
	(translate
		(map + usb-holder-position [-1.5 (* -1 wall-thickness) 2.1])
		(cube 28.666 30 10.4)
	)
)

(def usb-holder-notch-l
	(translate
		(map + usb-holder-position [-12 (+ 4.4 notch-offset) 2.1])
		(cube 10 1.3 10.4)
	)
)

(def usb-holder-notch-r
	(translate
		(map + usb-holder-position [9 (+ (if true 4.4 6.4) notch-offset) 2.1])
		(cube 10 1.3 10.4)
	)
)

(def model-right
	(difference
		(union
			key-sockets-all-shapes
			key-sockets-interconnecting-mesh-shape
			thumb-cluster-key-sockets-all-shapes
			thumb-cluster-key-sockets-interconnecting-mesh-shape
			thumb-cluster-case-walls
			thumb-cluster-to-keyboard-connecting-shape
			(difference
				case-walls
				usb-holder-space
				usb-holder-notch-l
				usb-holder-notch-r
				;; screw-insert-holes
			)
		)
		(translate [0 0 -20] (cube 350 350 40))
	)
)

;;(spit "things/right.scad" (write-scad model-right))
(try
	(spit "things/right.scad" (write-scad model-right))
	(catch Exception e
		(.printStackTrace e)
	)
)

(spit "things/left.scad" (write-scad (mirror [-1 0 0] model-right)))

;;(spit "things/right-test.scad"
;;	(write-scad
;;		(union model-right thumbcaps-type caps)
;;	)
;;)

(try
	(spit "things/right-plate.scad"
		(write-scad
			(extrude-linear
				{:height 2.6 :center false}
				(project
					;;(difference
						(union
							key-sockets-all-shapes
							key-sockets-interconnecting-mesh-shape
							case-walls
						)
						;;(translate [0 0 -10] screw-insert-screw-holes)
					;;)
				)
			)
		)
	)
	(catch Exception e
		(.printStackTrace e)
	)
)

;;(spit "things/right-plate-laser.scad"
;;	(write-scad
;;		(cut
;;			(translate [0 0 -0.1]
;;				(difference
;;					(union case-walls screw-insert-outers)
;;					(translate [0 0 -10] screw-insert-screw-holes)
;;				)
;;			)
;;		)
;;	)
;;)

(defn -main [dum] 1)  ; dummy to make it easier to batch
