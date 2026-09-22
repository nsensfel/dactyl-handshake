(ns dactyl-keyboard.dactyl
	(:refer-clojure :exclude [use import])
	(:require
		[clojure.core.matrix :refer [array matrix mmul]]
		[scad-clj.scad :refer :all]
		[scad-clj.model :refer :all]
		[unicode-math.core :refer :all]
	)
)

;; Consider origin (the {0, 0} point) as being on the top-left. This is where
;; standard keyboards have their Esc key.
;; {column} row} -> column x, row y.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FORWARD DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare rows-last-index)
(declare columns-last-index)

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
(def key-inter-row-margin 2.5)

;; Margin between two columns (horizontal space between keys).
(def key-inter-column-margin 1.0)

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
(def keyboard-center-height 8)

;; FIXME: That's probably rads anyway...
(def keyboard-tenting-angle 100)

;; Base curvature of the columns (rads).
(def columns-base-curvature (/ pi (- 12 6)))

;; Base curvature of the rows (rads).
(def rows-base-curvature (/ pi (+ 36 0)))

;; Lets you specify conditions for a key to use the 1.5u format.
;; The default example has none.
(defn key-is-1.5u? [column row]
	(cond
		(and (= row rows-count) (= column columns-count)) true ;; impossible cond.
		:else false
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
		(and
			(= column columns-last-index)
			(or (= row rows-last-index) (= row (dec rows-last-index)))
		)
			:void-no-walls

		:else :socket
	)
)

;; Lets you specify specific key offset.
;; The default example has none.
(defn key-offset [column row]
	(cond
		(and (= row rows-count) (= column columns-count)) true ;; impossible cond.
		:else [0 0 0]
	)
)

;; Lets you specify specific key column curvature
;; The default example has none.
(defn key-column-curvature [column row]
	(cond
		(and (= row rows-count) (= column columns-count)) pi ;; impossible cond.
		:else columns-base-curvature
	)
)

;; Lets you specify specific key row curvature
;; The default example has none.
(defn key-row-curvature [column row]
	(cond
		(and (= row rows-count) (= column columns-count)) pi ;; impossible cond.
		:else rows-base-curvature
	)
)

;; Padding around the keyboard's outermost switches to avoid any clipping with
;; the walls.
(def bezel-width 4)
(def bezel-depth 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thumb Cluster Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def thumb-cluster-rows-count 3)
(def thumb-cluster-columns-count 2)
(def thumb-cluster-rows-middle-index 1)
(def thumb-cluster-columns-middle-index 0)
(def thumb-cluster-columns-base-curvature 0)
(def thumb-cluster-rows-base-curvature 0)

(defn thumb-cluster-key-is-1.5u? [column row]
	(= column 0)
)

(defn thumb-cluster-key-status [column row]
	:socket
)

(defn thumb-cluster-key-offset [column row]
	[0 0 0]
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
(defn deg2rad [degrees]
	(* (/ degrees 180) pi)
)

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
(def key-sockets-outer-width (+ key-sockets-inner-width 3.2))
(def key-sockets-outer-height (+ key-sockets-inner-height 2.7))

(def key-socket-filled-shape
	(cube
		key-sockets-outer-width
		key-sockets-outer-height
		(+ key-sockets-thickness 0.5)
	)
)

;; FIXME: Magic numbers galore.
;; FIXME: It seems to me the socket shape should be different for 1u and 1.5u.
(def key-socket-shape
	(let
		[
			top-wall
				(->>
					(cube
						(+ key-sockets-inner-width 3)
						1.5
						(+ key-sockets-thickness 0.5)
					)
					(translate
						[
							0
							(+ (/ 1.5 2) (/ key-sockets-inner-height 2))
							(- (/ key-sockets-thickness 2) 0.25)
						]
					)
				)

			left-wall
				(->>
					(cube
						1.8
						(+ key-sockets-inner-height 3)
						(+ key-sockets-thickness 0.5)
					)
					(translate
						[
							(+ (/ 1.8 2) (/ key-sockets-inner-width 2))
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
(def keyhole-fill (->> (cube key-sockets-inner-height key-sockets-inner-width key-sockets-thickness)
                       (translate [0 0 (/ key-sockets-thickness 2)])))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(def cap-top-height (+ key-sockets-thickness sa-profile-key-height))

;; FIXME: That's too complicated to go without comment.
;; If I understand it correctly, this is computing the horizontal position of a
;; key.
(defn key-row-radius [column row]
	(+
		(/
			(/ (+ key-sockets-outer-height key-inter-column-margin) 2)
			(Math/sin (/ (key-column-curvature column row) 2))
		)
		cap-top-height
	)
)

;; FIXME: That's too complicated to go without comment.
;; If I understand it correctly, this is computing the vertical position of a
;; key.
(defn key-column-radius [column row]
	(+
		(/
			(/ (+ key-sockets-outer-width key-inter-row-margin) 2)
			(Math/sin (/ (key-row-curvature column row) 2))
		)
		cap-top-height
	)
)

;; FIXME: I don't really know what that's supposed to be.
;; FIXME: That's too complicated to go without comment.
;; It does not even seem to be used...
(defn column-x-delta [column row]
	(+
		-1
		(-
			(*
				(key-column-radius column row)
				(Math/sin (key-row-curvature column row))
			)
		)
	)
)

;; Compute or place a shape at a key's location.
(defn key-apply-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
	(let
		[
			column-radius (key-column-radius column row)
			column-angle
				(* (key-row-curvature column row) (- columns-middle-index column))

			row-angle
				(* (key-column-curvature column row) (- rows-middle-index row))

			row-radius (key-row-radius column row)

			placed-shape
				(->> shape
					;; Apply last touch offsets.
					(translate-fn (key-offset column row))

					;; Place the key in its row.
					(translate-fn [0 0 (- row-radius)])
					(rotate-x-fn  row-angle)
					(translate-fn [0 0 row-radius])

					;; Place the key in its column
					(translate-fn [0 0 (- column-radius)])
					(rotate-y-fn  column-angle)
					(translate-fn [0 0 column-radius])
				)
		]
		(->>
			placed-shape

			;; Apply the keyboard's tenting angle.
			(rotate-y-fn keyboard-tenting-angle)

			;; Lift to the keyboard's center height.
			(translate-fn [0 0 keyboard-center-height])
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
				key-socket-shape
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
				key-socket-filled-shape
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
						(if (key-is-1.5u? column row) 1.5 1)
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

(def key-1u-socket-top-right-corner
	(translate
		[
			(- (/ key-sockets-outer-width 1.95) location-dot-half-size)
			(- (/ key-sockets-outer-height 1.95) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

(def key-1u-socket-top-left-corner
	(translate
		[
			(+ (/ key-sockets-outer-width -1.95) location-dot-half-size)
			(- (/ key-sockets-outer-height 1.95) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

(def key-1u-socket-bottom-left-corner
	(translate
		[
			(+ (/ key-sockets-outer-width -1.95) location-dot-half-size)
			(+ (/ key-sockets-outer-height -1.95) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

(def key-1u-socket-bottom-right-corner
	(translate
		[
			(- (/ key-sockets-outer-width 1.95) location-dot-half-size)
			(+ (/ key-sockets-outer-height -1.95) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

(def key-1.5u-socket-top-right-corner
	(translate
		[
			(- (/ key-sockets-outer-width 1.2) location-dot-half-size)
			(- (/ key-sockets-outer-height  2) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

(def key-1.5u-socket-top-left-corner
	(translate
		[
			(+ (/ key-sockets-outer-width -1.2) location-dot-half-size)
			(- (/ key-sockets-outer-height  2) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

(def key-1.5u-socket-bottom-left-corner
	(translate
		[
			(+ (/ key-sockets-outer-width -1.2) location-dot-half-size)
			(+ (/ key-sockets-outer-height -2) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

(def key-1.5u-socket-bottom-right-corner
	(translate
		[
			(- (/ key-sockets-outer-width 1.2) location-dot-half-size)
			(+ (/ key-sockets-outer-height -2) location-dot-half-size)
			0
		]
		key-socket-location-dot
	)
)

;; FIXME: Name of this vs key-socket-bottom-right-corner not clear enough
;; TODO: these should check for key size to know which variant to use.
(defn key-socket-bottom-right-corner-dot [column row]
	(shape-place-at-key
		column
		row
		(if (key-is-1.5u? column row)
			key-1.5u-socket-bottom-right-corner
			key-1u-socket-bottom-right-corner
		)
	)
)

(defn key-socket-top-right-corner-dot [column row]
	(shape-place-at-key
		column
		row
		(if (key-is-1.5u? column row)
			key-1.5u-socket-top-right-corner
			key-1u-socket-top-right-corner
		)
	)
)

(defn key-socket-bottom-left-corner-dot [column row]
	(shape-place-at-key
		column
		row
		(if (key-is-1.5u? column row)
			key-1.5u-socket-bottom-left-corner
			key-1u-socket-bottom-left-corner
		)
	)
)

(defn key-socket-top-left-corner-dot [column row]
	(shape-place-at-key
		column
		row
		(if (key-is-1.5u? column row)
			key-1.5u-socket-top-left-corner
			key-1u-socket-top-left-corner
		)
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
					(key-socket-top-right-corner-dot (dec column) row)
					(key-socket-top-left-corner-dot column row)
					(key-socket-bottom-right-corner-dot (dec column) row)
					(key-socket-bottom-left-corner-dot column row)
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
					(key-socket-bottom-left-corner-dot column (dec row))
					(key-socket-bottom-right-corner-dot column (dec row))
					(key-socket-top-left-corner-dot column row)
					(key-socket-top-right-corner-dot column row)
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
					(key-socket-bottom-left-corner-dot (dec column) (dec row))
					(key-socket-bottom-right-corner-dot column (dec row))
					(key-socket-top-right-corner-dot (dec column) row)
					(key-socket-top-left-corner-dot column row)
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

(defn thumb-cluster-key-row-radius [column row]
	(+
		(/
			(/ (+ key-sockets-outer-height key-inter-column-margin) 2)
			(Math/sin (/ (thumb-cluster-key-column-curvature column row) 2))
		)
		cap-top-height
	)
)

(defn thumb-cluster-key-column-radius [column row]
	(+
		(/
			(/ (+ key-sockets-outer-width key-inter-row-margin) 2)
			(Math/sin (/ (thumb-cluster-key-row-curvature column row) 2))
		)
		cap-top-height
	)
)

(defn thumb-cluster-place-at-origin [shape]
	(->> shape
		(translate [0 0 0])
		(shape-place-at-key columns-last-index rows-last-index)
	)
)


(defn thumb-cluster-key-apply-geometry
	[
		translate-fn
		rotate-x-fn
		rotate-y-fn column
		row
		shape
	]
	(let
		[
			column-radius (thumb-cluster-key-column-radius column row)
			column-angle
				(*
					(thumb-cluster-key-row-curvature column row)
					(- thumb-cluster-columns-middle-index column)
				)

			row-angle
				(*
					(thumb-cluster-key-column-curvature column row)
					(- thumb-cluster-rows-middle-index row)
				)

			row-radius (thumb-cluster-key-row-radius column row)

			placed-shape
				(->> shape
					;; Apply last touch offsets.
					(translate-fn (thumb-cluster-key-offset column row))

					;; Place the key in its row.
					(translate-fn [0 0 (- row-radius)])
					(rotate-x-fn  row-angle)
					(translate-fn [0 0 row-radius])

					;; Place the key in its column
					(translate-fn [0 0 (- column-radius)])
					(rotate-y-fn  column-angle)
					(translate-fn [0 0 column-radius])
				)
		]
		(->>
			placed-shape
			(thumb-cluster-place-at-origin)
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
	(key-apply-geometry
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
				key-socket-shape
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
						(if (thumb-cluster-key-is-1.5u? column row) 1.5 1)
					)
					(shape-place-at-thumb-cluster-key column row)
				)
			)
		)
	)
)

(defn thumb-cluster-key-socket-bottom-right-corner-dot [column row]
	(shape-place-at-thumb-cluster-key
		column
		row
		(if (thumb-cluster-key-is-1.5u? column row)
			key-1.5u-socket-bottom-right-corner
			key-1u-socket-bottom-right-corner
		)
	)
)

(defn thumb-cluster-key-socket-top-right-corner-dot [column row]
	(shape-place-at-thumb-cluster-key
		column
		row
		(if (thumb-cluster-key-is-1.5u? column row)
			key-1.5u-socket-top-right-corner
			key-1u-socket-top-right-corner
		)
	)
)

(defn thumb-cluster-key-socket-bottom-left-corner-dot [column row]
	(shape-place-at-thumb-cluster-key
		column
		row
		(if (thumb-cluster-key-is-1.5u? column row)
			key-1.5u-socket-bottom-left-corner
			key-1u-socket-bottom-left-corner
		)
	)
)

(defn thumb-cluster-key-socket-top-left-corner-dot [column row]
	(shape-place-at-thumb-cluster-key
		column
		row
		(if (thumb-cluster-key-is-1.5u? column row)
			key-1.5u-socket-top-left-corner
			key-1u-socket-top-left-corner
		)
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
					(thumb-cluster-key-socket-top-right-corner-dot (dec column) row)
					(thumb-cluster-key-socket-top-left-corner-dot column row)
					(thumb-cluster-key-socket-bottom-right-corner-dot
						(dec column)
						row
					)
					(thumb-cluster-key-socket-bottom-left-corner-dot column row)
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
					(thumb-cluster-key-socket-bottom-left-corner-dot
						column
						(dec row)
					)
					(thumb-cluster-key-socket-bottom-right-corner-dot
						column
						(dec row)
					)
					(thumb-cluster-key-socket-top-left-corner-dot column row)
					(thumb-cluster-key-socket-top-right-corner-dot column row)
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
					(thumb-cluster-key-socket-bottom-left-corner-dot
						(dec column)
						(dec row)
					)
					(thumb-cluster-key-socket-bottom-right-corner-dot
						column
						(dec row)
					)
					(thumb-cluster-key-socket-top-right-corner-dot (dec column) row)
					(thumb-cluster-key-socket-top-left-corner-dot column row)
				)
			)
		)
	)
)

(def larger-plate
	(let
		[
			plate-height (/ (- sa-double-length key-sockets-outer-height) 3)
			top-plate
				(->>
					(cube key-sockets-outer-width plate-height shell-thickness)
					(translate
						[
							0
							(/ (+ plate-height key-sockets-outer-height) 2)
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
			plate-height (/ (- sa-double-length key-sockets-outer-height) 3)
			top-plate
				(->>
					(cube key-sockets-outer-width plate-height shell-thickness)
					(translate
						[
							0
							(/ (+ plate-height key-sockets-outer-height) 2)
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

;; The wall has a bezel. This finds a position for that bezel for a given key.
(defn key-get-bezel-position [column row direction]
	(let
		[
			[key-x-factor key-y-factor] (direction-to-vector direction 0.5)
			[bezel-x-factor bezel-y-factor] (direction-to-vector direction 1)
		]
		(map
			+
			(key-get-position
				column
				row
				[
					;; 0.5 because of centering. It reaches edges.
					(* key-x-factor key-sockets-outer-width)
					(* key-y-factor key-sockets-outer-height)
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

(defn case-upper-lip-inner-offset [direction]
	(let
		[
			[x y] (direction-to-vector direction wall-thickness)
		]
		[x y -1]
	)
)

(defn case-upper-lip-middle-offset [direction]
	(let
		[
			[x y] (direction-to-vector direction wall-xy-offset)
		]
		[x y wall-z-offset]
	)
)

(defn case-upper-lip-outer-offset [direction]
	(let
		[
			[x y] (direction-to-vector direction (+ wall-xy-offset wall-thickness))
		]
		[x y wall-z-offset]
	)
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
				(case-upper-lip-middle-offset direction-1)
				corner-location-1
			)
		)
		(place-function-1
			(translate
				(case-upper-lip-outer-offset direction-1)
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
				(case-upper-lip-middle-offset direction-2)
				corner-location-2
			)
		)
		(place-function-2
			(translate
				(case-upper-lip-outer-offset direction-2)
				corner-location-2
			)
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
					(case-upper-lip-middle-offset direction-1)
					corner-location-1
				)
			)
			(place-function-1
				(translate
					(case-upper-lip-outer-offset direction-1)
					corner-location-1
				)
			)
			(place-function-2
				(translate
					(case-upper-lip-middle-offset direction-2)
					corner-location-2
				)
			)
			(place-function-2
				(translate
					(case-upper-lip-outer-offset direction-2)
					corner-location-2
				)
			)
		)
	)
)

(defn key-wall-brace
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
		(partial shape-place-at-key column-2 row-2) direction-2
		corner-location-2
	)
)

(def right-wall
  (if pinky-15u
    (union
     ; corner between the right wall and back wall
     (if (> first-15u-row 0)
       (key-wall-brace columns-last-index 0 0 1 key-socket-top-right-corner columns-last-index 0 1 0 key-socket-top-right-corner)
       (union (key-wall-brace columns-last-index 0 0 1 key-socket-top-right-corner columns-last-index 0 0 1 wide-post-tr)
              (key-wall-brace columns-last-index 0 0 1 wide-post-tr columns-last-index 0 1 0 wide-post-tr)))
     ; corner between the right wall and front wall
     (if (= last-15u-row extra-cornerrow)
       (union (key-wall-brace columns-last-index extra-cornerrow 0 -1 key-socket-bottom-right-corner columns-last-index extra-cornerrow 0 -1 wide-post-br)
              (key-wall-brace columns-last-index extra-cornerrow 0 -1 wide-post-br columns-last-index extra-cornerrow 1 0 wide-post-br))
       (key-wall-brace columns-last-index extra-cornerrow 0 -1 key-socket-bottom-right-corner columns-last-index extra-cornerrow 1 0 key-socket-bottom-right-corner))

     (if (>= first-15u-row 2)
       (for [y (range 0 (dec first-15u-row))]
         (union (key-wall-brace columns-last-index y 1 0 key-socket-top-right-corner columns-last-index y 1 0 key-socket-bottom-right-corner)
                (key-wall-brace columns-last-index y 1 0 key-socket-bottom-right-corner columns-last-index (inc y) 1 0 key-socket-top-right-corner))))

     (if (>= first-15u-row 1)
       (for [y (range (dec first-15u-row) first-15u-row)] (key-wall-brace columns-last-index y 1 0 key-socket-top-right-corner columns-last-index (inc y) 1 0 wide-post-tr)))

     (for [y (range first-15u-row (inc last-15u-row))] (key-wall-brace columns-last-index y 1 0 wide-post-tr columns-last-index y 1 0 wide-post-br))
     (for [y (range first-15u-row last-15u-row)] (key-wall-brace columns-last-index (inc y) 1 0 wide-post-tr columns-last-index y 1 0 wide-post-br))

     (if (<= last-15u-row (- extra-cornerrow 1))
       (for [y (range last-15u-row (inc last-15u-row))] (key-wall-brace columns-last-index y 1 0 wide-post-br columns-last-index (inc y) 1 0 key-socket-bottom-right-corner)))

     (if (<= last-15u-row (- extra-cornerrow 2))
       (for [y (range (inc last-15u-row) extra-cornerrow)]
         (union (key-wall-brace columns-last-index y 1 0 key-socket-bottom-right-corner columns-last-index (inc y) 1 0 key-socket-top-right-corner)
                (key-wall-brace columns-last-index (inc y) 1 0 key-socket-top-right-corner columns-last-index (inc y) 1 0 key-socket-bottom-right-corner))))
     )
    (union (key-wall-brace columns-last-index 0 0 1 key-socket-top-right-corner columns-last-index 0 1 0 key-socket-top-right-corner)
           (if true
             (union (for [y (range 0 (inc rows-last-index))] (key-wall-brace columns-last-index y 1 0 key-socket-top-right-corner columns-last-index y 1 0 key-socket-bottom-right-corner))
                    (for [y (range 1 (inc rows-last-index))] (key-wall-brace columns-last-index (dec y) 1 0 key-socket-bottom-right-corner columns-last-index y 1 0 key-socket-top-right-corner)))
             (union (for [y (range 0 rows-last-index)] (key-wall-brace columns-last-index y 1 0 key-socket-top-right-corner columns-last-index y 1 0 key-socket-bottom-right-corner))
                    (for [y (range 1 rows-last-index)] (key-wall-brace columns-last-index (dec y) 1 0 key-socket-bottom-right-corner columns-last-index y 1 0 key-socket-top-right-corner)))
             )
           (key-wall-brace columns-last-index extra-cornerrow 0 -1 key-socket-bottom-right-corner columns-last-index extra-cornerrow 1 0 key-socket-bottom-right-corner)
           )))

(def handshake-thumb-offset (if true -0.3 -1.7))
(def handshake-thumb-wall
	(union
		(case-upper-lip-shapes
			handshakethumb-21-place 0 1 key-socket-top-right-corner
			handshakethumb-21-place 0 1 key-socket-top-left-corner
		)
		(case-upper-lip-shapes
			handshakethumb-21-place 0 1 key-socket-top-right-corner
			handshakethumb-20-place 1 0 thumb-post-tr
		)
		(case-upper-lip-shapes
			handshakethumb-20-place 1 0 thumb-post-tr
			handshakethumb-20-place 1 0 key-socket-bottom-right-corner
		)
		(hull-to-ground
			(shape-place-at-key-bezel
				3 -1
				(translate (case-upper-lip-outer-offset -1 0) key-socket-location-dot)
			)
			(shape-place-at-key-bezel
				2 -1
				(translate (case-upper-lip-outer-offset -1 0) key-socket-location-dot)
			)
			(handshakethumb-00-place thumb-post-tl)
			(handshakethumb-01-place key-socket-bottom-left-corner)
		)
		(hull-to-ground
			(shape-place-at-key-bezel
				4 -1
				(translate (case-upper-lip-outer-offset -1 0) key-socket-location-dot)
			)
			(shape-place-at-key-bezel
				3 -1
				(translate (case-upper-lip-outer-offset -1 0) key-socket-location-dot)
			)
			(handshakethumb-00-place thumb-post-tl)
			(handshakethumb-00-place key-socket-bottom-left-corner)
		)
		(hull
			(wall-and-case-upper-lip-shapes
				(partial shape-place-at-key 1 rows-last-index) 0 -1 key-socket-bottom-left-corner
				handshakethumb-00-place 1 -1 key-socket-bottom-left-corner
			)
			(wall-and-case-upper-lip-shapes
				handshakethumb-20-place 1 0 key-socket-bottom-right-corner
				handshakethumb-00-place 1 -1 key-socket-bottom-left-corner
			)
		)
		(hull
			(wall-and-case-upper-lip-shapes
				handshakethumb-21-place 0 1 key-socket-top-left-corner
				handshakethumb-01-place 0 0 key-socket-bottom-left-corner
			)
			(wall-and-case-upper-lip-shapes
				handshakethumb-01-place 0 0 key-socket-bottom-left-corner
				(partial shape-place-at-key 1 2) 0 -1 key-socket-bottom-left-corner
			)
		)
	)
)

(def case-walls
	(union
		;;thumb-wall-type
		right-wall
		; back wall
		(for
			[x (range 0 columns-count)]
			(key-wall-brace
				x 0 :south key-socket-top-left-corner-dot
				x 0 :south key-socket-top-right-corner-dot
			)
		)
		(for
			[x (range 1 columns-count)]
			(key-wall-brace
				x 0 0 1 key-socket-top-left-corner-dot
				(dec x) 0 0 1 key-socket-top-right-corner-dot
			)
		)
		; left wall
		(for
			[y (range 0 rows-last-index)];;(- rows-last-index innercol-offset))]
			(union
				(wall-and-case-upper-lip-shapes
					(partial shape-place-at-key-bezel y 1) -1 0 key-socket-location-dot
					(partial shape-place-at-key-bezel y -1) -1 0 key-socket-location-dot
				)
				(hull
					(key-socket-top-left-corner-dot 0 y)
					(key-socket-bottom-left-corner-dot 0 y)
					(shape-place-at-key-bezel y  1 key-socket-location-dot)
					(shape-place-at-key-bezel y -1 key-socket-location-dot)
				)
			)
		)
		(for
			[y (range 1 rows-last-index)];;(- rows-last-index innercol-offset))]
			(union
				(wall-and-case-upper-lip-shapes
					(partial shape-place-at-key-bezel (dec y) -1) -1 0 key-socket-location-dot
					(partial shape-place-at-key-bezel y  1) -1 0 key-socket-location-dot
				)
				(hull
					(key-socket-top-left-corner-dot 0 y)
					(key-socket-bottom-left-corner-dot 0 (dec y))
					(shape-place-at-key-bezel y 1 key-socket-location-dot)
					(shape-place-at-key-bezel (dec y) -1 key-socket-location-dot)
				)
			)
		)
		(wall-and-case-upper-lip-shapes
			(partial shape-place-at-key 0 0) 0 1 key-socket-top-left-corner
			(partial shape-place-at-key-bezel 0 1)
			(if true -0.6 -0.3)
			(if true 1 1.3)
			key-socket-location-dot
		)
		(wall-and-case-upper-lip-shapes
			(partial shape-place-at-key-bezel 0 1)
			(if true -0.6 -0.3)
			(if true 1 1.3)
			key-socket-location-dot
			(partial shape-place-at-key-bezel 0 1)
			-1
			0
			key-socket-location-dot
		)
		; front wall
		(key-wall-brace
			(+ innercol-offset 3) rows-last-index 0 -1 key-socket-bottom-left-corner
			(+ innercol-offset 3) rows-last-index 0 -1 key-socket-bottom-right-corner
		)
		(key-wall-brace
			(+ innercol-offset 3) rows-last-index 0 -1 key-socket-bottom-right-corner
			(+ innercol-offset 4) extra-cornerrow 0 -1 key-socket-bottom-left-corner
		)
		(for
			[x (range (+ innercol-offset 0) columns-count)]
			(key-wall-brace
				x extra-cornerrow 0 -1 key-socket-bottom-left-corner
				x extra-cornerrow 0 -1 key-socket-bottom-right-corner
			)
		)
		(for
			[x (range (+ innercol-offset 0) columns-count)]
			(key-wall-brace
				x extra-cornerrow 0 -1 key-socket-bottom-left-corner
				(dec x) extra-cornerrow 0 -1 key-socket-bottom-right-corner
			)
		)
	)
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
(def usb-holder-ref (key-get-position 0 0 (map - (case-upper-lip-middle-offset  0  -1) [0 (/ key-sockets-outer-height 2) 0])))
(def usb-holder-position (map + [(+ 18.8 holder-offset) 18.7 1.3] [(first usb-holder-ref) (second usb-holder-ref) 1.8]))
(def usb-holder-space  (translate (map + usb-holder-position [-1.5 (* -1 wall-thickness) 2.1]) (cube 28.666 30 10.4)))
(def usb-holder-notch-l  (translate (map + usb-holder-position [-12 (+ 4.4 notch-offset) 2.1]) (cube 10 1.3 10.4)))
(def usb-holder-notch-r  (translate (map + usb-holder-position [9 (+ (if true 4.4 6.4) notch-offset) 2.1]) (cube 10 1.3 10.4)))

; Screw insert definition & position
(defn screw-insert-shape [bottom-radius top-radius height]
  (union
   (->> (binding [*fn* 30]
                 (cylinder [bottom-radius top-radius] height)))))

(defn screw-insert [column row bottom-radius top-radius height offset]
  (let [shift-right   (= column columns-last-index)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row rows-last-index))
        position      (if shift-up     (key-get-position column row (map + (case-upper-lip-middle-offset  0  1) [0 (/ key-sockets-outer-height 2) 0]))
                        (if shift-down  (key-get-position column row (map - (case-upper-lip-middle-offset  0 -2.5) [0 (/ key-sockets-outer-height 2) 0]))
                          (if shift-left (map + (key-get-bezel-position row 0) (case-upper-lip-outer-offset -1 0))
                            (key-get-position column row (map + (case-upper-lip-middle-offset  1  0) [(/ key-sockets-outer-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)])))))

; Offsets for the screw inserts dependent on true & pinky-15u
(when (and pinky-15u true)
    (def screw-offset-tr [1 7 0])
    (def screw-offset-br [7 14 0]))
(when (and pinky-15u (false? true))
    (def screw-offset-tr [1 7 0])
    (def screw-offset-br [6.5 15.5 0]))
(when (and (false? pinky-15u) true)
    (def screw-offset-tr [-3.5 6.5 0])
    (def screw-offset-br [-3.5 -6.5 0]))
(when (and (false? pinky-15u) (false? true))
    (def screw-offset-tr [-4 6.5 0])
    (def screw-offset-br [-6 13 0]))
    
; Offsets for the screw inserts dependent on thumb-style & true
(when (and (= thumb-style "handshake") true)
    (def screw-offset-bl [9 4 0])
    (def screw-offset-tm [9.5 -4.5 0])
    (def screw-offset-bm [13 -7 0]))
(when (and (= thumb-style "handshake") (false? true))
    (def screw-offset-bl [-3.5 2 0])
    (def screw-offset-tm [9.5 -4.5 0])
    (def screw-offset-bm [13 -7 0]))
(when (and (= thumb-style "manuform") true)
    (def screw-offset-bl [5 -6 0])
    (def screw-offset-tm [9.5 -4.5 0])
    (def screw-offset-bm [8 -1 0]))

         (defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union (screw-insert 0 0         bottom-radius top-radius height [8 10.5 0])
         (screw-insert 0 rows-last-index   bottom-radius top-radius height screw-offset-bl)
         (screw-insert columns-last-index rows-last-index  bottom-radius top-radius height screw-offset-br)
         (screw-insert columns-last-index 0         bottom-radius top-radius height screw-offset-tr)
         (screw-insert (+ 2 innercol-offset) 0         bottom-radius top-radius height screw-offset-tm)
         (screw-insert (+ 1 innercol-offset) rows-last-index         bottom-radius top-radius height screw-offset-bm)))

; Hole Depth Y: 4.4
(def screw-insert-height 6)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.0 2))
(def screw-insert-top-radius (/ 3.9 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

; Wall Thickness W:\t1.65
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))

; Connectors between outer column and right wall when 1.5u keys are used
(def pinky-connectors
  (if pinky-15u
    (apply union
           (concat
            ;; Row connections
            (for [row (range first-15u-row (inc last-15u-row))]
              (hull-triangle-mesh
               (key-socket-top-right-corner-dot columns-last-index row)
               (shape-place-at-key columns-last-index row wide-post-tr)
               (key-socket-bottom-right-corner-dot columns-last-index row)
               (shape-place-at-key columns-last-index row wide-post-br)))
            (if-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
              (hull-triangle-mesh
               (key-socket-top-right-corner-dot columns-last-index (inc row))
               (shape-place-at-key columns-last-index row wide-post-br)
               (key-socket-bottom-right-corner-dot columns-last-index (inc row)))))
            (if-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
              (hull-triangle-mesh
               (key-socket-top-right-corner-dot columns-last-index row)
               (shape-place-at-key columns-last-index (inc row) wide-post-tr)
               (key-socket-bottom-right-corner-dot columns-last-index row))))

            ;; Column connections
            (for [row (range first-15u-row last-15u-row)]
              (hull-triangle-mesh
               (key-socket-bottom-right-corner-dot columns-last-index row)
               (shape-place-at-key columns-last-index row wide-post-br)
               (key-socket-top-right-corner-dot columns-last-index (inc row))
               (shape-place-at-key columns-last-index (inc row) wide-post-tr)))
            (if-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
              (hull-triangle-mesh
               (key-socket-bottom-right-corner-dot columns-last-index row)
               (shape-place-at-key columns-last-index row wide-post-br)
               (key-socket-top-right-corner-dot columns-last-index (inc row)))))
            (if-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
              (hull-triangle-mesh
               (key-socket-bottom-right-corner-dot columns-last-index row)
               (shape-place-at-key columns-last-index (inc row) wide-post-tr)
               (key-socket-top-right-corner-dot columns-last-index (inc row)))))
))))

(def model-right
	(difference
		(union
			key-sockets-all-shapes
			key-sockets-inner
			key-sockets-interconnecting-mesh-shape
			thumb-type
			thumb-connector-type
			(difference
				(union
					case-walls
					;; screw-insert-outers
				)
				usb-holder-space
				usb-holder-notch-l
				usb-holder-notch-r
				;; screw-insert-holes
			)
		)
		(translate [0 0 -20] (cube 350 350 40))
	)
)

(spit "things/right.scad" (write-scad model-right))

(spit "things/left.scad" (write-scad (mirror [-1 0 0] model-right)))

(spit "things/right-test.scad"
	(write-scad
		(union model-right thumbcaps-type caps)
	)
)

(spit "things/right-plate.scad"
	(write-scad
		(extrude-linear
			{:height 2.6 :center false}
			(project
				(difference
					(union
						key-sockets-all-shapes
						key-sockets-inner
						key-sockets-interconnecting-mesh-shape
						thumb-type
						thumb-connector-type
						case-walls
						thumbcaps-fill-type
						caps-fill
						screw-insert-outers
					)
					(translate [0 0 -10] screw-insert-screw-holes)
				)
			)
		)
	)
)

(spit "things/right-plate-laser.scad"
	(write-scad
		(cut
			(translate [0 0 -0.1]
				(difference
					(union case-walls screw-insert-outers)
					(translate [0 0 -10] screw-insert-screw-holes)
				)
			)
		)
	)
)

(defn -main [dum] 1)  ; dummy to make it easier to batch
