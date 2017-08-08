(ns traffic.core
  "A quick and dirty traffic/speed detector"
  (:require (clojure.data [csv :as csv])
            (clojure.java [io :as io])
            (bigml.api [core :as api]
                       [source :as source]
                       [dataset :as dataset]
                       [anomaly-detector :as anomaly-detector]
                       [anomaly-score :as anomaly-score])
            (com.climate [claypoole :as threads]))
  (:import (org.openimaj.image MBFImage FImage)
           (org.openimaj.math.geometry.shape Rectangle Shape)
           (org.openimaj.video.xuggle XuggleVideo XuggleVideoWriter)
           (org.openimaj.image.typography.general GeneralFont)))

;; Counted the number of pixels for a prius (in the lower lane) to get
;; an approximation of 'inches-per-pixel' so I can calculate vehicle
;; speed
(def prius-pixels 80)
(def prius-inches 179)
(def inches-per-pixel (/ prius-inches prius-pixels))

;; The approximate extra distance travelled for vehicles in the upper
;; lane thanks to the perspective of the webcamera. This was estimated
;; by comparing pixel lengths of similar SUVs across lanes.
(def top-row-multiplier 1.12)

(def video-file "traffic-small.mp4")

;; Expected video size and location of the lanes we're going to
;; observe with anomaly detectors.
(def video-height 112)
(def video-width 384)
(def tile-height 12)
(def tile-width 24)
(def lane-edge [52 15])
(def frame-sample-size 4096)

(def tile-count (* 2 (/ video-width tile-width)))

(def video-miles
  (double (/ (* (- video-width tile-width)
                inches-per-pixel)
             63360)))

(defn- mean [pixels]
  (/ (reduce + pixels) (count pixels)))

(defn- sqr [v] (* v v))

(defn- stats [pixels]
  (let [pixel-count (count pixels)
        mean (/ (reduce + pixels) pixel-count)
        std-dev (-> (reduce + (map #(sqr (- % mean)) pixels))
                    (/ pixel-count)
                    (Math/sqrt))]
    [mean std-dev]))

(defn- pixels [^FImage image x-offset y-offset]
  (let [result (transient [])]
    (dotimes [x tile-width]
      (dotimes [y tile-height]
        (conj! result
               (.getPixelNative image
                                (+ x-offset x)
                                (+ y-offset y)))))
    (persistent! result)))

(defn- calc-row-cells [^MBFImage frame lane-index]
  (let [y (lane-edge lane-index)
        pixel-count (* tile-width tile-height)
        grayscale (.flatten frame)]
    (mapv (fn [i]
            (let [x (* i tile-width)]
              (-> (mapv #(first (stats (pixels % x y))) (seq frame))
                  (into (stats (pixels grayscale x y))))))
          (range (/ (.getWidth frame) tile-width)))))

(defn- calc-cells [frame]
  (vec (apply concat (pmap #(calc-row-cells frame %) [0 1]))))

(defn- video->data [video]
  (println "Transforming video to data...")
  (let [video (if (string? video)
                (XuggleVideo. video)
                video)
        frame-count (.countFrames video)
        sample-ids (set (take frame-sample-size (shuffle (range frame-count))))]
    (println "Total frames:" frame-count)
    (loop [results (vec (repeat tile-count []))
           i 0]
      (when (and (pos? i) (zero? (mod i 500)))
        (println "Processed frames:" i))
      (if (and (.hasNextFrame video) (< i frame-count))
        (recur (if (sample-ids i)
                 (mapv conj results (calc-cells (.getNextFrame video)))
                 results)
               (inc i))
        results))))

(defn- data->detector [data]
  (let [src (api/get-final (source/create data))
        dataset (api/get-final (dataset/create src))
        detector (api/get-final (anomaly-detector/create dataset
                                                         :forest_size 64))
        local-detector (anomaly-score/detector detector)]
    ;; clean up temporary resources and return the local detector
    (dorun (map api/delete [src dataset detector]))
    local-detector))

(defn- data->detectors [data]
  (println "Building detectors...")
  (vec (threads/pmap 8 data->detector data)))

(defn- video->detectors [video]
  (data->detectors (video->data video)))

(defn- init-track [cell-row-count]
  {:track (vec (repeat cell-row-count nil))
   :smoothed-anomalies (vec (repeat cell-row-count 0))
   :last-timestamp 0
   :starts {}
   :trips []})

(defn- pos [v]
  (when (pos? v) v))

(defn- update-track
  [{:keys [starts track trips last-timestamp smoothed-anomalies]}
   anomalies timestamp & [track-id]]
  (let [time-diff (- timestamp last-timestamp)
        ;; Smooth the anomaly detections by giving them a 'cooldown'
        ;; time (80 ms)
        smoothed-anomalies (mapv #(if %2 80 (max (- %1 time-diff) 0))
                                 smoothed-anomalies
                                 anomalies)
        cell-count (/ video-width tile-width)
        last-cell (dec cell-count)
        finished (and (anomalies last-cell) (track (dec last-cell)))

        ;; sweep left on 'clumps' of anomalies
        track (vec (reduce (fn [new-track i]
                             (let [prv (first new-track)
                                   cur (track i)]
                               (conj new-track
                                     (when (or (nil? finished)
                                               (and (not= prv finished)
                                                    (not= cur finished)))
                                       (and (pos (smoothed-anomalies i))
                                            (or prv cur))))))
                           (list nil)
                           (reverse (range (dec cell-count)))))
        new-vehicle (when (and (anomalies 0) (nil? (track 0)))
                      ;; A silly way to do a unique-ish identifier
                      (str "veh-" (rand-int 1000000)))

        ;; sweep right on 'clumps' of anomalies
        track (vec (reduce (fn [new-track i]
                             (let [prv (new-track (dec (count new-track)))
                                   cur (track i)]
                               (conj new-track
                                     (and (pos (smoothed-anomalies i))
                                          (or prv cur)))))
                           (vector (or new-vehicle (track 0)))
                           (range 1 cell-count)))]
    {:starts (cond-> (dissoc starts finished)
               new-vehicle (assoc new-vehicle timestamp))
     :trips (cond-> trips
              finished (conj {:start (starts finished) :end timestamp}))
     :track track
     :smoothed-anomalies smoothed-anomalies
     :last-timestamp timestamp}))

(defn- update-tracks [tracks anomalies timestamp]
  (mapv #(update-track %1 (vec %2) timestamp %3)
        tracks
        (partition (/ (count anomalies) 2) anomalies)
        (range)))

(defn- truncated-speed [{:keys [start end] :as trip}]
  (when trip
    (/ (double (int (* 10 (/ video-miles (/ (- end start) 3600000)))))
       10)))

(defn highlight-video
  "Takes an input video and writes an output video with vehicle
   counts, speeds, and anomaly highlights overlayed."
  [input-file output-file & [detectors]]
  (let [video (XuggleVideo. input-file)
        detectors (or detectors (video->detectors video))
        _ (println "Writing highlighted video...")
        writer (XuggleVideoWriter. output-file
                                   (.getWidth video)
                                   (.getHeight video)
                                   (.getFPS video))
        font (GeneralFont. "Times New Roman" 0)
        cell-row-count (/ video-width tile-width)
        tracks
        (loop [^MBFImage frame (.getNextFrame video)
               tracks (vec (repeat 2 (init-track cell-row-count)))
               i 0]
          (when (and (pos? i) (zero? (mod i 500)))
            (println "Writing frame:" i))
          (let [timestamp (.getTimeStamp video)
                scores (vec (map (fn [detector cell]
                                   (let [score (detector cell true)]
                                     (when (>= score 0.525) score)))
                                 detectors
                                 (calc-cells frame)))
                anomalous-ids (vec (keep-indexed #(when %2 %1) scores))
                new-tracks (update-tracks tracks scores timestamp)
                finishing (mapv #(> (count (:trips (new-tracks %)))
                                    (count (:trips (tracks %))))
                                [0 1])
                last-trip (when (seq (mapcat :trips tracks))
                            (apply max-key :end
                                   (keep-indexed #(when-let [trip (last (:trips %2))]
                                                    (assoc trip :track-id %1))
                                                 tracks)))]
            (doseq [cell anomalous-ids]
              (let [row-index (int (/ cell cell-row-count))
                    col-index (mod cell cell-row-count)
                    ^Shape rect (Rectangle. (* col-index tile-width)
                                            (lane-edge row-index)
                                            tile-width tile-height)]
                (.drawShape ^FImage (.getBand frame 1) rect (float 1))))
            (let [speed (truncated-speed last-trip)
                  speed (if (and speed (pos? (:track-id last-trip)))
                          (* top-row-multiplier speed)
                          speed)]
              (.drawText frame
                         (str "Vehicles: " (count (mapcat :trips new-tracks))
                              "   Last Speed: "
                              (if speed
                                (/ (double (int (* 10 speed))) 10)
                                "--"))
                         20
                         (- video-height 8)
                         font
                         28))
            (.addFrame writer frame)
            (if (.hasNextFrame video)
              (recur (.getNextFrame video)
                     new-tracks
                     (inc i))
              new-tracks)))]
    (.close writer)
    tracks))
