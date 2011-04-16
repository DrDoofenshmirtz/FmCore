(ns fm.canvas
  (:import
    (java.awt
      Graphics2D BasicStroke Color GradientPaint
      RenderingHints BorderLayout)
    (javax.swing SwingUtilities JComponent JFrame)
    (java.awt.geom Arc2D Arc2D$Double)))

(defmacro do-in-gui-thread [& forms]
  `(SwingUtilities/invokeLater (fn [] ~@forms)))

(defn canvas [painter]
  (proxy
    [JComponent]
    []
    (paintComponent [^Graphics2D graphics]
      (proxy-super paintComponent graphics)
      (let [width (.getWidth this) height (.getHeight this)]
        (doto graphics
          (.setColor Color/WHITE)
          (.fillRect 0 0 width height))
        (and painter (painter graphics width height))))))

(defn open-canvas [width height painter]
  (do-in-gui-thread
    (doto (JFrame. "Canvas")
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (-> .getContentPane (.add (canvas painter) BorderLayout/CENTER))
      (.setSize width height)
      (.setLocationRelativeTo nil)
      (.setVisible true))))

(defn point-on-circle [radius angle-deg]
  (let [angle-rad (Math/toRadians angle-deg)]
    [(int (* (Math/cos angle-rad) radius))
     (int (* (Math/sin angle-rad) radius))]))

(defn paint-circle [^Graphics2D graphics width height]
  (let [center-x (int (/ width 2)) center-y (int (/ height 2))]
    (doto graphics
      (.setRenderingHint
        RenderingHints/KEY_ANTIALIASING
        RenderingHints/VALUE_ANTIALIAS_ON)
      (.setColor Color/BLUE))
    (doseq [point (map #(point-on-circle (int 200) (double %)) (range 0 361 0.25))]
      (.drawLine
        graphics
        center-x center-y
        (unchecked-add center-x (int (point 0))) (unchecked-add center-y (int (point 1)))))))

(defn paint-arc [^Graphics2D graphics width height]
  (doto graphics
    (.setRenderingHint
      RenderingHints/KEY_ANTIALIASING
      RenderingHints/VALUE_ANTIALIAS_ON)
    (.setPaint (GradientPaint. 0 0 Color/GREEN width height Color/RED))
    (.setStroke (BasicStroke. (float 50) BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))
    (.draw (Arc2D$Double. 0 25 width height 30 120 Arc2D/OPEN))))

(open-canvas 600 600 paint-arc)
