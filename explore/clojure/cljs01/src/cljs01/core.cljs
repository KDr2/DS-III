(ns cljs01.core
  (:require
   [cljs.math :as math]
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]))

;; utils ----

(defn app-center [app]
  [(-> app (.-screen) (.-width) (#(/ % 2)))
   (-> app (.-screen) (.-height) (#(/ % 2)))])

(defn show-on [app obj]
  (-> app (.-stage) (.addChild obj)))

(def init-opt (clj->js {:background "#1099bb" :resizeTo js/window
                        :antialias true :autoDensity true
                        :resolution js/window.devicePixelRatio}))

;; bunny ----
(defn get-bunny [texture x y]
  (let [bunny (new js/PIXI.Sprite texture)]
    (-> bunny (.-anchor) (.set 0.5))
    (set! (.-x bunny) x)
    (set! (.-y bunny) y)
    bunny))

(defn simple-rotator [obj rate]
  (fn [tick]
    (set! (.-rotation obj)
          (+ (.-rotation obj) (* rate (.-deltaTime tick))))))

;; rectangles ----

(defn line-rotator [l1 l2 l3 l4 rate]
  (fn [tick]
    (let [d (* rate (.-lastTime tick))
          p-4 (/ math/PI 4)
          r (-> (math/sin d) (* p-4) (- p-4))]
      ;; (.log js/console d r)
      (set! (.-rotation l1) r)
      (set! (.-rotation l2) (- r))
      (set! (.-rotation l3) (- r))
      (set! (.-rotation l4) r))))

(defn -new-rect [px py w h]
  (doto (new js/PIXI.Graphics) (.rect px py w h)
        (.fill "#996699") (.stroke 1)))

(defn -new-border []
  (doto (new js/PIXI.Graphics) (.moveTo 0 0)
        (.lineTo 0 100) (.stroke (clj->js {:width 2 :color 0xfeeb77}))))

(defn rectangles [app]
  (let [big (doto (new js/PIXI.Graphics) (.rect 100 100 240 200)
                  (.stroke (clj->js {:width 2 :color 0xfeeb77})))
        s1 (-new-rect 100 100 100 100)
        s2 (-new-rect 100 200 100 100)
        s3 (-new-rect 240 100 100 100)
        s4 (-new-rect 240 200 100 100)
        l1 (-new-border)
        l2 (-new-border)
        l3 (-new-border)
        l4 (-new-border)]
    (doall (for [e [big s1 s2 s3 s4 l1 l2 l3 l4]]
             (show-on app e)))
    (.set (.-pivot l1) 0 100)
    (.set (.-position l1) 200 300)
    (.set (.-pivot l2) 0 100)
    (.set (.-position l2) 240 300)
    (.set (.-pivot l3) 0 0)
    (.set (.-position l3) 200 100)
    (.set (.-pivot l4) 0 0)
    (.set (.-position l4) 240 100)
    (-> app (.-ticker) (.add (line-rotator l1 l2 l3 l4 0.0015)))))

;; entry ----
(defn -main
  [& args]
  (go
    (let [app (new js/PIXI.Application)
          _ (<p! (.init app init-opt))
          texture (<p! (js/PIXI.Assets.load "https://pixijs.com/assets/bunny.png"))
          bunny (apply get-bunny texture (app-center app))]
      (.appendChild js/document.body (.-canvas app))
      (show-on app bunny)
      (-> app (.-ticker) (.add (simple-rotator bunny 0.1)))
      (rectangles app))))

(-main)

(println "CLJS01 loaded!!!")
(.log js/console 123)
