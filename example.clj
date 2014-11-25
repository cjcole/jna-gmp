(defn mpz [x] (new org.cjcole.jnagmp.Mpz x))

(def np 2)
(def p (mpz 47))
(def q (.cdivQ (.sub p 1) 2))
(def g (mpz 3)) ; call "prtable" to find possible values

(def mpz0 (mpz 0))
(def mpz+1 (fn [x] (.add x (mpz 1))))
(defn mpz+ 
  ([] (mpz 0))
  ([a b] (.mod (.add a b) p)))
(defn mpz*
  ([] (mpz 1))
  ([a b] (.mod (.mul a b) p)))
(defn encr*
  ([] [(mpz 1) (mpz 1)])
  ([a b] [(.mod (.mul (first a) (first b)) p) (.mod (.mul (second a) (second b)) p)]))

(require '[clojure.core.reducers :as r])

(defn prgroup []
  (dotimes [i (.toInt q)] (println "i =" i ", g^i =" (.powm g (mpz i) p))))

(defn prtable []
  (print "     ")
  (loop [a mpz0]
    (do
      (print (str (format "%2d" (.toInt a)) (if (= a (.sub p 1)) "\n" " ")))
      (if (not (= a (.sub p 1))) (recur (.add a 1)))))
  (print "   +")
  (loop [a mpz0]
    (do
      (print (str (format "---" a) (if (= a (.sub p 1)) "\n" "")))
      (if (not (= a (.sub p 1))) (recur (.add a 1)))))
  (loop [g mpz0]
    (do
      (print (format "%2d | " (.toInt g)))
      (loop [a mpz0]
        (do
          (print (str (format "%2d" (.toInt (.powm g a p))) (if (= a (.sub p 1)) "\n" " ")))
          (if (not (= a (.sub p 1))) (recur (.add a 1)))))
      (if (not (= g (.sub p 1))) (recur (.add g 1))))))
(def xvec
  (loop [i 0 xvec []]
    (let [x (mpz (+ 13 i))] ; random from Z*q
      (if (= i np)
        xvec
        (recur (+ i 1) (conj xvec x))))))
(def rands
  (loop [i 0 rands []]
    (let [rand (mpz (+ 3 i))] ; random from Z*q
      (if (= i np)
        rands
        (recur (+ i 1) (conj rands rand))))))
(def mvec
  (loop [i 0 mvec []]
    (if (= i np)
      mvec
      (recur (+ i 1) (conj mvec (.add (mpz 1) (mpz i)))))))
(def x (.mod (r/fold mpz+ xvec) p))
(def y (.powm g x p)) ; a.k.a. "h"

(def group-index-map
  (loop [i (mpz 0) gim {}]
    (if (= i q)
      gim
      (recur (.add i 1) (assoc gim (.powm g i p) i)))))
(defn group-elem [m] (.powm g m p))
(defn group-index [gm] (group-index-map gm))

(defn encr 
  ([mvec] 
    (defn encr-helper [vec]
      (let [m (nth vec 0) r (nth vec 1)]
        (encr m r)))
     (r/fold encr* (map encr-helper (map vector mvec rands))))
  ([m r] 
    (let [result [(.powm g r p) (.mod (.mul (group-elem m) (.powm y r p)) p)]]
;      (println "m =" m ", y =" y ", r =" r ", result =" result)
;      (println "  gm =" (group-elem m) ", (.powm y r p) =" (.powm y r p))
      result)))
(defn decr [e] (group-index (.mod (.mul (second e) (.powm (first e) (.neg x) p)) p)))

; (def rands [(mpz 2) (mpz 4) (mpz 6)])
; (def mvec [(mpz 1) (mpz 1) (mpz 1)])
; (decr (encr mvec))

(defn logs-eq? [params exp]
  (let [g1 (nth params 0)
        h1 (nth params 1)
        g2 (nth params 2)
        h2 (nth params 3)
        r (mpz 22) ; random from Z*q
        c (mpz 21)] ; random from Z*q
    (let [a (.powm g1 r p)
          b (.powm g2 r p)
          y (.mod (.add (.mul c exp) r) q)]
      (let [p1 (.powm g1 y p)
            p2 (.mod (.mul (.powm h1 c p) a) p)
            q1 (.powm g2 y p)
            q2 (.mod (.mul (.powm h2 c p) b) p)]
        (and (= p1 p2) (= q1 q2))))))

(defn joint-oblivious-eq? [e1 e2]
  (let [a (mpz 8) ; random from Z*q
        m (.mod (.mul (first e1) (.invert (first e2) p)) p)
        s (.mod (.mul (second e1) (.invert (second e2) p)) p)
        sa (.powm s a p) ; s^a
        ma (.powm m a p)] ; m^a
    (defn make-max [xi]
      (.powm m (.mul a xi) p)) ; m^(a*x)
    (let [max (r/fold mpz* (map make-max xvec))]
      (println sa)
      (println max)
      (println ma)
      (= sa max)))) ; check logs

(defn oblivious-eq? [e1 e2]
  (let [
        a (mpz 8) ; random from Z*q
        m (.mod (.mul (first e1) (.invert (first e2) p)) p)
        s (.mod (.mul (second e1) (.invert (second e2) p)) p)
        sa (.powm s a p)
        max (.powm m (.mul a x) p) ;; TODO: use of "x"
        ma (.powm m a p)
       ]
    (and
      (= sa max)
      (logs-eq? [m ma s sa] a)
      (logs-eq? [g y ma max] x)))) ;; TODO: use of "x"

(def e (encr mvec))
(def esame (encr mvec))
(def ediff (encr mvec))
(def ediff (encr (map mpz+1 mvec)))

(oblivious-eq? e esame)
(oblivious-eq? e ediff)

(dotimes [r (.toInt q)]
  (dotimes [in (.toInt q)]
    (let [out (.toInt (decr (encr (mpz in) (mpz r))))]
      (if (not (= in out))
        (println (str (format "in=%2d, out=%2d, r=%2d" in out r)))))))

(dotimes [r1 (.toInt q)]
  ; (println r1)
  (dotimes [r2 0] ; (.toInt q)]
    (dotimes [m1 (.toInt q)] 
      (dotimes [m2 (.toInt q)]
        (let [
              e1 (encr (mpz m1) (mpz r1))
              e2 (encr (mpz m2) (mpz r2))
             ]
          (if (or
                (and (= m1 m2) (not (oblivious-eq? e1 e2)))
                (and (not (= m1 m2)) (oblivious-eq? e1 e2)))
            (println (str (format "%2d %2d %s" m1 m2
              (if (oblivious-eq? e1 e2) "true" "false"))))))))))
 