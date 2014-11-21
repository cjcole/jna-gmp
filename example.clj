(require '[clojure.core.reducers :as r])
(defn mpz [x] (new org.cjcole.jnagmp.Mpz x))
(defn mpz+ 
  ([] (mpz 0))
  ([a b] (.add a b)))
(def mpz0 (mpz 0))
(def np 2)
(def p (mpz 47))
(def q (.cdivQ (.sub p 1) 2))
(def g (mpz 25))
(def xvec
  (loop [i 0 xvec []]
    (let [x (mpz (+ 13 i))] ; random from Z*q
      (if (= i np)
        xvec
        (recur (+ i 1) (conj xvec x))))))
;; TODO: remove x, use xvec
(def x (.mod (r/fold mpz+ xvec) p))
(def yvec
  (loop [i 0 yvec []]
    (if (= i np)
      yvec
      (recur (+ i 1) (conj yvec (.powm g (nth xvec i) p))))))
;; TODO: remove y, use yvec
(def y (.powm g x p)) ; a.k.a. "h"

(defn encr [m r] [(.powm g r p) (.mod (.mul m (.powm y r p)) p)])
(defn decr [e] (.mod (.mul (second e) (.powm (first e) (.neg x) p)) p))

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

(defn oblivious-eq? [e1 e2]
  (let [
        a (mpz 8) ; random from Z*q
        m (.mod (.mul (first e1) (.invert (first e2) p)) p)
        s (.mod (.mul (second e1) (.invert (second e2) p)) p)
        sa (.powm s a p)
        max (.powm m (.mul a x) p)
        ma (.powm m a p)
       ]
    (and
      (= sa max)
      (logs-eq? [m ma s sa] a)
      (logs-eq? [g y ma max] x))))

(def r0 (mpz 14)) ; random from Z*q
(def r1 (mpz 17)) ; random from Z*q
(def r2 (mpz 18)) ; random from Z*q

(def m1 (mpz 1))
(def m2 (mpz 2))
(def e (encr m1 r0))
(def esame (encr m1 r1))
(def ediff (encr m2 r2))
(oblivious-eq? e esame)
(oblivious-eq? e ediff)

(dotimes [n1 (.toInt p)]
  (let [n2 (decr (encr (mpz n1) r1))]
    (if (not (= (mpz n1) n2))
      (println (str (format "%2d %2d" n1 (.toInt n2)))))))

(dotimes [n1 (.toInt q)] 
  (dotimes [n2 (.toInt q)]
    (let [
          e1 (encr (mpz (.powm g (mpz n1) p)) r1)
          e2 (encr (mpz (.powm g (mpz n2) p)) r2)
         ]
      (if (or
            (and (= n1 n2) (not (oblivious-eq? e1 e2)))
            (and (not (= n1 n2)) (oblivious-eq? e1 e2)))
        (println (str (format "%2d %2d %s" n1 n2
          (if (oblivious-eq? e1 e2) "true" "false"))))))))
