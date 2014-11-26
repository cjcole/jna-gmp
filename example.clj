; (load-file "../jna-gmp/example.clj")

(defn mpz [x] (new org.cjcole.jnagmp.Mpz x))

(def np 40)

; (def p (mpz 47))
; (def g (mpz 3)) ; free choice: call "prtable" to see possible values

; 'p' from https://www.ietf.org/rfc/rfc3526.txt
(def p (new org.cjcole.jnagmp.Mpz "FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AACAA68FFFFFFFFFFFFFFFF" 16))
; (println "p =" p)
(def q (.cdivQ (.sub p 1) 2))
; (println "q =" q)
(def g (mpz 2)) ; free choice: call "prtable" to see possible values
; (println "g =" g)

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

(defn vec-of [np from]
  (map mpz (map #(+ from %) (range 0 np))))
(defn sum-of [vec]
  (.mod (r/fold mpz+ vec) p))

(def xvec (vec-of np 1))
; (println "xvec =" xvec)
(def x (sum-of xvec))
; (println "x =" x)
(def y (.powm g x p)) ; a.k.a. "h"
; (println "y =" y)

(def rvec (vec-of np 2))
; (println "rvec =" rvec)
(def r (sum-of rvec))
; (println "r =" r)

(def mvec (vec-of np 3))
; (println "mvec =" mvec)
(def m (sum-of mvec))
; (println "m =" m)

(def max-group-index (.mod (.mul (mpz 52) np) p))
(def group-index-map
  (loop [i (mpz 0) gim {}]
    (if (= i max-group-index)
      gim
      (recur (.add i 1) (assoc gim (.powm g i p) i)))))
(defn group-elem [m] (.powm g m p))
(defn group-index [gm] (group-index-map gm))

(defn encr [mvec & {:keys [rvec] :or {rvec rvec}}]
  (defn encr-one [mi ri]
    (let [a (.powm g ri p) 
          b (.mod (.mul (group-elem mi) (.powm y ri p)) p)
          result [a b]]
      result))
  (defn encr-zipped [m+rvec]
    (let [mi (nth m+rvec 0) ri (nth m+rvec 1)]
      (encr-one mi ri)))
  (r/fold encr* (map encr-zipped (map vector mvec rvec))))
(defn decr [e] (group-index (.mod (.mul (second e) (.powm (first e) (.neg x) p)) p)))

(defn logs-eq? [params xvec & {:keys [rvec] :or {rvec rvec}}]
  (let [g1 (nth params 0)
        h1 (nth params 1)
        g2 (nth params 2)
        h2 (nth params 3)
        c (mpz 21)] ; random from Z*q (blind joint choice)
;    (println "------------------------")
;    (println "g1/g =" g1)
;    (println "h1/y =" h1)
;    (println "g2/ma =" g2)
;    (println "h2/max =" h2)
;    (println "c =" c)
    (defn joint-logs-eq? 
      ([params xvec]
        (defn accum
          ([] [(mpz 1) (mpz 1) (mpz 0)])
          ([a b] [(.mod (.mul (nth a 0) (nth b 0)) p) 
                  (.mod (.mul (nth a 1) (nth b 1)) p)
                  (.mod (.add (nth a 2) (nth b 2)) q)]))
        (defn joint-logs-eq?-helper [x+rvec]
          (let [xi (nth x+rvec 0) ri (nth x+rvec 1)]
            (joint-logs-eq? params xi ri)))
         (let [result (r/fold accum (map joint-logs-eq?-helper (map vector xvec rvec)))]
           result))
      ([params xi ri]
          (let [a (.powm g1 ri p)
                b (.powm g2 ri p)
                y (.mod (.add (.mul c xi) ri) q)]
;            (println "--------")
;            (println "xi =" xi)
;            (println "ri =" ri)
;            (println "a =" a)
;            (println "b =" b)
;            (println "y =" y)
            [a b y])))
  (let [result (joint-logs-eq? params xvec)
        a (nth result 0)
        b (nth result 1)
        y (nth result 2)
        p1 (.powm g1 y p)
        p2 (.mod (.mul (.powm h1 c p) a) p)
        q1 (.powm g2 y p)
        q2 (.mod (.mul (.powm h2 c p) b) p)]
    (let [result (and (= p1 p2) (= q1 q2))]
;      (println "-------------")
;      (println "a =" a)
;      (println "b =" b)
;      (println "y =" y)
;      (println "p1 =" p1)
;      (println "p2 =" p2)
;      (println "q1 =" q1)
;      (println "q2 =" q2)
      result))))

(defn oblivious-eq? [e1 e2]
  (let [a (mpz 8) ; random from Z*q (blind joint choice)
        m (.mod (.mul (first e1) (.invert (first e2) p)) p)
        s (.mod (.mul (second e1) (.invert (second e2) p)) p)
        sa (.powm s a p) ; s^a
        ma (.powm m a p)] ; m^a
    (defn make-max [xi]
      (.powm m (.mul a xi) p)) ; m^(a*x)
    (let [max (r/fold mpz* (map make-max xvec))]
;      (println "--------------------------------")
;      (println "m =" m)
;      (println "s =" s)
;      (println "sa =" sa)
;      (println "ma =" ma)
;      (println "max =" max)
      (and
        (= sa max)
        (logs-eq? [m ma s sa] [a]) ; log(m)(m^a) = log(s)(s^a)
        (logs-eq? [g y ma max] xvec))))) ; log(g)(y) = log(m^a)(m^(a*x))

;(println ":::::::::::::::")
(def ebase (encr mvec))
;(println "ebase =" + ebase)
(def esame (encr mvec :rvec (map mpz+1 rvec)))
;(println "esame =" + esame)
(def ediff (encr (map mpz+1 mvec)))
;(println "ediff =" + ediff)

(defn check-encr-decr []
  (dotimes [r (.toInt q)]
    (dotimes [in (.toInt q)]
      (let [out (.toInt (decr (encr [(mpz in)] :rvec [(mpz r)])))]
        (if (not (= in out))
          (println (str (format "in=%2d, out=%2d, r=%2d" in out r))))))))

(defn check-oblivious-eq? []
  (dotimes [r1 (.toInt q)]
    (dotimes [r2 0] ; to "q" instead of to 0
      (dotimes [m1 (.toInt q)]
        (dotimes [m2 (.toInt q)]
          (let [e1 (encr [(mpz m1)] :rvec [(mpz r1)])
                e2 (encr [(mpz m2)] :rvec [(mpz r2)])]
            (if (or
                  (and (= m1 m2) (not (oblivious-eq? e1 e2)))
                  (and (not (= m1 m2)) (oblivious-eq? e1 e2)))
              (println (str (format "%2d %2d %s" m1 m2
                (if (oblivious-eq? e1 e2) "true" "false")))))))))))

(println "should be true: " (oblivious-eq? ebase ebase))
(println "should be true: " (oblivious-eq? ebase esame))
(println "should be false: " (oblivious-eq? ebase ediff))
(println "should be true: " (oblivious-eq? esame ebase))
(println "should be true: " (oblivious-eq? esame esame))
(println "should be false: " (oblivious-eq? esame ediff))
(println "should be false: " (oblivious-eq? ediff ebase))
(println "should be false: " (oblivious-eq? ediff esame))
(println "should be true: " (oblivious-eq? ediff ediff))
