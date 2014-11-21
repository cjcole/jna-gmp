(defn mpz [x] (new org.cjcole.jnagmp.Mpz x))
(def mpz0 (mpz 0))
(def p (mpz 47))
(def q (.cdivQ (.sub p 1) 2))

; build table of (g^a) mod p
(do
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
