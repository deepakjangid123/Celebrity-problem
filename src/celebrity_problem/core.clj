(ns celebrity-problem.core
  (:gen-class))

(defonce who-knows-who {0 [false true false true true false]
                        1 [true false true true false true]
                        2 [false false false true false false]
                        3 [false false false false false false]
                        4 [true true true true false true]
                        5 [false true true true true false]})

(defonce celeb-not-found-msg
  "What you are looking for, is not here!")

(defonce celeb-found-msg
  "You have got your celebrity and the index is: ")

(defn knows?
  "Tells whether 'a' knows 'b' or not"
  [a b]
  (get (second a) (first b)))

(defn lookup-in-matrix
  "look in the matrix for celebrity entry and returns a msg if he/she
  is the celebrity or not"
  [celeb]
  (if (every? false? (second celeb))
    (str celeb-found-msg (first celeb))
    celeb-not-found-msg))

(defn celebrity
  "Returns celebrity(if there any), else returns a message
  celebrity not found"
  []
  (loop [info who-knows-who]
    (condp = (count info)
      0 celeb-not-found-msg
      1 (lookup-in-matrix (first info))
      (let [[a b] (seq info)]
        (cond
          ;; If a,b knows each other or they both don't know each other,
          ;; then drop both, both can't be celebrity
          (or
            (and (knows? a b) (knows? b a))
            (not (or (knows? a b) (knows? b a)))) (recur (drop 2 info))
          ;; If a knows b, but b doesn't know a then drop a
          (knows? a b) (recur (drop 1 info))
          ;; If b knows a, but a doesn't know b then drop b
          (knows? b a) (recur (apply vector a (drop 2 info))))))))

(celebrity)
