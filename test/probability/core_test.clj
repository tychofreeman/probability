(ns probability.core-test
  (:require [clojure.test :refer :all]
            [probability.core :refer :all]
            [clojure.math.numeric-tower :refer [expt]]))

(deftest exercises-pg-43
  (testing "#11 - when four cards are drawn from an ordinary deck of 52 playing cards, show that the probability of getting one card of each suit is 2197/20825"
    (is (= (/ 2197 20825) (/ (expt (choose 13 1) 4) (choose 52 4)))))
  (testing "#13a - What are the probabilities that a lot will pass inspection with 2 defective units?"
    (is (> 0.80  (float (/ (choose 18 4) (choose 20 4))))))
  (testing "#13b - with 3 defective units?"
    (is (> 0.63 (float (/ (choose 17 4) (choose 20 4))))))
  (testing "#15 - If a student places her two business books and 6 other books on a shelf in random order, what are the chances that exactly one book will be between the two biz books?"
    (is (> 1 (/ (* 2 6 (! 5) 6) (! 8)))))
  (testing "#17a - If two awards are randomly distributed among 14 boys and 11 girls, what are the chances that two boys will get both awards"
    (let [both-boys (/ (choose 14 2) (choose 25 2))
          both-girls (/ (choose 11 2) (choose 25 2))
          one-each (/ (* 14 11) (choose 25 2))]
      (is (= 1 (+ both-boys both-girls one-each)))
      (is (> one-each both-boys both-girls))))
  (testing "#18a - a test had 10 questions of 3 multiple-choice answers each; "
    (testing  "if the student chooses randomly, what are the chances that she will answer all questions correctly?"
      (is (not= 1 (float (/ 1 (expt 3 10))))))
    (testing "#18b - ... she will answer all questions incorrectly?"
      (is (not= 1 (float (/ (expt 2 10) (expt 3 10))))))
    (testing "#18c - ... she will answer exactly three questions correctly?"
      (is (not= 1 (/ (* (expt 2 7) (choose 10 3)) (expt 3 10)))))
    (testing "#18d - ... answer 5 correctly and 5 incorrectly"
      (is (not= 0.5 (/ (* (expt 2 5) (choose 10 5)) (expt 3 10)))))
    (testing "#18 - all of those patterns sum to 1"
      (is (= 1 (apply +
                      (/ 1 (expt 3 10)) ;; all right
                      (/ (expt 2 10) (expt 3 10)) ;; all wrong
                      (map (fn [n] (/ (* (expt 2 n) (choose 10 n)) (expt 3 10))) (range 1 10)) ;; all the possibles values between
                      )))
      ;; this should be equivalent - 1/3^10 = (2^0 * 10C0) / 3^10, etc
      (is (= (expt 3 10) (summa [n 0 10] (* (expt 2 n) (choose 10 n)))))))
  (testing "#19 - given a true/false test of 15 questions and a student who randomly chooses each answer, what is the probability that she chooses"
    (testing "#19a - all of the questions correctly?"
      (is (= (expt (/ 1 2) 15) (/ 1 (expt 2 15)))))
    (testing "#19b - exactly 5 questions correctly?"
      (is (not= 1 (/ (choose 15 5) (/ 1 (expt 2 15))))))
    (testing "#19c - exactly 7 questions correctly?"
      (is (not= 1 (/ (choose 15 7) (/ 1 (expt 2 15))))))
    (testing "19d - none of the questions correctly?"
      #_ "this is exactly the same as #19a")
    (is (= (expt 2 15) (summa [n 0 15] (choose 15 n)))))
  (testing "#20 - Assuming that a route of 3 cities is chosen randomly of 20 possible cities, including Buffalo NY. What is the probability that Buffalo is NOT included in the 3-city route?"
    (let [no-buffalo (/ (choose 19 3) (choose 20 3))
          yes-buffalo (/ (choose 19 2) (choose 20 3))]
      (is (< yes-buffalo no-buffalo))
      (is (= 1 (+ no-buffalo yes-buffalo))))) ;; this looks suspiciously like that one identity...
  (testing "#21 - A student has 5 business books and 3 foreign language books. What are the chances that she'll randomly choosing"
    (testing "two business books"
      (let [two-business-books (/ (choose 5 2) (choose 8 2))
            two-language-books (/ (choose 3 2) (choose 8 2))
            one-of-each (/ (* 5 3) (choose 8 2))]
        (is (= 1 (+ two-business-books two-language-books one-of-each)))))))

(deftest exercise-pg-80
  (testing "#1 - A group of officials believe that if they build a new arena and attract a pro team, they will profit by $2,050,000. But if they fail to attract a team, they will loose $500,000. If they don't build a new arena and attract a pro team, they will profit by $1,000,000. But only $100,000 without the team."
    (testing " - b) if odds are 2-1 against, what should they do to maximize profit?"
      (let [expected-profit-when-they-build (+ (* 0.67 -500000) (* 0.33 2050000))
            expected-profit-when-they-dont  (+ (* 0.67 100000) (* 0.33 1000000))]
        (is (<  expected-profit-when-they-build expected-profit-when-they-dont))))
    (testing " - c) if odds are 3-2 against, how should they maximize profit?"
      (let [expected-profit-with-new-arena (+ (* 0.6 -500000) (* 0.4 2050000))
            expected-profit-without-new-arena (+ (* 0.6 100000) (* 0.4 1000000))]
        (is (<  expected-profit-without-new-arena expected-profit-with-new-arena)))))) 

