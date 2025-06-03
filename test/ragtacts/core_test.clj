(ns ragtacts.core-test
  (:require [clojure.test :refer :all]
            [ragtacts.core :refer :all]
            [wkok.openai-clojure.api :as openai]))

(defn ask-mock [{:keys [model messages tools as]} opts]
  {:created 1677664795
   :id "chatcmpl-7QyqpwdfhqwajicIEznoc6Q47XAyW"
   :model model
   :object "chat.completion"
   :usage {:completion_tokens 1
           :prompt_tokens 1
           :total_tokens 2}
   :choices
   [{:finish_reason "stop"
     :index 0
     :logprobs nil
     :message
     (if tools
       {:role "assistant"
        :content nil
        :tool_calls
        (map (fn [tool]
               {:id "call_UCSWR9vFQb5HJIJl9TaY1QN1"
                :type "function"
                :function {:name (-> tool :function :name)
                           :arguments "{\"x\": 1}"}}) tools)}
       {:role "assistant"
        :content "Hello!"})}]})

(defn ^{:desc "Test tool"} test-tool [^{:type "number" :desc "Test arg"} x]
  "test tool")

(deftest ask-test
  (with-redefs [openai/create-chat-completion ask-mock]
    (testing "Simple ask test"
      (let [msgs (ask "Hello!")]
        (is (= {:user "Hello!"} (first msgs)))
        (is (string? (:ai (second msgs))))))

    (testing "Continued ask test"
      (let [msgs (-> (ask "Hello!")
                     (conj "How are you?")
                     ask)]
        (is (= [:user :ai :user :ai] (map ffirst msgs)))
        (is (string? (:ai (last msgs))))))

    (testing "Ask with messages"
      (let [msgs (ask [{:user "Hello!"}
                       {:ai "How are you?"}
                       {:user "My name is Ragtacts."}])]
        (is (= [:user :ai :user :ai] (map ffirst msgs)))
        (is (string? (:ai (second msgs))))))

    (testing "Ask with tools"
      (let [msgs (ask "Hello!" {:tools [#'test-tool]})]
        (is (string? (:ai (second msgs)))))

      (let [msgs (ask "Hello!" {:tools [#'test-tool] :as :values})]
        (is (= [{:test-tool "test tool"}] (:ai (second msgs))))))))

(deftest prompt-test
  (testing "Simple prompt test"
    (let [prompt (prompt "Hello {name}!" {:name "World"})]
      (is (= "Hello World!" prompt)))))


(deftest vector-store-test
  (testing "Simple search test"
    (let [db (vector-store)]
      (add db ["A" "B"])
      (is (= ["A" "B"] (search db "A")))))

  (testing "Search with metadata"
    (let [db (vector-store)]
      (add db [{:text "A" :metadata {:animal "dog"}}
               {:text "B" :metadata {:animal "cat"}}])
      (is (= ["A"] (search db "A" {:metadata {:animal "dog"}})))))

  (testing "Seach multiple vector stores"
    (let [db1 (vector-store)
          db2 (vector-store)]
      (add db1 ["A" "B"])
      (add db2 ["A" "B"])
      (is (= ["A" "A" "B" "B"] (search [db1 db2] "A"))))))
