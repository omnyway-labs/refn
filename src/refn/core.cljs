(ns refn.core
  (:require
   [reagent.core :as r]
   [re-frame.core :as rf]))

(defn qualified-name [kw]
  (if (qualified-keyword? kw)
    (str (namespace kw) "/" (name kw))
    (name kw)))

(defn- fx-id! [fx-id]
  (keyword (str (qualified-name fx-id) "!")))

(defn- as-fx! [{:keys [event]} [_ param]]
  (let [fx-id          (first event)
        side-effect-fx (fx-id! fx-id)]
    {side-effect-fx param}))

(defn assoc-db
  "Used as the body of `reg-event-db` to DRY out assignment of a value to an app-db key  
  `fx-id` - key for the app-db to assign the value.  
  The value is passed in as the `param` of the `reg-event-db` so is not explicitly an argument here."
  [fx-id]
  (fn [db [_ param]]
    (assoc db fx-id param)))

(defn assoc-in-db
  "Used as the body of `reg-event-db` to DRY out assignment of a value to an app-db key sequence path  
  `ks` - key sequence for the path into the app-db to assign the value.  
  The value is passed in as the `param` of the `reg-event-db` so is not explicitly an argument here."
  [ks]
  (fn [db [_ param]]
    (assoc-in db ks param)))

(def >>! rf/dispatch-sync)

(defn >>
  "Sweet Syntactic Sugar alternative to `rf/dispatch`
  Handles both case of just a keyword and a keyword and data just like `rf/dispatch`"
  ([form]
   (if (keyword? form)
     (partial >> form)
     (rf/dispatch form)))
  ([fx data]
   (rf/dispatch [fx data])))

(defn <<
  "Syntactic sugar for a always derefed `rf/subscribe`  
  - With one argument:  
    With just a `sub` argument `<<` acts just like `@(rf/subscribe sub)`
    One difference is `sub` can be just a keyword or a vector like a normal `rf/subscribe` would take)  
    Examples: `(<< :foobar)` or `(<< [:foobar])`  
  - With two arguments:  
    The first argument would be the same as `sub` for a single argument. Can be a keyword or a vector  
    The second argument `dispatcher` will be dispatched with `rf/dispatch` before the `(<< sub)` is called"
  ([sub]
   (let [f (comp deref rf/subscribe)]
     (if (keyword? sub)
       (f [sub])
       (f sub))))
  ([sub dispatcher]
   (rf/dispatch dispatcher)
   (<< sub)))

(defn |>
  "If `callback` is a keyword, will `rf/dispatch` as `[callback data]`
  Otherwise calls `(callback data)` as a regular function"
  [callback data]
  (if (keyword? callback)
    (>> [callback data])
    (callback data)))

(defn dissoc-in
  "Helper function for removing elements of a value in a map (usually app-db)  
  TODO: Explain this better  
  Example:
  ```clojure
  (dissoc-in {:a [{:x 1 :y 2}] :b [{:u 1 :v 2}]} :a :x 1)
  {:a (), :b [{:u 1, :v 2}]}
  ```"
  [m entity key value]
  (->> (get m entity)
       (remove #(= (get % key) value))
       (assoc m entity)))

(comment
  (conj-in {:a [{:x 1 :y 2}] :b [{:u 1 :v 2}]} :b {:a 1 :b 2})
  => {:a [{:x 1 :y 2}] :b [{:u 1 :v 2} {:a 1 :b 2}]})

(defn conj-in
  "Helper function for adding elements to a value in a map (usually app-db)  
  TODO: Explain this better  
  Example:
  ```clojure
  (conj-in {:a [{:x 1 :y 2}] :b [{:u 1 :v 2}]} :b {:a 1 :b 2})
  => {:a [{:x 1 :y 2}] :b [{:u 1 :v 2} {:a 1 :b 2}]}
  ```"
  ([m key value]
   (let [xs (get m key)]
     (assoc m key (conj xs value))))
  ([m key value uniq-key uniq-value]
   (let [xs (->> (get m key)
                 (remove #(= (get % uniq-key) uniq-value)))]
     (assoc m key (conj xs value)))))


;; resultset

(rf/reg-event-db
 :set-create-resultset
 (fn [db [_ entity data]]
   (let [{:keys [resultset]} db]
     (assoc db :resultset
            (conj-in resultset entity data)))))

(rf/reg-event-db
 :set-update-resultset
 (fn [db [_ entity data]]
   (let [{:keys [resultset]} db]
     (assoc db :resultset
            (conj-in resultset entity data :id (:id data))))))

(rf/reg-event-db
 :set-delete-resultset
 (fn [db [_ entity {:keys [id] :as data}]]
   (let [{:keys [resultset]} db]
     (assoc db :resultset
            (dissoc-in resultset entity :id id)))))

(rf/reg-event-db
 :set-resultset
 (fn [db [_ entity data]]
   (assoc-in db [:resultset entity] data)))

(defn- dispatch-with-param [fx entity param]
  (>> [fx ]))

(defn refn
  "Manages the state of the `:resultset` value in app-db.  
  Returns a `rf/reg-event-db` function that can be used as a callback-fn.  
  Makes it easy for `reg-event-fx` functions to sync remote db updates with an app-db cache of that data.
  Helps with list pagination of bulk data as well. See README for examples.  
  `op` - Operation one of `:create :update :delete :list` defaults to `list`  
  `entity` - Top level key inside `:resultset` in app-db"
  [op entity]
  (let [dispatch (fn [f p] (>> [f entity p]))
        fx (condp = op
             :create :set-create-resultset
             :update :set-update-resultset
             :delete :set-delete-resultset
             :list   :set-resultset
             :set-resultset)]
    (partial dispatch fx)))

;; subscriptions
(rf/reg-sub
 :resultset
 (fn [db param]
   (let [[_ entity filter-fn] param]
     (if filter-fn
       (->> (get-in db [:resultset entity])
            (filter filter-fn))
       (get-in db [:resultset entity])))))

(rf/reg-sub :lookup (fn [db _] db))

(def default {})

(rf/reg-event-db
 :initialize-db
 (fn [_ _] default))

(rf/reg-event-db
 :set-defaults
 (fn [db [_ default]]
   (merge db default)))

(defn defx [fx-id fn & {:keys [callback cache?
                               cache-key]}]
  (rf/reg-event-fx fx-id as-fx!)
  (rf/reg-fx (fx-id! fx-id) fn))

(defn defsub
  "Syntactic sugar for `rf/reg-sub`
  Will convert a namespaced `sub-id` to a plain keyword for the accessor for single arity  
  With two arguments:  
  - If the second argument `accessor` is a keyword,
      it is used as key to retrieve from the app-db  
  - If the second argument `accessor` is a function,
      it is used as the function for the `reg-sub` with the arguments being the app-db and the param passed in implicitly  
  - If the second argument is a vector, the vector will be passed in as the accessor path to `(get-in db)`"
  ([sub-id] (defsub sub-id (keyword (name sub-id))))
  ([sub-id accessor]
   (let [acc (if (keyword? accessor)
               [accessor]
               accessor)]
     (rf/reg-sub
      sub-id
      (if (fn? acc)
        (fn [db [_ param]]
          (acc db param))
        (fn [db _]
          (get-in db acc)))))))

(defn init!
  "Initializes the app-db defaults"
  [defaults]
  (rf/clear-subscription-cache!)
  (rf/dispatch-sync [:initialize-db])
  (>> [:set-defaults defaults]))
