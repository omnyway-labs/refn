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

(defn assoc-db [fx-id]
  (fn [db [_ param]]
    (assoc db fx-id param)))

(def >>! rf/dispatch-sync)

(defn >>
  ([form]
   (if (keyword? form)
     (partial >> form)
     (rf/dispatch form)))
  ([fx data]
   (rf/dispatch [fx data])))

(defn <<
  ([sub]
   (let [f (comp deref rf/subscribe)]
     (if (keyword? sub)
       (f [sub])
       (f sub))))
  ([sub dispatcher]
   (rf/dispatch dispatcher)
   (<< sub)))

(defn |> [callback data]
  (if (keyword? callback)
    (>> [callback data])
    (callback data)))

(defn dissoc-in
  "(dissoc-in {:a [{:x 1 :y 2}] :b [{:u 1 :v 2}]} :a :x 1)
  {:a (), :b [{:u 1, :v 2}]}"
  [m entity key value]
  (->> (get m entity)
       (remove #(= (get % key) value))
       (assoc m entity)))

(comment
  (conj-in {:a [{:x 1 :y 2}] :b [{:u 1 :v 2}]} :b {:a 1 :b 2})
  => {:a [{:x 1 :y 2}] :b [{:u 1 :v 2} {:a 1 :b 2}]})

(defn conj-in
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

(defn refn [op entity]
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
 (fn-traced [_ _] default))

(rf/reg-event-db
 :set-defaults
 (fn [db [_ default]]
   (merge db default)))

(defn defx [fx-id fn & {:keys [callback cache?
                              cache-key]}]
  (rf/reg-event-fx fx-id as-fx!)
  (rf/reg-fx (fx-id! fx-id) fn))

(defn defsub
  ;; Will convert a namespaced sub-id to a plain keyword for the accessor for single arity
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

(defn init! [defaults]
  (rf/clear-subscription-cache!)
  (rf/dispatch-sync [:initialize-db])
  (>> [:set-defaults defaults]))
