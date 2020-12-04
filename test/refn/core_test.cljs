(ns refn.core-test
  (:require
   [refn.core :refer [defx refn]]
   [ampltiude.gql :as gql]))


(defx ::create-record
  (fn [{:keys [name email]}]
    (gql/create! :person
                 {:input     {:name name :email email}
                  :on-create (refn :create :person)
                  :shape     [:id :name :email]})))


(defx ::lists-records
  (fn [{:keys [name email]}]
    (gql/list :person
              {:limit     100
               :on-list  (refn :list :person)
               :shape     [:id :name :email]})))

(defx ::delete-record
  (fn [{:keys [id]}]
    (gql/delete! :person
                 {:input     {:id id}
                  :on-create (refn :delete :person)
                  :shape     [:id]})))
