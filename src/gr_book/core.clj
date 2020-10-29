(ns gr-book.core
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as spec]

   [hickory.core :as h]
   [hickory.select :as s]))


(def _doc
  (-> "/Users/ivan/ChatExport_2020-10-28/messages9.html"
      slurp
      h/parse
      h/as-hickory
      ))


(defn find-first [tree sel]
  (first (s/select sel tree)))


(def sel-message
  (s/and (s/tag :div)
         (s/class :message)
         (s/not (s/class :service))))


(def sel-date
  (s/and
   (s/tag :div) (s/class :date) (s/attr :title)))


(def sel-from-name
  (s/and
   (s/tag :div) (s/class :from_name)))


(def sel-msg-text
  (s/and
   (s/tag :div) (s/class :text)))


(defn node->text [node]
  (-> node :content str/join str/trim))


(defn get-text [node-msg]
  (some-> node-msg
          (find-first sel-msg-text)
          node->text))

(defn complete-dot [text]
  (if (str/ends-with? text ".")
    text
    (str text ".")))

(defn join-replics [replics]
  (str/join " "
            (->> replics
                 (map str/capitalize)
                 (map complete-dot))))


(def sel-media
  (s/and
   (s/tag :div) (s/class :media_wrap)))


(def sel-sticker
  (s/and
   (s/tag :img) (s/class :sticker)))


(def sel-photo
  (s/and
   (s/tag :img) (s/class :photo)))


(defn joined? [node]
  (some-> node :attrs :class (str/includes? "joined")))


(defn get-message-nodes [tree]
  (s/select sel-message tree))


(defn get-message-date [node]
  (-> node :content second :content second :attrs :title))


(defn node->from-name [node]
  (-> node :content str/join str/trim))



(defn find-from-name [node]
  (some-> node
          (find-first sel-from-name)
          node->from-name))


(defn get-message-id [node]
  (-> node :attrs :id (subs 7) Integer/parseInt))


;;
;; Spec
;;

(spec/def ::series
  (spec/cat :head (complement joined?) :tail (spec/* joined?)))

(spec/def ::parse
  (spec/* ::series))


(def _blocks (spec/conform ::parse _nodes))

(defn block->paragraph
  [{:keys [head tail]}]
  (join-replics (cons (get-text head) (map get-text tail))))


#_
(-> _blocks first block->paragraph)

#_
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
