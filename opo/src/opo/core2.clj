(ns opo.core2
  (:import (javax.swing JList JFrame JScrollPane JButton)
           java.util.Vector))

(def symbols (->> (ns-publics 'clojure.core)
                      (map key)
                      sort
                      Vector.
                      JList.))

(defn show-info [] )

(def window (doto (JFrame. "Bridged outdoor")
                  (.setSize (java.awt.Dimension. 500 400))
                  (.add (JScrollPane. symbols))
                  (.add java.awt.BorderLayout/SOUTH
                        (doto (JButton. "Show info")
                          (.addActionListener (reify java.awt.event.ActionListener
                                                (actionPerformed [_ e]
                                                  (show-info))))))
                  (.setVisible true)))

JFrame
