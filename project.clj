(def ^:private test-file-pattern #".*_test.clj")

(defproject nand-board "0.1.0-SNAPSHOT"
  :description "Nand Board - Playing with NAND gates on a board"
  :url "http://server.fake/nand-board"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [quil "3.1.0"]]
  :main ^:skip-aot nand-board.core
  :test-paths ["src"]
  :test-selectors {:default [(fn [ns] (.endsWith (str ns) "-test"))
                             (constantly true)]}
  :target-path "target/%s"
  :profiles {:uberjar {:aot [nand-board.core]}}
  :jar-exclusions ~[test-file-pattern]
  :uberjar-exclusions ~[test-file-pattern])
