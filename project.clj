(defproject intermittent "0.1.0-SNAPSHOT"
  :description "Experiments with intermittent between-group cultural transmission"
  :url "http://example.com/FIXME"
  :license {:name "Gnu General Public License version 3.0"
            :url "http://www.gnu.org/copyleft/gpl.html"}
  ; re "~" http://stackoverflow.com/questions/30985714/how-to-reference-environmental-variable-or-home-dir-in-project-clj:
  :resource-paths [~(str (System/getenv "HOME") "/dist/mason/" "jar/mason.19.jar")
                   ~(str (System/getenv "HOME") "/dist/mason/" "libraries/itext-1.2.jar")
                   ~(str (System/getenv "HOME") "/dist/mason/" "libraries/jmf.jar")
                   ~(str (System/getenv "HOME") "/dist/mason/" "libraries/portfolio.jar")]
  ;:warn-on-reflection true
  ;:java-source-paths ["src/java"]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/data.csv "0.1.2"]
                 ;[incanter/incanter "1.5.6"]
                 [org.jfree/jcommon "1.0.21"]    ;"mason/libraries/jcommon-1.0.21.jar"
                 [org.jfree/jfreechart "1.0.17"] ;"mason/libraries/jfreechart-1.0.17.jar"
                 [org.beanshell/bsh "2.0b4"]]    ;"mason/libraries/bsh-2.0b4.jar"
  :source-paths ["src/clj"]
  :jvm-opts ["-Xmx2g"]
  :main intermit.Sim
  :aot [intermit.Sim intermit.layout intermit.SimWithUI] 
  :profiles {:gui   {:main intermit.SimWithUI} ; execute this with 'lein with-profile gui   run'
             :nogui {:main intermit.Sim}})     ; execute this with 'lein with-profile nogui run'

  ; jvm-opts ["-Xms1g"]
  ;:jvm-opts ["-Dclojure.compiler.disable-locals-clearing=true"] ; ???: FASTER, and may be useful to debuggers. see https://groups.google.com/forum/#!msg/clojure/8a1FjNvh-ZQ/DzqDz4oKMj0J
  ;:jvm-opts ["-XX:+TieredCompilation" "-XX:TieredStopAtLevel=1"] ; setting this to 1 will produce faster startup but will disable extra optimization of long-running processes
  ;:jvm-opts ["-XX:TieredStopAtLevel=4"] ; more optimization (?)
