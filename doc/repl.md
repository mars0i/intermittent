Notes on testing at the repl
===

To test data structures:
```clojure
(def s (Sim. 42))
(.start s)
```

To run:
```clojure
(sim.engine.SimState/doLoop intermit.Sim (into-array String []))
```
or:
```clojure
(intermit.SimWithUI/main (into-array String []))
```
or:
```clojure
(def s (Sim. 42))
(.setVisible (sim.display.Console. (intermit.SimWithUI. s)) true)
```

Then you can do things like this:

```clojure
(vec (.getMeanReligTimeSeries s))
```

```clojure
(require 'intermit.file)
(intermit.file/spit-ys "foo.csv" (.getMeanReligTimeSeries s) :append true)
(reset! (.meanReligSeries (.instanceState s)) [])
```
