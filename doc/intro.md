# Introduction to GeoRepl

GeoRepl is short for geometric REPL. REPL means read-evaluate-print-loop which is a feature that comes with many functional languages. GeoRepl is written in Clojure and strongly committed to the Clojure ecosystem. Clojure is a LISP dialect residing on the JVM, interacting smoothly with the Java cosmos.

One idea behind GeoRepl is to have a tool for dynamic geometry as powerful and as simple as a REPL is for hacking code in a functional language. So, amongst others, GeoRepl is a dynamic geometry tool like "Geometer's Sketchpad" or "GeoGebra". In addition to the sketchpad functionality allowing expeditions into geometry GeoRepl interacts with a Clojure REPL. Every geometric item on the sketchpad is automatically represented by a Clojure item in the REPL and vice versa. So there are two ways to modify and evaluate mathematical objects: applying the Sketchpad features to the geometric representation or applying Clojure to the algebraic representation. GeoRepl will keep both approaches in sync.

GeoRepl is meant to provide for an intuitive and easy to use bridge to the mathematical aspects of Clojure.

