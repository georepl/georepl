# georepl

A Clojure program designed to let you do Constructive Geometry and combine your geometrical objects with the Clojure ecosystem using a Repl.

## Usage

The program uses quil (https://github.com/quil/quil). To start the framework call the main function in georepl/src/georepl/main.clj. Then connect with the port configured in georepl/src/georepl/configuration.clj using an nRepl client of your choice.

For instance, if you are using Leiningen you might start a repl typing on the command line:

lein repl :connect 7888

This will start a repl which is connected to port 7888. If georepl was started with port 7888 configured you are connected now. Set the elements georepl namespace at the repl prompt:

user=> (ns georepl.elements)

Now you can access the shapes in your georepl drawing using their names, i.e:

georepl.elements=> Ln1

and the internal representation of the shape Ln1 shows in your repl.

## License

Copyright © 2015, 2016 Thomas Neuhalfen

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
