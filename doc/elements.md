# elements representation

During a session with GeoRepl shapes are created. Shapes can be combined to compound objects. Compounds can be part of other compounds and so on. On top of the hierarchy there is a drawing compound which represents the drawing itself and comprises of all other shapes and compounds representing the drawing.

The principal data structure of the elements namespace is the elements stack. This is the only atomar structure in GeoReepl and it is accessible from various threads.
Each time a shape or compound is created, modified, or deleted, i.e., the drawing is modified a new drawing compound containing the newly created situation is pushed on top of the elements stack. So all the states of the drawing are stored on the stack and can be accessed with undo and redo operations. The top of stack element represents the current state of the drawing.

The list-elems function recursively creates a list of all drawable (and currently marked visible) shapes from the current drawing (i.e. the top of stck element).

The constraints for a drawing compound are:

1. :type :compound
2. :subtype :drawing
3. :elems [<a vector of all elements be4longing to the compound>]
4. :size [<a two-dimensional vector representing the width and height of the drawing>]
5. :p-ref [<a two-dimensional vector representing the center point of the drawing>]
6. :filename <a string with the filename of the drawing>

