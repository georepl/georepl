# the Paint Layer

The implementation of the paint layer is built on top of a set of a framework's event functions. It contains the rudimentary operations and events of a sketchpad: get mouse- and keyboard events and draw some geometric primitives such as points, lines, and arcs.

The paint layer handles a loop which is provided by the runtime framework. Depending on the configured frame rate the draw function is called repeatedly. Before every call to draw the update function is called. Mouse events listened to by the paint layer are: mouse-pressed, mouse-released, mouse-moved and mouse-dragged. The basic operations, i.e. line, arc, and ellipse functions are used to display graphic primitives. More complex geometric objects than lines, arcs and circles are handled by other GeoRepl layers and drawn as a sequence of lines/arcs. A list of points to be drawn as arbitrary geometric object will be refered to as contour in the following.

There are two types of items which can be displayed: first of all, the trace of pixels just drawn. These data are temporary and maintained within the paint layer.
Second, the items to be displayed permanently. They are provided for by the elements layer as a list of primitives. The paint layer's draw function takes this list iteratively and displays them.


# the IState interface



The main data structure of this layer is a set called state comprising of these elements:

{:trace [[x1 y1 t1 clr1] [x2 y2 t2 clr2] ...] :cursor <p> :show-trace? <true/false> :complete? <true/false>}

:trace - the value of this key is a list of points. A point in this list is a four dimensional vector. The first and second are the x- and y-coordinates. These are (integer) screen-coordinates refering to the panel in which the user draws. The trace is the set of points resulting from dragging the mouse over the panel. The third coordinate is a timestamp in milliseconds which will be used to determine drawing velocities for freehand shape recognition. The last coordinate indicates the mouse button used with the mouse events.

:cursor - holds the current position of the mouse cursor. This is used in the context of the workbench stack of elements currently under construction.

:show-trace? - quil creates many frames per second first calling its update and then its draw functions. GeoRepl gets the already defined elements from the elements layer. In the paint layer only the trace of points occuring when the mouse is dragged over the panel can be displayed. In some cases this is not desirable. This flag determines whether the current trace is drawn.

:complete? - There are two situations regarded in the paint layer: Either an element is about to be drawn (mouse-dragged) or the drawing was finished (mouse-released). This flag indicates that the button was released and the drawing of the current shape is done.

The paint layer provides GeoRepl's higher levels with these functions:



# Event Handlers

These functions are called when certain events occur. They take a state as single parameter and return a state. A state is the set:

{:trace [[x1 y1 t1 clr1] [x2 y2 t2 clr2] ...]} :dragged? :complete?}

While :dragged? and :complete? are flags used only within the quil wrapper the value of :trace holds the interesting bits and pieces in form of a vector of point information. A point is a vector of two (2-D) integer screen display coordinates, followed by a timestamp in milliseconds since program start und an lcr flag field, indicating the mouse button held down while the mouse event was created.


(picked state)

     A point was picked on the sketchpad.

The state contains a trace vector with the picked point.

     (let [p (first (:trace state))
           x (first p)
           y (second p)])



(caught state)

The state contains a trace vector with the caught point. Catching points means to hold down the mouse button for a time without moving it. This event can be used to determine the closest neighbouring part of another element.


The state contains a trace vector with the caught point.

     (let [p (first (:trace state))
           x (first p)
           y (second p)])


(moved state)

(:trace state) contains the vector of points drawn with the mouse. This vector named trace can be used to determine lines, arcs or freehand shapes. The timestamps coming with the point vectors carry the drawing velocity which is important for the analysis of freehan shapes.

The state contains a trace vector with the contour points.

Another event which is determined in the elements- rather than the paint-layer and which is treated as a special case of the moved state is:

(dashed state)

     Something was striked through. Dashing in Georepl means to cut or delete an element (depending on situation).

The state contains a trace vector with start and end point.

     (let [p (first (:trace state))
           q (last (:trace state))
           x1 (first p)
           y1 (second p)
           x2 (first q)
           y2 (second q)])

More details on dashing in the description of the freehand layer.




# Primitives

The drawing primitives provided for by GeoRepl are:
draw-text, draw-point, draw-line, and draw-arc.





# Hints for Programmers

The status engine relies on most functions to return the current/modified state. A function

  (defn foo [state]
    ...
    (if ( ... )
      (assoc state ...)))

may lead to unpredictable results whenever the if-condition is not met.

So it's essential to provide for a correct return value:

  (defn foo [state]
    ...
    (if ( ... )
      (assoc state ...)
      state))



