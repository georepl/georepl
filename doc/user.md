#GeoRepl - API



##arc [cp radius start end]

creates a new Arc shape with the given coordinates. The new Arc shape is automatically selected.

*Results:
(visual -) The new Arc shape appears in green
(Repl -) The record representing the Arc



##change [& keyvals]

changing the parameters of an element in the Repl creates a new object although it may have the same :name as the original object. Such an element update would be Repl-local without effect for the visual representation. This function changes the parameters of the currently selected element and synchronizes this with the geometrical representation.

*Results:
(visual -) The new Circle shape appears in green
(Repl -) The record representing the Circle



##circle [cp radius]
##circle [p1 p2 p3]

creates a new Circle shape with the given coordinates. The circle is defined either by center point and radius or by three different points on the circle. The new Circle shape is automatically selected.

*Results:
(visual -) The new Circle shape appears in green
(Repl -) The record representing the Circle



##compound [& name]

creates a new Compound containing the elements defined by the given names. If no element is found for a name it is ignored. The elements of the new Compound are automatically selected.


*Results:
(visual -) The elements of the compound appear in green
(Repl -) The record representing the Compound element


##copy []

copies the selected element. A new name is assigned to the new element. All parameters but the names of the original and the copied elements are identical.


*Results:
(visual -) The copied element appears in green and is in the same place as the original one
(Repl -) The record representing the new element


##line [p q]

creates a new Line shape with the given coordinates. The new Line shape is automatically selected.

*Results:
(visual -) The new Line shape appears in green
(Repl -) The record representing the Line



##mirror [p1 p2]

moves the selected element by the vector v.

*Results:
(visual -) The selected element in its new position appears in green
(Repl -) The record representing the element




##move [v]

reflects the selected element along the vector v.

*Results:
(visual -) The selected element in its new position appears in green
(Repl -) The record representing the element



#point [p]

creates a new Point shape at the given coordinate p. The new Point shape is automatically selected.

*Results:
(visual -) The new Point shape appears in green
(Repl -) The record representing the Point


#rotate [angle]

rotates the currently selected element by the angle *angle*. Rotation is defined in mathematical order (counter-clockwise). The :p-ref of the element defines the center of rotation. This reference point may or may not be on the element.

*Results:
(visual -) The selected element in its new position appears in green
(Repl -) The record representing the element



##scale [factor]

scales the selected element by the factor *factor*. The element's :p-ref is the center (fix point) of the scalation. This reference point may or may not be on the element.


*Results:
(visual -) The selected element in its new position appears in green
(Repl -) The record representing the element




##select [name]

the following operations will relate to the selected element which may be any named shape including compounds.
Has no effect if no item with the given name exists.

*Results:
(visual -) The selected element appears in green
(Repl -) The record representing the selected element



##show [f-out mode]
##show [mode]

display cumulated internal state information. mode is a key of either:

:list-elements (list all elements in the current drawing)
:list-shapes (list all shapes in the current drawing)
:selected-elem (show the element currently selected).

With the f-out parameter an output function can be defined. For instance, setting f-out to identity returns the elements. By default the output function is prn.

*Results:
(visual -) No effect
(Repl -) depends on f-out



----ToDo
##settings [& keyvals]

##undo []

(defn cut []
  )


;;(defn redo [elem]

