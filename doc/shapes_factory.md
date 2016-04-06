# shapes creation

For every shape there is a factory object which controls the creation and modification of the shape. The factory objects satisfy the IShapesFactory protocol. A factory takes a temporary element and returnes an element. As a result, shapes factory objects provide these functions:

1. create
Most shapes comprise of more than one parameter wehich may be defined in arbitrary order. The factory 'knows' which parameters can be defined and provides for a 'quector'. That is a vector of questions which ask for the respective parameter and an :f function to handle the answer to the question. This :f function defines parameters respecting the shape's internal geometric constrains. The :g function transforms the shape according to the current parameter settings so it can be displayed on the screen.

2. refresh
Uses the :g function to return the shape with the current parameter settings.

3. update-element
Replaces the current shape with another.

4. current-element
Returns the current shape.

5. current-question
Returns the :f function for the next parameter to be defined.

6. finish
Defines the current state of the element to be final. The element is pushed, that is, a new drawing containing the former state of the drawing plus the shape is pushed onto the elements stack.

