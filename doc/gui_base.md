# Tansforming mouse events into georepl events

picked, snapped, dragged, dashed, moved



## a word on triple state parameters

A dual state parameter can represent two states. Yes or no, true or false, black or white, male or female. Boolean values are the most common way to express dual state in software programs. One advantage of booleans is that they can easily be toggled: (assign state :yesorno? (not :yesorno?).
But sometimes two possible states are not enough. Black, white, red, blue, and yellow. This is typically implemented as an enumeration. Changing from one value to another typically cannot be done by a simple toggeling operation. We have to implement every possible case. This is what case statements are for.
A triple state parameter is something in between. It can represent three states: yes, no, undecided; true, false, unknown. Boolean objects in Java are triple state parameters. They can be true or false and they can be nil which may cause mistakes sometimes because you need two evaluations: first, is there a value and if so, is it true or false?
A better way to represent a triple state is a parameter which can be either -1, 0, or 1. -1 may stand for false, 1 for true and 0 for 'no value'. You can implement a toggle by multiplying -1. If there was a true-false value the new value is the opposite, just like with booleans. But if the value was unset it remains unset. Multiplication can be used as well to reset a triple state parameter to 'no value'. Just multiply by 0.

In gui-base the :button-released parameter is an example of a triple state parameter: the mouse-pressed event handler sets it to 1 (button pressed). When the mouse butten remains pressed for a while this leads to a snapped event in the gui layer and the information whether or not the mouse button was pressed becomes irrelevant. Moreover, the mouse-released after snapping must be ignored to avoid a picked event. That's why :button-released is toggled in the mouse-released event and reset after snapping was initiated.
