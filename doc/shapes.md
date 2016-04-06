# simple and compound shapes

Shapes are representations of geometrical objects. Simple shapes are those which can be drawn immediately, like points, lines, circles, arcs, contours, and text. Shapes are data erecords which satisfy the IShape interface. The Ishape interface provides for some operations which can be called on all shapes: the basic transformations of the two-dimensional vector space, i.e. translation by a given vector, rotation around a reference point, scaling with reference point. If no explicit reference point is given for scaling or rotation, the shapes scale or rotate related to their :p-ref points. Into that, all objects with IShape interface provide for a next-point function which can be used to define the distance of the shape from a given point.
All shapes by definition starthave a reference point. These may be one of the points the shape comprises of but this is not always the case. Into that, the reference points on different levels in the shapes hierarchy differ.The referenc point of a line may be one of its end points but the :-ref of the triangle compound containing this line is the center of the triangle.
As a result,

# IShape objects
constrains:
  :visible < 1 if the shape is to be drawn>
  :p-ref <a vector denoting the reference point for the object>

# simple shapes
Simple shapes in GeoRepl are:

1. point
constrains:
  :p1 <the point as coordinate vector>

2. line
constrains:
  :p1 <the first end point of the line>
  :p2 <the second end point of the line>

3. circle
  :p-center <the center point of the circle>
  :radius <the radius of the circle>

4. arc
  :p-center <the center point of the arc>
  :radius <the radius of the arc>
  :p-start <the start point of the arc>
  :p-end <the end point of the arc>

Start and end point of an arc a1 are in mathematical order. So, exchanging start and end point yields an arc a2 so that a1 + a2 form a full circle.

5. contour
  p-list <a list of points defined by two-dimensional vectors>

6. text
  :str <the textual content as string>
  :top-left <the top left corner point of the box surrounding the text>
  :bottom-right <the bottom right corner point of the box surrounding the text>


