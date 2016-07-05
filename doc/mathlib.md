

#intersect-straight-line-circle [p1 p2 cp radius]

Return the list of intersection points of a straight line and a circle. There may be two, one, or no common points. The returned list will be empty in the latter case.

The straight line is given by two (distinct) points p1 and p2.
The circle is defined by its center point cp and a radius. If the radius is (nearly) zero the circle is a point cp and the function returns this point if it is on the line.

The slope of the line through p1 = [x1 y1] and p2 = [x2 y2] is

(y2 - y1) / (x2 - x1)

Now every point p = [x y] on the line has the same slope so we can write:

(y - y1) / (x - x1) = (y2 - y1) / (x2 - x1) = slope

or, to gain the form of a function f(x):

y = Ax + B

where

A = (y2 - y1) / (x2 - x1)
B = ((x2 - x1) y1) - ((y2 - y1) x1) / (x2 - x1)


In order to find the intersections of line and circle, however, we prefer another form of line description:

y = Ax + B := -(a / b) x - (c / b) = ((c - ax) / b) =>

ax + by = c

where

a = (y1 - y2)
b = (x2 - x1)
c = ((x2 - x1) y1) + ((y1 - y2) x1) = b y1 + a x1

To get the intersection points of the line and a circle defined as:

(x - xcenter)(x - xcenter) + (y - ycenter)(y - ycenter) = r*r

we move line and circle so the circle's center point is [0 0] so the new equations for straight line and circle become:

x x + y y = r r

and

ax + by = d

where

d = c - a * xcenter - b * ycenter = a (x1 - xcenter) + b (y1 - ycenter).

If  r*r (a*a + b*b) = d*d the line touches the circle in one point (tangent).
If  r*r (a*a + b*b) > d*d the intersections are:

x1/2 = xcenter + [1/(a*a+b*b)] * [ad +/- b * sqrt (disc)]
y1/2 = ycenter + [1/(a*a+b*b)] * [bd +/- a * sqrt (disc)]

where quot = (a*a + b*b) and disc = (r*r*quot - d*d)

