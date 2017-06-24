import turtle
import numpy as np
# nodepos <node number> <xcoor> <ycoor> <label>
# elem <element number> <start node> <end node>
# restr <node> <0:free, 1:roller, 2:pin, 3:fixed> <orientation>
# orientation: 0 for upright, for others, degree in ccw (either 90 or 270)
# pload: <node> <magn> <orientation>
# eload: <element> <py1> <py2> <orientation>
# xdim : (node_i, node_j)
mydict = {'nodepos':[(1, 0., 0., 'A'),
                     (2, 0., 5., 'B'),
                     (3, 5., 5., 'C'),
                     (4, 5., 0., 'D')],
          'elem':   [(1, 1, 2),
                     (2, 2, 3),
                     (3, 3, 4)],
          'restr':  [(1, 3, 0),
                     (4, 1, 90)],
          'pload':  [(2, 25., 0),
                     (3, 12.5, 270)],
          'eload':  [(1, 10., 10., 0.)],
          'xdim' :  [(1, 4)],
          'ydim' :  [(1, 2)]
         }
                  
class Frame:
    def __init__(self, indict, scale):
        self.indict = indict
        self.scale = scale
    def draw_elements(self):
        scale = self.scale
        nodepos = self.indict['nodepos']
        elem = self.indict['elem']
        turtle.pensize(1.5)
        for element in elem:
            snod = element[1]
            enod = element[2]
            sxcoor = nodepos[snod-1][1]*scale
            sycoor = nodepos[snod-1][2]*scale
            scoorid = nodepos[snod-1][3]
            excoor = nodepos[enod-1][1]*scale
            eycoor = nodepos[enod-1][2]*scale
            ecoorid = nodepos[enod-1][3]
            # Write first node's label
            turtle.penup()
            turtle.setposition(sxcoor+10, sycoor-30) # for frames
#            turtle.setposition(sxcoor+0, sycoor-50) # for beams
            turtle.write(scoorid, align='left', font=("", 15, ""))
            # Draw element
            turtle.setposition(sxcoor, sycoor)
            turtle.pendown()
            turtle.setposition(excoor, eycoor)
            # Write second node's label
            turtle.penup()
            turtle.setposition(excoor+10, eycoor-30) # for frames
#            turtle.setposition(excoor+0, eycoor-50)   # for beams
            turtle.write(ecoorid, align='left', font=("", 15, ""))
        return
    def draw_fixed_support(self, orientation, length=50):
        l = length/2.
        ticks = 6
        inc = length/(ticks-1)
        turtle.setheading(orientation)
        turtle.backward(l)
        for i in range(1, ticks):
            turtle.right(135)
            turtle.forward(5)
            turtle.left(180)
            turtle.forward(5)
            turtle.right(45)
            turtle.forward(inc)
        turtle.right(135)
        turtle.forward(5)
        return
    def draw_triangle(self):
        turtle.right(60)
        turtle.forward(20)
        turtle.right(120)
        turtle.forward(20)
        turtle.right(120)
        turtle.forward(20)
        # return turtle to lower left corner
        turtle.right(180)
        turtle.forward(20)
        # head to the other corner of the base
        turtle.right(240)
        return
    def draw_pin_support(self, orientation):
        # orientation of the support
        turtle.setheading(orientation)
        # draw the triangle
        self.draw_triangle()
        # draw the base
        turtle.forward(10)
        self.draw_fixed_support(orientation, 25)
        return
    def draw_roller_support(self, orientation):
        turtle.setheading(orientation)
        self.draw_triangle()
        # draw circles
        for i in range(2):
            turtle.forward(5)
            turtle.left(180)
            turtle.circle(4)
            turtle.right(180)
            turtle.forward(5)
        # draw base
        turtle.backward(10)
        turtle.penup()
        turtle.right(90)
        turtle.forward(8)
        turtle.pendown()
        self.draw_fixed_support(orientation, 25)
        return
    def draw_supports(self):
        scale = self.scale
        restr = self.indict['restr']
        nodepos = self.indict['nodepos']
        for res in restr:
            node = res[0]
            support = res[1]
            orientation = res[2]
            if support == 0:
                pass
            xcoor = nodepos[node-1][1]*scale
            ycoor = nodepos[node-1][2]*scale
            turtle.penup()
            turtle.setposition(xcoor, ycoor)
            turtle.pendown()
            if support == 1:
                self.draw_roller_support(orientation)
            elif support == 2:
                self.draw_pin_support(orientation)
            elif support == 3:
                self.draw_fixed_support(orientation)
        return
    def draw_arrow(self, arrowsize=10):
        turtle.begin_fill()
        turtle.right(30)
        for i in range(3):
            turtle.right(120)
            turtle.forward(arrowsize)
        turtle.end_fill()
        turtle.left(30)
        return
    def draw_point_load(self, load, orientation):
        loadtext = ''.join([str(load),' kN'])
        turtle.setheading(orientation)
        turtle.penup()
        turtle.backward(100)
        turtle.write(loadtext, align='center', font=("Times New Roman", 18, ""))
        turtle.pendown()
        turtle.pensize(1.2)
        turtle.forward(90)
        self.draw_arrow()
        return
    def draw_distributed_load(self, sxcoor, sycoor, excoor, eycoor, py1, py2,
                              orientation):
        loadarrow = 50.
        #
        # Draw the line at the base, turn to the starting position
        turtle.penup()
        turtle.setposition(sxcoor, sycoor)
        turtle.setheading(orientation)
        turtle.backward(10)
        # Note original coordinates
        xso = turtle.xcor()
        yso = turtle.ycor()
        turtle.setposition(excoor, eycoor)
        turtle.backward(10)
        xfo = turtle.xcor()
        yfo = turtle.ycor()
        turtle.pendown()
        turtle.setposition(xso, yso)
        # Get local coordinate along member axis, decide if horiz. or vertic.
        if sxcoor == excoor: # Vertical member
            xs = yso
            xf = yfo
            ys = xso
            yf = xfo
            alignment = 'v'
        elif sycoor == eycoor: # Horizontal member
            xs = xso
            xf = xfo
            ys = yso
            yf = yfo
            alignment = 'h'
        xdiff = xf - xs
        narrow = 11.
        x = 0.
        step = xdiff / narrow
        # 
        # Make distinctions between different load cases
        #
        # Uniform loading
        if py1 == py2:
            loadtext = ''.join([str(py1), ' kN/m'])
            turtle.backward(loadarrow)
            turtle.forward(loadarrow)
            self.draw_arrow(arrowsize=5)
            turtle.backward(loadarrow)
            while x < xdiff:
                turtle.left(90)
                turtle.forward(step)
                turtle.right(90)
                turtle.forward(loadarrow)
                self.draw_arrow(arrowsize=5)
                turtle.backward(loadarrow)
                x = x + step
            turtle.penup()
            # go to midpoint of the load to write the label
            if alignment == 'h':
                turtle.setx(turtle.xcor() - xdiff/2)
                turtle.write(loadtext, align='center', 
                             font=("Times New Roman", 18, ""))
            elif alignment == 'v':
                turtle.sety(turtle.ycor() - xdiff/2)
                turtle.write(loadtext, align='right', 
                             font=("Times New Roman", 18, ""))
        # Triangle starting from 0 ending at Py2
        if (py1 == 0) or (py2 == 0):
            if py1 == 0:
                sgn = 1.
                loadtext = ''.join([str(py2), ' kN/m'])
                phase = 0.
                yp1 = 0.
                yp2 = loadarrow*2
                slope = np.arctan(yp2 / xdiff)
                slopedeg = slope*180/np.pi
                x = xs + step
                y = ys + np.tan(slope) * step
                xs = xs
                yco = 0
            elif py2 == 0:
                sgn = -1.
                loadtext = ''.join([str(py1), ' kN/m'])
                turtle.penup()
                turtle.setposition(xfo, yfo)
                turtle.pendown()
                yp1 = loadarrow*2
                yp2 = 0.
                slope = np.arctan(yp1 / xdiff)
                slopedeg = slope*180/np.pi
                phase = (180 - 2*slopedeg)
                x = xf - step
                y = yf + np.tan(slope) * step
                xs = xf # Drawing from left to right
                yco = 0
            while abs(x - xs) <= xdiff:
                turtle.left(90 + slopedeg + phase)
                dy = step*np.tan(slope)
                yco += dy
                dist = np.sqrt(dy ** 2 + step ** 2)
                turtle.forward(dist)
                turtle.right(90 + slopedeg + phase)
                turtle.forward(yco)
                self.draw_arrow(arrowsize=5)
                turtle.backward(yco)
                x = x + step*sgn
            turtle.write(loadtext, align='center', 
                         font=("Times New Roman", 18, ""))
        return
    def draw_loads(self):
        scale = self.scale
        pload = self.indict['pload']
        eload = self.indict['eload']
        nodepos = self.indict['nodepos']
        elem  = self.indict['elem']
        # Point loads
        for pl in pload:
            node  = pl[0]
            magn  = pl[1]
            orientation = pl[2]
            xcoor = nodepos[node - 1][1]*scale
            ycoor = nodepos[node - 1][2]*scale
            turtle.penup()
            turtle.setposition(xcoor, ycoor)
            self.draw_point_load(magn, orientation)
        for el in eload:
            elno = el[0]
            py1  = el[1]
            py2  = el[2]
            orientation = el[3]
            # Coordinates of the element nodes
            snod = elem[elno - 1][1]
            enod = elem[elno - 1][2]
            sxcoor = nodepos[snod - 1][1]*scale
            sycoor = nodepos[snod - 1][2]*scale
            excoor = nodepos[enod - 1][1]*scale
            eycoor = nodepos[enod - 1][2]*scale
            self.draw_distributed_load(sxcoor, sycoor, excoor, eycoor,
                                       py1, py2, orientation)
        return
    def draw_dim(self, length, alignment):
        scale = self.scale
        endline = 10.
        crossline = 10.
        # Draw first end
        turtle.left(90)
        turtle.forward(endline/2)
        turtle.backward(endline)
        turtle.forward(endline/2)
        turtle.right(45)
        turtle.forward(crossline/2)
        turtle.backward(crossline)
        turtle.forward(crossline/2)
        turtle.right(45)
        # Draw dimline
        aln = ['center', 'right'][alignment]
        dimtext = ''.join([str(length/scale), ' m'])
        turtle.forward(length/2)
        turtle.write(dimtext, align = aln, 
                     font=("Times New Roman", 18, ""))
        turtle.forward(length/2)
        # Draw second end
        turtle.left(90)
        turtle.forward(endline/2)
        turtle.backward(endline)
        turtle.forward(endline/2)
        turtle.right(45)
        turtle.forward(crossline/2)
        turtle.backward(crossline)
        turtle.forward(crossline/2)
        turtle.right(45)
    def draw_dimensions(self):
        scale = self.scale
        # Position of the dimension lines
        xdimline = -100
        #ydimline = -200
        ydimline = -100
        # ---------------------
        nodepos = self.indict['nodepos']
        elem = self.indict['elem']
        xdim = self.indict['xdim']
        ydim = self.indict['ydim']
        # Draw xdims
        for node_tuple in xdim:
            snod = node_tuple[0]
            enod = node_tuple[1]
            sxc = nodepos[snod - 1][1]*scale
            exc = nodepos[enod - 1][1]*scale
            length = abs(exc - sxc)
            print 'snod = %d, enod = %d, sxc = %d, exc = %d, length = %d' %(
                        snod, enod, sxc, exc, length)
            turtle.penup()
            turtle.setposition(sxc, xdimline)
            turtle.setheading(0)
            turtle.pendown()
            self.draw_dim(length, 0)
        # Draw ydims
        for node_tuple in ydim:
            snod = node_tuple[0]
            enod = node_tuple[1]
            syc = nodepos[snod - 1][2]*scale
            eyc = nodepos[enod - 1][2]*scale
            length = abs(eyc - syc)
            print 'snod = %d, enod = %d, syc = %d, eyc = %d, length = %d' %(
                        snod, enod, syc, eyc, length)
            turtle.penup()
            turtle.setposition(ydimline, syc)
            turtle.setheading(90)
            turtle.pendown()
            self.draw_dim(length, 1)
        return
    def draw_all(self):
        self.draw_elements()
        self.draw_supports()
        self.draw_loads()
        self.draw_dimensions()
        return
# 
# Test 
#
if __name__ == '__main__':
    # Compute the scale to fit the figure in the page
    # 
    maxxdiff = 350.  # only change this one (either 300 or 350)
    xmin = 0.
    xmax = 0.
    for node in mydict['nodepos']:
        if node[1] < xmin:
            xmin = node[1]
        if node[1] > xmax:
            xmax = node[1]
    actualxd = xmax - xmin
    scale = maxxdiff / actualxd
    # Make it fullscreen so that the image does not get clipped
    turtle.setup(width=1.0, height=1.0)
    turtle.clearscreen()
    deneme = Frame(mydict, scale)
    turtle.speed(0)
    deneme.draw_all()
    turtle.penup()
    turtle.setpos(-10,-10)
    raw_input()

