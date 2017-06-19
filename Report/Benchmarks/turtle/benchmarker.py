from sys import argv
import turtle
import canvasvg
import numpy as np
from diagram import Frame
complementary = argv[1]
outfile = ''.join([complementary[:-4], '.svg'])
#
# Read the contents of the complementary input file
#
infile = open(complementary, 'r')
infile.readline()
infile.readline()
# Nodes
nnode = int(infile.readline().split()[0])
chart_dict = {'nodepos':[], 'elem':[], 'restr':[], 'pload':[],
              'eload':[], 'xdim':[], 'ydim':[] }
for i in range(nnode):
    cline = infile.readline().split()
    nn = int(cline[0])
    xc = float(cline[1])
    yc = float(cline[2])
    label = cline[3]
    node_tuple = (nn, xc, yc, label)
    chart_dict['nodepos'].append(node_tuple)
infile.readline()
# Elements
nelem = int(infile.readline().split()[0])
for i in range(nelem):
    elem_tuple = tuple([int(i) for i in infile.readline().split()])
    chart_dict['elem'].append(elem_tuple)
infile.readline()
# Restraints
nrestr = int(infile.readline().split()[0])
for i in range(nrestr):
    restr_tuple = tuple([int(i) for i in infile.readline().split()])
    chart_dict['restr'].append(restr_tuple)
infile.readline()
# Point Loads
npoint = int(infile.readline().split()[0])
for i in range(npoint):
    cline = infile.readline().split()
    node_number = int(cline[0])
    magnitude = float(cline[1])
    orientation = float(cline[2])
    pload_tuple = (node_number, magnitude, orientation)
    chart_dict['pload'].append(pload_tuple)
infile.readline()
# Element Loads
neloads = int(infile.readline().split()[0])
for i in range(neloads):
    cline = infile.readline().split()
    el_number = int(cline[0])
    py1 = float(cline[1])
    py2 = float(cline[2])
    orientation = float(cline[3])
    eload_tuple = (el_number, py1, py2, orientation)
    chart_dict['eload'].append(eload_tuple)
infile.readline()
# Dimensions (x-direction)
nxdim = int(infile.readline().split()[0])
for i in range(nxdim):
    xdim_tuple = tuple([int(i) for i in infile.readline().split()])
    chart_dict['xdim'].append(xdim_tuple)
infile.readline()
# Dimensions (y-direction)
nydim = int(infile.readline().split()[0])
for i in range(nydim):
    ydim_tuple = tuple([int(i) for i in infile.readline().split()])
    chart_dict['ydim'].append(ydim_tuple)
# 
# Compute the scale to fit the figure in the page
# 
maxxdiff = 350.  # only change this one (either 300 or 350)
xmin = 0.
xmax = 0.
for node in chart_dict['nodepos']:
    if node[1] < xmin:
        xmin = node[1]
    if node[1] > xmax:
        xmax = node[1]
actualxd = xmax - xmin
scale = maxxdiff / actualxd
# Make it fullscreen so that the image does not get clipped
turtle.setup(width=1.0, height=1.0)
# Create Frame object and draw the figure
frame = Frame(chart_dict, scale)
turtle.clearscreen()
turtle.hideturtle()
turtle.speed(100)
frame.draw_all()
canv = turtle.getscreen().getcanvas()
canvasvg.saveall(outfile, canv)
