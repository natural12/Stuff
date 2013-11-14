#!/usr/bin/python
import sys
import math


for line in sys.stdin:
	line = line.strip()
	x,y=line.split('\t')
	x_lo=str(math.floor(float(x)*10)/10)
	y_lo=str(math.floor(float(y)*10)/10)
	
	print '%s_%s\t1' % (x_lo,y_lo)
