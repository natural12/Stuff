#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys


current_word = None
current_count = 0

for line in sys.stdin:
    line = line.strip()
    key,count = line.split('\t', 1)
    try:
        count = int(count)
    except ValueError:
        continue
    if current_word == key:
        current_count += count
    else:
        if current_word:
            x, y = current_word.split('_')
            x_hi=str(round(float(x)+0.1,1))
            y_hi=str(round(float(y)+0.1,1))
            print '%s,%s,%s,%s,%s' % (x,x_hi,y,y_hi,str(current_count))
        current_count = count
        current_word= key

if current_word == key:
     x, y = current_word.split('_')
     x_hi=str(round(float(x)+0.1,1))
     y_hi=str(round(float(y)+0.1,1))
     print '%s,%s,%s,%s,%s' % (x,x_hi,y,y_hi,str(current_count))
            
            
            
            
