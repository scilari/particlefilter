#!/usr/bin/env python3

import matplotlib.pyplot as plt
import pandas as pd
import os

path = os.path.abspath(os.path.dirname(__file__))

data = pd.read_csv(path + '/input/points.csv')
xs = data.x
ys = data.y
print("Data size", len(xs))
plt.figure()
plt.plot(xs, ys, 'o')
plt.axis('equal')
plt.savefig(path + '/output/points.png')
