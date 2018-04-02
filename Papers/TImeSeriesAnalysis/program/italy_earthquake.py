import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import matplotlib.pyplot as plt # plotting

from subprocess import check_output
print(check_output(['ls', './input']).decode('utf8'))

df = pd.read_csv('./input/query.txt', delimiter='|')
df = df.set_index('Time')




