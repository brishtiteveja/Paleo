import pandas as pd
import numpy as np
from pandas import Series, DataFrame, Panel
import matplotlib.pyplot as plt

ao = np.loadtxt('monthly.ao.index.b50.current.ascii')

dates = pd.date_range('1950-01', '2017-08', freq='M')

AO = Series(ao[:,2], index=dates)

nao = np.loadtxt('norm.nao.monthly.b5001.current.ascii')
dates_nao = pd.date_range('1950-01', '2017-08', freq='M')

NAO = Series(nao[:, 2], index=dates_nao)

aonao = DataFrame({'AO' : AO, 'NAO' : NAO})

import datetime
ix_nao = aonao.ix[(aonao.AO > 0) & (aonao.NAO < 0) 
         & (aonao.index > datetime.datetime(1980,1,1)) 
         & (aonao.index < datetime.datetime(1989,1,1)),
         'NAO']

ix_nao.plot(kind='bar')


