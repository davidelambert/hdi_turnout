#%%
import pandas as pd
import numpy as np
import statsmodels as sm
import scipy as sp
import matplotlib.pyplot as plt

#%%
cnty = pd.read_csv(
        "/Users/delamb/data/nc16/330_county_panel_08-16_subset.csv"
        )



#%% pretty output for summary stats
def summary(obj):
    d = sp.stats.describe(obj)
    q25 = np.percentile(obj, 25)
    med = np.percentile(obj, 50)
    q75 = np.percentile(obj, 75)
    print('   N =', d[0])
    print(' min =', d[1][0].round(2),
          '  q25 =', q25.round(2),
          '  med =', med.round(2),
          '  q75 =', q75.round(2),
          '  max =', d[1][1].round(2))
    print('mean =', d[2].round(2),
          '   SD =', round(d[3]**.5, 2),
          '  Var =', d[3].round(2),
          ' skew =', np.float16(d[4]).round(2),
          ' kurt =', np.float16(d[5]).round(2))

#%%
hdi_summary = summary(cnty.hdi)
hdi_summary

#%%
loghdi_summary = summary(np.log(cnty.hdi))
loghdi_summary



#%%
%matplotlib inline
plt.style.use('seaborn')


#%%
np.


