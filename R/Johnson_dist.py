# -*- coding: utf-8 -*-
"""
Created on Sun Jul 17 11:57:48 2022

@author: santi
"""

import math
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats
from scipy.stats import johnsonsu, johnsonsb

y = [15.007157, 9.121596, 14.084931, 13.606425, 13.023422,
         12.258205, 13.070061, 13.746903, 5.176362, 12.404762,
         7.584873, 13.095810, 11.751567, 12.117844, 12.794653,
         13.311182, 13.128248, 12.728639]

sb = johnsonsb.fit(y)
su = johnsonsu.fit(y)

weib = scipy.stats.exponweib.fit(y, floc=0, fa=1)
beta = scipy.stats.beta.fit(y, floc=0,fscale=16)

scipy.stats.cramervonmises(y, 'exponweib', args = weib)
scipy.stats.cramervonmises(y, 'beta', args = beta)

scipy.stats.kstest(y, 'exponweib', args = weib)
scipy.stats.kstest(y, 'beta', args = beta)


logLik_sb = np.sum( johnsonsb.logpdf(y, sb[0], sb[1], loc = sb[2], scale = sb[3])) 
BIC_sb = 4*math.log(18) - 2*logLik_sb
logLik_su = np.sum(johnsonsu.logpdf(y, su[0], su[1], loc = su[2], scale = su[3])) 
BIC_su = 4*math.log(18) - 2*logLik_su

scipy.stats.cramervonmises(y, 'johnsonsb', args=sb)
scipy.stats.cramervonmises(y, 'johnsonsu', args=su)

scipy.stats.kstest(y, 'johnsonsb', args= sb)
scipy.stats.kstest(y, 'johnsonsu', args = su)


a , b, loc, scale = sb
sim_sb = np.maximum(johnsonsb.rvs(a , b, loc, scale, size = 100000), 0)
a , b, loc, scale = su
sim_su = np.maximum(johnsonsu.rvs(a , b, loc, scale, size = 100000), 0)


plt.hist(sim_sb, bins = 30)
plt.hist(sim_su, bins = 30)
