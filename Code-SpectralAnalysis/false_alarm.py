from numpy import *
import scipy
import matplotlib
import matplotlib.pyplot as plt
import astropy
from scipy import constants as spconstants
from scipy.special import gamma
randn = random.randn
SNRt = array([0.01, 0.05, 0.1])
Nvec = 10.**(arange(0, 5.05, 0.05))

fig=plt.figure()
for s in SNRt:
    Nfa = Nvec * exp(-Nvec*s**2)
    plt.plot(Nvec, Nfa, '-', lw=2, label=r'$\rm (S/N)_t = %6.2f $'%(s))
    Nmax = 1./s**2
    Nfa_max = Nmax / e
    plt.plot(Nmax, Nfa_max, 'ko')
   
plt.xscale('log')
plt.yscale('log')
plt.axis(ymin=0.01)
plt.xlabel(r'$\rm DFT \ Length $')
plt.ylabel(r'$\rm Number \ of \ False \ Alarms $')
plt.title(r'$\rm False \ positives \ for \ threshold \ = \ expected \
                 line \ amplitude $')
plt.legend(loc=2)
plt.show()
