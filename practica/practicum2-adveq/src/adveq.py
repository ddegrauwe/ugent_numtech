# setup
%matplotlib inline
import matplotlib.pyplot as plt
import matplotlib.animation
plt.rcParams["animation.html"] = "jshtml"
import numpy as np

# function to animate results from a given output file
def showResults(filename='output.dat',experiments=None,ylim=None):
    # load data
    with open(filename) as fid:
        # read header
        header=fid.readline().split()
        nx=int(header[0])
        dx=float(header[1])
        nt=int(header[2])
        dt=float(header[3])
        nExperiments=int(header[4])
        # read experiment names
        expNames=fid.readline().split('"')[1::2]
        # read data
        data=np.loadtxt(filename,skiprows=2)
        data=data.reshape((int(data.shape[0]/nExperiments),nExperiments,nx))
    
    if experiments is None:
        experiments=range(nExperiments)
    
    # vertical extent
    if ylim is None:
        ylim=[-np.max(np.abs(data)),np.max(np.abs(data))]
    
    # define time and space
    t = np.arange(nt+1)*dt
    x = np.arange(nx)*dx

    # create empty figure
    fig, ax = plt.subplots(figsize=(12,8));
    h = ax.axis([0,nx*dx,ylim[0],ylim[1]])
    ll=[None]*nExperiments
    
    for iExperiment in experiments:
        lll, = ax.plot([],[],label=expNames[iExperiment])
        ll[iExperiment]=lll
    tt=plt.title('')
    plt.legend(loc=1)

    # define plot function for a single time step
    def plotResults(it):
        tt.set_text('%3i/%3i'%(it,nt))
        for iExperiment in experiments:
            ll[iExperiment].set_data(x, data[it,iExperiment,:])
    
    # animate over timesteps
    ani = matplotlib.animation.FuncAnimation(fig, plotResults, frames=data.shape[0])
    plt.close()
    display(ani)

# show results
filename='upstream/output.dat'
showResults(filename)
