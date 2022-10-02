# R code to show results of the oscillation equation
# the results are supposed to be stored in 3 columns
# (time-exact-numerical) in a file 'output.dat'

# read the file
y=read.table('output.dat',header=TRUE)

# plot exact and numerical result
plot(y$time,y$exact,ylim=c(-3,3),type='b',pch=1,col=1)
points(y$time,y$numerical,type='b',pch=2,col=2)
