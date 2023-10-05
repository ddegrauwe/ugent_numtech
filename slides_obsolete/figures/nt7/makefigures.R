rm(list=ls())

load('wind.Rdata')

# thinning
t=10
ix=seq(t/2,nrow(u)-t/2,by=t)
iy=seq(t/2,ncol(u)-t/2,by=t)
u=u[ix,iy]
v=v[ix,iy]

# scale
s=1/max(u**2+v**2)

#
plot.new()

