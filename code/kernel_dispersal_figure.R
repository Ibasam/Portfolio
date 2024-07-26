## DATA ##
mu=0
beta=29.5
h=0.8 #dispersal rate 20%

#dispersal laplace distribution
lap = function(mu, beta, distance) {(1/(2*beta))*exp(-(distance-mu)/beta)} # Laplace

## EXAMPLE KERNEL ##

distance<-seq(from=0, to=135, by=15) #10 populations separated by 15km
attrac_d=lap(mu, beta, distance) #kernel

#equal pop size
ratio_area1=rep(0.1,10) #equal pop size
attrac_ad=attrac_d*ratio_area1 # influence of area on attractivity
proba_attract_ad=attrac_ad/sum(attrac_ad) #proba of attractivity du site fonction distance at aire
sum(proba_attract_ad)
proba_dispersion=proba_attract_ad*(1-h)

#variable pop size
ratio_area2=c(rep(0.05,5),rep(0.15,5)) # small #big=0.08
attrac_ad2=attrac_d*ratio_area2 # influence of area on attractivity
proba_attract_ad2=attrac_ad2/sum(attrac_ad2) #proba of attractivity du site fonction distance at aire
sum(proba_attract_ad2)
proba_dispersion2=proba_attract_ad2*(1-h)

## FIGURE 2 B ##
plot(proba_dispersion~distance, ylim=c(0,0.1),pch=19,cex=1.3, xlab="Distance from donnor population (km)",ylab="Dispersal probability")
lines(proba_dispersion~distance)
points(proba_dispersion2~distance,pch=17,cex=1.3)
lines(proba_dispersion2~distance, lty=2)
legend("topright",cex=1,title="Recipient populations",legend=c("Equal size","Variable size"),col="black",pch=c(19,17), border=NA, bty='n',lty=c(1,2))


