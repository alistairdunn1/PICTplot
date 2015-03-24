mean(data$month)library(PICTplot)
pp(xlim=c(180, 190), ylim = c(-14,-26),fill="green")
pp.depth(100,col="gray40")
pp.depth(250,col="gray40")
pp.depth(500,col="gray40")
pp.eez()
box()

library(PICTplot)
pp(xlim=c(180, 187), ylim = c(-19,-27),fill="green")
rect(par()$usr[2],par()$usr[1],par()$usr[4],par()$usr[3],col="gray95")


pp.polygon(m10000$long,m7000$lat,col="gray92",border=NA)
pp.polygon(m5000$long,m5000$lat,col="gray90",border=NA)
pp.polygon(m4000$long,m4000$lat,col="gray85",border=NA)
pp.polygon(m3000$long,m3000$lat,col="gray80",border=NA)
pp.polygon(m2000$long,m2000$lat,col="gray75",border=NA)
pp.polygon(m1000$long,m1000$lat,col="gray70",border=NA)
pp.polygon(m750$long,m750$lat,col="gray65",border=NA)
pp.polygon(m500$long,m500$lat,col="gray60",border=NA)
pp.polygon(m250$long,m250$lat,col="gray55",border=NA)
pp.polygon(m100$long,m100$lat,col="gray50",border=NA)
pp.polygon(pp.coast$long,pp.coast$lat,col="green")
pp.eez()
box()



