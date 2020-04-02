mta_by_eco.df<-read.csv("Tables/mta_by_eco.csv")
mta_by_eco.df$delta_2C<-mta_by_eco.df$mta_2C - mta_by_eco.df$mta_now
mta_by_eco.df$delta_4C<-mta_by_eco.df$mta_4C - mta_by_eco.df$mta_now
centr_now.df<-read.csv("Tables/ecoregion_centr_now.csv")

plot.df<-merge(mta_by_eco.df, centr_now.df, by="ECO_ID")

pdf("Graphs/deltaMTAbyLat.pdf", 20, 10)
par(mfrow=c(1,2))
plot(abs(plot.df$y_now), plot.df$delta_2C, pch=20, xlab="abs(ecoregion latitude)", ylab="mta_2C-mta_now", main="+2C")
plot(abs(plot.df$y_now), plot.df$delta_4C, pch=20, xlab="abs(ecoregion latitude)", ylab="mta_4C-mta_now", main="+4C")
dev.off()

pdf("Graphs/deltaMTAbyLatFull.pdf", 20, 10)
par(mfrow=c(1,2))
plot(plot.df$y_now, plot.df$delta_2C, pch=20, xlab="ecoregion latitude)", ylab="mta_2C-mta_now", main="+2C")
plot(plot.df$y_now, plot.df$delta_4C, pch=20, xlab="ecoregion latitude", ylab="mta_4C-mta_now", main="+4C")
dev.off()
