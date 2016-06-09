source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data.R")

#Voltage plot function
# v is voltage data propagated from data cleaning function (pit_dat)

#TO DO
# Need to fix width, height and par so it changes depending on number of unique readers


fig_name<-paste(path_to_folder,"pit_volt_plot",".png")
png(fig_name, height=1200, width=1200)
par(mfrow=c(length(unique(v$reader)),1), mar=c(1.5,1.5,1,1.5), oma=c(4,4,0,0), cex=1.5)
v2<- dplyr::arrange(v,datetime)
d_ply(v2, c("reader"), function(dat){
  plot(volt ~ datetime, data = dat,
       ylim = c(min(dat$volt)-0.5, max(dat$volt)+0.5),
       xlim=c(min(dat$datetime), max(dat$datetime)),
       type = "b",
       axes = FALSE)
  x_range<- seq(min(as.Date(dat$date)), max(as.Date(dat$date)), by = "days")
  r <- range(dat$datetime)
  axis.POSIXct(1, at = seq(r[1], r[2], by = "weeks"), format = "%b %d", cex.axis = 1.35)
  axis(side=2, col="black")
  mtext(dat$reader[1], side=3, adj=0.02, line=-1, cex=1.5)
  box()
})
dev.off()

