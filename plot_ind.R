#TO DO:
#INTEGRATE CODE THAT ALLOWS USER TO SPECIFY TEMPORAL RESOLUTION (# BY HOUR, DAY, WEEK,MONTH,YEAR,ALL)

source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data.R")

#Creates a detection history plot for each PIT-tagged fish

#Function has to assign a station number to each station name
#Still need to incorporate this next line of code
all_det$reader_no <- ifelse(all_det$reader == "bridge_counter", 1, ifelse(all_det$reader == "bridge_reach34_ant1", 2, 3))

plot_ind <- function(x){

    pdf(file = "~/Dropbox (InStream)/Projects/62 - PIT R & D/4 - Figures & Tables/ind_tracks.pdf", width = 7.25, height = 10)
    par(mfrow = c(4, 2), oma = c(2,3,1,1), mar = c(1.5,1.5,2,1.5))

    d_ply(all_det, c("tag_code"), function(y){
  
    yy <- y[order(y$date_time),]
    r <- range(yy$date_time)
    
    plot(date_time ~ reader_no, data = yy,
         xlim = c(1, length(unique(all_det$reader))),
         ylim = c(min(yy$date_time), max(yy$date_time)),
         type = "b",
         axes = FALSE)

    axis.POSIXct(2, at = seq(r[1], r[2], by = "week"), 
                format = "%b %d", 
                cex.axis = 0.75)
    
    axis(1, at = c(1, 2, 3), labels = c("Counter", "R34-A1", "R34-A2"), col = "black")
    
    mtext(paste("PIT ID: ", yy$tag_code[1]), 
          side = 3, 
          adj = 0.05, 
          cex = 0.5, 
          line = 0)
    
    box()
    
  })
    
dev.off()

}

plot_ind(filter(all_det))