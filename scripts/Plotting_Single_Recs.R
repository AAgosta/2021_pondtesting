
####################took code from PondTest_FunctionUpdate:

library(tidyverse)
library(lubridate)
library(here)
library(ggrepel)
# looking at just single files

# custom version of floor() function to use with decimal places

floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)

# function to calculate the number of bins to split the data into
# uses the custom floor_dec function above to avoid weird empty bins 
# that might affect median/mean calculations for plot
bins <- function(start, end){
  start <- ymd_hms(start)
  end <- ymd_hms(end)
  time.elapsed <- floor_dec(round(difftime(end, start), 2)) # elapsed time of pond test
  floor(as.numeric(time.elapsed)*24*4) # number of days * 24 hours * 4 15-min bins in an hour
}

# 2019-20 start and end times
bins("2019-02-08 6:57:44", "2019-06-18 21:20:00")
# for TC use 12528 bins 

# make beacon tag list 
testtag <- c("F7AA") # For TC2 rec

# set up empty dataframe to hold median # of dets per bin
med_dets_bin <-data.frame(matrix(NA, nrow = length(list.files("./raw")), 
                                 ncol = 2))
# dummy variable for filling empty DF
g <- 0


# loop to load all of the data, manipulate it for plotting and then calculate the 
# median number of detections per bin for each receiver and save that value to a separate data frame
for(i in list.files("./raw")){
  dat <- read.csv(paste0("./raw/", i), header=F)        #read in each file
  names(dat)<- c("Filename", "RecSN", "DT", "FracSec", "Hex", "CRC", "validFlag", "TagAmp", "NBW") #rename columns
  last <- nrow(dat)
  if (mdy_hms(dat$DT[last]) < "2019-01-01 08:00:00"){
    write.csv(dat, paste0("data_output/bad_files/",dat$RecSN[1], ".jst"))
  } else {
    dat <-  dat %>% 
      mutate(DT = mdy_hms(dat$DT)) %>% 
      filter(DT > "2019-01-14 16:00:00" & DT < "2019-06-19 22:00:00" )
    dat<- dat[dat$Hex %in% testtag, ]
    dat <- data.frame(dat, cuts = cut(dat$DT, breaks = "15 min", labels = F))
    print("here")
    
    # new df with the 15 bin and count of dets in that bin
    z <- dat%>%
      group_by(RecSN, cuts)%>%
      summarise(N = n(),DTmax = max(DT))
    
    # make new df with zero values and plot that
    x <- data.frame("cuts" = seq(1:12528), Freq = rep(NA, 12528))
    y <- left_join(x, z)
    y$N[is.na(y$N)] <- 0
    
    g <- g + 1
    med_dets_bin[g, ] <- c(as.character(dat$RecSN[1]),  median(y$N))
    
    # save the summarized data for easy plotting later on (eventually splitting into two loops until i find a better way to implement)
    varname <- paste0("SN", dat$RecSN[1])
    #assign(varname, y)
    saveRDS(y, paste0("data_output/for_plots_TC/", varname, ".rds")) # i changed the folder name here for the TC 
  }
}


for(i in list.files("data_output/for_plots_TC")){
  dat <- readRDS(paste0("data_output/for_plots_TC/", i))        #read in each file
  rec.plot <- ggplot() + 
    geom_bar(data = dat, mapping = aes(x = cuts, y = N),stat = "identity") + 
    geom_hline(yintercept = mean(as.numeric(med_dets_bin$X2), na.rm = T), color = "coral") + 
    geom_hline(yintercept = mean(as.numeric(med_dets_bin$X2), na.rm = T)-2*sd(as.numeric(med_dets_bin$X2), na.rm = T), 
               color = "coral", lty = 2) + 
    geom_hline(yintercept = mean(as.numeric(med_dets_bin$X2), na.rm = T)+2*sd(as.numeric(med_dets_bin$X2), na.rm = T), 
               color = "coral", lty = 2) + 
    geom_label_repel(aes(x = 0, y = mean(as.numeric(med_dets_bin$X2), na.rm = T),
                         label = "Overall Med. Dets per Bin")) +
    geom_hline(yintercept = median(dat$N), color = "dodgerblue") + 
    geom_label_repel(aes(x = 0, y = median(dat$N),label = paste0("Med. # of Dets for Rec ", dat$RecSN[1]))) +
    scale_x_continuous(breaks = y$cuts[seq(0, nrow(y), 500)], labels=as.Date(y$DTmax)[seq(0, nrow(y), 500)]) +
    theme_classic() + 
    ggsave(paste0("plots/", dat$RecSN[1], "_plot.pdf"))
}

# hand crank the median number of dets:
sn20176001 <- `SN2017-6001`
median(sn20176001$N) # yea so the median detection is 3 in each 15 min interval. which isn't great. 
sum(sn20176001$N)/nrow(sn20176001)
3/15
