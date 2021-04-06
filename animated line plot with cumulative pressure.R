require(readxl); # read excel files
require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(reshape2); # convert data into wide and long format
require(magrittr) # pipe and related stuff
#install.packages('gganimate')
require(gganimate)

setwd('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/fermentations curves/ankom/')
source('../../summarySE.R')
#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions


ankom <- readxl::read_xls('../1_beer vs wine w ANKOM.xls',sheet = 1,col_names = T) # read the procossed ankom excel from teh first sheet
a <-  c("E1","CBS7001","WS3470")
e<-  c("E1","WS3470","CBS7001")
b <- c("beer-wort","wine-must")
ba <- "Ctrl"
d <- c('I', 'II', 'III')
names(ankom) <-
c('Date_Time',2,3, # first 3 column rep date/time, 2 and 3
  paste(rep(a,each=3),d,b[1],sep="_"), # beer wort reps with the 3 samples
  paste(ba,d[1],b[1],sep="_"), # add the beer wort negative control
  paste(rep(e,each=3),d,b[2],sep="_"), # wine must reps with the 3 samples
  paste(ba,d[1],b[2],sep="_"), # add the wine must negative control
 24:51)

not_all_na <- function(x) {all(!is.na(x))} # drop all column with no data function
ankom %<>% select_if(not_all_na)
ankom <-   ankom[-1,-c(2,7,12,15)] # drop extra naming first row and no info second column 
 
## convert all column into numeric column except the first column
ankom[,-1] <- apply(ankom[,-1],2,as.numeric)

ankom %>%  reshape2::melt(id="Date_Time") ->tall_ankom # using the first 2 column as the identifiers

tall_ankom %<>% tidyr::separate(variable, into=c('sample','rep','substate'),sep="_")

#the function to make our life easier

#apply our new function on our data, using the samples and variable, and use the value as input. This collapses the rep column
summarySE(tall_ankom,groupvars = c("Date_Time", "sample","substate"), measurevar = 'value') -> ankom_stat

# turn all the name column into factor
ankom_stat$sample <- factor(ankom_stat$sample, levels = c("E1","CBS7001","WS3470","Ctrl"))
ankom_stat$substate <- factor(ankom_stat$substate, levels = c("beer-wort","wine-must"))

#plot of standard error of mean

p <- ggplot(ankom_stat, aes(x=Date_Time, y=value, colour=sample,group=sample)) # absolute pressure
p <- ggplot(ankom_stat, aes(x=Date_Time, y=cumsum(value), colour=sample,group=sample)) # cumulative pressure plot
#p <- ggplot(ankom_stat, aes(x=Date_Time, y=log2(cumsum(value)), colour=sample,group=sample)) #log transformed cumulative pressure
p <- ggplot(ankom_stat, aes(x=Date_Time, y=log2(cumsum(value)), colour=sample,group=sample))
p+ geom_line() + geom_point()+facet_grid(substate~sample)

p+ # geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1) +
  geom_line(size=1) +
  #geom_point()+
 facet_grid(~substate)#+
  transition_reveal(Date_Time)

# plot of
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.05) # move them .05 to the left and right

#mother of all plots
p +
  #  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1, position=pd) +
  # add error bars based on the value and standard deviation, with the width of the error bar tips as 0.1, black and slightly dodge each other
  geom_line(position=pd, size=1) +
  # add line plot of 1 mm thickness, dodging other lines slightly
  #geom_point() +

  facet_grid(~substate)+  # add a scatter plot, with white filled circles
  xlab("Days") +
  # change the label of x from variable to relevant name
  ylab("Absolute Pressure") +
  # change the label of y from value to relevant name
  ggtitle("Plot of pressure change\n in incubated samples") +
          # Set tick every 0.5
  theme_bw() + # change the theme to be black and white
  scale_colour_manual(
    name="Samples", values = c("E1"='red',"WS3470"='blue',"CBS7001"='darkgreen', "Ctrl"= 'grey'), # sample color scheme
        guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
       plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black",angle = 10,hjust = 1, colour = "grey50"))# +
  transition_reveal(Date_Time)

#ggsave('Plot of  pressure generated over time.png') # save the resulting plot as a png
#ggsave('Plot of pressure generated over time.pdf') # save the resulting plot as a pdf
#anim_save("animated_line_plot_log_transformed_cumulative_pressure_change.gif")
#anim_save("animated_line_plot_with_points.gif")

# 