rm(list=ls())
library(ggplot2)
library(ggpubr)
setwd("~/KayleeStuff/Smith_Lab/Data/Rat_Iron/Correlations/FetHepcLiverIron")
Data_file <- "FL_Hepcidin_Iron_ForCorrelation.csv"

#reads a file in a table format and creates a data frame from it
data <- read.csv(Data_file, header=TRUE) 
data_frame <- data.frame(data)

#initialize a high resolution graph
png("HEPC_MatvsFetLiv_Correlation_CtValue.png", units="in", width=7, height=7, res=600) 

#Scatter plot with correlation analyses
sp <- ggscatter(data_frame, x = "Cq.Averages", y = "Total.Iron", #creates graph
                color = "Biological.Sets", size = 3, #sets color and shape based on pre-set variables
                #palette = c("black", "gray"), #sets color of points
                add = "reg.line", #adds a regression line
                add.params = list(color = "black"), #adds color of regression line
                title = "Correlation of Fetal Liver Hepcidin and Nonheme Iron",  #sets main graph title
                xlab = "Ct of Fetal Liver Hepcidin", ylab = "Nonheme Iron", #sets x-axis and y-axis labels
                legend = "right", 
                ellipse = FALSE) + #when true, adds an ellipse around the points
    stat_cor(method = "pearson", #uses Pearson correlation to analyze correlation
              label.x.npc = "middle", label.y.npc = "top", size = 6) #moves correlation numbers and label to desired location and size
print(sp)

#release PNG file graph into directory
dev.off() 

#adjust p values to account for multiple hypothesis testing
pvals <- c(0.19, 0.43, 0.54, 0.35)
p.adjust(pvals, method="BH")