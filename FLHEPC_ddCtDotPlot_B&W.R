#Change Cq File Biological Set Names to IS-MD, IS-PAE, etc. before beginning
#Average together any Cq values from same sample (leaving only biological replicates, no technical replicates)
#Copy and paste correct upper and lower bounds, and means to numbers (ex. all IS-0 get same upper, lower, and mean)
#Delete standard error for each sample
rm(list=ls())
library(ggplot2)
library(dplyr)
library(extrafont)
loadfonts(device = "win")
setwd("C:/Users/khelfri/Documents/KayleeStuff/Smith_Lab/Data/Rat_Iron/Hepcidin/FetalLiverHepcidin/Analysis")

#reads a file in a table format and creates a data frame from it
datafile <- "Combined_FL_Hepcidin_Cqs.csv"
data1 <- read.csv(datafile, header=TRUE) 
dataframe_Original <- data.frame(data1)

#write in the calibrator sample mean from the average of your Ct calculations for your calibrator sample
IS_MD_mean <-  0.424666907777778

#this function is the calculation for the new Ct's that have been compared to IS-0
Function1 <- function(Ct) {
  FUN <- ((2^-(Ct - IS_MD_mean))*100)
  return(FUN)
}
#this function adjusts the lower, upper, and mean values
Function2 <- function(value) {
  FUN <- (value*100)
  return(FUN)
}
#for each sample, this subtracts the calibrator sample mean, and calculates the 2^-ddCt, and adjusts lower, upper, and mean
Adj_Ct <- sapply(dataframe_Original$Cq.Averages, match.fun("Function1"), USE.NAMES = TRUE)
Adj_lower <- sapply(dataframe_Original$lower, match.fun("Function2"), USE.NAMES = TRUE)
Adj_upper <- sapply(dataframe_Original$upper, match.fun("Function2"), USE.NAMES = TRUE)
Adj_mean <- sapply(dataframe_Original$mean, match.fun("Function2"), USE.NAMES = TRUE)

#convert values from above into individual dataframes
dataframe_AdjCtCalcs <- data.frame(Adj_Ct)
dataframe_AdjLower <- data.frame(Adj_lower)
dataframe_AdjUpper <- data.frame(Adj_upper)
dataframe_AdjMean <- data.frame(Adj_mean)

#creates one final dataframe
dataframe_AdjCtCalcs$Biological.Sets<-dataframe_Original$Biological.Sets #adds the Biological.Sets labels back to the dataframe
dataframe_AdjCtCalcs$Sample.IDs<-dataframe_Original$Sample.IDs #adds the sample IDs back to the dataframe
dataframe_AdjCtCalcs$lower<-dataframe_AdjLower$Adj_lower #adds in adjusted lower error
dataframe_AdjCtCalcs$upper<-dataframe_AdjUpper$Adj_upper #adds in adjusted upper error
dataframe_AdjCtCalcs$mean<-dataframe_AdjMean$Adj_mean #adds in adjusted mean
dataframe_AdjCtCalcs$Alcohol<-dataframe_Original$Alcohol
dataframe_AdjCtCalcs$Iron<-dataframe_Original$Iron
dataframe_AdjCtCalcs$Sex<-dataframe_Original$Sex
dataframe_AdjCtCalcs <- dataframe_AdjCtCalcs[,c(2,3,1,4,5,6,7,8,9)] #rearranges the columns so Biological.Sets is first

Males <- dataframe_AdjCtCalcs[ which(dataframe_AdjCtCalcs$Sex=='Male'), ]
Males <- with(Males,  Males[order(Biological.Sets) , ])
Males <- data.frame(Males)
Females <- dataframe_AdjCtCalcs[ which(dataframe_AdjCtCalcs$Sex=='Female'), ]
Females <- with(Females,  Females[order(Biological.Sets) , ])
Females <- data.frame(Females)

sets <- c("CON", "CON+Iron", "ALC", "ALC+Iron")

df <- (data.frame(rep(sets, c(9,8,9,9))))
colnames(df)[1] <- "Group" 
df <- with(df,  df[order(Group) , ])
df <- data.frame(df)

Males <- cbind(Males, df)
Females <- cbind(Females, df)

#Males
png("FetLiv_Hepc_ddCtDotPlot_Males_B&W.png", units="in", width=5, height=7, res=600)

sets <- c("CON", "ALC", "CON+Iron", "ALC+Iron")

p<- ggplot(Males, aes(x=df, y=mean)) + 
  geom_bar(stat="identity", color="black", aes(fill = df), position=position_dodge()) +
  scale_fill_manual(values=c("CON" = "grey100",
                             "ALC" = "grey51",
                             "CON+Iron" = "grey75",
                             "ALC+Iron" = "grey30"
                             )) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) + 
  geom_point(aes(x=df, y=Adj_Ct),
             size = 1.25, shape = 19, position = position_jitter(width=.15, height=0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,600)) +
  ggtitle("Fetal Liver Hepcidin- Males") +
  ylab(expression("Hepcidin expression relative to CON")) + 
  theme(plot.title = element_text(hjust = 0.5, size=18, family = "Times New Roman"), 
        axis.title.x = element_blank(), #gets rid of x-axis label
        axis.title.y = element_text(size = 20, family = "Times New Roman"),
        panel.grid.major = element_blank(), #gets rid of major gridlines
        panel.grid.minor = element_blank(), #gets rid of minor gridlines
        panel.background = element_blank(), #turns background white instead of gray
        axis.line = element_line(colour = "black"), 
        legend.position="none", #gets rid of legend
        axis.text=element_text(size=22, color = "black", family = "Times New Roman"), #sets size of x and y axis labels
        axis.text.x=element_text(angle = 45, vjust = .95, hjust = 0.9, family = "Times New Roman"),
        axis.title=element_text(size=22,face="bold", family = "Times New Roman")) + 
  scale_x_discrete (limits = sets)
print(p)
dev.off() 

#Females
png("FetLiv_Hepc_ddCtDotPlot_Females_B&W.png", units="in", width=5, height=7, res=600)

sets <- c("CON", "ALC", "CON+Iron", "ALC+Iron")

p<- ggplot(Females, aes(x=df, y=mean)) + 
  geom_bar(stat="identity", color="black", aes(fill = df), position=position_dodge()) +
  scale_fill_manual(values=c("CON" = "grey100",
                             "ALC" = "grey51",
                             "CON+Iron" = "grey75",
                             "ALC+Iron" = "grey30"
                             )) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) + 
  geom_point(aes(x=df, y=Adj_Ct),
             size = 1.25, shape = 19, position = position_jitter(width=.15, height=0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,600)) +
  ggtitle("Fetal Liver Hepcidin- Females") +
  ylab(expression("Hepcidin expression relative to CON")) + 
  theme(plot.title = element_text(hjust = 0.5, size=18, family = "Times New Roman"), 
        axis.title.x = element_blank(), #gets rid of x-axis label
        axis.title.y = element_text(size = 20, family = "Times New Roman"),
        panel.grid.major = element_blank(), #gets rid of major gridlines
        panel.grid.minor = element_blank(), #gets rid of minor gridlines
        panel.background = element_blank(), #turns background white instead of gray
        axis.line = element_line(colour = "black"), 
        legend.position="none", #gets rid of legend
        axis.text=element_text(size=22, color = "black", family = "Times New Roman"), #sets size of x and y axis labels
        axis.text.x=element_text(angle = 45, vjust = .95, hjust = 0.9, family = "Times New Roman"),
        axis.title=element_text(size=22,face="bold", family = "Times New Roman")) + 
  scale_x_discrete (limits = sets)
print(p)
dev.off() 
