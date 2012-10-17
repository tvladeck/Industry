#install.packages("ggplot2")
#install.packages("reshape")
#install.packages("RColorBrewer")
library("ggplot2")
library("reshape")
library("RColorBrewer")

#####PRE-1998 DSM DATA################
# http://www.eia.gov/totalenergy/data/annual/showtext.cfm?t=ptb0813#
# Data is in nominal dollars
DSM.pre.1998 <- data.frame("Thousand.Dollars"=c(872935, 1177457, 1803773, 2348094, 2743533, 2715657, 2421284, 1902197, 1636020, 1420920),
                           "Years"=1989:1998)

####Import Data on DSM Spending####

DSM.Spending <- read.delim("U:/Global Product/USA/Industry/EIA/DSM Spending/DSM Spending.txt")

DSM.Spending <- rbind(DSM.Spending, DSM.pre.1998)

DSM.Spending$Billion.Dollars <- DSM.Spending$Thousand.Dollars / 1000000

# Federal Spending Data
# All spending is through the DOE
# From Uday's work. 
# Original Spreadsheet is in Global Product/USA/Cross Cutting/DOE Detail.xls
# Amount is in millions of allocated spending
# This is a superset of all federal spending that might have gone towards energy efficiency
# Programs:
#   State Energy Program
#   Weatherization
#   Energy Efficiency Conservation Block Grants
#   Earmarks
#   Miscellanea
Federal.Spending <- melt(read.csv("U:/Global Product/USA/Cross Cutting/Federal Energy Assistance.csv"),
                         id="Year")
Weatherization 


####Make the plot!####

Graph.3 <- ggplot(DSM.Spending, aes(x=Years,y=Billion.Dollars)) + geom_line() +
            aes(ymin=0) + 
            scale_y_continuous("Billion Dollars") + scale_x_continuous("") + 
            opts(title="Utility DSM Spending 1989-2010")

Graph.3
