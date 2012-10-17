#install.packages("ggplot2")
#install.packages("reshape")
#install.packages("RColorBrewer")
library("ggplot2")
library("reshape")
library("RColorBrewer")

strip = function(df,tech){
  ##strips the data frames of unnecessary columns
  newdf <- data.frame(cbind(df[, 1], 
             df[, 5], 
             df[, 6]))
  newdf$technology <- tech
  colnames(newdf) <- c("id", "start.year", "end.year", "tech")
  newdf
  }

Convert.To.Year = function(df){
  start <- sapply(2001:2010, function(x){nrow(subset(df, start.year==x))})
  end   <- sapply(2001:2010, function(x){nrow(subset(df, end.year==x))})
  start - end
  }

CHP.incentives <- read.csv("U:/Global Product/USA/Industry/DSIRE/RData - CHP Incentives.csv")
CHP.incentives <- Convert.To.Year(strip(CHP.incentives, "CHP"))

compressed.air.incentives <- read.csv("U:/Global Product/USA/Industry/DSIRE/RData - Compressed Air Incentives.csv")
compressed.air.incentives <- Convert.To.Year(strip(compressed.air.incentives, "Compressed Air"))

heat.recovery.incentives <- read.csv("U:/Global Product/USA/Industry/DSIRE/RData - Heat Recovery Incentives.csv")
heat.recovery.incentives <- Convert.To.Year(strip(heat.recovery.incentives, "Heat Recovery"))


motor.incentives <- read.csv("U:/Global Product/USA/Industry/DSIRE/RData - Motor Incentives.csv")
motor.incentives <- Convert.To.Year(strip(motor.incentives, "Motor Incentives"))

equipment.incentives <- read.csv("U:/Global Product/USA/Industry/DSIRE/RData - Processing and Mftg Equipment Incentives.csv")
equipment.incentives <- Convert.To.Year(strip(equipment.incentives, "Equipment Incentives"))

steam.upgrade.incentives <- read.csv("U:/Global Product/USA/Industry/DSIRE/RData - Steam System Upgrade Incentives.csv")
steam.upgrade.incentives <- Convert.To.Year(strip(steam.upgrade.incentives, "Steam"))

vsd.motor.incentives <- read.csv("U:/Global Product/USA/Industry/DSIRE/RData - VSD Motor Incentives.csv")
vsd.motor.incentives <- Convert.To.Year(strip(vsd.motor.incentives, "VSD Motor"))


master.tech.df <- data.frame(
                        "Years" = 2001:2010,
                        "CHP" = CHP.incentives, 
                        "Compressed Air" = compressed.air.incentives, 
                        "Heat Recovery" = heat.recovery.incentives,
                        "Motors" = motor.incentives,
                        "Mftg Equipment" = equipment.incentives,
                        "Steam" = steam.upgrade.incentives,
                        "VSD Motors" = vsd.motor.incentives)

number.over.time <- t(sapply(1:10, function(x){colSums(master.tech.df[1:x, -1])}))
rownames(number.over.time) <- 2001:2010
number.over.time <- melt(number.over.time)

policies.graph <- ggplot(number.over.time, aes(x=X1, y=value, colour=X2)) +
                    geom_line() + facet_wrap(~X2,ncol=1)

# 
# master.tech.df <- melt(master.tech.df, id=c("id", "start.year", "end.year"))
# 
# years.boolean <- sapply(2001:2012, function(x){
#                                       sapply(master.tech.df[, 2], function(i, j) if (i<j) TRUE else FALSE, j=x)
#                                     })
# 
# colnames(years.boolean) <- sapply(2001:2012, function(x){as.character(x)})
#master.tech.df <- cbind(master.tech.df, years.boolean)



