#install.packages("ggplot2")
#install.packages("reshape")
#install.packages("RColorBrewer")
library("ggplot2")
library("reshape")
library("RColorBrewer")


Percent.Energy.MGMT <- read.csv(
  "U:/Global Product/USA/Industry/RData - Percent of Facilities using Energy MGMT.csv"
  )

colnames(Percent.Energy.MGMT)[1] <- "Years"

Percent.Cogen <- read.csv("U:/Global Product/USA/Industry/RData - Percent of Facilities with Cogen.csv")

Percent.Energy.Tech <- read.csv("U:/Global Product/USA/Industry/RData - Percent of Facilities with Energy Technologies.csv")
colnames(Percent.Energy.Tech)[1:2] <- c("Years", "Sector")

Percent.Energy.Tech <- melt(Percent.Energy.Tech, id=c("Years","Sector"))
Percent.Energy.MGMT <- melt(Percent.Energy.MGMT, id="Years")
Percent.Cogen <- melt(Percent.Cogen, id="Year")

## Energy Management

c1 <- ggplot(na.omit(Percent.Energy.MGMT), aes(x=variable,y=value ,fill=variable)) + 
        geom_bar(width=0.7, position = position_dodge(width=2)) + facet_grid(~Years) + 
        scale_x_discrete("variable",expand=c(0.0001,0.55)) + 
        scale_y_continuous("value",breaks=c(0,0.25,0.5),labels=c("0","25%","50%")) +       
        coord_flip()


## Cogen
c2 <- ggplot(Percent.Cogen, aes(x=Year,y=value,colour=variable))+geom_line()+facet_wrap(~variable,ncol=2)


## Energy Tech

Percent.Energy.Tech$value2 <- as.numeric(
                                    as.character(
                                        gsub(
                                            pattern="%",
                                            replacement="",
                                            Percent.Energy.Tech$value
                                             )))

dat <- data.frame(x = rnorm(1000), y = rnorm(1000), z = sample(0:40, 1000, TRUE))
my.col <- colorRampPalette(brewer.pal(11, "RdBu"))(diff(range(dat$z)))


c3 <- ggplot(na.omit(Percent.Energy.Tech), aes(x=Sector,y=value2,fill=Sector)) + 
        geom_bar(width=0.7, position = position_dodge(width=2)) + 
        facet_grid(Years ~ variable) + 
        scale_x_discrete("Sector",expand=c(0.0001,0.55)) + 
        scale_y_continuous("value2",breaks=c(0,10,20,30,40,50)) + coord_flip() 


c4 <- ggplot(na.omit(Percent.Cogen), aes(x=variable,y=value,fill=variable)) + 
        geom_bar(width=0.7, position = position_dodge(width=2)) + 
        facet_grid(~Year) + 
        scale_x_discrete("Sector",expand=c(0.0001,0.55)) + 
        scale_y_continuous("value2",breaks=c(0,10,20,30,40,50)) + coord_flip()







# Percent.Computer.Control.Building  <- melt(cbind(Percent.Energy.Tech[, 1:3], "Type"="Technology"), id=c("Years","Sector","Type"))
# Percent.Computer.Control.PandE     <- melt(cbind(Percent.Energy.Tech[, 1:2], 
#                                                  "Computer Control of Processes and Equipment"=Percent.Energy.Tech[, 4],
#                                                  "Type"="Technology"
#                                                  ), 
#                                           id=c("Years","Sector","Type")    )
# Percent.WHR                        <- melt(cbind(Percent.Energy.Tech[, 1:2], 
#                                                  "WHR"=Percent.Energy.Tech[, 5],
#                                                  "Type"="Technology"
#                                                  ), 
#                                            id=c("Years","Sector","Type"))
# Percent.Adjustable.Motors          <- melt(cbind(Percent.Energy.Tech[, 1:2], 
#                                                  "Adjustable Speed Motors"=Percent.Energy.Tech[, 6],
#                                                  "Type"="Technology"
#                                                  ), 
#                                            id=c("Years","Sector","Type"))
# Percent.Oxy.Fuel.Firing            <- melt(cbind(Percent.Energy.Tech[, 1:2], 
#                                                  "Oxy Fuel Firing"=Percent.Energy.Tech[, 7],
#                                                  "Type"="Technology"
#                                                  ), 
#                                            id=c("Years","Sector"))