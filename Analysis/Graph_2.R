#install.packages("ggplot2")
#install.packages("reshape")
#install.packages("RColorBrewer")
library("ggplot2")
library("reshape")
library("RColorBrewer")

Industrial.Changes <- read.csv("U:/Global Product/USA/Industry/EIA/MECS/RData - Industry Graph 2.csv")

Industrial.Changes <- melt(Industrial.Changes, id="Years")

Industrial.Changes$min <- sapply(Industrial.Changes$value, function (x) {min(x, 0)})
Industrial.Changes$max <- sapply(Industrial.Changes$value, function (x) {max(x, 0)})
Industrial.Changes$index <- 0:23 %% 4

graph2 <- ggplot(Industrial.Changes, aes(xmin=index-0.25, xmax=index+0.25,
                                         ymin=min, ymax=max,fill=variable)) + 
          geom_rect() + 
          scale_x_continuous(breaks=c(0,1,2,3),
            labels=c("1994", "1998", "2002", "2006")) +
          scale_fill_hue(name="Technology",breaks=c("Cogeneration",
            "Energy.Management","Computer.Control.of.Building",
            "Computer.Control.of.Equipment...Processes","WHR",
            "Adjustable.Speed.Motors"), labels=c("Cogeneration",
            "Holistic Energy Management", "Automated Control of Building Envirionment","Automated Control of Equipment and Processes","WHR",
            "Adjustable Speed Motors")) +
          facet_wrap(~variable) + opts(title="Percentage Changes in Energy Technology and Practices in Industrial Facilities 1994-2006")
          

graph2 + opts(legend.position=FALSE)
