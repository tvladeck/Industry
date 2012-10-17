#install.packages("ggplot2")
#install.packages("reshape")
#install.packages("RColorBrewer")
library("ggplot2")
library("reshape")
library("RColorBrewer")


# Data: Deflated Industrial GDP
# Source: BEA
# URL: http://www.bea.gov/industry/gdpbyind_data.htm
# Industry value-add, deflated
# Data is in millions of 2005 Dollars
Deflated.Value.Add <- read.csv("U:/Global Product/USA/Industry/BEA/Deflated Value Add.csv")


Industrial.Emissions <- read.csv("U:/Global Product/USA/Industry/EIA/RData - Industrial Emissions.csv")

Deflated.Value.Add$Total <- rowSums(Deflated.Value.Add[, -1]) / 1000000 ## Trillions USD
Deflated.Value.Add <- Deflated.Value.Add[, c(1, 5)]

colnames(Industrial.Emissions) <- c("Year","Total")
colnames(Deflated.Value.Add) <- c("Year","Total")

Industrial.Emissions$type <- "Emissions"
Deflated.Value.Add$type <- "Production"

Graph.1.Data <- rbind(Industrial.Emissions, Deflated.Value.Add)

Graph.1 <- ggplot(Graph.1.Data, aes(x=Year,y=Total,colour=type)) + geom_line() +
            facet_wrap(~type,ncol=1,scale="free_y") + scale_y_continuous("") +
            scale_x_continuous("") + scale_colour_hue(name="") + aes(ymin=0) +
            opts(title="Emissions and GDP (2005 USD) of Industry 1980-2010") + guides(colour=FALSE)
                                                      