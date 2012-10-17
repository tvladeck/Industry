library("ggplot2")
library("reshape")
library("RColorBrewer")

IAC.Assessments <- read.csv("U:/Global Product/USA/Industry/IAC/IAC Assessments.csv")


years <- 1987:2012
assessments.by.year <- data.frame("Years"=years)


for (i in (1:26)) {
  year <- assessments.by.year[i, 1]
  number.of.assessments <- nrow(subset(IAC.Assessments, FY==year))
  assessments.by.year$number[i] <- number.of.assessments
}

assessments.graph <- ggplot(assessments.by.year,
                            aes(x=Years,y=number)) + geom_line() + aes(ymin=0) +
                            scale_y_continuous("") + scale_x_continuous("") + 
                            opts(title="Number of Energy Audits Performed Through Industrial Assessment Centers 1987-2012")