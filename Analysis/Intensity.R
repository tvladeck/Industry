library("reshape")
library("hash")
library("ggplot2")

# Industrial Production Index
# information: http://www.federalreserve.gov/releases/g17/table1_2.htm
# actual data: http://www.federalreserve.gov/releases/g17/ipdisk/ip_sa.txt
# seasonally adjusted
IPI <- read.table(
  "U:/Global Product/USA/Industry/Federal Reserve/Industrial_Production_1986_2012_SA.txt", 
  quote="\"", 
  fill=T
)

colnames(IPI) <- c("Sector", "Year", 1:12)

# Sources: Annual Energy Outlooks
# Years listed in "Source" column
# Units listed in "Units" Column

Sectoral.Emissions <- read.csv("U:/Global Product/USA/Industry/EIA/Sectoral Emissions.csv")

Adjust.Emissions <- function(number, unit) {
  atomic.weight.carbon = 12.017
  atomic.weight.carbon.dioxide = 44.0095
  ratio = atomic.weight.carbon.dioxide / atomic.weight.carbon
  
  if (unit == "MMT") {
    return(number)
  } 
  else if (unit == "MMTCE") {
    return(ratio * number)  
  }
  
}

Sectoral.Emissions$MMT <- mapply(Adjust.Emissions, 
                                 Sectoral.Emissions$Carbon.Emissions, 
                                 Sectoral.Emissions$Units)

# NAICS Categories of the Sectors I'm looking at. 
Categories <- hash()
.set(
  Categories,
  "Aluminum" = 3313,
  "Chemicals" = 325,
  "Cement" = 3273,
  "Food" = 311,
  "Glass" = 3272,
  "Iron and Steel" = c(3311, 3312),
  "Paper" = 322,
  "Refining" = 32411  
)


Indices <- hash()
.set(
  Indices,
  "Aluminum" = "G3313",
  "Chemicals" = "G325",
  "Cement" = "G3273",
  "Food" = "G311",
  "Glass" = "G3272",
  "Iron and Steel" = "G3311A2",
  "Paper" = "G322",
  "Refining" = "G32411"    
)

Index.by.Sector <- function(category)
{
  
  df <- subset(IPI, Sector == Indices[[category]])
  
  Average.Production.Index <- function(year)
  {
    row <- subset(df, Year == year)
    if(year != 2012)
    {
      index <- rowMeans(row[3:14]) 
    } else
    {
      index <- rowMeans(row[3:11])
    }
    
    return(index)
  }
  
  Production.Index <- data.frame(
    "Year" = 1998:2011,
    "Index" = sapply(1998:2011, Average.Production.Index)
  )
  
  return(Production.Index)
  
}

Efficiency.Index.by.Sector <- function(category)
{
  series <- subset(Sectoral.Emissions, Sector == category)$MMT / Index.by.Sector(category)$Index
  index <- series / series[1]
  return(index)
}

Efficiency.Indices <- function()
{
  df <- data.frame(
  "Year" = 1998:2011
  )
  for (i in keys(Indices))
  {
    df[[i]] = Efficiency.Index.by.Sector(i)    
  }
  df <- subset(subset(df, Year != 2002), Year!=2008) ## have to remove these years due to changes in sources
  df <- melt(df, id="Year")
  return(df)  
}

Create.Emissions.Index.Plot <- function()
{
  df <- Efficiency.Indices()
  plot <- ggplot(df, aes(x=Year, y=value, color=variable)) + geom_line()
  return(plot)
}













