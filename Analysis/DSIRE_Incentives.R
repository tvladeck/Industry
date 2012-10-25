library("reshape")
library("ggplot2")
library("stringr")

#Incentives <- read.csv("U:/Global Product/USA/Cross Cutting/DSIRE Incentives.txt")
#Incentives <- subset(Incentives, Industrial==1)

dateregex <- "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}"

Extract.Date <- function(string) str_extract(string, dateregex)
Extract.Year <- function(string) as.numeric(format(as.Date(Extract.Date(string), "%m/%d/%Y"), "%Y"))

Expired <- function(status = NA, expiration_date = NA)
{
  if(is.na(status)) status <- "none"
  expr_date <- Extract.Date(expiration_date)
  if(status == "Expired" || status == "Exclude") 
  {
    expired <- T
  } else if (!is.na(expr_date)) 
  {
    expired <- T  
  } else
  {
    expired <- F
  }
  
  expired
}

Years.Active <- function(status = NA, creation_date = NA, date_effective = NA, date_enacted = NA,
                         expiration_date = NA, last_updated = NA)
{
  expired <- Expired(status, expiration_date)
  creation <- Extract.Year(creation_date)
  enacted <- Extract.Year(date_enacted)
  effective <- Extract.Year(date_effective)
  expiration <- Extract.Year(expiration_date)
  updated <- Extract.Year(last_updated)
  
  if(expired)
  {
    if(!is.na(expiration))
    {
      last_year <- expiration
    }
    else
    {
      last_year <- updated
    }
  } else
  {
    last_year <- 2012
  }
  
  if(!is.na(effective))
  {
    first_year <- effective
  } else
  {
    first_year <- creation
  }
  
  if(is.na(first_year) || is.na(last_year)) return(NA)
  
  range <- first_year:last_year
  years <- hash(2000:2012, rep(0, length(2000:2012)))
  for(i in range) years[[as.character(i)]] <- years[[as.character(i)]] + 1
  years <- values(years)
  return(years)
}

Listoyears <- function(df)
{
  val <- mapply(Years.Active, df$Status, df$Creation_Date, df$Date_effective, 
         df$Date_Enacted, df$Expiration_Date, df$Last_Updated)
  val
}
  
Yearstogram <- function(df)
{
  listoyears <- Listoyears(df)
  #iter <- 0
  if(class(listoyears) == "list")
  {
    total <- rep(0, 13)
    names(total) <- 2000:2012
    for(i in 1:length(listoyears))
    {
      #iter <- iter + 1
      if(length(listoyears[[i]]) < 13) next
      if(class(listoyears[[i]]) != "numeric") next
      total <- total + listoyears[[i]] 
    }
  }
  else if(class(listoyears) == "matrix")
  {
    return(rowSums(mat))    
  }
  return(total)
}

tech.categories <- hash()
.set(
  tech.categories,  
  "Solar_Thermal_Process_Heat" = "Solar Thermal Process Heat", ## nothing
  "En_Eff" = "General Energy Efficiency", ## nothing
  "Cogeneration" = "Cogeneration", ## nothing
  "boilers" = "Boilers", ## nothing
  "heat_pumps" = "Heat Pumps",
  "combined_heat_power" = "Combined Heat and Power", ## nothing
  "heat_recovery" = "Heat Recovery",
  "steam_system_upgrades" = "Steam System", 
  "compressed_air" = "Compressed Air",
  "energy_mgmt_systems_controls" = "Energy Controls", 
  "motors" = "Motors", ## error
  "motor_asd_vsd" = "Variable Speed Drive Motors",
  #"otheren_eff" = "Other Energy Efficiency",
  "processing_manufacturing_equipment" = "Processing Equipment",
  "unspecified_en_eff" = "Unspecified Energy Efficiency"
)

Build.Total.Dataset <- function()
{
  total <- data.frame(
    "Total" = Yearstogram(Incentives)
  )
  
  for (j in keys(tech.categories))
  {
    total[[tech.categories[[j]]]] = Yearstogram(subset(Incentives, Incentives[[j]]==1))
  }
  
  return(total)
 
}






