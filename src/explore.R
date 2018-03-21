rm(list=ls())
library(devtools)
library(dplyr)
library(tidyr)
library(stats)
library(fitdistrplus)
library(reshape2)
library(openxlsx)
library(readxl)
library(plotly)
library(ggplot2)
library(data.table)
library(maps)
library(mapdata)
library(ggmap)
library(colorspace)
library(ggalt)


#!!! IMPORTANT - set to working directory
setwd("~/R/project_city")


Sys.setenv("plotly_username"="srishtimishra56")
Sys.setenv("plotly_api_key"="DNwM8bZ4gfSuh53MQqId")
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiY29mZmVlaW5zcGFjZSIsImEiOiJjamE0ZDc0M3YxOWwyMndsZnl6MTZuZzhwIn0.O5CBBZO47nPJ2HwSRl_TPg')

# Extra functions  ------------------------------
#View dataframes inside master
getdf <- function(x){
  # x <- as.character(paste(x))
  # print(x)
  return(as.data.frame(df_master[x]))
}
#---------------------------------------------------

  
# Read data
  # read all sheets from workbook
    read_excel_allsheets <- function(filename) {
      sheets <- readxl::excel_sheets(filename)
      x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
      names(x) <- sheets
      x
     }
  
  # Read Dataset of infrastructure, citizen services, budgets, civic works
    df_master <- read_excel_allsheets("data/CIVIC HACKATHON March 2016 (copy).xlsx")
    typeof(df_master)
    names(df_master)  
    # View(df_master$`Ward Master`)
    df_master$`Ward Master`$`Pop. Density`[grep(",", df_master$`Ward Master`$`Pop. Density`)] <- as.numeric(gsub(",", "", df_master$`Ward Master`$`Pop. Density`[grep(",", df_master$`Ward Master`$`Pop. Density`)] ))
    df_master$`Ward Master`$`Pop. Density` <- as.numeric(df_master$`Ward Master`$`Pop. Density`)
    
  #Create AssemblyDf (list of Constituencies in the City)
    # View(df_master$`Assembly Master`)
    df_master$`Assembly Master` <- NULL
    df_master$`Assembly Master`$`Assembly Constituency` <-  data.frame(unique(df_master$`Ward Master`$`Assembly Constituency`))
    df_master$`Assembly Master` <- as.data.frame(df_master$`Assembly Master`)
    df_master$`Assembly Master`$`Area (sq km)` <- 1
    df_master$`Assembly Master`$`Total Pop (2011)` <- 1
    df_master$`Assembly Master`$`Pop density` <- 1
    df_master$`Assembly Master`$`Total Pop (2001)` <- 1
    df_master$`Assembly Master`$`%Inc/Dec` <- 1

    
    
    colnames(df_master$`Assembly Master`)[1] <- "Assembly Constituency"

    for(x in df_master$`Assembly Master`$`Assembly Constituency`) {
      df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$`Area (sq km)` <-  (sum(as.numeric(df_master$`Ward Master`[(df_master$`Ward Master`$`Assembly Constituency` == x), ]$`Area (sq km)`)))
      df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$`Total Pop (2011)` <- (sum(as.numeric(df_master$`Ward Master`[(df_master$`Ward Master`$`Assembly Constituency` == x), ]$`Total Population (2011)`)))
      df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$`Pop density` <-  round(df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$`Total Pop (2011)` / df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$`Area (sq km)`, 0 )
      df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$`Total Pop (2001)` <- (sum(as.numeric(df_master$`Ward Master`[(df_master$`Ward Master`$`Assembly Constituency` == x), ]$`Total Population (2001)`)))
      df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$`%Inc/Dec` <- round( (as.numeric(df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$`Total Pop (2011)`) - as.numeric(df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$`Total Pop (2001)`) ) /as.numeric(df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$`Total Pop (2001)`) * 100 , 2) 

      }
  
    # View(df_master$`Assembly Master`)
    df_master$`Assembly Master` <- as.data.frame(df_master$`Assembly Master`)
    
    #get_map only if not saved in .RData file 
    #city_map <- get_map(location='Bangalore,India',zoom=11)
    #   #city_map_test <- get_map(location='Bangalore,India',zoom=12, maptype = "hybrid")
    #save(city_map, file = "xy.RData")
    load("xy.RData")
    
  # Extract individual dataframes
    # df_master_list <- list()
    # p = 1
    # for(i in names(df_master)){
    #   #nam <- paste0("`",i,"`")
    #   #print(i)
    #    nam <- paste0("df_", i)
    #    df_master_list[p] <-  nam
    #    p <- p+1
    #    assign(nam, as.data.frame(df_master[i]))
    # }
  
  # Data set of complaints uncooment later on
            df_complaints <- read.xlsx("data/CivicHackathon_Second Data Set/ICMyC_complaints.xlsx")
              df_complaints$Complaint.created <- as.Date(df_complaints$Complaint.created, origin = "1899-12-30") 
            # 
            # # # 
              df_complaints$LatitudeEdit1 <- gsub( "_x.*$", "", as.character(df_complaints$Latitude))
              df_complaints$LongitudeEdit1 <- gsub( "_x.*$", "", as.character(df_complaints$Longitude))
              df_complaints$testingLat <-  sub('.*\\;', '', df_complaints$LatitudeEdit1)
              df_complaints$testingLong <- sub('.*\\;', '', df_complaints$LongitudeEdit1)
            #  
              df_complaints$Latitude <- df_complaints$testingLat
              df_complaints$Longitude <- df_complaints$testingLong
               df_complaints$Latitude <- as.numeric(df_complaints$Latitude)
               df_complaints$Longitude <- as.numeric( df_complaints$Longitude )
              df_complaints$LatitudeEdit1 <- NULL
              df_complaints$LongitudeEdit1 <- NULL
              df_complaints$testingLat <-  NULL
              df_complaints$testingLong <- NULL
              df_complaints$testing <-NULL
            #  
            # ## # write.csv(df_complaints , "newcomplaints.csv");
                #f <- read.csv("newcomplaints.csv", header = T)
            # #  #View(df_complaints)
              df_sel <- df_complaints[df_complaints$Complaint.status != "Resolved", ]
              df_sel <- df_sel[df_sel$Complaint.status != "Closed", ]
              ggmap(city_map) + geom_point(aes(x=Longitude, y=Latitude),
                                           data = df_sel,
                                           alpha = 0.2,
                                           color = "dark orange"   ) +
              ggtitle(label = "Unresolved Complaints across Bangalore City - All categories")
 
  
  #View( df_master$`Budget 15-16`)
  
# Create civic indicators data
  indicator_df <- data.frame(indicator = c("infrastructure", "citizen services", "budgets", "civic work"), num = c(1,2,3,4))
  
  
# Check for NA's in master dataset
    df_summary <- sapply(df_master, function(y) sum(length(which(is.na(y)))))
    df_summary <- data.frame(df_summary)
    names(df_summary)[1] <- "na_count"
    #df_summary
    na_names <- row.names(subset(df_summary, na_count != 0))
    no_na_names <- row.names(subset(df_summary, na_count == 0))

  # Examine NA coloumns in each data frame
    for(x in na_names){
      #print(x)
      df_na_summary <- sapply(as.data.frame(df_master[x]), function(y) sum(length(which(is.na(y)))))
      rowsCount <- nrow(as.data.frame(df_master[x]))
      df_na_summary <- data.frame(df_na_summary)
      colnames(df_na_summary)[1] <- 'na_count'
      df_na_summary$percentOfNaVal <- round(df_na_summary$na_count / rowsCount * 100 , 2) 
      # df_na_summary
      if(colSums(df_na_summary)[1] == 0){
        print(paste0("No na's: ", x))
  
      }
      else {
        print(paste0( colSums(df_na_summary)[1] , " NA's in ", x, "\n"))
        print(paste("Number of Rows : ", rowsCount ))
        print(df_na_summary)
        cat("\n\n")
      }
    }


    # In Parks:
    #infrastructure
    # View(df_master$Parks)
      # Drop lights, source of water - too many NA's
      #Keep "SL No"  "Ward No" "Ward Name" "Address of the Park" "Latitude"  "Longitude" "Area in acres" 
      df_master$Parks <- df_master$Parks[ , c(1,4:9) ]
      df_master$Parks$Indicator <- NULL
      df_master$Parks$Indicator <- list(c(indicator_df$num[indicator_df$indicator == "infrastructure"],df_master$Parks$Indicator, indicator_df$num[indicator_df$indicator == "citizen services"]))
      df_master$Parks <- merge(x = df_master$Parks, y = df_master$`Ward Master`[ , c("Ward No", "Assembly Constituency")], by = "Ward No", all.x=TRUE)
      df_master$Parks[, "Ward No"][df_master$Parks[, "Ward No"] == "NA"] <- 7777
      df_master$Parks[, "Ward Name"][df_master$Parks[, "Ward Name"] == "NA"] <- "City (Unidentified)"      
    #View(df_master$Parks)
      # Make a Park Quality df
      # Measure Quality as Area of Park for every 100 people
      # Measure Quality as percentage of Ward Area
     #View(df_master$ParkQuality)   
      df_master$ParkQuality <- NULL
      df_master$ParkQuality <- as.data.frame(unique(df_master$Parks$`Assembly Constituency`))
      colnames(df_master$ParkQuality) <- "Assembly Constituency"
      df_master$ParkQuality <- merge(x = df_master$ParkQuality, y = df_master$`Assembly Master`[ , c("Assembly Constituency", "Area (sq km)" , "Total Pop (2011)", "%Inc/Dec", "Pop density" )], by = "Assembly Constituency", all.x=TRUE)
     df_master$Parks$`Area in acres` <- as.numeric( df_master$Parks$`Area in acres`)
      x <- which( is.na(df_master$Parks$`Area in acres`) | df_master$Parks$`Area in acres` == 0)
      for(i in x)
          df_master$Parks[i, "Area in acres"] <- 0.01
      
      df_master$ParkQuality$`Park Area` <- 1
      df_master$ParkQuality$`Count` <- 1
      df_master$ParkQuality$`Latitude` <- 1
      df_master$ParkQuality$`Longitude` <- 1
      df_master$ParkQuality$PopScore <- 0.001
      df_master$ParkQuality$AreaScore <- 0.001
      
      df_master$`Assembly Master`$Latitude <- 1
      df_master$`Assembly Master`$Longitude <- 1

      asb <- unique(df_master$Parks$`Assembly Constituency`)
      
     #Get Park quality by Assembly
      for(x in asb){
        df_master$ParkQuality[(df_master$ParkQuality$`Assembly Constituency` == x), ]$`Park Area`<-  (sum(as.numeric(df_master$Parks[(df_master$Parks$`Assembly Constituency` == x), ]$`Area in acres`))) * 0.00404686
        df_master$ParkQuality[(df_master$ParkQuality$`Assembly Constituency` == x), ]$`Count` <-  as.numeric(count(df_master$Parks[(df_master$Parks$`Assembly Constituency` == x), ]))
        df_master$ParkQuality[(df_master$ParkQuality$`Assembly Constituency` == x), ]$`Latitude` <- as.numeric(mean( df_master$Parks[(df_master$Parks$`Assembly Constituency` == x), ]$Latitude, na.rm = T))
        df_master$ParkQuality[(df_master$ParkQuality$`Assembly Constituency` == x), ]$`Longitude` <- as.numeric(mean( df_master$Parks[(df_master$Parks$`Assembly Constituency` == x), ]$Longitude, na.rm = T))
        df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$`Longitude` <- df_master$ParkQuality[(df_master$ParkQuality$`Assembly Constituency` == x), ]$`Longitude`
        df_master$`Assembly Master`[(df_master$`Assembly Master`$`Assembly Constituency` == x), ]$Latitude <-  df_master$ParkQuality[(df_master$ParkQuality$`Assembly Constituency` == x), ]$`Latitude`
      }  
     
      
      df_master$ParkQuality$`Pop density`[grep(",", df_master$ParkQuality$`Pop density`)] <- as.numeric(gsub(",", "", df_master$ParkQuality$`Pop density`[grep(",", df_master$ParkQuality$`Pop density`)] ))
      
      #Park area in sq m per Hundred thousand people
      df_master$ParkQuality$PopScore <- (df_master$ParkQuality$`Park Area` *1000000 / df_master$ParkQuality$`Total Pop (2011)` )
      #Ranges from 0.00057 to 0.01468 per 100 thousand people, i.e, 0.001 sq m to 0.13 sq m per 1000,000 people.
      
      df_master$ParkQuality$AreaScore <- (df_master$ParkQuality$`Park Area` / df_master$ParkQuality$`Area (sq km)` *100 )
      

      df_master$ParkQuality$isScore <- 1
      #View(df_master$ParkQuality)   
      
      
      #city coords
      city_lat <- 12.9716
      city_long <- 77.5946        
      
      # Assemblies/Constituencies in the city 
      ggmap(city_map) + geom_point(aes(x=Longitude, y=Latitude, size = `Pop density`),
                                   data = df_master$`Assembly Master`,
                                   alpha = 0.7,
                                   color = "dark blue"   ) +
        geom_text( data = head(df_master$`Assembly Master`[order(df_master$`Assembly Master`$`Pop density`, decreasing= T),], n = 12), 
                   aes( x = Longitude, y = Latitude, label = `Assembly Constituency`),check_overlap = TRUE, fontface = "bold", hjust = 1.3, size = 3.5, color = "black") +
        geom_encircle(aes(x=Longitude, y=Latitude),
                      data = df_master$`Assembly Master`, size = 2, color = "orange") +
          ggtitle(label = "Assemblies/Constituencies in the city") +
        theme(plot.title = element_text(hjust = 0.5))
        
      
      # #  #View(df_complaints)
      df_sel <- df_complaints[df_complaints$Complaint.status != "Resolved", ]
      df_sel <- df_sel[df_sel$Complaint.status != "Closed", ]
      ggmap(city_map) + geom_point(aes(x=Longitude, y=Latitude),
                                   data = df_sel,
                                   alpha = 0.2,
                                   color = " dark orange" , show.legend =T) +
        geom_point(aes(x=Longitude, y=Latitude, size = `Pop density`),
                   data = df_master$`Assembly Master`,
                   alpha = 1,
                   color = " dark blue"   )

    
      # Bloat Area and Population scores to plot on graph
     
      plot_ly(df_master$ParkQuality, x = ~`Assembly Constituency` , y = ~`Total Pop (2011)`, text = ~PopScore, type = 'scatter', mode = 'markers',
                   marker = list(size = ~PopScore*50, opacity = 0.5, color = 'rgb(255, 65, 54)' ), visible = T,  name = "ppl") %>%
        add_trace(df_master$ParkQuality, x = ~`Assembly Constituency`, y = ~`Area (sq km)`, text = ~AreaScore, type = 'scatter', mode = 'markers',
                  marker = list(size = ~AreaScore*50, opacity = 0.5, color = 'rgb(29, 155, 79)'), visible = F, name = "area") %>%
        layout(
          title = "Park Scores across Bangalore City",
          xaxis = list(showgrid = F),
          yaxis = list(showgrid = F),
          
          updatemenus = list(
            list(
              type= 'dropdown',
              
              buttons = list(
                list(
                  label = "Park area (in sq km) per 100,000 people",
                  method = "update",
                  args = list(list(visible = c(TRUE, FALSE)),
                              list(title = "Park area (in sq km) per 100,000 people", yaxis = list(title = "Total Population") 
                                   ))),
                list(
                  label = "Park area (in sq km) available in Ward",
                  method = "update",
                  args = list(list(visible = c(FALSE, TRUE)),
                              list(title = "Park area (in sq km) available in Ward", yaxis = list(title = "Ward Area") 
                              ))) 
              )
            )
          )
        )
      
      # Area of ward used for Parks
      plot_ly(df_master$ParkQuality, x = ~`Assembly Constituency` , y = ~`Area (sq km)` , type = 'bar', name = 'Ward Area') %>%
        add_trace(y = ~(`Park Area`/`Area (sq km)` * 100), name = 'Park Area') %>%
        layout(yaxis = list(title = '% Area used for Parks in each Constituency'), barmode = 'stack')
       
      
      
      
  # In Footpaths
      #View(df_master$Footpaths)    
      #Keep "Footpaths.S..No." "Footpaths.Ward.No." "Footpaths.Ward.Name" "Footpaths.Length.in.mtr" "Footpaths.Width.in.mtr" 
      df_master$Footpaths <- df_master$Footpaths[ , c(1,5,6, 9, 10)]
      df_master$Footpaths$Indicator <- (indicator_df$num[indicator_df$indicator == "infrastructure"])
      colnames(df_master$Footpaths)[2] <- "Ward No"
      df_master$Footpaths <- merge(x = df_master$Footpaths, y = df_master$`Ward Master`[ , c("Ward No", "Assembly Constituency")], by = "Ward No", all.x=TRUE)
      df_master$Footpaths[, "Ward No"][df_master$Footpaths[, "Ward No"] == "NA"] <- 7777
      df_master$Footpaths[, "Ward Name"][df_master$Footpaths[, "Ward Name"] == "NA"] <- "City (Unidentified)"      
    
    
      #Footpath quality was calculated as the
      #   %Footpath Walkable	= Walkable Length/Total Length x 100
    # In Footpath quality
     # View(getdf("Footpath quality"))
      df_master$`Footpath quality` <-  df_master$`Footpath quality`[, -1]
      df_master$`Footpath quality`$Indicator <- indicator_df$num[indicator_df$indicator == "infrastructure"]
      colnames(df_master$`Footpath quality`)[1] <- "Ward No"
      df_master$`Footpath quality` <- merge(x = df_master$`Footpath quality`, y = df_master$`Ward Master`[ , c("Ward No", "Assembly Constituency")], by = "Ward No", all.x=TRUE)
      df_master$`Footpath quality`$isScore <- 1
      df_master$`Footpath quality`$Score <- 1
    
      asb <- unique(df_master$`Footpath quality`$`Assembly Constituency`)
      
      #Get Footpath quality by Assembly
      for(x in asb){
         df_master$`Footpath quality`[(df_master$`Footpath quality`$`Assembly Constituency` == x), ]$`Score` <- as.numeric(mean( df_master$`Footpath quality`[(df_master$`Footpath quality`$`Assembly Constituency` == x), ]$`Footpath Score`, na.rm = T))
      }  
      
      df_master$`Footpath quality` <- df_master$`Footpath quality`[ , c(6,8,5,7)]
      df_master$`Footpath quality` <- unique(df_master$`Footpath quality`)
     
      df_master$`Footpath quality` <- merge(x = df_master$`Footpath quality`, y = df_master$`Assembly Master`[ , c("Assembly Constituency", "Latitude", "Longitude")], by = "Assembly Constituency", all.x=TRUE )
      
      #Compare with complaints
      #Find rows containing "footpath", "sidewalk" (inferred from wordCloud)
      df_complaints_fp <- NULL
      df_complaints_fp <- df_complaints[grep("*Footpath*", df_complaints$`Sub.category`), ]
      df_complaints_fp <- df_complaints_fp[df_complaints_fp$Complaint.status != "Resolved", ]
      names(df_complaints_fp)[4] <- "Ward No"
      df_complaints_fp <- merge(x = df_complaints_fp, y = df_master$`Ward Master`[ , c("Ward No", "Assembly Constituency")], by = "Ward No", all.x=TRUE)
      df_complaints_plot <- df_complaints_fp[ ,c(12,13,15)]
     # View(df_master$`Footpath quality`)
    
    #get_map only if not saved in .RData file 
    #city_map <- get_map(location='Bangalore,India',zoom=11)
    #   #city_map_test <- get_map(location='Bangalore,India',zoom=12, maptype = "hybrid")
     #save(city_map, file = "xy.RData")
     load("xy.RData")
     #aes(title = "Footpath Quality Scores versus Unresolved complaints across the City")
    ggmap(city_map, alpha = 0.2) +
       geom_point( data = df_master$`Footpath quality`, aes(x = Longitude, y = Latitude, color = Score),
                  alpha = 1,   size = 15 ) +
       geom_point(aes(x = Longitude, y = Latitude, fill = "Complaints"), data = df_complaints_plot, color = "red", alpha = 0.3,size = 1) +
       scale_colour_gradient(low = "light blue", high="dark blue") +
      scale_fill_manual(name = "Type", values = c("Complaints" = "red")) +
      ggtitle(label = "Footpath Quality Scores versus Unresolved complaints across the City")
     

    
  # In BMTC Bus Stops
      # View(df_master$`BMTC Bus stops`)
      #infrastructure
      #Keep BMTC.Bus.stops.SL.No" "BMTC.Bus.stops.Ward.No""BMTC.Bus.stops.Ward.Name" "BMTC.Bus.stops.Bus.Stops"
      #"BMTC.Bus.stops.Latitude"  "BMTC.Bus.stops.Longitude"
      #Replace Na's in Ward No with 5555 and Ward Name with 'Outside'
      newEntry <- data.frame("Outside",0, 0, 0, 0, 0, 0, 0)
      names(newEntry) <- names(df_master$`Assembly Master`)
      df_master$`Assembly Master` <- rbind(df_master$`Assembly Master`, newEntry) 
      
      newEntry <- data.frame(7777, "Outside", "Outside", "Outside", "Outside", "Outside", "Outside", 0, 0, 0, 0, 0, 0, 0 )
      names(newEntry) <- names(df_master$`Ward Master`)
      df_master$`Ward Master` <- rbind(df_master$`Ward Master`, newEntry) 
      
      df_master$`BMTC Bus stops` <- df_master$`BMTC Bus stops`[, c(1, 3:7)]
      
      df_master$`BMTC Bus stops`[is.na( df_master$`BMTC Bus stops`$`Ward No`),]$`Ward No` <- 7777
      df_master$`BMTC Bus stops`$Indicator <- list(c(indicator_df$num[indicator_df$indicator == "infrastructure"], indicator_df$num[indicator_df$indicator == "citizen services"]))
      df_master$`BMTC Bus stops` <- merge(x = df_master$`BMTC Bus stops`, y = df_master$`Ward Master`[ , c("Ward No", "Assembly Constituency")], by = "Ward No", all.x=TRUE)
      
      df_master$BusStopsQuality <- NULL
      df_master$BusStopsQuality <- as.data.frame(unique(df_master$`BMTC Bus stops`$`Assembly Constituency`))
      colnames(df_master$BusStopsQuality) <- "Assembly Constituency"
      df_master$BusStopsQuality <- merge(x = df_master$BusStopsQuality, y = df_master$`Assembly Master`[ , c("Assembly Constituency", "Area (sq km)" , "Total Pop (2011)", "%Inc/Dec", "Pop density", "Latitude", "Longitude" )], by = "Assembly Constituency", all.x=TRUE)
      df_master$BusStopsQuality$`Count` <-  NULL
      asb <- unique(df_master$BusStopsQuality$`Assembly Constituency`)

      for(x in asb){
        df_master$BusStopsQuality[(df_master$BusStopsQuality$`Assembly Constituency` == x), "Count"] <- length(which(df_master$`BMTC Bus stops`$`Assembly Constituency` == x))

      }  

      load("xy.RData")
    
      df_plt <- df_master$BusStopsQuality[df_master$BusStopsQuality$`Assembly Constituency` != "Outside", ]
      df_plt <- df_plt[df_plt$`Assembly Constituency` != "Sri N Chandrashekar Raju", ]
      
      ggmap(city_map) + geom_point(aes(x=Longitude, y=Latitude, size = Count),
                                        data = df_plt,
                                        alpha = 0.7,
                                        color = "dark blue", show_guide = FALSE) + 
        geom_point(aes(x=Longitude, y=Latitude, size = `Pop density`),
                   data =df_plt, 
                   alpha = 0.3,
                   color = "green",show_guide = FALSE) +
        geom_encircle(aes(x=Longitude, y=Latitude),
                      data = df_plt, size = 2, color = "blue") +
        scale_size_continuous(range = c(3, 15)) +
        scale_colour_manual(name = 'Colours', 
                            values =c('Number of Bus stops'='dark blue','Population density'='green'), labels = c('Count','Pop density'))+
        ggtitle(label = "Population Density and Number of Bus Stops per Constituency Across the City")

      
      
    #In Street lighting quality
      #View(getdf("Street lighting quality"))
      df_master$`Street lighting quality` <- df_master$`Street lighting quality`[ , -1]
      df_master$`Street lighting quality`$Indicator <- indicator_df$num[indicator_df$indicator == "infrastructure"]
      df_master$`Street lighting quality`$isScore <- 1; 
      colnames(df_master$`Street lighting quality`)[1] <- "Ward No"
      df_master$`Street lighting quality` <- merge(x = df_master$`Street lighting quality`, y = df_master$`Ward Master`[ , c("Ward No", "Assembly Constituency")], by = "Ward No", all.x=TRUE)
      df_master$`Street lighting quality`$Score <- 1
      
      asb <- unique(df_master$`Street lighting quality`$`Assembly Constituency`)
      
      #Get Street Lighting quality by Assembly
      for(x in asb){
        df_master$`Street lighting quality`[(df_master$`Street lighting quality`$`Assembly Constituency` == x), ]$`Score` <- as.numeric(mean( df_master$`Street lighting quality`[(df_master$`Street lighting quality`$`Assembly Constituency` == x), ]$`SL Score`, na.rm = T))
      }  
      
      df_master$`Street lighting quality` <- df_master$`Street lighting quality`[ , c(6,7,1)]
      df_master$`Street lighting quality` <- unique(df_master$`Street lighting quality`)
      
      df_master$`Street lighting quality` <- merge(x = df_master$`Street lighting quality`, y = df_master$`Assembly Master`[ , c("Assembly Constituency", "Latitude", "Longitude")], by = "Assembly Constituency", all.x=TRUE )
      
      #Compare with complaints
      #Find rows containing "Streetlights" from Sub Category(inferred from wordCloud)
      df_complaints_fp <- NULL
      df_complaints_fp <- df_complaints[grep("*Streetlight*", df_complaints$`Sub.category`), ]
      df_complaints_fp <- df_complaints_fp[df_complaints_fp$Complaint.status != "Resolved", ]
      names(df_complaints_fp)[4] <- "Ward No"
      df_complaints_fp <- merge(x = df_complaints_fp, y = df_master$`Ward Master`[ , c("Ward No", "Assembly Constituency")], by = "Ward No", all.x=TRUE)
      df_complaints_plot <- df_complaints_fp[ ,c(12,13,15)]

      #get_map only if not saved in .RData file 
      #city_map <- get_map(location='Bangalore,India',zoom=11)
      #   #city_map_test <- get_map(location='Bangalore,India',zoom=12, maptype = "hybrid")
      #save(city_map, file = "xy.RData")
      load("xy.RData")
      #aes(title = "Footpath Quality Scores versus Unresolved complaints across the City")

      ggmap(city_map, alpha = 0.2) +
        geom_point( data = df_master$`Street lighting quality`, aes(x = Longitude, y = Latitude, label = `Assembly Constituency`, color = Score),
                    alpha = 1, size = 15) +
        geom_point(aes(x = Longitude, y = Latitude, fill = "Complaints"), data = df_complaints_plot, color = "blue", alpha = 0.4,size = 1) +
        scale_colour_gradient(low = "yellow", high="dark orange") +
        scale_fill_manual(name = "Type", values = c("Complaints" = "blue")) +
        geom_text( data = head(df_master$`Street lighting quality`[order(df_master$`Street lighting quality`$Score, decreasing= T),], n = 50), 
                   aes( x = Longitude, y = Latitude, label = `Assembly Constituency`),check_overlap = TRUE, fontface = "bold", hjust = 1.2, size = 3, color = "black") +
        ggtitle(label = "Street lighting quality Scores versus Unresolved complaints across the City")
      
      

    # In BTP casualities (Bangaloer Traffic Police):
      #citizen services
      # Keep "S. No."  "Traffic Police Station" "Latitude" "Longitude"  "Casualties/Injuries Combined 2013" 
      # "Casualties/Injuries Combined 2014"
      # View(df_master$`BTP casualities`)
      df_master$`BTP casualities` <- df_master$`BTP casualities`[ , c(1, 3:7)]
      #df_master$`BTP casualities` <- merge(x = df_master$`BTP casualities`, y = df_master$`Ward Master`[ , c("Ward No", "Assembly Constituency")], by = "Ward No", all.x=TRUE)
      
      
      # Ignore stations with missing injuries (yearly) values?? 5 out of 42
        #View(df_master$`BTP casualities`)
        # no locations
        df_master$`BTP casualities` <- subset(df_master$`BTP casualities`, !is.na(`Casualties/Injuries Combined 2013`) & !is.na(`Casualties/Injuries Combined 2014`) )
        df_master$`BTP casualities`$Indicator <- (indicator_df$num[indicator_df$indicator == "citizen services"])
        
    # In BCP (Bangalore City Police)
        #citizen services
        # Keep "BCP.Police.Station.Name"  "BCP.Latitude" "BCP.Longitude" "BCP.No..of.FIR....01.Jan.2013.to.31.Dec.2013."
        # "BCP.No..of.FIR....01.Jan.2014.to.10.Sep.2014."
        #View(df_master$BCP)
        # no locations
        df_master$BCP <- df_master$BCP[complete.cases(df_master$BCP) , ]
        df_master$BCP$Indicator <- (indicator_df$num[indicator_df$indicator == "citizen services"])
        #df_master$BCP <- merge(x = df_master$BCP, y = df_master$`Ward Master`[ , c("Ward No", "Assembly Constituency")], by = "Ward No", all.x=TRUE)
        
 
      
    #In Contractor Bills
      # Rtgs - Real Time Gross Settlement, BR = ???? , CBR =?? ???? 
      #!! Work on this
      # View(getdf("Contractor bills"))
      df_master$`Contractor bills` <- df_master$`Contractor bills`[ , c(1, 4:22)]
      df_master$`Contractor bills`$Indicator <- indicator_df$num[indicator_df$indicator == "budgets"]
      df_master$`Contractor bills`[, "Ward No"][df_master$`Contractor bills`[, "Ward No"] == "NA"] <- 7777
      df_master$`Contractor bills`[, "Ward Name"][df_master$`Contractor bills`[, "Ward Name"] == "NA"] <- "City (Unidentified)"      
      
      
    
  
# Read columns from rest of the sets
    # "Ward Master" "Road master" "Public toilets" "Playgrounds"  "BBMP Job codes"  "Fire services" "Footpath quality"        "Pedestrian Quality"     
    # "Street lighting quality" "Bus stop scores"        
      
    #In Ward Master
      # Keep "Ward.Master.Ward.No"  "Ward.Master.Ward.Name" "Ward.Master.Zone..CE." "Ward.Master.Ward.Classification" "Ward.Master.Area..sq.km."            "Ward.Master.No.of.Households"        "Ward.Master.Total.Population..2001."
      # "Ward.Master.Total.Population..2011." "Ward.Master.Inc.Dec" "Ward.Master..Inc.Dec"  "Ward.Master.Pop..Density"   
      #View(getdf("Ward Master"))
      df_master$`Ward Master` <-  df_master$`Ward Master`[ ,c(1:4,7:14)]
      df_master$`Ward Master`$Indicator <- 5

    #In Road master
      # Don't need Road master
      #Keep "Road.master.S.No."   "Road.master.Street.Name"   "Road.master.Road.Category" "Road.master.Length.in.Km" 
      #View(getdf("Road master"))
      df_master$`Road master`$Indicator <- indicator_df$num[indicator_df$indicator == "infrastructure"]
      
      
    # In BBMP Job Codes
      #Keep "BBMP.Job.codes.SL.No" "BBMP.Job.codes.Date"  "BBMP.Job.codes.Ward.NO"  "BBMP.Job.codes.Ward.Name"               
      # "BBMP.Job.codes.Job.Code"  "BBMP.Job.codes.Job.Description" "BBMP.Job.codes.Estimate....Rs.in.Lakhs." 
      # "BBMP.Job.codes.Budget.Code"  "BBMP.Job.codes.Budget.Head"  
      # View(getdf("BBMP Job codes"))
      df_master$`BBMP Job codes` <- df_master$`BBMP Job codes`[ , c(1,2,6:12)]
      df_master$`BBMP Job codes`$Indicator <- indicator_df$num[indicator_df$indicator == "civic work"]
      colnames(df_master$`BBMP Job codes`)[3] <- "Ward No" 
      df_master$`BBMP Job codes`[, "Ward No"][df_master$`BBMP Job codes`[, "Ward No"] == "NA"] <- 7777
      df_master$`BBMP Job codes`[, "Ward Name"][df_master$`BBMP Job codes`[, "Ward Name"] == "NA"] <- "City (Unidentified)"      
      df_master$`BBMP Job codes` <- merge(x = df_master$`BBMP Job codes`, y = df_master$`Ward Master`[ , c("Ward No", "Assembly Constituency")], by = "Ward No", all.x=TRUE)
      
    # In Fire services
      #  "Fire.services.Sl..No" "Fire.services.Zone"  "Fire.services.Fire.Station.Address"  "Fire.services.Latitude"  
      # "Fire.services.Longitude"  "Fire.services.No.of.Staff" "Fire.services.Remarks"
      # View(getdf("Fire services"))
      df_master$`Fire services`$Indicator <- indicator_df$num[indicator_df$indicator == "citizen services"]
      df_master$`BBMP Job codes` <- merge(x = df_master$`BBMP Job codes`, y = df_master$`Ward Master`[ , c("Ward No", "Assembly Constituency")], by = "Ward No", all.x=TRUE)
      
      
