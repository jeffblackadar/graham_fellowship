library(RMySQL)
library(ggmap)
#D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1), 144-161. URL
#http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

register_google(key = "AIzaSyBPy7MkWGMc4lwOjJkc9bpaxMJITOGfwUs")
setwd("C:/a_orgs/carleton/hist3814/R/graham_fellowship")


london <- geocode("London,UK")

#read in the sample data from a newspaper search of German+Submarine
wartData <- read.csv(file="gazette_potato_warts_edited_4.csv", header=TRUE, sep=",")

rowCounter = 0
for(counter in 1:length(wartData$Wales)){
  
  print(counter)
  #print(wartData$Wales[counter])
  if(tolower(wartData$Wales[counter])=="wales"){
    
    print(wartData$location[counter])
    allotmentLocationGeoCode <- tryCatch(
      {
        geocode(paste(wartData$location[counter],sep=""))
      },
      error=function(cond) {
        #geocode("Cardiff,UK")
        
        message(cond)
        london
        #print(paste("Geocode error: ",wartData$location[counter],sep=""))
      },
      warning=function(cond) {
        message(cond)
        #geocode("Cardiff,UK")
        london
        #print(paste("Geocode warning: ",wartData$location[counter],sep=""))
      },
      finally={
        london
      }
    )
    print(allotmentLocationGeoCode)
    
    allotmentLocationLat = allotmentLocationGeoCode[[2]]
    allotmentLocationLong = allotmentLocationGeoCode[[1]]
    

    
    if(rowCounter == 0){
      rowCounter<-rowCounter+1
      df<-data.frame(allotmentLocationLong,allotmentLocationLat,rowCounter,wartData$allotment[counter])
      names(df)<-c("x","y", "rowCounter","allotment")
      errorsdf<-data.frame("allotmentLocation")
      names(errorsdf)<-c("allotment")
      
    }else{
      rowCounter<-rowCounter+1
      dg<-data.frame(allotmentLocationLong,allotmentLocationLat,rowCounter,wartData$allotment[counter])
      names(dg)<-c("x","y", "rowCounter","allotment")
      df <- rbind(df, dg) 
    }
    
    if(allotmentLocationLong == -0.1277583){
      print(paste(wartData$location[counter],""))
      errorsdg<-data.frame(wartData$location[counter])
      names(errorsdg)<-c("allotment")
      errorsdf <- rbind(errorsdf,errorsdg)
    }
    
  }
}

#wdf <- data.frame(x=dbRows$newspaper_place_long, y=dbRows$newspaper_place_lat,newspaperTitle=dbRows$newspaper_title)

#opens a map of Wales

mapWales <- get_map(location = c(lon = -3.5, lat = 52.4000),color = "color",source = "google",maptype = "roadmap",zoom = 8)

library(ggrepel)
#ggmap(mapWales, base_layer = ggplot(aes(x = x, y = y, size = 3), data = df))  + geom_point(color="blue", alpha=0.3) + geom_text_repel(aes(x = x, y = y, size = 3, label=rowCounter), data = df,box.padding = unit(0.5, "lines"))
ggmap(mapWales, base_layer = ggplot(aes(x = x, y = y, size = 3), data = df))  + geom_point(color="blue", alpha=0.3) + geom_text_repel(aes(x = x, y = y, size = 3, label=""), data = df,box.padding = unit(0.5, "lines"))



# will want to use this to get plot newspapers we have used in a search
#SELECT tbl_newspapers.newspaper_title as newspaper_title,tbl_newspapers.newspaper_place,tbl_newspapers.newspaper_country , tbl_newspaper_search_results.newspaper_id as newspaper_id  FROM tbl_newspaper_search_results left join tbl_newspapers on tbl_newspaper_search_results.newspaper_id = tbl_newspapers.newspaper_id where true group by newspaper_id order by newspaper_title;




