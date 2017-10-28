outputFileCsv <- "gazette_potato_warts.csv"
outputFileCsvCon<-file(outputFileCsv, open = "w")

library(XML)
#xml_file <- "https://www.thegazette.co.uk/all-notices/notice/data.feed?end-publish-date=1918-11-11&text=potatoes+wart+schedule&start-publish-date=1914-08-03&location-distance-1=1&service=all-notices&categorycode-all=all&numberOfLocationSearches=1&results-page=20"
#Don't think I need the results page
xml_file <- "https://www.thegazette.co.uk/all-notices/notice/data.feed?end-publish-date=1918-11-11&text=potatoes+wart+schedule&start-publish-date=1914-08-03&location-distance-1=1&service=all-notices&categorycode-all=all&numberOfLocationSearches=1"
xmlfile <- xmlTreeParse(readLines(xml_file)[1])

topxml <- xmlRoot(xmlfile)
topxml <- xmlSApply(topxml,function(x) xmlSApply(x, xmlValue))
xml_df <- data.frame(t(topxml), row.names=NULL)
totalPagesReturned<-as.integer(xml_df$total)

if(totalPagesReturned>0){
  #read in 10 pages at a time
  for(gatherPagesCounter in 0:(floor(totalPagesReturned/10))+1){
  # for now, so that I don't stress out the server I am getting only the first pages
  #for(gatherPagesCounter in 0:0+1){
    print (gatherPagesCounter)
    
    xml_file <- paste("https://www.thegazette.co.uk/all-notices/notice/data.feed?end-publish-date=1918-11-11&text=potatoes+wart+schedule&start-publish-date=1914-08-03&location-distance-1=1&service=all-notices&categorycode-all=all&numberOfLocationSearches=1&results-page=",(gatherPagesCounter),sep='')
    xmlfile <- xmlTreeParse(readLines(xml_file)[1])
    
    topxml <- xmlRoot(xmlfile)
    topxml <- xmlSApply(topxml,function(x) xmlSApply(x, xmlValue))
    xml_df <- data.frame(t(topxml), row.names=NULL)
    
    print(paste("page number:",xml_df$page.number[1],sep=""))
    print(paste("page size:",xml_df$page.size[1],sep=""))
    print(paste("page start:",xml_df$page.start[1],sep=""))
    print(paste("page stop:",xml_df$page.stop[1],sep=""))
    print(paste("page total:",xml_df$total[1],sep=""))
    
    #Entry names are entry, entry.1, entry.2.... entry.9
    for(entriesCounter in 0:9){
      if(entriesCounter==0){
        entriesCounterName=""
      }else{
        entriesCounterName=paste(".",entriesCounter,sep="")
      }
      
      entry<-xml_df[paste("entry",entriesCounterName,sep="")]
      
      #entry<-xml_df$entry.2[1]
      entryId<-entry[[1,1]]$id
      print(paste("ID:",entryId,sep=""))
      
      entryTitle<-entry[[1,1]]$title
      print(paste("Title:",entryTitle,sep=""))
      
      entryUpdated<-entry[[1,1]]$updated
      print(paste("updated:",entryUpdated,sep=""))
      
      entryPublished<-entry[[1,1]]$published
      print(paste("Published:",entryPublished,sep=""))
      
      #now get the pdf of the page,convert it to text
      url <- paste(entry[[1,1]]$id,"/data.pdf",sep="")
      #dest <- tempfile(fileext = ".pdf")
      dest<-paste("C:\\a_orgs\\carleton\\hist3814\\R\\graham_fellowship\\gazette_files\\",gsub(":", "-", gsub("/", "-", entry[[1,1]]$id)),".pdf",sep="")
      
      download.file(url,dest , mode = "wb")
      
      # set path to pdftotxt.exe and convert pdf to text
      
      exe <- "C:\\a_orgs\\carleton\\hist3814\\R\\graham_fellowship\\pdftools\\bin64\\pdftotext.exe"
      system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)
      
      
            # get txt-file name and open it
      filetxt <- sub(".pdf", ".txt", dest)
      
      while(!file.exists(filetxt)){
        #wait until file exists
      }
      
      #shell.exec(filetxt); shell.exec(filetxt) # strangely the first try always throws an error..
      
      
      library(tm)
      
      txt <- readLines(filetxt) # don't mind warning..
      
      #loop through each instance of schedule
      noticeText<-""
      pos = grep('SCHEDULE', txt)
      for(found in pos){
        print (txt[found])
        noticeText<-txt[found]
        if(length(txt)>found){
          print(txt[found+1])
          noticeText<-paste(noticeText,txt[found+1],sep="")
          }
        if(length(txt)>found+1){
          print(txt[found+2])
          noticeText<-paste(noticeText,txt[found+2],sep="")
        }
        
        writeLines(paste("\"",entryId,"\",\"",entryTitle,"\",\"",entryUpdated,"\",\"",entryPublished,"\",\"",noticeText,"\"",sep=""),outputFileCsvCon)
        #writeLines(paste("\"",entryTitle,"\",\"",entryUpdated,"\",\"",entryPublished,"\",\"",noticeText,"\"",sep=""),outputFileCsvCon)
        
        
      }
      
      
      pos# thanks to https://stackoverflow.com/questions/1174799/how-to-make-execution-pause-sleep-wait-for-x-seconds-in-r
      # wait 2 seconds - don't stress the server
      p1 <- proc.time()
      Sys.sleep(2)
      proc.time() - p1
    }
  }
  
}else{
  print("no pages found")
}

close(outputFileCsvCon)
