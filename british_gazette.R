library(XML)
xml_file <- "https://www.thegazette.co.uk/all-notices/notice/data.feed?end-publish-date=1918-11-11&text=potatoes+wart+schedule&start-publish-date=1914-08-03&location-distance-1=1&service=all-notices&categorycode-all=all&numberOfLocationSearches=1&results-page=20"
xmlfile <- xmlTreeParse(readLines(xml_file)[1])

topxml <- xmlRoot(xmlfile)
topxml <- xmlSApply(topxml,function(x) xmlSApply(x, xmlValue))
xml_df <- data.frame(t(topxml), row.names=NULL)
totalPagesReturned<-as.integer(xml_df$total)

if(totalPagesReturned>0){
  #for(gatherPagesCounter in 0:(floor(totalPagesReturned/10))+1){
  # for now, so that I donw stress out the server I am getting only the first pages
  for(gatherPagesCounter in 0:0+1){
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
    print(paste("ID:",entry[[1,1]]$id,sep=""))
    print(paste("Title:",entry[[1,1]]$title,sep=""))
    print(paste("updated:",entry[[1,1]]$updated,sep=""))
    print(paste("Published:",entry[[1,1]]$published,sep=""))
    
    #now get the pdf of the page,convert it to text
    url <- paste(entry[[1,1]]$id,"/data.pdf",sep="")
    dest <- tempfile(fileext = ".pdf")
    download.file(url, dest, mode = "wb")
    
    # set path to pdftotxt.exe and convert pdf to text
    
    exe <- "C:\\a_orgs\\carleton\\hist3814\\R\\graham_fellowship\\pdftools\\bin64\\pdftotext.exe"
    system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)
    
    # get txt-file name and open it
    filetxt <- sub(".pdf", ".txt", dest)
    shell.exec(filetxt); shell.exec(filetxt) # strangely the first try always throws an error..
    
    
    library(tm)
    
    txt <- readLines(filetxt) # don't mind warning..
    
    # thanks to https://stackoverflow.com/questions/1174799/how-to-make-execution-pause-sleep-wait-for-x-seconds-in-r
    # wait 2 seconds - don't stress the server
    p1 <- proc.time()
    Sys.sleep(2)
    proc.time() - p1
    
    
    }
  }

}else{
  print("no pages found")
}
  


#xml_df
# 
# xml_df$page.number[1]
# xml_df$page.size[1]
# xml_df$page.start[1]
# xml_df$page.stop[1]
# xml_df$total[1]
# 
# 
# 
# entry<-xml_df$entry.2[1]
# entry[[1]]$id
# entry[[1]]$title
# entry[[1]]$updated
# entry[[1]]$published



#Thanks to: https://dzone.com/articles/reading-and-text-mining-pdf

# here is a pdf for mining

# url <- "https://www.thegazette.co.uk/London/issue/28981/page/9556/data.pdf"
# dest <- tempfile(fileext = ".pdf")
# download.file(url, dest, mode = "wb")
# 
# # set path to pdftotxt.exe and convert pdf to text
# 
# exe <- "C:\\a_orgs\\carleton\\hist3814\\R\\graham_fellowship\\pdftools\\bin64\\pdftotext.exe"
# system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)
# 
# # get txt-file name and open it
# filetxt <- sub(".pdf", ".txt", dest)
# shell.exec(filetxt); shell.exec(filetxt) # strangely the first try always throws an error..
# 
# 
# library(tm)
# 
# txt <- readLines(filetxt) # don't mind warning..


# 
# 
# xml_file <- "https://www.thegazette.co.uk/notice/29815/data.xml"
# xmlfile <- xmlTreeParse(readLines(xml_file)[1])
# #class(xmlfile)
# topxml <- xmlRoot(xmlfile)
# topxml <- xmlSApply(topxml,function(x) xmlSApply(x, xmlValue))
# xml_df <- data.frame(t(topxml), row.names=NULL)
# xml_df$total

