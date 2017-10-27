
outputFileCsv <- "wales_papers.csv"
outputFileCsvCon<-file(outputFileCsv, open = "w")

#thanks to
#https://statistics.berkeley.edu/computing/r-reading-webpages

thepage = readLines("http://newspapers.library.wales/search?alt=full_text%3A%22allotment%22+AND+full_text%3A%22society%22+OR+full_text%3A%22societies%22&range%5Bmin%5D=1914-08-03T00%3A00%3A00Z&range%5Bmax%5D=1918-11-20T00%3A00%3A00Z")
# get rid of the tabs
thepage = trimws(gsub("\t"," ",thepage))

# find number of results
for (entriesCounter in 1:550){
  #print(paste(entriesCounter,thepage[entriesCounter],sep=""))
  if(thepage[entriesCounter] == '<input id=\"fl-decade-0\" type=\"checkbox\" class=\"facet-checkbox\" name=\"decade[]\" value=\"1910\"  facet />')  {
    print(thepage[entriesCounter+1])
    tmpline = thepage[entriesCounter+1]
    tmpleft = gregexpr(pattern ='"1910',tmpline)
    tmpright = gregexpr(pattern ='</span>',tmpline)
    numberResults = substr(tmpline, tmpleft[[1]]+8, tmpright[[1]]-2)
    numberResults = trimws(gsub(",","",numberResults))
    numberResults = as.numeric(numberResults)
  }
}


#for(gatherPagesCounter in 1:(floor(numberResults/10))+1){
for(gatherPagesCounter in 1:99){
  
  thepage = readLines(paste("http://newspapers.library.wales/search?alt=full_text%3A%22allotment%22+AND+full_text%3A%22society%22+OR+full_text%3A%22societies%22&range%5Bmin%5D=1914-08-03T00%3A00%3A00Z&range%5Bmax%5D=1918-11-20T00%3A00%3A00Z","&page=",gatherPagesCounter,sep=""))
  # get rid of the tabs
  thepage = trimws(gsub("\t"," ",thepage))
  
  
  for (entriesCounter in 900:1573){
    #print(paste(entriesCounter,thepage[entriesCounter],sep=""))
    if(thepage[entriesCounter] == '<h2 class=\"result-title\">')  {
      # url
      entryId = trimws(gsub("\"","",gsub("\">","",gsub("<a href=","",thepage[entriesCounter+1]))))
      print(entryId)
      entryUrl <- paste("http://newspapers.library.wales/",entryId,sep="")
      # title
      entryTitle = trimws(gsub("</a>","",thepage[entriesCounter+2]))
      print( entryTitle)
    }
    
    if(thepage[entriesCounter] == '<ul class=\"result-meta row\">')  {
      
      print(thepage[entriesCounter+2])
      
      entryPaperTitle = trimws(gsub("</a>","",thepage[entriesCounter+3]))
      print(entryPaperTitle)
      
      # title
      
      # date
      
      entryPublished = trimws(gsub("</span>","",thepage[entriesCounter+8]))
      print(entryPublished)
      #page 
      
      entryPage=trimws(gsub("</li>","",thepage[entriesCounter+13]))
      print(entryPage)
      entryUpdated=""
      noticeText=""
      writeLines(paste("\"",entryId,"\",\"",entryUrl,"\",\"",entryPaperTitle,"\",\"",entryTitle,"\",\"",entryUpdated,"\",\"",entryPublished,"\",\"",entryPage,"\",\"",noticeText,"\"",sep=""),outputFileCsvCon)
    }
  }
  
  
  
  # thanks to https://stackoverflow.com/questions/1174799/how-to-make-execution-pause-sleep-wait-for-x-seconds-in-r
  # wait 2 seconds - don't stress the server
  p1 <- proc.time()
  Sys.sleep(5)
  proc.time() - p1
}

close(outputFileCsvCon)

