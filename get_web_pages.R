
# to run this, in your R working directory make a directory that matches the variable "workingSubDirectory" (or change below)

setwd("C:/a_orgs/carleton/hist3814/R/graham_fellowship")
newspaperArchiveName = "National Library of Wales, Welsh Newspapers Online"
searchBaseURL = "http://newspapers.library.wales/"
searchDateRangeMin = "1914-08-03"
searchDateRangeMax = "1918-11-20"
searchDateRange = paste("&range%5Bmin%5D=",searchDateRangeMin,"T00%3A00%3A00Z&range%5Bmax%5D=",searchDateRangeMax,"T00%3A00%3A00Z",sep="")

# searchTerms = paste("search?alt=full_text%3A%22","allotment","%22+","AND","+full_text%3A%22","society","%22+","OR","+full_text%3A%22","societies","%22",sep="")
# searchURL = paste(searchBaseURL,searchTerms,searchDateRange,sep="")
# print(searchURL) 
# workingSubDirectory = "wales2"

#food production societies
#?alt=full_text%3A"Food+Production+society"+OR+full_text%3A"Food+Production+societies"
searchTerms = paste("search?alt=full_text%3A","%22Food+Production+society%22","OR","+full_text%3A","%22Food+Production+societies%22",sep="")
searchURL = paste(searchBaseURL,searchTerms,searchDateRange,sep="")
print(searchURL)
workingSubDirectory = "hist4500_food_production_societies"

#hist4500-royal-horticultural-society
# searchTerms = paste("search?alt=full_text%3A","%22royal+horticultural+society%22",sep="")
# searchURL = paste(searchBaseURL,searchTerms,searchDateRange,sep="")
# print(searchURL) 
# workingSubDirectory = "hist4500-royal-horticultural-society"




### functions
# function generates a footnote close to the Chicago style - each footnote will need editing for italics, spelling and punctuation.
generateFootNote<-function(articleTitle,newspaperName, editionDate, ArchiveName, articleURL){
  
  # Example from 
  # Chicago-Style Citation for Assignments in History: Notes & Bibliography Format (2015-2016)
  #“The Coming of Calgary: The Future Chicago of Western Canada,” The Times, January 25,
  #1912, The Times Digital Archive 1785-1985.
  
  return(paste(articleTitle,", ",newspaperName,", ", editionDate,", ", ArchiveName,", ",articleURL,", Accessed ", format(Sys.Date(), "%b %d %Y"),sep=""))
}

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if (hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

getEntitiesInText<-function(rawText){
  library(NLP)
  library(openNLP)
  library(RWeka)
  
  print(1)
  word_ann <- Maxent_Word_Token_Annotator()
  sent_ann <- Maxent_Sent_Token_Annotator()
  print(2)
  if (!rawText==""){
    #print(rawText)
    article_annotations <-  annotate(rawText, list(sent_ann, word_ann))
    article_doc <-AnnotatedPlainTextDocument(rawText, article_annotations)
    sents(article_doc)
    
    person_ann <- Maxent_Entity_Annotator(kind = "person")
    location_ann <- Maxent_Entity_Annotator(kind = "location")
    organization_ann <- Maxent_Entity_Annotator(kind = "organization")
    
    pipeline <- list(sent_ann, word_ann, person_ann, location_ann, organization_ann)
    article_annotations <- annotate(rawText, pipeline)
    article_doc <-  AnnotatedPlainTextDocument(rawText, article_annotations)
    
    
    peopleIn<-sort(unique(entities(article_doc, kind = "person")))
    locationsIn<-sort(unique(entities(article_doc, kind = "location")))
    organizationsIn<-sort(unique(entities(article_doc, kind = "organization")))

    
    return(c(peopleIn,locationsIn, organizationsIn))
  } else
    # empty text - return nothing
    return(c("","",""))
}


# function dowloads the text of an individual article
processArticleWebPage<-function(articleURL, articleTitle, articleID, articleDate){
  theArticlepage = readLines(articleURL)
  
  # wait 5 seconds - don't stress the server
  p1 <- proc.time()
  Sys.sleep(2)
  proc.time() - p1
  
  # get rid of the tabs
  theArticlepage = trimws(gsub("\t"," ",theArticlepage))
  articleText=""
  #look for something like: <span itemprop="name">OUR MILITARY COLUMN</span>
  findLine = paste("<span itemprop=\"name\">",articleTitle,"</span>",sep="")
  print(findLine)
  print(articleURL)
  
  for (articleLinesCounter in 1:length(theArticlepage)){
    #print(paste(articleLinesCounter,theArticlepage[articleLinesCounter],sep=""))
    if(theArticlepage[articleLinesCounter] == findLine){
      
      repeat{
        articleLinesCounter=articleLinesCounter+1
        if(theArticlepage[articleLinesCounter]=="<span itemprop=\"articleBody\">"){
          break
        }
      }
      
      repeat{
        articleLinesCounter=articleLinesCounter+1
        if(theArticlepage[articleLinesCounter]=="</p>"){
          break
        }
        articleText=paste(articleText,theArticlepage[articleLinesCounter],sep="")
      }
      #found the article, breaking out
      articleLinesCounter=length(theArticlepage)
    }
  }
  # write the article to a file
  outputFileHTML <- paste(gsub("/","-",articleID),".htm",sep="")
  outputFileHTMLCon<-file(paste(workingSubDirectory,"/",outputFileHTML,sep=""), open = "w")
  
  writeLines(paste("<h1>",articleTitle,"</h1>",sep=""),outputFileHTMLCon)
  writeLines(paste("<p>published: ",articleDate,"</p>",sep=""),outputFileHTMLCon)
  writeLines(paste("<a href=\"",articleURL,"\">",articleURL,"</a><p>",sep=""),outputFileHTMLCon)
  
  entities<-getEntitiesInText(articleText)
  writeLines("<p>people: ",outputFileHTMLCon)
  for (ent in entities[1]){
    writeLines(ent,outputFileHTMLCon)
  }
  writeLines("<p>locations: ",outputFileHTMLCon)
  for (ent in entities[2]){
    writeLines(ent,outputFileHTMLCon)
  }  
  writeLines("<p>organizations: ",outputFileHTMLCon)
  for (ent in entities[3]){
    writeLines(ent,outputFileHTMLCon)
  }  
  writeLines("<p>",outputFileHTMLCon)
  
  writeLines(articleText,outputFileHTMLCon)
  close(outputFileHTMLCon)
  
  shortArticleText<-articleText
  if(nchar(shortArticleText)>99){
    shortArticleText=substr(shortArticleText,1,99)
  }
  
  return(c(outputFileHTML,shortArticleText))
}



outputFileCsv <- paste(workingSubDirectory,"/1",workingSubDirectory,"_papers.csv",sep="")
outputFileCsvCon<-file(outputFileCsv, open = "w")
lineOut<-paste("Entry Number","\",\"","Entry Id","\"","Entry Url","\",\"", "Newspaper Title","\",\"","Article Title","\",\"","Entry Updated","\",\"","Date Published","\",\"","Page Number","\",\"","Citation","\",\"","Article start","\",\"","Notice Text","\"",sep="")
writeLines(lineOut,outputFileCsvCon)


outputFileHTMLList <- paste(workingSubDirectory,"/1",workingSubDirectory,"_papers.html",sep="")
outputFileHTMLListCon<-file(outputFileHTMLList, open = "w")

#thanks to
#https://statistics.berkeley.edu/computing/r-reading-webpages


thepage = readLines(searchURL)
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

entriesProcessed = 0
# go through each page of search results
for(gatherPagesCounter in 1:(floor(numberResults/12)+1)){
#for(gatherPagesCounter in 1:3){
  
  thepage = readLines(paste(searchURL,"&page=",gatherPagesCounter,sep=""))
  # get rid of the tabs
  thepage = trimws(gsub("\t"," ",thepage))
  
  for (entriesCounter in 500:length(thepage)){
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
      
      # title
      entryPaperTitle = trimws(gsub("</a>","",thepage[entriesCounter+3]))
      print(entryPaperTitle)
      
      # date
      entryPublished = trimws(gsub("\"","",gsub("</span>","",thepage[entriesCounter+8])))
      print(entryPublished)
      
      # page
      entryPage=trimws(gsub("\"","",gsub("</li>","",thepage[entriesCounter+13])))
      print(entryPage)
      entryUpdated=""
      
      noticeText=""
      entriesProcessed = entriesProcessed+1
      
      processArticleReturn=processArticleWebPage(entryUrl, entryTitle, entryId,entryPublished)
      articleFile=processArticleReturn[1]

      footNote=generateFootNote(entryTitle,entryPaperTitle, entryPublished, newspaperArchiveName, entryUrl)
      lineOut<-paste(entriesProcessed,",\"",entryId,"\",\"",entryUrl,"\",\"",entryPaperTitle,"\",\"",entryTitle,"\",\"",entryUpdated,"\",\"",entryPublished,"\",\"",entryPage,"\",\"",footNote,"\",\"",processArticleReturn[2],"\",\"",noticeText,"\"",sep="")
      print (lineOut)
      
      writeLines(lineOut,outputFileCsvCon)
      
      writeLines(paste(entriesProcessed," <a href=\"",articleFile,"\">","local file","</a> <a href=\"",entryUrl," ","\">","web","</a> ",entryTitle,"  ",processArticleReturn[2],"<br>",sep=""),outputFileHTMLListCon)
      
      #clean up memory, the NLP work is memory intensive.  Keep the objects we need, including functions
      objectsToKeep<-c("localuserpassword","gatherPagesCounter","entriesCounter","entities","thepage","searchURL", "workingSubDirectory","processArticleWebPage","generateFootNote","getEntitiesInText","outputFileCsvCon","outputFileHTMLListCon","entriesProcessed","newspaperArchiveName","numberResults")
      rm(list=setdiff(ls(),objectsToKeep ))
      gc()
    }
  }
  
  # thanks to https://stackoverflow.com/questions/1174799/how-to-make-execution-pause-sleep-wait-for-x-seconds-in-r
  # wait 3 seconds - don't stress the server
  p1 <- proc.time()
  Sys.sleep(3)
  proc.time() - p1
  
  
}

close(outputFileCsvCon)
close(outputFileHTMLListCon)


