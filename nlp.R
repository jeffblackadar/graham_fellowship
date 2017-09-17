#This code is from
#https://rpubs.com/lmullen/nlp-chapter

library(rJava)
library(NLP)
library(openNLP)
library(RWeka)
#install.packages("RMySQL")
library(RMySQL)


#https://www.r-bloggers.com/connecting-r-to-mysqlmariadb/
#library(DBI)
#mydb <- dbConnect(RMySQL::MySQL(), group = "group-name")
#dbListTables(mydb)

#connect to the database !!! There is a way better way to do this,


mydb = dbConnect(MySQL(), user='localuser', password=localuserpassword, dbname='corpus_entities', host='localhost')

#RMySQL.settingsfile<-"C:\\ProgramData\\MySQL\\MySQL Server 5.7\\my.cnf"
#mydb2 <- dbConnect(RMySQL::MySQL(), group = "corpus_entities")


dbListTables(mydb)
#dbListFields(mydb, 'sourcedocument')


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

#openconnecton to read file
#inputFile <- "equityurls.txt"
#using a shorter test file, each edition takes a long time.
inputFile <- "equityurls_1970.txt"

inputCon  <- file(inputFile, open = "r")

#open connectons to wite output files
outputFilePeopleHtml <- "equityeditions_people.html"
outputFilePeopleHtmlCon<-file(outputFilePeopleHtml, open = "w")
outputFileLocationsHtml <- "equityeditions_locations.html"
outputFileLocationsHtmlCon<-file(outputFileLocationsHtml, open = "w")
outputFileOrganizationsHtml <- "equityeditions_organizations.html"
outputFileOrganizationsHtmlCon<-file(outputFileOrganizationsHtml, open = "w")

#set up web page at top of html
writeLines('<html><head><title></title></head><body><h1>Editions of the Shawville Equity</h1><h2>With people referenced.</h2><table border=1>', outputFilePeopleHtmlCon)
writeLines('<tr><th>Date of<br> edition</th><th>.pdf</th><th>.txt</th><th>People</th></tr>', outputFilePeopleHtmlCon)
writeLines('<html><head><title></title></head><body><h1>Editions of the Shawville Equity.</h1><h2>With locations referenced.</h2><table border=1>', outputFileLocationsHtmlCon)
writeLines('<tr><th>Date of<br> edition</th><th>.pdf</th><th>.txt</th><th>Locations</th></tr>', outputFileLocationsHtmlCon)
writeLines('<html><head><title></title></head><body><h1>Editions of the Shawville Equity.</h1><h2>With organizations referenced.</h2><table border=1>', outputFileOrganizationsHtmlCon)
writeLines('<tr><th>Date of<br> edition</th><th>.pdf</th><th>.txt</th><th>Organizations</th></tr>', outputFileOrganizationsHtmlCon)

while (length(urlLine <-
              readLines(inputCon, n = 1, warn = FALSE)) > 0) {
  urlLineElements = strsplit(urlLine, "/")
  #make sure it is a complete line
  if (length(urlLineElements[[1]]) > 8) {
    #Note the %Y indicates a 4 year date
    dateOfEdition <-
      as.Date(
        paste(
          urlLineElements[[1]][7],
          '/',
          urlLineElements[[1]][8],
          '/',
          urlLineElements[[1]][9],
          sep = ""
        ),
        "%Y/%m/%d"
      )
    print(dateOfEdition)
    outputLine <-
      paste(
        urlLine,
        ',',
        urlLineElements[[1]][7],
        ',',
        urlLineElements[[1]][8],
        ',',
        urlLineElements[[1]][9],
        ',',
        format(dateOfEdition, format = "%a"),
        sep = ""
      )
    #print (outputLine)
    
    #read in a file
    #inputEquityTextFile <-
    #paste("c:\\a_orgs\\carleton\\hist3814\\equity\\83471_1960-09-08.txt",
    #     sep = "")
    
    #set file name, make it look like this: 83471_1920-06-10.pdf
    if (length(urlLineElements[[1]])==10){
      #editions with dates like these (/2010/08/25/01/) are supplements  
      writeLines(paste('<td><a href="',urlLine,'">',format(dateOfEdition,format='%Y/%m/%d'),' (sup.)','</a></td>',sep=""),outputFilePeopleHtmlCon)
      writeLines(paste('<td><a href="',urlLine,'">',format(dateOfEdition,format='%Y/%m/%d'),' (sup.)','</a></td>',sep=""),outputFileLocationsHtmlCon)
      writeLines(paste('<td><a href="',urlLine,'">',format(dateOfEdition,format='%Y/%m/%d'),' (sup.)','</a></td>',sep=""),outputFileOrganizationsHtmlCon)
      editionFileName<-paste('83471_',urlLineElements[[1]][7],'-',urlLineElements[[1]][8],'-',urlLineElements[[1]][9],'-',urlLineElements[[1]][10],sep='')  
    } else {
      writeLines(paste('<td><a href="',urlLine,'">',format(dateOfEdition,format='%Y/%m/%d'),'</a></td>',sep=""),outputFilePeopleHtmlCon)
      writeLines(paste('<td><a href="',urlLine,'">',format(dateOfEdition,format='%Y/%m/%d'),'</a></td>',sep=""),outputFileLocationsHtmlCon)
      writeLines(paste('<td><a href="',urlLine,'">',format(dateOfEdition,format='%Y/%m/%d'),'</a></td>',sep=""),outputFileOrganizationsHtmlCon)
      editionFileName<-paste('83471_',urlLineElements[[1]][7],'-',urlLineElements[[1]][8],'-',urlLineElements[[1]][9],sep='')        
    }
    
    writeLines(paste('<td><a href="',urlLine,editionFileName,'.pdf">.pdf</a></td>',sep=""),outputFilePeopleHtmlCon)
    writeLines(paste('<td><a href="',urlLine,editionFileName,'.txt">.txt</a></td>',sep=""),outputFilePeopleHtmlCon)
    writeLines(paste('<td><a href="',urlLine,editionFileName,'.pdf">.pdf</a></td>',sep=""),outputFileLocationsHtmlCon)
    writeLines(paste('<td><a href="',urlLine,editionFileName,'.txt">.txt</a></td>',sep=""),outputFileLocationsHtmlCon)
    writeLines(paste('<td><a href="',urlLine,editionFileName,'.pdf">.pdf</a></td>',sep=""),outputFileOrganizationsHtmlCon)
    writeLines(paste('<td><a href="',urlLine,editionFileName,'.txt">.txt</a></td>',sep=""),outputFileOrganizationsHtmlCon)
    
    #Store equity edition into database
    #first, check if it is already in the database
    query<-paste("select * from sourcedocuments where sourcedocumentname='",editionFileName,"'",sep='')
    print (query)
    rs = dbSendQuery(mydb,query)
    dbRows<-dbFetch(rs)
    #dbFetch() always returns a data.frame with as many rows as records were fetched and as many columns as fields in the result set, even if the result is a single value or has one or zero rows
    #print (nrow(dbRows))
    
    if (nrow(dbRows)==0){
      query<-paste("INSERT INTO sourcedocuments (sourcedocumentname,sourcedocumentbaseurl,sourcedocumentfileextension1,sourcedocumentfileextension2) VALUES('",editionFileName,"','",urlLine,"','pdf','txt')",sep='')
      print (query)
      rsInsert = dbSendQuery(mydb,query)
      dbClearResult(rsInsert)
    }
    
    dbClearResult(rs)
    
    
    #Perform Natural Language Processing on the file
    
    inputEquityTextFile <-paste("c:\\a_orgs\\carleton\\hist3814\\equity\\",editionFileName,".txt",sep="")
    
    inputEquityTextFileCon  <- file(inputEquityTextFile, open = "r")
    equityEditionText <-
      paste(readLines(inputEquityTextFileCon, n = -1, warn = TRUE),
            collapse = "\n")
    close(inputEquityTextFileCon)
    
    equityEditionString <- as.String(equityEditionText)
    
    word_ann <- Maxent_Word_Token_Annotator()
    sent_ann <- Maxent_Sent_Token_Annotator()
    
    equityEdition_annotations <-
      annotate(equityEditionString, list(sent_ann, word_ann))
    # class(equityEdition_annotations)
    # head(equityEdition_annotations)
    equityEdition_doc <-
      AnnotatedPlainTextDocument(equityEditionString, equityEdition_annotations)
    sents(equityEdition_doc)
    
    # This does not work for me.
    #sents(bio_doc) %>% head(2)
    
    # bio = equityEditionString
    # bio_annotations = equityEdition_annotations
    # bio_doc = equityEdition_doc
    
    
    person_ann <- Maxent_Entity_Annotator(kind = "person")
    location_ann <- Maxent_Entity_Annotator(kind = "location")
    organization_ann <- Maxent_Entity_Annotator(kind = "organization")
    
    pipeline <- list(sent_ann,
                     word_ann,
                     person_ann,
                     location_ann,
                     organization_ann)
    equityEdition_annotations <- annotate(equityEditionString, pipeline)
    equityEdition_doc <-
      AnnotatedPlainTextDocument(equityEditionString, equityEdition_annotations)
    
    peopleInEdition<-sort(unique(entities(equityEdition_doc, kind = "person")))
    topicsInEditionString<-paste(c(peopleInEdition), collapse=', ' )
    thisCell<-paste('<td>',topicsInEditionString,'</td>',sep="")
    writeLines(thisCell,outputFilePeopleHtmlCon)
    
    
    locationsInEdition<-sort(unique(entities(equityEdition_doc, kind = "location")))
    topicsInEditionString<-paste(c(locationsInEdition), collapse=', ' )
    thisCell<-paste('<td>',topicsInEditionString,'</td>',sep="")
    writeLines(thisCell,outputFileLocationsHtmlCon)
    
    
    organizationsInEdition<-sort(unique(entities(equityEdition_doc, kind = "organization")))
    topicsInEditionString<-paste(c(organizationsInEdition), collapse=', ' )
    thisCell<-paste('<td>',topicsInEditionString,'</td>',sep="")
    writeLines(thisCell,outputFileOrganizationsHtmlCon)
    
    
    
    
    writeLines('</tr>', outputFilePeopleHtmlCon)
    writeLines('</tr>', outputFileLocationsHtmlCon)
    writeLines('</tr>', outputFileOrganizationsHtmlCon)
  }
}


close(inputCon)
dbDisconnect(mydb)

writeLines('</table></body></html>', outputFilePeopleHtmlCon)
writeLines('</table></body></html>', outputFileLocationsHtmlCon)
writeLines('</table></body></html>', outputFileOrganizationsHtmlCon)
close(outputFilePeopleHtmlCon)
close(outputFileLocationsHtmlCon)
close(outputFileOrganizationsHtmlCon)


