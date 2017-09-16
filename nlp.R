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
print ("Enter the password for the MySQL localuser:")
localuserpassword <- readline(prompt = "")

mydb = dbConnect(MySQL(), user='localuser', password=localuserpassword, dbname='corpus_entities', host='localhost')

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
inputFile <- "equityurls_test2.txt"
inputCon  <- file(inputFile, open = "r")

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
      #writeLines(paste('<td><a href="',urlLine,'">',format(dateOfEdition,format='%Y/%m/%d'),' (sup.)','</a></td>',sep=""),outputFileHtmlCon)
      editionFileName<-paste('83471_',urlLineElements[[1]][7],'-',urlLineElements[[1]][8],'-',urlLineElements[[1]][9],'-',urlLineElements[[1]][10],sep='')  
    } else {
      #writeLines(paste('<td><a href="',urlLine,'">',format(dateOfEdition,format='%Y/%m/%d'),'</a></td>',sep=""),outputFileHtmlCon)
      editionFileName<-paste('83471_',urlLineElements[[1]][7],'-',urlLineElements[[1]][8],'-',urlLineElements[[1]][9],sep='')        
    }
    
    
    #Store equity edition into database
    #first, check if it is already in the database
    query<-paste("select sourcedocumentname='",editionFileName,"' from sourcedocuments",sep='')
    print (query)
    rs = dbSendQuery(mydb,query)
    dbRows<-dbFetch(rs)
    dbRows[[1]]
    if (length(dbRows[[1]])>0){
      if (dbRows[[1]]==0){
      query<-paste("INSERT INTO sourcedocuments (sourcedocumentname,sourcedocumentbaseurl,sourcedocumentfileextension1,sourcedocumentfileextension2) VALUES('",editionFileName,"','",urlLine,"','pdf','txt')",sep='')
      print (query)
      rsInsert = dbSendQuery(mydb,query)
      dbClearResult(rsInsert)
      }
    }
    
    dbClearResult(rs)
    
    

    
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
    
    sort(unique(entities(equityEdition_doc, kind = "person")))
    sort(unique(entities(equityEdition_doc, kind = "location")))
    sort(unique(entities(equityEdition_doc, kind = "organization")))
    
  }
}
close(inputCon)
dbDisconnect(mydb)



