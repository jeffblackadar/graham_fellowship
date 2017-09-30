# this program will connect to the database of source_documents, people, organization and location entities.
# for each entity it will list what editions they appear in in markdown

outputFilePeopleListHtml <- "C:/a_orgs/carleton/hist3814/R/graham_fellowship/people/list_people.html"
outputFilePeopleListHtmlCon<-file(outputFilePeopleListHtml, open = "w")
writeLines('<html><head><title></title></head><body><h1>List of people in the editions of the Shawville Equity</h1>', outputFilePeopleListHtmlCon)

library(RMySQL)
library(stringr)

output_mydb = dbConnect(MySQL(), user='localuser', password=localuserpassword, dbname='corpus_entities', host='localhost')

# process people.  This will become a function after this is working
output_query<-paste("select name from entities_people  where ucase(name) like 'A%' order by name",sep='')
output_rs = dbSendQuery(output_mydb,output_query)
output_dbRows<-dbFetch(output_rs, 999999)
if (nrow(output_dbRows)==0){
  print (paste("Problem: zero rows for ",output_query,sep=''))
} else {
  for (i in 1:nrow(output_dbRows)) {
    print(output_dbRows[i, 1])
    
    output_query<-paste("Select entities_people.id_entities_person, entities_people.name, source_documents.source_document_name, source_documents.source_document_base_url, source_documents.source_document_file_extension_1, source_documents.source_document_file_extension_2 from entities_people left join people_x_sourcedocuments on entities_people.id_entities_person = people_x_sourcedocuments.id_entities_person left join source_documents on people_x_sourcedocuments.id_source_document = source_documents.id_source_document where entities_people.name = '",output_dbRows[i, 1],"' group by source_documents.source_document_name order by entities_people.name ",sep='')
    output_entity_rs = dbSendQuery(output_mydb,output_query)
    output_personRows<-dbFetch(output_entity_rs)
    if (nrow(output_personRows)==0){
      print (paste("Problem: zero rows for ",output_query,sep=''))
    } else {
      # create a file
      
      personFileName=output_personRows[1,1]
      personName=output_personRows[1,2]
      #personFileName = gsub(" ", "+", output_dbRows[i, 1])
      #personFileName = gsub("/", "+", personFileName, fixed=TRUE)
      #personFileName = gsub(",", "+", personFileName, fixed=TRUE)
      #personFileName = str_replace_all(personFileName, '\\"', "++")
      #personFileName = str_replace_all(personFileName, "¦", "+")
      
      #personFileName = str_replace_all(personFileName, ",", "+")
      #personFileName = str_replace_all(personFileName, '\\t', "++")
      
      
      
      writeLines(paste("<br><a href=",personFileName,".html>",personName,"</a>",sep=""),outputFilePeopleListHtmlCon)

      outputFilePeopleHtml <- paste("C:/a_orgs/carleton/hist3814/R/graham_fellowship/people/",personFileName,".html",sep="")
      outputFilePeopleHtmlCon<-file(outputFilePeopleHtml, open = "w")
      writeLines(paste("<html><head><title></title></head><body><h1>",personName,"</h1>",sep=""), outputFilePeopleHtmlCon)
      for (j in 1:nrow(output_personRows)) {
        writeLines(paste("<br><a href=",output_personRows[j, 4],output_personRows[j, 3],".",output_personRows[j, 6],">",output_personRows[j, 3],"</a>",sep=""),outputFilePeopleHtmlCon)
      }
      
      close(outputFilePeopleHtmlCon)
    }
  }
}

output_dbRows<-0
dbClearResult(output_rs)
dbDisconnect(output_mydb)

close(outputFilePeopleListHtmlCon)

