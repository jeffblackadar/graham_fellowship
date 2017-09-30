# this program will connect to the database of source_documents, people, organization and location entities.
# for each entity it will list what editions they appear in in markdown

library(RMySQL)
output_mydb = dbConnect(MySQL(), user='localuser', password=localuserpassword, dbname='corpus_entities', host='localhost')

# process people.  This will become a function after this is working
output_query<-paste("select name from entities_people order by name",sep='')
output_rs = dbSendQuery(output_mydb,output_query)
output_dbRows<-dbFetch(output_rs)
if (nrow(output_dbRows)==0){
  print (paste("Problem: zero rows for ",output_query,sep=''))
} else {
  for (i in 1:nrow(output_dbRows)) {
    print(output_dbRows[i, 1])
  }
}
