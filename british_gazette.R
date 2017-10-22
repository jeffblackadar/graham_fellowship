#install.packages("rjson")
#library("rjson")
#json_file <- "http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"
#json_data <- fromJSON(file=json_file)


#json_file <- "https://www.thegazette.co.uk/all-notices/notice/data.json?end-publish-date=1918-11-11&text=potatoes+wart+schedule&start-publish-date=1914-08-03&location-distance-1=1&service=all-notices&categorycode-all=all&numberOfLocationSearches=1"

#json_data <- fromJSON(readLines(json_file)[1])

library(XML)
xml_file <- "https://www.thegazette.co.uk/all-notices/notice/data.feed?end-publish-date=1918-11-11&text=potatoes+wart+schedule&start-publish-date=1914-08-03&location-distance-1=1&service=all-notices&categorycode-all=all&numberOfLocationSearches=1&results-page=20"
xmlfile <- xmlTreeParse(readLines(xml_file)[1])
#class(xmlfile)
topxml <- xmlRoot(xmlfile)
topxml <- xmlSApply(topxml,function(x) xmlSApply(x, xmlValue))
xml_df <- data.frame(t(topxml), row.names=NULL)
xml_df$total


#xml_df

xml_df$page.number[1]
xml_df$page.size[1]
xml_df$page.start[1]
xml_df$page.stop[1]
xml_df$total[1]



entry<-xml_df$entry.2[1]
entry[[1]]$id
entry[[1]]$title
entry[[1]]$updated
entry[[1]]$published






# here is a pdf for mining

url <- "https://www.thegazette.co.uk/London/issue/28981/page/9556/data.pdf"
dest <- tempfile(fileext = ".pdf")
download.file(url, dest, mode = "wb")

# set path to pdftotxt.exe and convert pdf to text

exe <- "C:\\a_orgs\\carleton\\hist3814\\R\\graham_fellowship\\pdftools\\bin64\\pdftotext.exe"
system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

# get txt-file name and open it
filetxt <- sub(".pdf", ".txt", dest)
shell.exec(filetxt); shell.exec(filetxt) # strangely the first try always throws an error..

# do something with it, i.e. a simple word cloud
library(tm)


txt <- readLines(filetxt) # don't mind warning..




xml_file <- "https://www.thegazette.co.uk/notice/29815/data.xml"
xmlfile <- xmlTreeParse(readLines(xml_file)[1])
#class(xmlfile)
topxml <- xmlRoot(xmlfile)
topxml <- xmlSApply(topxml,function(x) xmlSApply(x, xmlValue))
xml_df <- data.frame(t(topxml), row.names=NULL)
xml_df$total

