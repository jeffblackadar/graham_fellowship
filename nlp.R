#This code is from
#https://rpubs.com/lmullen/nlp-chapter

library(rJava)
library(NLP)
library(openNLP)
library(RWeka)

#read in a file
inputEquityTextFile <-paste("c:\\a_orgs\\carleton\\hist3814\\equity\\83471_1960-09-08.txt",sep="")
inputEquityTextFileCon  <- file(inputEquityTextFile, open = "r")
equityEditionText<- paste(readLines(inputEquityTextFileCon, n = -1, warn = TRUE), collapse = "\n")
close(inputEquityTextFileCon)

equityEditionString <- as.String(equityEditionText)

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

equityEdition_annotations <- annotate(equityEditionString, list(sent_ann, word_ann))
# class(equityEdition_annotations)
# head(equityEdition_annotations)
equityEdition_doc <- AnnotatedPlainTextDocument(equityEditionString, equityEdition_annotations)
sents(equityEdition_doc)

# This does not work for me.
#sents(bio_doc) %>% head(2)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")