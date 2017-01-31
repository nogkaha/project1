#getting ready.
original_wd<-getwd()  
setwd("C:/Google Drive/work/new-spirit/Artists")
Sys.setlocale("LC_ALL", "Hebrew")
library(xlsx)
library(dplyr)
#getting and cleaning mailing list
mailing_list<- read.xlsx2("./participants/mailing_list_toQP.xlsx",header=T,sheetIndex = 1,stringsAsFactors=F)
mailing_list<-mailing_list %>% 
  filter(!grepl("new-spirit", mailing_list$mail)) %>%
  filter(mail!=c("shirisinparis@gmail.com")) 

#reading working file:
dataset <- readRDS(file="./data-Artists/working_file") 

#attaching sum to mailing list in order to create answering report.
response_sum<-left_join(mailing_list,select(dataset,respondent_email,max_point),
                        by=c("mail"="respondent_email")) %>%
  arrange(max_point)

#making a new_mailing list (including everyone who didn't finish)
not_responded<-response_sum %>% 
  filter(is.na(max_point)|max_point<3 & max_point>-1) %>% select(-max_point)

#exporting list to Excel 
write.xlsx(not_responded, "./outputs/Did_not_response.xlsx", sheetName="email_list", 
           col.names=TRUE, row.names=F, append=FALSE,showNA = F)
write.csv(not_responded,file="./outputs/Did_not_response.csv",col.names=F,row.names=F,fileEncoding = "UTF-8")
write.xlsx(select(response_sum,-shevet,-gender), "./outputs/response_sum.xlsx", sheetName="response_sum", 
           col.names=TRUE, row.names=F, append=FALSE,showNA = F)


ds_gender<- left_join(select(dataset, respondent_email,new_gender),
                      mailing_list,
                      by=c("mail"="respondent_email"))