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

#attaching sum to mailing list:
response_sum<-left_join(mailing_list,select(clean_ds2,respondent_email,sum),
                        by=c("mail"="respondent_email"))



#compare working file with mailing list. #to check!
not_respond<-anti_join(mailing_list,clean_ds,by=c("mail"="respondent_email"))

#exporting list to Excel 
write.xlsx(not_respond, "./outputs/Did_not_response.xlsx", sheetName="email_list", 
           col.names=TRUE, row.names=F, append=FALSE,showNA = F)
write.csv(not_respond,file="./outputs/Did_not_response.csv",col.names=T,row.names=F,fileEncoding = "UTF-8")
write.xlsx(response_sum, "./outputs/response_sum.xlsx", sheetName="response_sum", 
           col.names=TRUE, row.names=F, append=FALSE,showNA = F)