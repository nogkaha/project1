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
clean_ds<-dataset %>% filter(!is.na(respondent_email))
clean_ds$sum<-ifelse(!is.na(clean_ds$make_project)|
                       as.numeric(clean_ds$lived_in_past)!=1 & !is.na(clean_ds$lived_in_past),3,
                     ifelse(!is.na(clean_ds$city_general7),2,
                            ifelse(!is.na(clean_ds$privious_interaction),1,0)))

#keep only the best response for each subject. 
clean_ds2<-clean_ds %>% group_by(respondent_email) %>% 
  mutate(max_point=max(sum)) %>% filter(sum==max_point)


#attaching sum to mailing list:
response_sum<-left_join(mailing_list,select(clean_ds2,respondent_email,sum),
                        by=c("mail"="respondent_email"))


#clean_ds<-clean_ds %>% filter(sum!=0)




#compare working file with mailing list. #to check!
not_respond<-anti_join(mailing_list,clean_ds,by=c("mail"="respondent_email"))

#exporting list to Excel 
write.xlsx(not_respond, "./outputs/Did_not_response.xlsx", sheetName="email_list", 
           col.names=TRUE, row.names=F, append=FALSE,showNA = F)
write.csv(not_respond,file="./outputs/Did_not_response.csv",col.names=T,row.names=F,fileEncoding = "UTF-8")
write.xlsx(response_sum, "./outputs/response_sum.xlsx", sheetName="response_sum", 
           col.names=TRUE, row.names=F, append=FALSE,showNA = F)