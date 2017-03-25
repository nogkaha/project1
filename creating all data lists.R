#getting ready.
original_wd<-getwd()  
setwd("C:/Google Drive/work/new-spirit/Artists")
Sys.setlocale("LC_ALL", "Hebrew")
library(xlsx)
library(dplyr)
options(stringsAsFactors = F)
#getting and cleaning mailing list
mailing_list<- read.xlsx2("./participants/mailing_list_toQP.xlsx",header=T,sheetIndex = 1,stringsAsFactors=F)
mailing_list<-mailing_list %>% 
  filter(!grepl("new-spirit", mailing_list$mail)) %>%
  filter(mail!=c("shirisinparis@gmail.com")) 

#reading working file and attaching metadata.
dataset <- readRDS(file="./data-Artists/working_file") %>% 
  left_join(mailing_list,by=c("respondent_email"="mail"))

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

#creating a list for updating gender data
ds_gender<- select(dataset, respondent_email,gender,new_gender,FirstName, LastName) %>%
  filter(is.na(gender)|gender=="") %>% 
  select(respondent_email, FirstName, LastName,new_gender)

#creating a list for updating email data
new_email<-dataset %>% 
  select(respondent_email,FirstName, LastName,email) %>% #subsetting to relevant cols
  filter(!email=="") %>% #keeping responses with new-email
  filter(as.character(tolower(email))!=as.character(tolower(respondent_email)))   #making sure it's new.

#creating a list of participants who moved away
source ("C:/Google Drive/work/new-spirit/Artists/analyzing/project1/FUN_get_labels.R") #load a function to match var's values
factor_labels<-read.xlsx("./data-Artists/QP_Nov_2016/factor_labels.xlsx",sheetIndex = 1,encoding = "UTF-8")
new_location<-dataset %>% 
  select(respondent_email,FirstName, LastName,place:reasons_leaving5) %>% #subsetting to relevant cols 
  filter(place!=1) #keeping responses who moved away
#get the labels:
new_location<-new_location %>%
  mutate_at(vars(place),funs(get_labels(new_location,"place",factor_labels))) %>%
  mutate_at(vars(lived_in_past),funs(get_labels(new_location,"lived_in_past",factor_labels)))
  
#getting headings labels:
heading_path<-"./data-Artists/QP_Nov_2016/headings.xlsx"
headings<- read.xlsx(heading_path,sheetIndex = 1, encoding="UTF-8",header = T,as.data.frame = T)
#recoding labels according to the var's name:
loc_names<-colnames(new_location)
new_location[,6:10]<-sapply(loc_names[6:10],
                           function(x) {ifelse(new_location[,x]=="1",
                                               headings[match(x,headings$new_var_name),4]
                                               ,NA)})
#making a list of text comments
text_comments<-dataset %>% 
  filter(!make_project_t==""|!comments=="") %>% #keeping responses with text
  select(respondent_email,FirstName, LastName,make_project_t,comments)  #subsetting to relevant cols 
text_comments<-sapply(text_comments, function(x){
  x<-gsub("[\r\n]", " ",x) 
  gsub("\\[כן\\]", "",x)})

#rapping the data in an excel file
write.xlsx(as.data.frame(dataset), "./outputs/new_data_lists.xlsx", sheetName="alldata", 
           col.names=TRUE, row.names=F, append=FALSE,showNA = F)
write.xlsx(text_comments, "./outputs/new_data_lists.xlsx", sheetName="comments", 
           col.names=TRUE, row.names=F, append=T,showNA = F)
write.xlsx(as.data.frame(ds_gender),"./outputs/new_data_lists.xlsx", sheetName="new_gender", 
           col.names=T, row.names=F, append=T,showNA = F)
write.xlsx(as.data.frame(new_location), "./outputs/new_data_lists.xlsx", sheetName="location", 
           col.names=TRUE, row.names=F, append=T,showNA = F)
write.xlsx(as.data.frame(new_email), "./outputs/new_data_lists.xlsx", sheetName="new_emails", 
           col.names=TRUE, row.names=F, append=T,showNA = F)
