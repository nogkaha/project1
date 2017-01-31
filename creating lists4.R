#getting ready.
 original_wd<-getwd()  
 setwd("C:/Google Drive/work/new-spirit/Artists")
 Sys.setlocale("LC_ALL", "Hebrew")
 library(xlsx)
 library(dplyr)
#reading working file:
 dataset <- readRDS(file="./data-Artists/working_file")
 
#making a list with new data on gender.#TO FIX IT#I think that the gender field was 
 #updated during the survey so I need to join 
 #the mailing list to the new field and to check what are the new values. 

 # %>% #subsetting to relevant cols 
  
#making a list with new data on email
new_email<-dataset %>% 
  select(respondent_email,email) %>% #subsetting to relevant cols
  filter(!email=="") %>% #keeping responses with new-email
  filter(as.character(email)!=as.character(respondent_email))#making sure it's new.
  
#making a list of participants who moved away
new_location<-dataset %>% 
  select(respondent_email,place:reasons_leaving5) %>% #subsetting to relevant cols 
  filter(place!=1) %>% #keeping responses who moved away
  mutate(place=recode(as.character(place),
                                   "2"="the center",
                                   "3"= "north",
                                   "4"="south",
                                   "5"="other"),
         lived_in_past=recode(as.character(lived_in_past),
                        "1"="left 3 months ago",
                        "2"="left more than 3 months ago",
                        "3"="never lived in Jerusalem"))
 
#recoding the "1" as the col's labels:
  new_location[,4:8]<-sapply(new_location[,4:8],
                           function(x) ifelse (x=="1",substr(attr(x,"label"),19,nchar(attr(x,"label"))) ,NA))

#making a list of text comments
  text_comments<-dataset %>% 
    filter(!make_project_t==""|!comments=="") %>% #keeping responses with text
    select(respondent_email,make_project_t,comments)  #subsetting to relevant cols 
  text_comments<-sapply(text_comments, function(x){
    x<-gsub("[\r\n]", " ",x) 
    gsub("\\[כן\\]", "",x)})
  
#rapping the data in an excel file
write.xlsx(text_comments, "./outputs/data_lists.xlsx", sheetName="comments", 
             col.names=TRUE, row.names=F, append=FALSE,showNA = F)
write.xlsx(ds_gender,"./outputs/data_lists.xlsx", sheetName="gender", 
           col.names=TRUE, row.names=F, append=T,showNA = F)
write.xlsx(new_location, "./outputs/data_lists.xlsx", sheetName="location", 
           col.names=TRUE, row.names=F, append=T,showNA = F)
write.xlsx(new_email, "./outputs/data_lists.xlsx", sheetName="new_emails", 
           col.names=TRUE, row.names=F, append=T,showNA = F)
