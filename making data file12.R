#getting ready.
original_wd<-getwd()  
setwd("C:/Google Drive/work/new-spirit/Artists")
Sys.setlocale("LC_ALL", "Hebrew")
library(xlsx)
library(dplyr)
library(tidyr)
options(stringsAsFactors = F)

#get data 
males_path<-"./data-Artists/QP_Nov_2016/males.xlsx"
females_path<-"./data-Artists/QP_Nov_2016/females.xlsx"
var_class_path<-"./data-Artists/QP_Nov_2016/var_class.xlsx"
heading_path<-"./data-Artists/QP_Nov_2016/headings.xlsx"
var_class<-read.xlsx2(var_class_path,header=T,sheetIndex = 2,endRow = 77,stringsAsFactors=F, colClasses = c(rep("character",4)))
dataset_m<- read.xlsx2(males_path,header=F,sheetIndex = 3,startRow = 3,colIndex = c(1:76),stringsAsFactors=F,colClasses = var_class$class_males)
dataset_f<- read.xlsx2(females_path,header=F,sheetIndex = 3,startRow = 3,colIndex = c(1:74),stringsAsFactors=F,colClasses = var_class$class_females)
factor_labels<-read.xlsx("./data-Artists/QP_Nov_2016/factor_labels.xlsx",sheetIndex = 1,encoding = "UTF-8")

#tidying the females' ds
dataset_f<-left_join(dataset_f,dataset_m[ ,c("X1", "X13")],by=c("X7" = "X1"),all.x=T) #adding missing info from the male's ds. 
dataset_f$X13.x<-dataset_f$X13.y #copying it to the right place. 
dataset_f$X13.y<-NULL #deleting the spare col
dataset_f <- cbind(dataset_f[,1:16],newX17=NA,newX18=c(2),dataset_f[,17:ncol(dataset_f)])#adding 2 missing cols to the female ds 
names(dataset_f)<-names(dataset_m) #cordinating the names

#binding the females and the males to one ds
rawfile<-rbind(dataset_m,dataset_f) 
#tidying the new rawfile

#Adding variable names from a look-up table 
headings<- read.xlsx(heading_path,sheetIndex = 1, encoding="UTF-8",header = T,as.data.frame = T)
names(rawfile)<-headings$new_var_name #renaming variables

#editing the raw data, droping vars.
 vars2drop<-c("duplicate","ip_address","seq_number","Custom_variable 4","Custom_variable 5","country_code","region")
 
 ## define a helper function for turning blanks to NA
 empty_as_na <- function(x){
   if("factor" %in% class(x)) x <- as.character(x)     
   ifelse(as.character(x)!="", x, NA)
   }

rawfile<- rawfile %>% 
 select(-one_of(vars2drop)) %>% select(-starts_with("intro"))%>% #droping unnecessary vars 
   mutate_each(funs(empty_as_na(.))) %>%
   filter(!is.na(place) & !is.na(respondent_email))%>% #filtering out incomplete data, and the females who were redirected.
  mutate(progress=ifelse(!is.na(make_project),3,
                         ifelse(!is.na(city_general7),2,
                                ifelse(!is.na(privious_interaction),1,
                                       ifelse(lived_in_past!="1" & !is.na(lived_in_past),-99,0))))) %>%
  group_by(respondent_email) %>%  #keep only the best response per user.
  mutate(max_point=max(progress)) %>% filter(progress==max_point) %>% ungroup() %>%
  arrange(max_point) %>%
  select (-progress)

source ("C:/Google Drive/work/new-spirit/Artists/analyzing/project1/FUN_get_labels.R") #load a function to match var's values
dataset<-rawfile %>% 
  mutate_at(vars(gender),funs(get_labels(rawfile,"gender",factor_labels))) %>%
  mutate(new_gender=ifelse(is.na(gender),gender_mailing,gender)) %>%
  mutate_at(vars(group),funs(get_labels(rawfile,"group",factor_labels))) %>%
  select(-gender_mailing,-gender,-shevet_mailing,-shevet,-time_to_complete) 
  
View(dataset) 
#saving raw and working files and closing.
 saveRDS(rawfile,file="./data-Artists/raw_file")
 saveRDS(dataset,file="./data-Artists/working_file")
 setwd(original_wd)
  
