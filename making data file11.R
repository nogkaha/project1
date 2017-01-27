#getting ready.
original_wd<-getwd()  
setwd("C:/Google Drive/work/new-spirit/Artists")
Sys.setlocale("LC_ALL", "Hebrew")
library(xlsx)
#library(Hmisc)
library(dplyr)
library(tidyr)
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
dataset<-rbind(dataset_m,dataset_f) 
#tidying the new dataset

#Adding variable labels from index
headings<- read.xlsx(heading_path,sheetIndex = 1, encoding="UTF-8",header = T,as.data.frame = T)
#var.labels <- as.character(headings$new_label_hebrew)
#label(dataset) <- lapply(c(1:76), function(x) label(dataset[,x]) = var.labels[x])
names(dataset)<-headings$new_var_name #renaming variables

#editing the raw data, droping vars, changing vars' class.
 vars2drop<-c("ip_address","seq_number","Custom_variable 4","Custom_variable 5","country_code","region")
 
 ## define a helper function for turning blanks to NA
 empty_as_na <- function(x){
   if("factor" %in% class(x)) x <- as.character(x)     
   ifelse(as.character(x)!="", x, NA)
 }
 ## define a helper function for get a vector of labels for specific var
 set_factor_labels<-function(q_name) {
   x<-factor_labels %>% filter(new_var_name==q_name) %>%
     select(q_label)
   x[1:nrow(x),1]
 }
 
 dataset<- dataset %>% 
  select(-one_of(vars2drop)) %>% select(-starts_with("intro"))%>% #droping unnecessary vars 
   mutate_each(funs(empty_as_na(.))) %>%
   filter(!is.na(place))%>%  #filtering out incomplete data, and the females who were redirected.
   mutate_at(vars(gender),funs(factor(gender,labels=(set_factor_labels("gender")))))%>% #building new vars: gender, group
   mutate_at(vars(group),funs(factor(group,labels=(set_factor_labels("group")))))%>%
   mutate_at(vars(matches("gender")),funs(as.character(.)))
 
dataset$new_gender<-ifelse(is.na(dataset$gender),
                             dataset$gender_mailing,
                             dataset$gender)

View(dataset)
  
  #saving working file and closing.
  saveRDS(dataset,file="./data-Artists/working_file")
  setwd(original_wd)
  
  
#dealing with the numeric data
  #numeric_index<-names(select(dataset,networking1:networking3,
  #            art_scene1:art_scene4,
  #           city_general1:city_general7))