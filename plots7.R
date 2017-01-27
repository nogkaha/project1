#getting ready.
original_wd<-getwd()  
setwd("C:/Google Drive/work/new-spirit/Artists")
Sys.setlocale("LC_ALL", "Hebrew")
library(xlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ReporteRs)
#reading working file and metainfo files:
dataset <- readRDS(file="./data-Artists/working_file")
options(stringsAsFactors = F)
headings<- read.xlsx("./data-Artists/QP_Nov_2016/headings.xlsx",sheetIndex = 1, encoding="UTF-8",header = T,as.data.frame = T)
factor_labels<-read.xlsx("./data-Artists/QP_Nov_2016/factor_labels.xlsx",sheetIndex = 1,encoding = "UTF-8",colClasses = rep("character",4))
source("C:/Users/nogka/Documents/R/FAN_set_factor_labels.R")
source("C:/Users/nogka/Documents/R/FAN_n.bar.R")
#plotting questions with only 1's
 #preperaing data,  
 #errors: in the total_n., in the labels (to fix in the index) 
  sum_data<-dataset %>% select(c(activity1:which_project5, reasons_leaving1:reasons_leaving5,when_visit1:when_visit5)) %>%
      mutate_all(funs(as.numeric(.))) %>%
      gather(q_name,value) %>%
    group_by(q_name) %>% 
    summarise(sum_value=sum(na.omit(value)),n_value=n()) %>% 
    ungroup() %>%
    mutate(pct=sum_value/n_value) %>%
    left_join(headings[,3:4],by=c("q_name"="new_var_name")) #%>%
g1<-n.bar(data=sum_data,filter_by = "activ",xlab = "",ylab = "")
g2<-n.bar(data=sum_data,filter_by = "visit",y="sum_value",xlab = "",ylab = "",is.pct = FALSE)
g3<-n.bar(data=sum_data,filter_by = "project",y="sum_value", xlab = "",ylab = "",is.pct = FALSE)
g4<-n.bar(data=sum_data,filter_by = "reason",y="sum_value",xlab = "",ylab = "",is.pct = FALSE)

#prepearin facorial data for plotting 
f_index<-c("place","lived_in_past","education",
           "privious_interaction","basic_familarity","close_relations",
           "prof_relations", "cross_group1","cross_group2","cross_group3","cross_group4",
           "stay_in_town","org_activities","visit_us","make_project")
f_data<- dataset %>% 
  select(one_of(f_index)) %>% 
  gather(q_name,level) %>% filter(!is.na(level)) %>%
  count(q_name,level) %>%
  group_by(q_name) %>% mutate(sum1=sum(as.numeric(n),na.rm=T),
                              pct=n/sum1) %>% ungroup() %>%
  left_join(factor_labels[,c(1,2:3)], by = c("q_name" = "new_var_name", "level" = "q_level"))

f1<-n.bar(data=f_data,filter_by = "stay_in",x="q_label",xlab = "",ylab = "")
f2<-n.bar(data=f_data,filter_by = "education",x="q_label",xlab = "",ylab = "")

#relations in the network / exp. for plotting few question with several levels each. 
#plotting 
f3<-ggplot(f_data %>% filter(q_name %in% 
                               c("prof_relations","basic_familarity","close_relations")), 
                   aes(x=q_name,y=pct, fill=reorder(q_label,-as.numeric(level)),
                       label=scales::percent(round(pct,2))))+
  geom_bar(stat = "identity", width = 0.4,color="gray")+
  xlab(NULL)+ ylab("אחוז משיבים")+
  geom_text(size = 4, position = position_stack(vjust = .5))+
  scale_fill_brewer(direction = +1, name="")+
  scale_y_continuous(labels = scales::percent)
print(f3)

#plotting education by group
#making data for the plot
education_data<-dataset %>%
  filter(!is.na(education)) %>%
  select(education,group) %>%
  mutate(stage=ifelse (as.numeric(education)>5,"Student","Not Student"))%>%
  mutate(education=factor(education,labels=set_factor_labels("education")))%>%
  count(education,stage,group) %>%
  arrange(education,desc(group))%>%
  mutate(pct=n/sum(n),
         ypos=cumsum(n) - 0.5 *n)
#plotting
bar_education<-ggplot(education_data,aes(x=reorder(education,n),y=n,fill=group))+ 
  geom_bar(stat="identity",color='black') +
  geom_text(aes(label=n,y=ypos),size=4.5)+
  scale_y_continuous(name="מספר משיבים")+
  scale_x_discrete(name="Education") +
  scale_fill_brewer(name="מגדר",palette="Pastel1")
print(bar_education)
#plotting gender by group
 #making data for the plot
   gender_data<-dataset %>%
    select(new_gender,group) %>%
    filter(!is.na(group)) %>%
    count(group,new_gender) %>%
    arrange(group,desc(new_gender))%>%
    mutate(pct=n/sum(n),
           ypos=cumsum(n+0.9) - 0.5 *n) # Calculate label positions
 #plotting         
   bar_gender<-ggplot(gender_data,aes(group, n, fill=new_gender,label=n))+ 
     geom_bar(stat="identity",color='black') +
  geom_text(size = 4, position = position_stack(vjust = .5))+
     scale_y_continuous(name="מספר משיבים")+
     scale_x_discrete(name="שבט") +
     scale_fill_manual(name="מגדר",values=c("#EF8A62", "#67A9CF"))
      print(bar_gender)
   
bar_gender<-ggplot(gender_data,aes(group, n, fill=new_gender))+ 
  geom_bar(stat="identity",color='black') +
  geom_text(aes(label=n, y=ypos),size=4.5)+
  scale_y_continuous(name="מספר משיבים")+
  scale_x_discrete(name="שבט") +
  scale_fill_manual(name="מגדר",values=c("#EF8A62", "#67A9CF"))

#plotting means:
#means_index<-c(38:40,45:55)
means_data<-dataset %>% 
  select(networking1:networking3,art_scene1:art_scene4,city_general1:city_general7) %>%
  mutate_all(funs(as.numeric)) %>%
  gather("var_name","value") %>% group_by(var_name) %>%
  summarise(mean=mean(value,na.rm = T),
            count=sum(!is.na(value)),
            low=mean(between(value,1,2),na.rm=T),
            medium = mean(between(value,3,3),na.rm=T),
            high = mean(between(value ,4,5),na.rm=T)) %>%
  left_join(headings[,3:4],by=c("var_name"="new_var_name")) %>%
  gather("sum_type","value",-one_of(c("var_name","count","new_label_hebrew")))

means_data$sum_type <- factor(means_data$sum_type, levels = c("mean","high","medium","low"))

means_bar<-ggplot(means_data[means_data$sum_type=="mean",], 
                  aes(x=reorder(new_label_hebrew, value),y=value, 
                      fill=group, label=round(value,1)))+ 
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip(ylim = c(1,5),expand = F) +
  xlab("קטגוריות")+
  ylab("ערך ממוצע")+
  geom_text(size = 4, position = position_stack(vjust = .95))+
scale_fill_brewer(palette = "Pastel2")
print(means_bar)

dist_data<-means_data %>% filter(stat %in% c("low","medium","high"))

#dist chart, general.
dist_data$new_label_hebrew=factor(dist_data$new_label_hebrew,
                         levels=rev(levels(reorder(dist_data[dist_data$stat=="low",]$new_label_hebrew,
                                                   dist_data[dist_data$stat=="low",]$value))))

bar_dist<-ggplot(dist_data, aes(x=new_label_hebrew,y=value, fill=stat, 
                                label=scales::percent(round(value,2))))+
  geom_bar(stat = "identity",position = "fill", width = 0.7)+
  coord_flip() +
  xlab("קטגוריות")+
  ylab("ערך ממוצע")+
  geom_text(size = 4, position = position_stack(vjust = .95))+
  scale_y_continuous(labels = scales::percent)
print(bar_dist)

#dist by group1
bar_dist_group1<-ggplot(dist_data %>% filter(group=="art"), 
                       aes(x=new_label_hebrew,y=value, fill=stat,
                           label=scales::percent(round(value,2))))+
  geom_bar(stat = "identity",position = "fill", width = 0.7)+
  coord_flip() +
  xlab("קטגוריות")+
  ylab("ערך ממוצע")+
  geom_text(size = 4, position = position_stack(vjust = .95))+
  scale_y_continuous(labels = scales::percent)
print(bar_dist_group)


  
  
  
 

filename <- "report1.pptx" # the document to produce
pptx(template = "temp_artists.pptx") %>% 
  addSlide(slide.layout = "Title and Content" ) %>%
  addPlot( function() print(bar_gender)) %>%
  addSlide(slide.layout = "Title and Content" ) %>%
  addPlot( function() print(bar_education)) %>%
  addSlide(slide.layout = "Title and Content" ) %>%
  addPlot( function() print(leaving_graph)) %>%
  addSlide(slide.layout = "Title and Content" ) %>%
  addPlot( function() print(bar_means)) %>%
  addSlide(slide.layout = "Title and Content" ) %>%
  addPlot( function() print(bar_dist)) %>%
  addSlide(slide.layout = "Title and Content" ) %>% 
  addPlot( function() print(bar_dist_group1)) %>% 
  writeDoc( file = filename )
