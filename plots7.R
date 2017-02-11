#getting ready.
original_wd<-getwd()  
setwd("C:/Google Drive/work/new-spirit/Artists")
Sys.setlocale("LC_ALL", "Hebrew")
library(xlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
options(stringsAsFactors = F)
#reading working file and metainfo files:
dataset <- readRDS(file="./data-Artists/working_file")
headings<- read.xlsx("./data-Artists/QP_Nov_2016/headings.xlsx",sheetIndex = 1, encoding="UTF-8",header = T,as.data.frame = T)
factor_labels<-read.xlsx("./data-Artists/QP_Nov_2016/factor_labels.xlsx",sheetIndex = 1,encoding = "UTF-8",colClasses = rep("character",4))


#plotting questions with only 1's
 #making a table with the correct number of n for each question
n_data<-dataset %>% select(privious_interaction,place,visit_us,activity1) %>%
  gather(q_name,value) %>%
  count(q_name,value) %>%
  filter(!is.na(value)) %>%
  filter((q_name=="place" & value!="1") | 
         (q_name=="activity1") |
         (q_name=="privious_interaction" & value!="1") |
         (q_name=="visit_us" & value=="1")) %>%
  group_by(q_name) %>% summarise(total_n=sum(n))
  
  
 #preperaing data,  
  sum_data<-dataset %>% select(c(activity1:which_project5, reasons_leaving1:reasons_leaving5,when_visit1:when_visit5)) %>%
    mutate_all(funs(as.numeric(.))) %>%
    gather(q_name,value) %>%
    filter(!is.na(value)) %>%
    group_by(q_name) %>% 
    summarise(sum_value=sum(na.omit(value))) %>% ungroup() %>%
    mutate(total_n=ifelse(grepl("reasons",q_name),
                          n_data[match("place",n_data$q_name),2],
                          ifelse( grepl("activity",q_name),
                                  n_data[match("privious_interaction",n_data$q_name),2],
                                  ifelse( grepl("when",q_name),
                                          n_data[match("visit_us",n_data$q_name),2],
                                          ifelse(grepl("project",q_name),
                                                  n_data[match("activity1",n_data$q_name),2],
                                                  "could not find"))))) %>%
    mutate(total_n=as.numeric(total_n)) %>%
    mutate(pct=sum_value/total_n) %>%
    left_join(headings[,3:4],by=c("q_name"="new_var_name")) %>%
    mutate(label_with_n=paste("(",sum_value,") ",new_label_hebrew,sep=""))%>%
    select(q_name,label_with_n,sum_value,total_n,everything())
    #adding n's to the labels

#prepearin facorial data for plotting 
f_index<-c("place","lived_in_past","education",
           "privious_interaction","basic_familarity","close_relations",
           "prof_relations", "cross_group1","cross_group2","cross_group3","cross_group4",
           "stay_in_town","org_activities","visit_us","make_project")
f_data<- dataset %>% 
  select(one_of(f_index)) %>% 
  gather(q_name,level) %>% filter(!is.na(level)) %>%
  count(q_name,level) %>%
  group_by(q_name) %>% mutate(total_n=sum(as.numeric(n),na.rm=T),
                              pct=n/total_n) %>% ungroup() %>%
  left_join(factor_labels[,c(1,2:3)], by = c("q_name" = "new_var_name", "level" = "q_level")) %>%
mutate(label_with_n=paste("(",n,") ",q_label,sep="")) #adding n's to the labels


plots_index<-read.xlsx("./data-Artists/QP_Nov_2016/plots_index.xlsx",
                       sheetIndex = 1, encoding="UTF-8",header = T,as.data.frame = T)
source("C:/Users/nogka/Documents/R/FAN_n.bar.R")
for (i in seq_along(plots_index$plot_data)) {
  g_names<-paste("g_",1:nrow(plots_index),sep="")
  g_temp<-n.bar(data=get(plots_index[i,"plot_data"]),
                x_val=plots_index[i,"x_values"],
                y_val=plots_index[i,"y_values"],
                filter_by = plots_index[i,"filter_by"],
                xlab = plots_index[i,"x_lab"],
                ylab = plots_index[i,"y_lab"],
                is.pct = plots_index[i,"is.pct"],
                fill_col=plots_index[i,"color_set"],
                reorder_x=plots_index[i,"reorder"])
  assign(g_names[i],g_temp)
  print(eval(parse(text=g_names[i])))
}


#relations in the network / exp. for plotting few question with several levels each. 
#adjusting the data:
 f_data_m<- f_data %>% filter(q_name %in% 
                               c("prof_relations","basic_familarity","close_relations"))%>%
  left_join(headings[,c("new_var_name","new_label_hebrew")], 
            by = c("q_name" = "new_var_name")) %>%
  mutate(label_with_n=paste("(",total_n,") ",new_label_hebrew,sep=""))
  
#plotting                              
f_1<-ggplot(f_data_m,aes(x=label_with_n,y=pct, fill=reorder(q_label,-as.numeric(level)),
                       label=scales::percent(round(pct,2))))+
  geom_bar(stat = "identity", width = 0.4,color="gray")+
  xlab("קשרים ברשת")+ ylab("")+
  geom_text(size = 5, position = position_stack(vjust = .5))+
  scale_fill_brewer(direction = +1, name="")+
  scale_y_continuous(labels = scales::percent)
#print(f_1)


#plotting gender by group
 #making data for the plot
   gender_data<-dataset %>%
    select(new_gender,shevet0=group) %>%
    filter(!is.na(shevet0)) %>%
    count(shevet0,new_gender) %>%
    mutate(pct=n/sum(n)) %>%
     group_by(shevet0) %>%
   mutate(tot_shevet=sum(n)) %>% ungroup() %>%
     mutate(label_with_n=paste("(",tot_shevet,") ",shevet0,sep=""))
  
   
#plotting         
   p_12<-ggplot(gender_data,aes(x=reorder(label_with_n,tot_shevet),y=n, fill=new_gender,label=n))+ 
     geom_bar(stat="identity",color='gray', width = 0.5) +
  geom_text(aes(y=n+1.4),size = 5, position = position_stack(vjust = .5))+
     scale_y_continuous(name="מספר משיבים")+
     scale_x_discrete(name="שבט") +
     scale_fill_manual(name="מגדר",values=c("#EF8A62", "#67A9CF"))
    print(p_12)
#cross group interactions:
 #preparing data:
    cross_group<- dataset %>% filter(max_point>1) %>%
      select(starts_with("cross_"),group) %>% 
      gather(q_name,level,-group) %>% filter(!is.na(level)) %>%
      count(group,q_name,level) %>%
      group_by(group,q_name) %>% mutate(total_n=sum(as.numeric(n),na.rm=T),
                                        pct=n/total_n) %>% ungroup() %>%
      left_join(factor_labels[,c(1,2:3)], by = c("q_name" = "new_var_name", "level" = "q_level")) %>%
      left_join(headings[,3:4],by=c("q_name"="new_var_name")) %>%
      mutate(label_with_n=paste("(",total_n,") ",group,sep="")) %>% #adding n's to the labels
      arrange(group,q_name,level ) 
   
    grp_lev<-unique(cross_group$group) #setting factor in the desired order
    new_lev<-grp_lev[c(5,3,2,1,4)]
    cross_group$group<-with(cross_group,factor(group,levels = new_lev))
  #plotting:    
    p_13<-ggplot(cross_group,aes(x=q_label,y=n, fill=q_label,label=n))+ 
      geom_bar(stat="identity",color='gray',width = 0.3) +
      geom_text(aes(y=n+10),size = 4, vjust = 1)+
      scale_y_continuous(name="מספר משיבים")+
      scale_x_discrete(name="") +
      scale_fill_manual(values=c("#EF8A62", "#67A9CF"),guide=F)+
      facet_grid(group~new_label_hebrew)
    print(p_13)

    
#plotting means:
means_data<-dataset %>% 
  select(networking1:networking3,art_scene1:art_scene4,city_general1:city_general7) %>%
  mutate_all(funs(as.numeric)) %>%
  gather("var_name","value") %>% 
  left_join(headings[,3:5],by=c("var_name"="new_var_name")) %>%
  group_by(q_group) %>% mutate(group_mean=mean(value,na.rm=T)) %>%
  group_by(q_group,new_label_hebrew,var_name,group_mean) %>%
  summarise(mean=mean(value,na.rm = T),
            n=sum(!is.na(value)),
            low=mean(between(value,1,2),na.rm=T),
            medium = mean(between(value,3,3),na.rm=T),
            high = mean(between(value ,4,5),na.rm=T)) %>%
  gather("sum_type","value",-one_of(c("var_name","n","new_label_hebrew","q_group","group_mean"))) %>%
  mutate(label_with_n=paste("(",n,") ",new_label_hebrew,sep=""))

source("C:/Users/nogka/Documents/R/FAN_n.bar.R")
for (j in seq_along(unique(means_data$q_group))) {
  g_names<-paste("m_",1:length(unique(means_data$q_group)),sep="")
  g_temp<-m.bar(filter_by = unique(means_data$q_group)[j])
  assign(g_names[j],g_temp)
  print(eval(parse(text=g_names[j])))
}


  m_4<-ggplot(means_data[means_data$sum_type=="mean",], 
                    aes(x=reorder(label_with_n, value),y=value, fill=q_group,
                        label=round(value,1)))+ 
    geom_bar(stat = "identity", width = 0.4+length(unique(means_data[["label_with_n"]]))*0.02, 
             color="gray")+
    coord_flip(ylim = c(1,5),expand = F) +
    xlab("")+
    ylab("Avg")+
    geom_text(size = 5, position = position_stack(vjust = .95))+
    scale_fill_manual(values = c('#8dd3c7','#ffffb3','#bebada'),
                     guide_legend(title = "קבוצת היגדים"))
 print(m_4) 
 
means_data$sum_type <- factor(means_data$sum_type, levels = c("mean","high","medium","low"))

dist_data<-means_data %>% filter(sum_type %in% c("low","medium","high")) %>%
  mutate(sum_type_heb=ifelse(sum_type=="low","נמוך"
                             ,ifelse(sum_type=="medium", "בינוני", 
                                "גבוה")))%>%
  mutate(type_order=ifelse(sum_type=="low",3,
                           ifelse(sum_type=="medium",2,1)))



#dist by group1

for (j in seq_along(unique(dist_data$q_group))) {
  g_names<-paste("d_",1:length(unique(means_data$q_group)),sep="")
  ds<-filter(dist_data, q_group==unique(means_data$q_group)[j])
  
  g_temp<-ggplot(ds, aes(x=label_with_n,y=value, fill=reorder(sum_type_heb, type_order), 
                                label=scales::percent(round(value,2))))+
    geom_bar(stat = "identity", width = 0.4)+
    xlab("")+ylab("")+
    geom_text(size = 5, position = position_stack(vjust = .5))+
    scale_fill_manual(values = c("#1a9641","#fdae61","#d7191c"),
                      guide_legend(title = "קטגורית ציון"))+
    scale_y_continuous(labels = scales::percent)
  
  assign(g_names[j],g_temp)
  #print(eval(parse(text=g_names[j])))
}
#dist chart, general.
dist_data$label_with_n=factor(dist_data$label_with_n,
                              levels=rev(levels(reorder(dist_data[dist_data$sum_type=="low",]$label_with_n,
                                                        dist_data[dist_data$sum_type=="low",]$value))))

d_4<-ggplot(dist_data, aes(x=label_with_n,y=value, fill=reorder(sum_type_heb, type_order), 
                           label=scales::percent(round(value,2))))+
  geom_bar(stat = "identity", width = 0.6)+
  coord_flip() +xlab("")+ylab("")+
  geom_text(size = 5, position = position_stack(vjust = .5))+
  scale_fill_manual(values = c("#1a9641","#fdae61","#d7191c"),
                    guide_legend(title = "קטגורית ציון"))+
  scale_y_continuous(labels = scales::percent)
#print(d_4)

#reporting results to a pptx file:
filename <- "report8.pptx" # the document to produce
slides_index<-read.xlsx("./data-Artists/QP_Nov_2016/slides_index.xlsx",
                       sheetIndex = 1, encoding="UTF-8",header = T,as.data.frame = T)
library(ReporteRs)
ppt_report<-pptx(template = "temp_artists.pptx") #choosing tmplt
for (i in seq_along(slides_index$slide_num)){
  ppt_report<-addSlide(ppt_report,
                       slide.layout = slide.layouts(ppt_report)[slides_index$slide_theme[i]])
  if (!is.na(slides_index$slide_head[i])){
    ppt_report <-addTitle(ppt_report, slides_index$slide_head[i])
    }
  ppt_report<-addPlot(ppt_report, function() print(get(slides_index$plot1[i])))  
  if (!is.na(slides_index$plot2[i])){
    ppt_report <-addPlot(ppt_report, function() print(get(slides_index$plot2[i])))
  }
  print(c("done",i))
}
writeDoc(ppt_report,file = filename )

