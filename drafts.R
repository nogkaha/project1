#DRAFTS
scale_y_continuous(limits = c(0,5))+
#making means data version old:
means_data<-dataset %>% 
  select(networking1:networking3,art_scene1:art_scene4,city_general1:city_general7) %>%
  mutate_all(funs(as.numeric)) %>%
  gather("var_name","value") %>% group_by(var_name) %>%
  summarise(mean=mean(value,na.rm = T),
            n=sum(!is.na(value)),
            low=mean(between(value,1,2),na.rm=T),
            medium = mean(between(value,3,3),na.rm=T),
            high = mean(between(value ,4,5),na.rm=T)) %>%
  left_join(headings[,3:5],by=c("var_name"="new_var_name")) %>%
  gather("sum_type","value",-one_of(c("var_name","n","new_label_hebrew","q_group"))) %>%
  mutate(label_with_n=paste("(",n,") ",new_label_hebrew,sep=""))


#dealing with the numeric data
numeric_index<-names(select(rawfile,networking1:networking3,
            art_scene1:art_scene4,
           city_general1:city_general7))


annotation_custom(geom="text", x=3, y=80, label=paste("n=",g_temp$data[i,3]),
                  color="black")+
  
#a loop for plots;
for (i in seq_along(nm)) {
  g_names<-paste("g_",nm[1:length(nm)],sep="")
  
  g_temp<-ggplot(sum_data[grep(nm[i],sum_data$q_name),], 
                 aes(x=label_with_n,
                     y=pct,
                     fill=label_with_n, 
                     label=scales::percent(round(pct,2))))
  
  g_temp<- g_temp+ geom_bar(stat = "identity", width = 0.7,color="gray")+
    xlab("")+ ylab("")+
    geom_text(aes(y=pct+0.02), size = 4, position = position_stack(vjust = 1) )+
    scale_fill_manual(values = c("#fbb4ae",'#b3cde3','#fed9a6','#decbe4',"#e5d8bd"),guide=FALSE)+
    scale_y_continuous(labels = scales::percent)
  assign(g_names[i],g_temp)
  print(eval(parse(text=x[i])))
}

##unused code:

#plotting education by group # I think I don't need this one, better to do some
#thing that I need for sure and then play with it.
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