#DRAFTS
#dealing with the numeric data
#numeric_index<-names(select(rawfile,networking1:networking3,
#            art_scene1:art_scene4,
#           city_general1:city_general7))

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