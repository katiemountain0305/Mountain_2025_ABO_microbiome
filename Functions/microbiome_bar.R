func_bar <- function(data, x_var, out_var, title, subtitle, 
                     x_title, y_title, fill_lab){
  # Aesthetics - need to add additional colours based on Valencia output
  fill_colors <- c("#3F5243","#EA8382", "#FEFFB8","#C2FCB7", "#75A97D" )
  fill_labels <- c("V", "IV", "III", "II","I")
  
  
  
  #Percentage
  percentage <- data %>% 
    group_by_(x_var) %>% 
    count_(out_var) %>% 
    mutate(Percent = round(n/sum(n)*100, 1),
           Percentage = round(n/sum(n)*100,2), 
           sum_n = sum(n)) %>% 
    as.data.frame()
  
  percentage <- percentage %>% 
    mutate( x_var_n = percentage[,1] %>%paste0(., "\n", "(n=", sum_n, ")") )
  
  
  # ABO Bar
  ggplot(percentage, 
         aes(x = x_var_n, y = Percent, fill = CST_simple)) + 
    geom_bar(aes(fill= CST_simple), stat = "identity", colour="black") + 
    scale_fill_manual(values = fill_colors, labels=fill_labels) + 
    geom_text(aes(label = round(Percentage)), 
              position = position_stack(vjust = 0.5), size = 5)+
    labs(title = title, 
         subtitle = subtitle,
         x = x_title,
         y = y_title, 
         fill = "CST") +
    theme_bw() + 
    scale_y_continuous(limits = c(0, 115), n.breaks = 6 , minor_breaks = waiver())+
    theme(plot.title = element_text(hjust = 0, size = 14, , face= "bold"), 
          plot.subtitle = element_text(hjust = 0, size = 12, , face= "bold"), 
          axis.title.x = element_text(size = 14, , face= "bold"),
          axis.text.x = element_text(size = 12, colour="black", face= "bold"), 
          axis.title.y = element_text(size = 14, , face= "bold"), 
          axis.text.y = element_text(size = 12, colour="black", face="bold"),
          legend.position = "right") 
}

func_chi <- function(data, x_var, out_var){ 
  
  #get counts 
  data<-data%>%group_by_(x_var) %>% mutate(count= n()) %>% ungroup() 
  data<-data %>% mutate_(var=x_var, CST_simple = out_var)
  data <- data %>% mutate(x_var_n = var %>% paste0(., "\n", "(n=", count, ")") %>% factor())
  
  #contingency table 
  contingency_table <- table(data$x_var_n, data$CST_simple)
  fishertable = pairwise_prop_test(contingency_table, p.adjust.method = "BH", correct=FALSE)%>% 
    mutate(p = p %>% round(., 3), p.adj = p.adj %>% round(.,3))
  fishertable
}