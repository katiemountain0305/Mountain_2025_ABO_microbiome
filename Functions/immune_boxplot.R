## Box Plot

```{r inlcude = FALSE}
func_box <- function(data, x_var, out_var, 
                     x_title, y_title){
  # Aesthetics - need to add additional colours based on Valencia output
  fill_colors <- c("#f8e16c", "#CB9C9F","#A4BADD")
  
  #get counts
  data<-data%>%group_by_(x_var) %>% mutate(count= n()) %>% ungroup() 
  data<-data %>% mutate_(var=x_var, 
                         y_var = out_var)
  data <- data %>% mutate(x_var_n = var %>% paste0(., "\n", "(n=", count, ")") %>% factor())
  #plot
  
  ggplot(data, aes(x=x_var_n, y= y_var))+
    geom_boxplot(outlier.shape = NA)+
    geom_jitter(aes(colour=blood_group, shape=GAforcerclage, size=GAforcerclage))+
    scale_color_manual(name= "ABO Status", values = fill_colors, guide = "none")+
    scale_shape_manual(name= "GA at Delivery", values = c(15,17,18, 1))+
    scale_size_manual(name= "GA at Delivery",values = c(3,3, 2, 1))+
    theme_bw()+
    scale_y_log10()+
    labs( x = x_title,
          y = y_title)+
    theme( plot.subtitle = element_text(hjust = 0, size = 10), 
           axis.title.x = element_text(size = 12, color = "black", face = "bold"),
           axis.text.x = element_text(size = 12, colour = "black", face = "bold"), 
           axis.title.y = element_text(size = 12, colour = "black", face = "bold"), 
           axis.text.y = element_text(size = 10),
           legend.position = "right") 
}

func_counts <- function(data, x_var){
  
  #get counts
  data<-data%>%group_by_(x_var) %>% mutate(count= n()) %>% ungroup() 
  data<-data %>% mutate_(var=x_var)
  data <- data %>% mutate(var_n = var %>% paste0(., "\n", "(n=", count, ")") %>% factor())
  
  
}