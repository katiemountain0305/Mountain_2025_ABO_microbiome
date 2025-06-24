## Generic Bar Function Blood group Comparisons


```{r bar_function, include=FALSE}
func_bar <- function(data, x_var, out_var, subtitle, 
                     x_title, y_title, fill_lab){
  # Aesthetics
  fill_colors <- c("#FFFBC9", "#CB9C9F","#A4BADD")
  fill_labels <- c("A", "B", "O")
  
  
  
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
         aes(x = x_var_n, y = Percent, fill = x_var_n)) + 
    geom_bar(aes(colour = out_var), stat = "identity") + 
    scale_colour_manual(values = c("black", "white"),  guide="none")+
    scale_fill_manual(values = fill_colors) + 
    geom_text(aes(label = round(Percentage, 1)), 
              position = position_dodge(width=1), size = 4, vjust=-0.5)+
    labs( subtitle = subtitle,
          x = x_title,
          y = y_title) +
    theme_bw() + 
    scale_y_continuous(limits = c(0, 40), n.breaks = 6 , minor_breaks = waiver())+
    theme(plot.title = element_text(hjust = 0, size = 10), 
          plot.subtitle = element_text(hjust = 0, size = 9, colour = "black", face = "bold"), 
          axis.title.x = element_text(size = 11, colour = "black", face = "bold"),
          axis.text.x = element_text(size = 11, colour="black", face = "bold"), 
          axis.title.y = element_text(size = 11, color = "black", face= "bold"), 
          axis.text.y = element_text(size = 10, face = "bold"),
          legend.position = "none") 
}

```