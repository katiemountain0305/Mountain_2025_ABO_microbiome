## Chi-Sq tables 

```{r, include=FALSE}
#function to write contingency table and OR 
func_OR_Chi <- function(data, exploratory_var ){
  contingency_table1<-table(exploratory_var,data$MTL)
  contingency_table2<-table( exploratory_var, data$GA34x)
  contingency_table3 <-table(exploratory_var, data$Preterm.Term)
  contingency_table4 <- table(exploratory_var, data$GA28)
  contingency_table5 <- table(exploratory_var, data$short_cervix)
  
  #Short Cervix   
  fisherstable5<-pairwise_prop_test(contingency_table5,  p.adjust.method = "none", alternative = "less", correct = FALSE)
  outputtable5<- fisherstable5 %>% 
    mutate(Outcome = "Short Cervix")
  
  #MTL    
  fisherstable1<-pairwise_prop_test(contingency_table1,  alternative = "less", correct = FALSE)
  outputtable1<- fisherstable1 %>% 
    mutate(Outcome = "MTL")
  
  #28 weeks 
  fisherstable4<-pairwise_prop_test(contingency_table4,  alternative = "less", correct = FALSE)
  outputtable4<- fisherstable4 %>% 
    mutate(Outcome = "28 weeks")
  
  
  #34 weeks
  fisherstable2<-pairwise_prop_test(contingency_table2,   alternative= "less", correct = FALSE)
  outputtable2<- fisherstable2 %>% 
    mutate(Outcome = "34 weeks")
  
  
  #37 weeks 
  fisherstable3<-pairwise_prop_test(contingency_table3,  alternative = "less", correct = FALSE)
  outputtable3<- fisherstable3 %>% 
    mutate(Outcome = "37 weeks")
  
  
  output <- rbind(outputtable5,outputtable1, outputtable4,
                  outputtable2,
                  outputtable3) %>% kable()
  
  output
}