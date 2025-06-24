#Graphics for CCA plots

func_plot= function(data, title){
  data %>% 
    ggplot(aes(x=x, y=y, shape = pch)) +
    scale_x_continuous(limits = c(-1, 1))+
    scale_y_continuous(limits = c(-1, 1))+
    annotate("path",x=0+1*cos(seq(0,2*pi,length.out=100)),y=0+1*sin(seq(0,2*pi,length.out=100)), alpha = 1, size =0.5, color = "grey")+
    annotate("path",x=0+0.5*cos(seq(0,2*pi,length.out=100)),y=0+0.5*sin(seq(0,2*pi,length.out=100)), alpha = 1, size = 0.5, color = "grey")+
    geom_point(aes(color = Block), show.legend = FALSE)+ scale_shape_identity()+scale_color_manual(values = c("#053061", "#67001f","#b2182b"))+
    geom_text_repel(aes(label= names, fontface=font, color = Block), size=4, point.padding=0.1, box.padding = 0.1, show.legend = FALSE, direction = "both")+
    labs(x = "Component 1", y = "Component 2")+
    ggtitle(title) +
    theme_bw()+theme(plot.title = element_text(colour = "black", face = "bold"))
}