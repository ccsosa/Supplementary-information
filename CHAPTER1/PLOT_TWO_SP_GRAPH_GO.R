
require(igraph);require(RColorBrewer);require(qdap)
#add dir to create the file
#add graph_two_GOspecies graph using Categories option
#comp_species_graph_GO <- GOCompare::graph_two_GOspecies(x_input,species1  = "A.thaliana",species2 = "B. oleracea",option = "GO",)

plot_twosp_GO <- function(dir,comp_species_graph_GO){
  GO_TWO <- igraph::graph_from_data_frame(comp_species_graph_GO$edges, 
                                           directed = FALSE,
                                           vertices =  comp_species_graph_GO$nodes)
  node_GO<-
    data.frame(name=V(GO_TWO)$name,
               GO_WEIGHT=V(GO_TWO)$GO_WEIGHT)
  #write.csv(node_CAT,paste0(dir,"/","CAT_NODE_VALUES.csv"),row.names=F)
  
  color <- V(GO_TWO)$GO_WEIGHT
  valcolor <- cut((V(GO_TWO)$GO_WEIGHT), breaks = 5)
  palette2 <- brewer.pal(n = 5, name = "RdYlBu")
  val_levels <- levels(valcolor)
  valcolor2 <- valcolor
  valcolor <- as.character(valcolor)
  
  for(i in 1:length(val_levels)){
    valcolor[valcolor %in% val_levels[[i]]] <- palette2[[i]]
    
  };rm(i)
  
  
  val_levels2 <- as.character(unlist(qdap::bracketXtract(as.character(val_levels))))
  val_levels2
  
  pdf(file = paste0(dir,"/","GO_TWO.pdf"),   # The directory you want to save the file in
      width = 9.1, # The width of the plot in inches
      height = 8.3) # The height of the plot in inches
  
  
  
  plot(GO_TWO, layout= layout_in_circle,#layout.circle,#layout_nicely,#layout.fruchterman.reingold,#layout_in_circle,#layout_nicely,#layout_in_circle,#layout.fruchterman.reingold,
       #main="Combined species functional categories",
       vertex.color=valcolor,
       # vertex.label=LETTERS[1:10],                    # Character vector used to label the nodes
       vertex.label.color="black",
       vertex.size=5,#abs(V(GO_TWO)$GO_WEIGHT)*20,
       #vertex.label.family="Times",                   # Font family of the label (e.g.“Times”, “Helvetica”)
       vertex.label.font=1,                  # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
       vertex.label.cex=0.7,                 # Font size (multiplication factor, device-dependent)
       vertex.label.dist=1.5,                           # Distance between the label and the vertex
       vertex.label.degree=-1 ,
       #vertex.size= V(GO_TWO)$COMB_WEIGHT,
       # The position of the label in relation to the vertex (use pi)
       #edge.color=rep(c("red","pink"),5),           # Edge color
       edge.arrow.width=1,                          # Arrow width, defaults to 1
       edge.lty=c("solid"),
       vertex.label = ifelse(V(GO_TWO)$GO_WEIGHT >= quantile(V(GO_TWO)$GO_WEIGHT)[4], V(GO_TWO)$name, NA)
  )
  
  #,s
  #edge.width=(E(GO[[1]])$WEIGHT)*10,
  #vertex.label.cex=(V(GO[[1]])$GO_WEIGHT)
  
  legend('topleft',
         title = "Node weight value (ranges)",
         legend=rev(val_levels2),
         pt.cex=1,
         cex=0.6,
         col=rev(palette2),
         pch=21,
         pt.bg=rev(palette2),
         horiz = F,
         box.lty=0)
  
  dev.off()
  
  return("DONE!")
  
}
