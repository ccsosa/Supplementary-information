
require(igraph);require(RColorBrewer);require(qdap)
#add dir to create the file
#add graph_two_GOspecies graph using Categories option
#comp_species_graph <- GOCompare::graph_two_GOspecies(x_input,species1  = "A.thaliana",species2 = "B. oleracea",option = "Categories")

plot_twosp_CAT <- function(dir,comp_species_graph){
  CAT_TWO <- igraph::graph_from_data_frame(comp_species_graph$edges, 
                                           directed = FALSE,
                                           vertices =  comp_species_graph$nodes)
  node_CAT <-
    data.frame(name=V(CAT_TWO)$name,
               CAT_WEIGHT=V(CAT_TWO)$CAT_WEIGHT,
               SHARED_WEIGHT=V(CAT_TWO)$SHARED_WEIGHT,
               COMB_WEIGHT=V(CAT_TWO)$COMBINED_WEIGHT)
  
  #write.csv(node_CAT,paste0(dir,"/","CAT_NODE_VALUES.csv"),row.names=F)
  
  
  color <- V(CAT_TWO)$COMBINED_WEIGHT
  valcolor <- cut((V(CAT_TWO)$COMBINED_WEIGHT), breaks = 5)
  palette2 <- brewer.pal(n = 5, name = "RdYlBu")
  val_levels <- levels(valcolor)
  valcolor2 <- valcolor
  valcolor <- as.character(valcolor)
  val_levels2 <- as.character(unlist(qdap::bracketXtract(as.character(val_levels))))
  #val_levels2
  
  for(i in 1:length(val_levels)){
    valcolor[valcolor %in% val_levels[[i]]] <- palette2[[i]]
  };rm(i)
  
  
  pdf(file = paste0(dir,"/","CATTWO.pdf"),   # The directory you want to save the file in
      width = 8, # The width of the plot in inches
      height = 6,) # The height of the plot in inches
  
  
  
  plot(CAT_TWO, layout=layout_in_circle,#layout_in_circle,#layout_nicely,#layout_in_circle,#layout.fruchterman.reingold,
       #main="Combined species functional categories",
       vertex.color=valcolor,
       # vertex.label=LETTERS[1:10],                    # Character vector used to label the nodes
       vertex.label.color="black",
       vertex.size=abs(V(CAT_TWO)$COMBINED_WEIGHT)*10,
       #vertex.label.family="Times",                   # Font family of the label (e.g.“Times”, “Helvetica”)
       vertex.label.font=1,                  # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
       vertex.label.cex=0.4,                 # Font size (multiplication factor, device-dependent)
       vertex.label.dist=2,                           # Distance between the label and the vertex
       vertex.label.degree=4 ,
       #vertex.size= V(CAT_TWO)$COMB_WEIGHT,
       # The position of the label in relation to the vertex (use pi)
       #edge.color=rep(c("red","pink"),5),           # Edge color
       edge.arrow.width=1,                          # Arrow width, defaults to 1
       edge.lty=c("solid")#,s
       #edge.width=(E(GO[[1]])$WEIGHT)*10,
       #vertex.label.cex=(V(GO[[1]])$GO_WEIGHT)
  )
  legend('topleft',
         title = "Combined node weight value (ranges)",
         legend=rev(val_levels2),
         pt.cex=1,
         cex=0.6,
         col=rev(palette2),
         pch=21,
         pt.bg=rev(palette2),
         horiz = F,
         box.lty=0)
  
  
  
  
  # Step 2: Create the plot with R code
  
  # Step 3: Run dev.off() to create the file!
  dev.off()
  return("DONE!")
  
}
