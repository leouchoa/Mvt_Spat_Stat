library(ggtern)

alr_transformed_with_locations_UNIQUE <- read.csv("data/alr_transformed_with_locations_UNIQUE.csv", row.names=NULL)

soil_cmps <- setNames(alr_transformed_with_locations_UNIQUE[,c("prop1_areia","prop2_argila","prop3_silte")],
                     c("Areia","Argila","Silte"))

ggtern(data=soil_cmps,aes(Areia,Argila,Silte)) + 
 geom_point() + 
 # theme_minimal()
 theme_linedraw() + 
 theme_arrowlarge() +
 theme(tern.axis.arrow = element_line(size = 3))

ggtern(data=soil_cmps,aes(Areia,Argila,Silte)) + 
 geom_point() + 
 # theme_minimal()
 theme_linedraw() + 
 theme_arrowlarge() +
 theme(tern.axis.arrow = element_line(size = 3)) + 
 stat_confidence_tern(
  geom = "polygon", 
  mapping = aes(fill = ..level..),
  colour = "white", 
  breaks = c(0.95,0.99),
  alpha = 0.5
  ) + 
 geom_mask() +
 guides(fill = FALSE)
