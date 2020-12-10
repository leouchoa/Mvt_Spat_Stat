library(soiltexture)
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


# ----------------- Label -----------------

soil_cmps_w_coords <- 
 setNames(
  alr_transformed_with_locations_UNIQUE[,c("prop1_areia","prop2_argila","prop3_silte","coord_x","coord_y")],
  c("SAND","CLAY","SILT","coord_x","coord_y")
 )


soil_cmps_w_coords <- 
 cbind(
  soil_cmps_w_coords[,1:3]*100,
  soil_cmps_w_coords[,4:5]
 )

class_labels <- as.data.frame(
 TT.points.in.classes(
  soil_cmps_w_coords[,1:3],
  class.sys= "SiBCS13.TT")
)

soil_cmps_w_coords <- cbind(soil_cmps_w_coords,label = apply(class_labels,1,function(x){
 aux_term <- colnames(class_labels)[which(as.logical(x))];
 ifelse(length(aux_term) != 1,paste0(aux_term,collapse = "/"),aux_term)
}))

table(soil_cmps_w_coords$label)


ctb_map_image_unique_loc_constrained_v2 <- readRDS("ctb_map_image_unique_loc_constrained_v2.rda")


# soil_dts_map_plot_v2 <- 
 ggmap(ctb_map_image_unique_loc_constrained_v2) + 
 geom_point(
  data = setNames(
   data.frame(soil_dts@coords,label = soil_cmps_w_coords$label),
   c("coord_x","coord_y","Etiqueta")
   ), 
  mapping = aes(x = coord_x,y = coord_y,color = Etiqueta, size = 1.5)
  ) + 
  scale_color_viridis_d(
   # option = "cividis"
   ) + 
  guides(size = FALSE) + 
  labs(
   x = "Longitude",
   y = "Latitude"
  )
