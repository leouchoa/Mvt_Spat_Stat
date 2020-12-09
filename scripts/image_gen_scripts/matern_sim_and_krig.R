library(tidyverse)
library(geoR)
library(RandomFields)

x <- seq(0, 1, len=100)

model <- RMmatern(nu = 0.5, var = 3,scale = 1)

z <- RFsimulate(model=model, x, x)

# X11();plot(z)

cbind(coordinates(z),z@data) %>% 
 ggplot(aes(coords.x1,coords.x2, fill = variable1)) + 
 geom_tile() + 
 scale_fill_viridis_c() + 
 labs(
  fill = "Escala"
 )

idx <- sample.int(nrow(coordinates(z)),85)

sampled_df <-cbind(coordinates(z)[idx,],
                   z = z@data[idx,]) 

empVariog <- variog(coords = sampled_df[,1:2], data = sampled_df[,3])

contrast <- variofit(empVariog, 
                     ini.cov.pars = c(sigma2 = 3, phi = 1),
                     cov.model = "matern", fix.nugget = FALSE
                     )

prep <- krige.control(type.krige = "ok",  
                      cov.model = contrast$cov.model,
                      cov.pars = contrast$cov.pars,
                      nugget = contrast$nugget)
loci <- expand.grid(X = x,
                    Y = x)
kc <- krige.conv(coords = cbind(x, x), data = z@data, 
                 locations = loci,
                 krige = prep)
