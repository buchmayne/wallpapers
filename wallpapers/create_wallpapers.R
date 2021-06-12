library(tidyverse)
library(tidycensus)
library(sf)
library(fasterize)
library(raster)
library(reshape)



multnomah_pop <- get_decennial(
  geography = 'block',
  variables = 'H010001', 
  geometry = T, 
  state = 'OR', 
  county = 'Multnomah', 
  year = 2010
  )

washington_pop <- get_decennial(
  geography = 'block',
  variables = 'H010001', 
  geometry = T, 
  state = 'OR', 
  county = 'Washington', 
  year = 2010
)

clackamas_pop <- get_decennial(
  geography = 'block',
  variables = 'H010001', 
  geometry = T, 
  state = 'OR', 
  county = 'Clackamas', 
  year = 2010
)

pdx_metro <- rbind(multnomah_pop, clackamas_pop) %>% 
  rbind(washington_pop)


# population density
pdx_metro$density <- pdx_metro$value / st_area(pdx_metro)

# reproject sf object
pdx_metro <- st_transform(pdx_metro, crs = 6855)

# create empty raster and rasterize sf polygons
r <- raster(ncol = 100, nrow = 100)
extent(r) <- extent(pdx_metro)

pdx_metro
popDensRaster <- fasterize(st_collection_extract(pdx_metro, "POLYGON"), r, field = 'density', fun = 'last')



coords <- as.data.frame(coordinates(popDensRaster))
values <- data.frame(pop = values(popDensRaster),
                     x = coords$x,
                     y = coords$y)


# standardize values for easier visualization
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
values$pop[is.na(values$pop)] <- 0
values$pop_st <- range01(values$pop)*0.06 # this scalar will affect how peaky the peaks are. Higher value = peakier
# values$pop_st[values$pop > 0.0828908] <- 0 # make maximum 0...a weird block NW of fairmount park
values$x_st <- range01(values$x)
values$y_st <- range01(values$y)



xquiet <- scale_x_continuous("", breaks=NULL)
yquiet <- scale_y_continuous("", breaks=NULL)
quiet <- list(xquiet, yquiet)

values$ord <- values$y
values_s <- values[order(-values$ord),]


# Create an empty plot called p
p <- ggplot()

backgroundColor <- 'turquoise' # other colors I like: blue #1D3A7D, gray #B2B6BF, coral #F88379
lineColor <- 'white'

# This loops through each line of latitude and produced a filled polygon that will mask out the 
# lines beneath and then plots the paths on top.The p object becomes a big ggplot2 plot.
for (i in unique(values_s$ord)){
  p <- p + geom_polygon(data = values_s[values_s$ord == i,], aes(x_st, pop_st + y_st, group = y_st), 
                        size = 0.5, fill = backgroundColor, col = backgroundColor) + 
    geom_path(data = values_s[values_s$ord == i,],aes(x_st, pop_st + y_st, group = y_st), size = 0.5, 
              lineend = 'round', col = lineColor)
}

# size controls the line thickness

# Display plot and save as pdf
pdf(file = "~/Desktop/wallpapers/populationLines_turquoise.pdf", width=10, height=10)
p + theme(panel.background = element_rect(fill = backgroundColor,colour = backgroundColor)) + quiet
dev.off()