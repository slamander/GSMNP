
{
  source("https://raw.githubusercontent.com/slamander/load_packages/main/prepare_libraries.R")
  require(tidyverse) 
  
  c("sp",
    "rgdal",
    "rgeos",
    "raster",
    "tidyverse",
    "readr",
    "tigris",
    "elevatr",
    "spatialEco",
    "rnaturalearth",
    "ggthemes",
    "wesanderson") %>%
    
    prepare_libraries(packages = .)
}

########## study areas
# set projection
proj <- "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83"

# southeastern region
se <- states(class = "sp") %>%
  subset(NAME %in% c("Tennessee", "North Carolina",
                     "Arkansas", "South Carolina",
                     "Louisiana", "Mississippi",
                     "Alabama", "Florida",
                     "Georgia")) %>%
  spTransform(proj)

# southeastern DEM
se_elv <- get_elev_raster(se, z = 6, proj = proj) %>%
  crop(se) %>%
  mask(se)



plot(se_elv > 0.5)


# Great Smoky Mountians National Park
# Get shapefile from IRMA portal
download.file(url = paste("https://irma.nps.gov/DataStore/DownloadFile/658912"), destfile = "parks.zip", mode='wb', cacheOK=FALSE)

temp <- tempfile()

unzip(zipfile = "parks.zip", exdir = temp)

file <- list.files(temp, pattern = ".shp$", full.names=TRUE)

gsmnp_nps <- readOGR(file) %>%
  subset(UNIT_NAME == "Great Smoky Mountains National Park") %>%
  spTransform(proj) %>%
  gSimplify(tol = 75)

plot(gsmnp_nps) # the shapefile features roads and weird slivers... 

# Download coarse shapefile to cut off the road slivers
download.file(url = paste("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_parks_and_protected_lands.zip"), 
              destfile = "gsmnp_ne.zip", mode='wb', cacheOK=FALSE)

temp_ne <- tempfile()

unzip(zipfile = "gsmnp_ne.zip", exdir = temp_ne)

ne_shp <- list.files(temp_ne, pattern = ".shp$", full.names=TRUE)

# transform the polygons
gsmnp_ne <- readOGR(ne_shp[1]) %>%
  subset(unit_name == "Great Smoky Mountains NP") %>%
  spTransform(proj)

# Clip out the extraneous slivers of polygons
gsmnp <- gIntersection(gsmnp_nps, gsmnp_ne)

plot(gsmnp)

ext <- extent(gsmnp) %>%
  as("SpatialPolygons") %>%
  gBuffer(width = 8000, byid = T) %>%
  spsample(100, "regular")
crs(ext) <- crs(gsmnp)

plot(ext); lines(gsmnp)

# GSMNP DEM
gsmnp_elv <- elevatr::get_elev_raster(ext, 
                                      prj = proj, 
                                      z = 11) %>%
  crop(gsmnp) 
names(gsmnp_elv) <- "elv"

plot(gsmnp_elv); lines(gsmnp)

gsmnp_hs <- hillShade(terrain(gsmnp_elv, opt = "slope"),
                      terrain(gsmnp_elv, opt = "aspect"))

tn_ext <- se %>%
  subset(NAME == "Tennessee") %>%
  gIntersection(as(extent(gsmnp_elv), "SpatialPolygons"))

pal <- wes_palette("Zissou1", 256, type = "continuous")

ggplot() + 
  geom_raster(data = as.data.frame(gsmnp_elv, xy = T), aes(x = x, y = y, fill = elv)) +
  geom_raster(data = as.data.frame(gsmnp_hs, xy = T), aes(x = x, y = y, alpha = 1 - layer), fill = "gray20") +
  geom_polygon(data = tn_ext, aes(x = long, y = lat, group = group), fill = NA, col = "maroon", size = 0.25) +
  geom_polygon(data = gsmnp, aes(x = long, y = lat, group = group), fill = NA, col = "grey20", size = 0.25) +
  geom_polygon(data = as(extent(gsmnp), "SpatialPolygons"), aes(x = long, y = lat, group = group), fill = NA, col = "grey20", size = 0.25) +
  scale_fill_gradientn("Elevation (m)", colors = pal, na.value = NA) + 
  scale_alpha(guide = FALSE, range = c(0,1)) + 
  coord_cartesian(xlim = c(-864541.4, -775453.6), ylim = c(4019379, 4054983)) +
  theme_map() + 
  theme(legend.position = "bottom")




