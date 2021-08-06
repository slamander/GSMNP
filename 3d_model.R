

### Prepping workspace for rayshader

## convert to matrix for rayshader
alt_gsmnp_matrix <- alt_gsmnp %>%
  raster_to_matrix()

## covert sp objects to sf
gsmnp_sf <- gsmnp %>%
  st_as_sf()

## convert spdf objects to sf
gsmnp_spdf_sf <- st_as_sf(gsmnp_spdf)

## convert sf waterways object into line overlay for rayshader
stream_layer <- generate_line_overlay(gsmnp_streams, linewidth = 1, color = "skyblue2", extent = extent(gsmnp_sf), heightmap = alt_gsmnp_agg_matrix)


#### plot rayshader object in 2d
alt_gsmnp_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_water(
    detect_water(alt_gsmnp_matrix, 
                 min_area = 50),
    color = "imhof3") %>%
  add_shadow(ray_shade(alt_gsmnp_matrix, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(alt_gsmnp_matrix), 0) %>%
  plot_map()


#### plot rayshader object in 3d
alt_gsmnp_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_water(
    detect_water(alt_gsmnp_matrix, 
                 min_area = 100),
    color = "imhof3") %>%
  add_shadow(ray_shade(alt_gsmnp_matrix, zscale = 3), 0.5,
             progbar = interactive()) %>%
  add_shadow(ambient_shade(alt_gsmnp_matrix), 0) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>%
  add_overlay(generate_point_overlay(st_as_sf(gsmnp_spdf), color = "red", size = 3,
                                     extent = extent(alt_gsmnp), 
                                     heightmap = alt_gsmnp_matrix))  %>%
  plot_3d(alt_gsmnp_matrix, zscale = 10, fov = 0, theta = 135, zoom = 0.5, phi = 45, windowsize = c(1000, 800))

alt_gsmnp_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_water(
    detect_water(alt_gsmnp_matrix, 
                 min_area = 100),
    color = "imhof3") %>%
  add_shadow(ray_shade(alt_gsmnp_matrix, zscale = 3), 0.5,
             progbar = interactive()) %>%
  add_shadow(ambient_shade(alt_gsmnp_matrix), 0) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>%
  add_overlay(generate_point_overlay(st_as_sf(gsmnp_spdf), color = "red", size = 3,
                                     extent = extent(alt_gsmnp), 
                                     heightmap = alt_gsmnp_matrix))  %>%
  plot_3d(alt_gsmnp_matrix, zscale = 10, fov = 0, theta = 135, zoom = 0.5, phi = 45, windowsize = c(1000, 800))


alt_gsmnp_matrix %>%
  sphere_shade(texture = "desert",
               progbar = interactive()) %>%
  add_water(
    detect_water(alt_gsmnp_matrix, 
                 min_area = 100),
    color = "imhof3") %>%
  add_shadow(ray_shade(alt_gsmnp_matrix, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(alt_gsmnp_matrix), 0) %>%
  plot_3d(alt_gsmnp_matrix, zscale = 10, fov = 0, theta = 135, zoom = 0.5, phi = 45, windowsize = 600)
render_points(extent = extent(alt_gsmnp),
              lat = gsmnp_spdf_sf$Latitude_utm, 
              long = gsmnp_spdf_sf$Longitude_utm,
              zscale = 5,
              altitude = 0,
              color = "black")
render_points(extent = extent(alt_gsmnp),
              lat = gsmnp_spdf_sf$Latitude_utm, 
              long = gsmnp_spdf_sf$Longitude_utm,
              zscale = 5,
              heightmap = alt_gsmnp_matrix,
              color = "red")

save_obj("3dmodel")
