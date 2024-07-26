# Return map (aus or nz) filled with data points
create_map = function(jobs_df, country_shp) { # Assumes jobs_df already filtered to chosen region
  ## Get City Job Counts and POINT-objects
  country_city_points = jobs_df %>% # contains both counts and points
    group_by(`city`,`region`) %>%
    summarise(
      `Latitude` = mean(`city_lat`, na.rm=T), # coordinates may not be exact but in the right area which is good enough
      `Longitude` = mean(`city_lng`, na.rm=T),
      `Number of Jobs` = n()
    ) %>%
    na.omit() %>% rename(`Region`=`region`, `City`=`city`) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) # convert coordinates to sf-object (point)
  ## Get Region Job Counts and POLYGON-objects
  country_region_polygons = jobs_df %>%
    group_by(`region`) %>%
    summarise(`Number of Jobs` = n()) %>%
    rename(`Region`=`region`) %>%
    left_join(x=country_shp, y=. , by=c("Region"))
  if (length(unique(jobs_df$`region`)) == 1) { # Plot Polygon for Only One Region
    country_region_polygons = country_region_polygons %>% na.omit() # removes other regions  (should contain NA for counts)
  } else { # Plot Polygon for All Regions
    country_region_polygons = country_region_polygons %>% mutate(`Number of Jobs` = ifelse(is.na(`Number of Jobs`), 0, `Number of Jobs`)) # replace NA (regions with no jobs) with 0
  }
  ## Return Plot
  p = ggplot() +
    geom_sf(data=country_region_polygons, aes(fill=`Number of Jobs`, group=`Region`, text=paste("Region:", `Region`, "<br>Total Number of Jobs:", `Number of Jobs`))) + # plots polygons of regions
    scale_fill_gradient(low = "lightgrey", high = "blue") + # colorscale for regions polygon
    geom_sf(data=country_city_points, aes(group=`City`, text=paste("City:", `City`, "<br>Number of Jobs:", `Number of Jobs`)), color="orange", size=0.75) + # plots points of city-region
    labs(title="Job Locations") + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) # adjust title position
  return( ggplotly(p, tooltip=c("text"), dynamicTicks=T) )
}

## NOTE: https://stackoverflow.com/questions/64565574/how-to-properly-store-shapefiles-when-deploying-r-shiny-app
# Files required by a published Shiny app should go in a folder named www. If you've got them in the same folder as app.R, they won't be detected properly.

## Old Version
# create_map = function(df, country) {
#   points = df %>%
#     group_by(`city_lat`,`city_lng`,`region`) %>%
#     count() %>% na.omit() %>%
#     rename(`Number of Jobs`=`n`)
#   if (country == "Australia") {
#     map_coords = ozmap_states
#     p = ggplot() + 
#       geom_sf(data=map_coords, colour="black", fill="white") +
#       geom_point(data=points, aes(x=`city_lng`, y=`city_lat`, size=`Number of Jobs`,
#                                   fill=`region`)) +
#       scale_size(range = c(.1, 15), name="")
#   }
#   if (country == "New Zealand") {
#     map_coords = map_data('nz')
#     p = ggplot() +
#       geom_polygon(data=map_coords, aes(x=`long`, y=`lat`, group=`group`), color="black", fill="white") +
#       geom_point(data=points, aes(x=`city_lng`, y=`city_lat`, size=`Number of Jobs`,
#                                   fill=`region`)) +
#       scale_size(range = c(.1, 15), name="")
#   }
#   p = p + labs(x="Longitude", y="Latitude", title='Job Locations') +
#     theme_minimal() + 
#     theme(plot.title = element_text(hjust = 0.5))
#   return(ggplotly(p, dynamicTicks=T))
# }