# Return map (aus or nz) filled with data points
create_map = function(df, country) {
  points = df %>%
    group_by(`city_lat`,`city_lng`,`region`) %>%
    count() %>% na.omit() %>%
    rename(`Number of Jobs`=`n`)
  if (country == "Australia") {
    map_coords = ozmap_states
    p = ggplot() + 
      geom_sf(data=map_coords, colour="black", fill="white") +
      geom_point(data=points, aes(x=`city_lng`, y=`city_lat`, size=`Number of Jobs`,
                                  fill=`region`)) +
      scale_size(range = c(.1, 15), name="")
  }
  if (country == "New Zealand") {
    map_coords = map_data('nz')
    p = ggplot() +
      geom_polygon(data=map_coords, aes(x=`long`, y=`lat`, group=`group`), color="black", fill="white") +
      geom_point(data=points, aes(x=`city_lng`, y=`city_lat`, size=`Number of Jobs`,
                                  fill=`region`)) +
      scale_size(range = c(.1, 15), name="")
  }
  p = p + labs(x="Longitude", y="Latitude", title='Job Locations') +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
  return(ggplotly(p, dynamicTicks=T))
}