library(tidyverse)
library(plotly)
library(ggplot2)
library(ozmaps) # map for aus
library(sf) # map for aus

# setwd(".\\Indeed-Jobs-Dashboard")

aus = read_csv("all_data_jobs_australia.csv")
nz = read_csv("all_data_jobs_new_zealand.csv")

# Map of Job Locations
create_map = function(temp="australia") {
  data = aus
  map_coords = ozmap_states
  map_coords
  points = data %>%
    group_by(`city_lat`,`city_lng`,`region`) %>%
    count() %>%
    na.omit() %>%
    rename(`Number of Jobs`=`n`)
  p = ggplot() +
    geom_sf(data=map_coords, colour="black", fill="white") +
    geom_point(data=points, aes(x=`city_lng`, y=`city_lat`, size=`Number of Jobs`,
                                fill=`region`)) +
    scale_size(range = c(.1, 15), name="")
  if (temp == "New Zealand") {
    map_coords = map_data('nz')
    p = ggplot() +
      geom_polygon(data=map_coords, aes(x=`long`, y=`lat`, group=`group`),
                   color="black", fill="white") +
      geom_point(data=points, aes(x=`city_lng`, y=`city_lat`, size=`Number of Jobs`,
                                  fill=`region`)) +
      scale_size(range = c(.1, 15), name="")
  }
  p = p + labs(title = sprintf("Job Locations in %s%s"," ", temp),
               x="Longitude", y="Latitude") +
    theme_minimal()
  return(ggplotly(p, dynamicTicks=T))
}
create_map()

ozmap("states")

# x = aus %>%
#   replace_na(list(`job_type`="Not Specified", `remote_type`="Not Remote")) %>%
#   filter(`remote_type` == "Not Remote") %>%
#   pull(`job_type`) %>% str_split(pattern=",") %>% unlist() %>%
#   str_trim() %>% table() %>% as_tibble() # `.` is the job type
# unique(x$.)

# ### Map of New Zealand
# library(maps)
# # load NZ state map data
# main_nz = map_data('nz')
# # points on map
# nz_points = nz %>%
#   filter(`region` != "Chatham Islands") %>%
#   group_by(`city_lat`,`city_lng`,`region`) %>%
#   count() %>%
#   na.omit() %>%
#   rename(`Number of Jobs`=`n`)
# # plot
# p = ggplot() +
#   geom_polygon(data=main_nz, aes(x=`long`, y=`lat`, group=`group`),
#                color="black", fill="white") +
#   geom_point(data=nz_points, aes(x=`city_lng`, y=`city_lat`, size=`Number of Jobs`,
#                                  fill=`region`)) +
#   scale_size(range = c(.1, 15), name="")
# ggplotly(p)
# 
# 
# ### Map of Australia
# library(ozmaps)
# library(sf)
# 
# sf_oz = ozmap("states")
# 
# aus_points = aus %>%
#   group_by(`city_lat`,`city_lng`,`region`) %>%
#   count() %>%
#   na.omit() %>%
#   rename(`Number of Jobs`=`n`)
# 
# p = ggplot() + 
#   geom_sf(data=sf_oz, colour="black", fill="white") +
#   geom_point(data=aus_points, aes(x=`city_lng`, y=`city_lat`, size=`Number of Jobs`,
#                                  fill=`region`)) +
#   scale_size(range = c(.1, 15), name="")
# ggplotly(p, dynamicTicks=T)




# data = aus %>%
#   select(`date_posted`,`remote`) %>%
#   na.omit() %>%
#   mutate(`date_posted`=as.Date(`date_posted`, format="%d-%m-%Y")) %>%
#   group_by(`date_posted`,`remote`) %>%
#   count() %>%
#   mutate(`remote`=ifelse(`remote`==T, "Remote", "Non-Remote")) %>%
#   pivot_wider(names_from=`remote`, values_from=`n`) %>%
#   replace_na(list(`Non-Remote`=0, `Remote`=0)) %>%
#   ungroup()
# 
# fig = plot_ly(data, x= ~`date_posted`, type="scatter", y= ~`Non-Remote`, 
#         name="Non-Remote", mode="lines+markers") %>%
#   add_trace(y= ~`Remote`, name="Remote", mode="lines+markers") %>%
#   layout(title="Number of Remote & Non-Remote Listings per Day", 
#          yaxis=list(title="Count"),
#          xaxis=list(title="Date Posted")
#   )
# return(fig)

# # filter data
# salary_types = c("Yearly"="year","Monthly"="month","Weekly"="week","Daily"="day","Hourly"="hour")
# data = rv$df %>%
#   filter(!is.na(`salary`)) %>%
#   filter(!is.na(`salary_type`)) %>%
#   filter(!is.na(`region`)) %>%
#   mutate(`region`=as.factor(`region`)) %>%
#   filter(`salary_type` == salary_types["Yearly"])
# # plot
# box = plot_ly(data, x= ~`salary`, y= ~`region`, type="box", color=~`region`,
#               legendgroup= ~`region`, showlegend=F) %>%
#   layout(yaxis=list(showticklabels=F))
# hist = plot_ly(data, x= ~`salary`, type="histogram", color= ~`region`,
#                legendgroup= ~`region`) %>%
#   layout(barmode="stack", legend=list(x=1, y=0.5))
# fig = subplot(box, hist, nrows=2, shareX=T, heights = c(0.2, 0.8)) %>%
#   layout(
#     title=list(text=sprintf("Salary Distribution for %s%s", 
#                             ifelse(input$region_dash == "All", "", paste0(input$region_dash, ", ")), 
#                             input$country_dash
#                             )
#                )
#   )
# return(fig)

# # donut plot for job type
# data = aus %>%
#   replace_na(list(`job_type`="Not Specified", `remote_type`="Not Remote"))
# jt = data %>%
#   filter(`remote_type` == "Not Remote") %>%
#   pull(`job_type`) %>% str_split(pattern=",") %>% unlist() %>%
#   table() %>% as_tibble() # `.` is the job type
# rt = data %>%
#   filter(`remote_type` != "Not Remote") %>%
#   pull(`remote_type`) %>% str_split(pattern=",") %>% unlist() %>%
#   table() %>% as_tibble() # `.` is the job type
# fig = plot_ly() %>%
#   add_pie(data=jt, labels= ~`.`, values= ~`n`, name="Job Type", hole=0.4,
#           domain=list(row=0, column=0), textinfo='label+percent', textposition="inside") %>%
#   add_pie(data=rt, labels= ~`.`, values= ~`n`, name="Remote Type", hole=0.4,
#           domain=list(row=0, column=1), textinfo='label+percent', textposition="inside") %>%
#   layout(
#     title = sprintf("Job Types in %s%s", 
#                     ifelse(t == "All", "", "temp, "), 
#                     "Australia"),
#     showlegend = F,
#     grid=list(rows=1, columns=2),
#     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#     annotations=list(
#       # subplot 1 - job type
#       list(x=0.20, y=1, text="Non-Remote Job Type", showarrow=FALSE),
#       # subplot 2 - remote type
#       list(x=0.80, y=1, text="Remote Job Type", showarrow=FALSE)
#     )
#   )
# fig

# jt = aus %>%
#   replace_na(list(`job_type`="Not Specified")) %>%
#   pull(`job_type`) %>% str_split(pattern=",") %>% unlist() %>%
#   table() %>% as_tibble() %>%
#   mutate(`Percent`=round(`n`/sum(`n`)*100, 2))
# # Compute the cumulative percentages (top of each rectangle)
# jt$ymax = cumsum(jt$`Percent`)
# # Compute the bottom of each rectangle
# jt$ymin = c(0, head(jt$ymax, n=-1))
# jt$`Job Type` = jt$`.` # proper name column
# # Donut chart
# fig = jt %>%
#   group_by(`Job Type`) %>%
#   plot_ly(labels= ~`Job Type`, values = ~`Percent`) %>%
#   add_pie(hole=0.6) %>%
#   layout(
#     showlegend = T,
#     # title = list(text=sprintf("%s Scoring Makeup", input$dashboard_team), y=0.975, x=0.5, xanchor='center', yanchor='top'),
#     xaxis = list(showgrid = F, zeroline = T, showticklabels = F),
#     yaxis = list(showgrid = F, zeroline = T, showticklabels = F),
#     legend=list(x=1, y=0.5)
#   )

# skills_barplot = function(col) {
#   p = aus[,col] %>%
#     na.omit() %>%
#     mutate(`Skills`=str_split(get(col), ",")) %>%
#     select(`Skills`) %>% unlist() %>%
#     table() %>% as_tibble() %>%
#     mutate(`Percent`=round(`n`/sum(`n`)*100, 2)) %>%
#     arrange(desc(`Percent`)) %>% head(10) %>%
#     ggplot(aes(x=reorder(`.`, `Percent`), y=`Percent`, 
#                text=sprintf("%s\nPercent: %.2f", `.`, `Percent`))) +
#     geom_bar(stat="identity") +
#     coord_flip() +
#     labs(x="Tool/Skill", y="% of Jobs")
#   ggplotly(p, tooltip=c("text"))
# }
# skills_barplot("data_tools")
# skills_barplot("hard_skills")
# skills_barplot("soft_skills")

# # region
# aus$region %>%
#   na.omit() %>%
#   unique()
