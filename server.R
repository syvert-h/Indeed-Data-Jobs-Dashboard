library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(maps) # map for nz
library(ozmaps) # map for aus
library(sf) # map for aus

function(input, output, session) {
  ### SETUP ###
  # Store active dataframes
  rv = reactiveValues(
    aus = read_csv(".\\data\\all_data_jobs_australia.csv"),
    nz = read_csv(".\\data\\all_data_jobs_new_zealand.csv") %>%
      filter(`region` != "Chatham Islands"),
    df = NULL
  )
  observeEvent(input$country_dash, {
    # store current dataframe
    if (input$country_dash == "New Zealand") {rv$df = rv$nz} else {rv$df = rv$aus}
    # fill region dropdown
    regions = rv$df$`region` %>% unique() %>% na.omit()
    output$region_dash = renderUI({
      selectInput(
        "region_dash",
        label="Select a region:",
        choices=c("All", sort(regions)) # NOTE: currently ""
      )
    })
  })
  observeEvent(input$region_dash, {
    if (input$country_dash == "New Zealand") {rv$df = rv$nz} else {rv$df = rv$aus}
    if (input$region_dash != "All") {
      rv$df = rv$df %>% filter(`region` == input$region_dash)
    }
  })
  
  ### OUTPUT ###
  ## Skills Barplots
  skills_barplot = function(col) {
    p = rv$df[,col] %>%
      na.omit() %>%
      mutate(`Skills`=str_split(get(col), ",")) %>%
      select(`Skills`) %>% unlist() %>%
      table() %>% as_tibble() %>%
      mutate(`Percent`=round(`n`/sum(`n`)*100, 2)) %>%
      arrange(desc(`Percent`)) %>% head(10) %>%
      ggplot(aes(x=reorder(`.`, `Percent`), y=`Percent`, 
                 text=sprintf("%s\nPercent: %.2f", `.`, `Percent`))) +
      geom_bar(stat="identity", fill="#3c8dbc") +
      coord_flip() +
      labs(x="Tool/Skill", y="% of Jobs") +
      theme_minimal()
    ggplotly(p, tooltip=c("text"))
  }
  output$data_tools = renderPlotly({skills_barplot("data_tools")})
  output$hard_skills = renderPlotly({skills_barplot("hard_skills")})
  output$soft_skills = renderPlotly({skills_barplot("soft_skills")})
  
  ## Job Type Donut Charts
  output$jobtype_plot = renderPlotly({
    data = rv$df %>%
      replace_na(list(`job_type`="Not Specified", `remote_type`="Not Remote"))
    jt = data %>%
      filter(`remote_type` == "Not Remote") %>%
      pull(`job_type`) %>% str_split(pattern=",") %>% unlist() %>%
      str_trim() %>% table() %>% as_tibble() # `.` is the job type
    rt = data %>%
      filter(`remote_type` != "Not Remote") %>%
      pull(`remote_type`) %>% str_split(pattern=",") %>% unlist() %>%
      str_trim() %>% table() %>% as_tibble() # `.` is the job type
    fig = plot_ly() %>%
      add_pie(data=jt, labels= ~`.`, values= ~`n`, name="Job Type", hole=0.4,
              domain=list(row=0, column=0), textinfo='label+percent', textposition="inside") %>%
      add_pie(data=rt, labels= ~`.`, values= ~`n`, name="Remote Type", hole=0.4,
              domain=list(row=0, column=1), textinfo='label+percent', textposition="inside") %>%
      layout(
        title=list(text=sprintf("Job Types in %s%s", 
                        ifelse(input$region_dash == "All", "", paste0(input$region_dash, ", ")), 
                        input$country_dash),
                   y=0.975
                   ),
        showlegend = F,
        grid=list(rows=1, columns=2),
        xaxis = list(showgrid=F, zeroline=F, showticklabels=F),
        yaxis = list(showgrid=F, zeroline=F, showticklabels=F),
        annotations=list(
          # subplot 1 - job type
          list(x=0, y=0, text="Non-Remote", showarrow=F),
          # subplot 2 - remote type
          list(x=1, y=0, text="Remote", showarrow=F)
        )
      )
    return(fig)
  })
  
  ## Salary Hist+Box Plots
  salary_plot = function() {
    # filter data
    salary_types = c("Yearly"="year","Monthly"="month","Weekly"="week","Daily"="day","Hourly"="hour")
    data = rv$df %>%
      filter(!is.na(`salary`)) %>%
      filter(!is.na(`salary_type`)) %>%
      filter(!is.na(`region`)) %>%
      filter(`salary_type` == salary_types[input$salary_tab])
    # plot
    box = plot_ly(data, x= ~`salary`, y= ~`region`, type="box", color=~`region`,
                  legendgroup= ~`region`, showlegend=F) %>%
      layout(yaxis=list(showticklabels=F))
    hist = plot_ly(data, x= ~`salary`, type="histogram", color= ~`region`,
                   legendgroup= ~`region`) %>%
      layout(
        barmode="stack", 
        legend=list(x=1, y=0.5, title=list(text="Region")),
        xaxis=list(title="Salary"),
        yaxis=list(title="Count")
      )
    fig = subplot(box, hist, nrows=2, shareX=T, heights = c(0.2, 0.8)) %>%
      layout(
        title=list(text=sprintf("Salary Distribution for %s%s", 
                                ifelse(input$region_dash == "All", "", paste0(input$region_dash, ", ")), 
                                input$country_dash
        )
        )
      )
    return(fig)
  }
  output$salary_year = renderPlotly({salary_plot()})
  output$salary_month = renderPlotly({salary_plot()})
  output$salary_weekly = renderPlotly({salary_plot()})
  output$salary_daily = renderPlotly({salary_plot()})
  output$salary_hourly = renderPlotly({salary_plot()})
  # Salary Table
  output$salary_table = renderDataTable({
    salary_types = c("Yearly"="year","Monthly"="month","Weekly"="week","Daily"="day","Hourly"="hour")
    data = rv$df %>%
      select(`city`, `region`, `salary`, `salary_type`) %>%
      na.omit() %>%
      filter(`salary_type` == salary_types[input$salary_tab])%>% 
      arrange(desc(`salary`))
    return(data)
  }, options=list(dom="t", scrollY="420px"))
  
  # Remote Jobs Line Plot
  output$remote_num_plot = renderPlotly({
    data = rv$df %>%
      select(`date_posted`,`remote`) %>%
      na.omit() %>%
      mutate(`date_posted`=as.Date(`date_posted`, format="%d-%m-%Y")) %>%
      filter(`date_posted` > Sys.Date() %m+% months(-2)) %>% # past 2 months
      group_by(`date_posted`,`remote`) %>%
      count() %>%
      mutate(`remote`=ifelse(`remote`==T, "Remote", "Non-Remote")) %>%
      pivot_wider(names_from=`remote`, values_from=`n`) %>%
      replace_na(list(`Non-Remote`=0, `Remote`=0)) %>%
      ungroup()
    fig = plot_ly(data, x= ~`date_posted`, type="scatter", y= ~`Non-Remote`, 
                  name="Non-Remote", mode="lines+markers") %>%
      add_trace(y= ~`Remote`, name="Remote", mode="lines+markers") %>%
      layout(title="Number of Remote & Non-Remote Listings per Day",
             font=list(size = 10),
             yaxis=list(title="Count", showgrid=F),
             xaxis=list(title="Date Posted", showgrid=F),
             legend=list(x=0, y=-0.2, orientation='h')
      )
    return(fig)
  })
  
  # Top Hiring Companies Plot
  output$top_hiring_plot = renderPlotly({
    p = rv$df$`company` %>%
      na.omit() %>%
      table() %>% as_tibble() %>%
      arrange(desc(`n`)) %>% head(10) %>%
      ggplot(aes(x=reorder(`.`, `n`), y=`n`, 
                 text=sprintf("%s\nCount: %d", `.`, `n`))) +
      geom_bar(stat="identity", fill="#3c8dbc") +
      coord_flip() +
      labs(y="Number of Openings", x="Company", title="Top Hiring Companies") +
      theme_minimal()
    ggplotly(p, tooltip=c("text"))
  })
  
  # Education Level Donut Chart
  output$education_pie = renderPlotly({
    data = rv$df %>%
      replace_na(list(`degree_type`="Not Specified")) %>%
      pull(`degree_type`) %>% str_split(pattern=",") %>% unlist() %>%
      table() %>% as_tibble() # `.` is the job type
    fig = plot_ly() %>%
      add_pie(data=data, labels= ~`.`, values= ~`n`, name="Degree Type", hole=0.4, 
              textinfo='label+percent', textposition="inside") %>%
      layout(
        title = sprintf("Education Level Required in %s%s", 
                        ifelse(input$region_dash == "All", "", paste0(input$region_dash, ", ")), 
                        input$country_dash)
      )
    return(fig)
  })
  
  # Map of Job Locations
  create_map = function() {
    data = rv$df
    map_coords = ozmap("states")
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
    if (input$country_dash == "New Zealand") {
      map_coords = map_data('nz')
      p = ggplot() +
          geom_polygon(data=map_coords, aes(x=`long`, y=`lat`, group=`group`),
                       color="black", fill="white") +
          geom_point(data=points, aes(x=`city_lng`, y=`city_lat`, size=`Number of Jobs`,
                                         fill=`region`)) +
          scale_size(range = c(.1, 15), name="")
    }
    p = p + labs(title = sprintf("Job Locations in %s%s", 
                                 ifelse(input$region_dash == "All", "", paste0(input$region_dash, ", ")), 
                                 input$country_dash),
                 x="Longitude", y="Latitude") +
      theme_minimal()
    return(ggplotly(p, dynamicTicks=T))
  }
  output$jobs_map = renderPlotly({create_map()})
}
