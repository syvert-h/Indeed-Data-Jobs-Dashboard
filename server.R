library(shiny)
library(tidyverse)
library(plotly)
library(sf)
library(aws.s3)

function(input, output, session) {
  # Store active dataframe
  rv = reactiveValues(
    df = NULL, # holds chosen country df
    data = NULL, # holds active data for visuals
    country = NULL, # holds active country
    region = NULL # holds active region(s)
  )
  
  ### Define the dataframes
  ## Setup AWS S3 Access
  s3BucketName = scan("bucketname.txt", what="txt")
  s3Filename_aus = scan("s3_filename_aus.txt", what="txt")
  s3Filename_nz = scan("s3_filename_nz.txt", what="txt")
  readRenviron(".Renviron") # read from current directories .Renviron file (not the default one)
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = Sys.getenv("AWS_ACCESS_KEY_ID"),
    "AWS_SECRET_ACCESS_KEY" = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    "AWS_DEFAULT_REGION" = Sys.getenv('AWS_DEFAULT_REGION') # S3 bucket region
  )
  # Note: Error 301 -- from Sys.getenv() not working as intended; value extracted from .Renviron not correct
  # Note: Error 401 -- make sure permissions in both IAM user and S3 Bucket are correct; PLUS make sure IAM User created with pragmatic access (should have option for user and password at creation)
  nz_df = s3read_using(read.csv, object=s3Filename_nz, bucket=s3BucketName)
  aus_df = s3read_using(read.csv, object=s3Filename_aus, bucket=s3BucketName)

  ## Load Each Country's Shapefile
  # Note: wasn't recognising file path when using "\\" so used "/" instead (P.S. it is shown on the deployment preview that it uses "/" and not "\")!
  shp_lst = list(
    "Australia" = st_read("./AUS_Region_Simplified_SHP/STE_2021_AUST_GDA2020.shp") %>% # path may change
      select(`STE_NAME21`,`geometry`) %>%
      rename(`Region`=`STE_NAME21`, `Geometry`=`geometry`) %>%
      filter(!`Region` %in% c("Outside Australia","Other Territories")),
    "New Zealand" = st_read("./NZ_Region_Simplified_SHP/regional-council-2023-clipped-generalised.shp") %>% # path may change
      select(`REGC2023_2`,`geometry`) %>%
      rename(`Region`=`REGC2023_2`,`Geometry`=`geometry`) %>%
      mutate( `Region` = gsub("Region", "", `Region`), `Region` = str_trim(`Region`) ) %>%
      filter(`Region` != "Area Outside")
  )
  
  # Temporary text instructions (at startup)
  output$temp_text = renderText({"Click 'Apply' to get started!"})
  
  # Store current country's data to update region dropdown
  observeEvent(input$country, {
    if (input$country == "Australia") {rv$df = aus_df} else {rv$df = nz_df}
    rv$df = rv$df %>% mutate(
      # `date_posted` = as.Date(`date_posted`, format="%d-%m-%Y"),
      `date_posted` = as.Date(`date_posted`, format="%Y-%m-%d"),
      `remote` = ifelse(`remote` == 'True', 'Remote', 'Non-Remote')
    )
    # update region options
    regions = rv$df$`region` %>% unique() %>% na.omit() %>% sort()
    updateSelectInput(session, 'region', choices=c('All', regions))
  })
  
  # Store (possibly) filtered data for visuals
  observeEvent(input$go, {
    current = rv$df
    if (input$region != 'All') {current = current %>% filter(`region` == input$region)}
    if (!is.null(input$keywords)) {current = current %>% filter(grepl(tolower(input$keywords), `title`))}
    rv$data = current
    rv$country = input$country
    rv$region = input$region
  })
  
  # DataTable for the active data
  output$datatable = renderDataTable({
    req(input$go) # executes following code if 'present' (not null)
    rv$data %>%
      arrange(desc(`date_posted`)) %>%
      select(-c(`city_lat`,`city_lng`,`country`,`job_description`))
  }, options=list(scrollX=TRUE, autoWidth = TRUE))
  
  # Dynamic Headers (H2)
  source('R/server_h2_header.R')
  output$skills_header = renderUI({get_header('Skills In-demand', rv$region, rv$country)})
  
  # String column word frequency barplot
  source('R/server_string_barplot.R')
  output$data_tools = renderPlotly({
    req(input$go)
    get_string_barplot(rv$data, 'data_tools')
  })
  output$hard_skills = renderPlotly({
    req(input$go)
    get_string_barplot(rv$data, 'hard_skills')
  })
  output$soft_skills = renderPlotly({
    req(input$go)
    get_string_barplot(rv$data, 'soft_skills')
  })
  
  # Job Types Header
  output$job_types_header = renderUI({get_header('Job Types', rv$region, rv$country)})
  
  # Job Types Donut Chart
  output$job_types = renderPlotly({
    req(input$go)
    jt = rv$data %>%
      filter(`remote` == 'Non-Remote') %>%
      separate_rows(`job_type`, sep=',\\s*') %>%
      count(`job_type`) %>%
      mutate(`job_type` = ifelse(`job_type`=='', 'Not Specified', `job_type`))
    rt = rv$data %>%
      filter(`remote` == 'Remote') %>%
      separate_rows(`remote_type`, sep=',\\s*') %>%
      count(`remote_type`) %>%
      mutate(`remote_type` = ifelse(`remote_type`=='', 'Not Specified', `remote_type`))
    fig = plot_ly() %>%
      add_pie(data=jt, labels= ~`job_type`, values= ~`n`, name="Job Type", hole=0.4,
              domain=list(row=0, column=0), textinfo='label+percent', textposition="inside") %>%
      add_pie(data=rt, labels= ~`remote_type`, values= ~`n`, name="Remote Type", hole=0.4,
              domain=list(row=0, column=1), textinfo='label+percent', textposition="inside") %>%
      layout(
        showlegend = F,
        grid=list(rows=1, columns=2),
        xaxis = list(showgrid=F, zeroline=F, showticklabels=F),
        yaxis = list(showgrid=F, zeroline=F, showticklabels=F),
        annotations=list(
          list(x=0, y=0, text="Non-Remote", showarrow=F), # subplot 1 - job type
          list(x=1, y=0, text="Remote", showarrow=F) # subplot 2 - remote type
        )
      )
    return(fig)
  })
  
  # Job Types Count Line Chart
  output$job_types_count = renderPlotly({
    req(input$go)
    data = rv$data %>%
      select(`date_posted`,`remote`) %>% na.omit() %>%
      filter(`date_posted` > Sys.Date() %m+% months(-3)) %>% # past 3 months
      group_by(`date_posted`,`remote`) %>% count() %>%
      pivot_wider(names_from=`remote`, values_from=`n`) %>%
      replace_na(list(`Non-Remote`=0, `Remote`=0)) %>% ungroup()
    fig = plot_ly(data, x= ~`date_posted`, type="scatter", y= ~`Non-Remote`, 
                  name="Non-Remote", mode="lines+markers") %>%
      add_trace(y= ~`Remote`, name="Remote", mode="lines+markers") %>%
      layout(
        title="Number of Remote/Non-Remote Listings per Day", 
        font=list(size = 10),
        yaxis=list(title="Count", showgrid=F),
        xaxis=list(title="Date Posted", showgrid=F),
        legend=list(x=0, y=-0.2, orientation='h'),
        hovermode = "x unified"
      )
    return(fig)
  })
  
  # Job Market Header
  output$market_headers = renderUI({get_header('Job Market', rv$region, rv$country)})
  
  # Job Location Chart
  source('R/server_job_map.R')
  output$jobs_map = renderPlotly({
    req(input$go)
    country_shp = shp_lst[[input$country]]
    create_map(rv$data, country_shp)
  })
  
  # Education Required Donut Chart
  output$education = renderPlotly({
    req(input$go)
    data = rv$data %>%
      separate_rows(`degree_type`, sep = ",\\s*") %>%
      count(`degree_type`) %>%
      mutate(`degree_type`=ifelse(`degree_type`=='', 'Not Specified', `degree_type`))
    fig = plot_ly() %>%
      add_pie(data=data, labels= ~`degree_type`, values= ~`n`, name="Degree Type", hole=0.4, 
              textinfo='label+percent', textposition="inside") %>%
      layout(
        title = 'Education Required'
      )
    return(fig)
  })
  
  # Top Hiring Barplot
  output$top_hiring = renderPlotly({
    req(input$go)
    data = rv$data %>% count(`company`) %>% 
      na.omit() %>% arrange(desc(`n`)) %>% head(10)
    fig = plot_ly(data, x = ~`n`, y = ~`company`, type='bar', orientation='h') %>%
      layout(
        title = 'Top Hiring Employers',
        yaxis = list(
          categoryorder = 'total ascending',
          title = list(text = 'Company')
        ),
        xaxis = list(title = list(text = 'Number of Openings'))
      )
    return(fig)
  })
  
  # Salary Header
  output$salary_header = renderUI({get_header('Salaries', rv$region, rv$country)})
  
  # Salary Hist+Box Plots
  source('R/server_salary_chart.R', local=T) # local=TRUE since function requires both input$go and input$salary_tab
  output$salary_year = renderPlotly({salary_plot(rv$data)})
  output$salary_month = renderPlotly({salary_plot(rv$data)})
  output$salary_week = renderPlotly({salary_plot(rv$data)})
  output$salary_day = renderPlotly({salary_plot(rv$data)})
  output$salary_hour = renderPlotly({salary_plot(rv$data)})
  
  # Salary DataTable
  output$salary_table = renderDataTable({
    req(input$go)
    salary_types = c("Yearly"="year","Monthly"="month","Weekly"="week","Daily"="day","Hourly"="hour")
    data = rv$data %>%
      filter(`salary_type` == salary_types[input$salary_tab]) %>%
      select(`city`, `region`, `salary`) %>% na.omit() %>%
      arrange(desc(`salary`))
    return(data)
  }, options=list(dom="t", scrollY="400px"), rownames=F)
}
