library(shiny)
library(plotly)
library(DT)

fluidPage(
  titlePanel('Indeed Data Jobs'),
  
  sidebarLayout(
    sidebarPanel(width=2,
                 selectInput('country',
                             label='Country:',
                             choices=c('Australia','New Zealand')
                 ),
                 selectInput('region',
                             label='Region:',
                             choices=c() # will be updated
                 ),
                 textInput('keywords',
                           label='Keyword Search Job Title:',
                           value=''
                 ),
                 actionButton('go', label='Apply')
    ),
    
    mainPanel(width=10,
              tabsetPanel(
                tabPanel('Dashboard',
                         fluidRow(
                           column(uiOutput('skills_header'), width=5),
                           column(uiOutput('job_types_header'), width=7)
                         ),
                         fluidRow(
                           column(width=5,
                                  tabsetPanel(id='skills_tab',
                                              tabPanel('Data Tools', plotlyOutput('data_tools')),
                                              tabPanel('Hard Skills', plotlyOutput('hard_skills')),
                                              tabPanel('Soft Skills', plotlyOutput('soft_skills'))
                                  )
                           ),
                           column(width=7,
                                  plotlyOutput('job_types', height='300px'),
                                  plotlyOutput('job_types_count', height='150px')
                           )
                         ),
                         fluidRow(column(uiOutput('salary_header'), width=12)),
                         fluidRow(
                           column(width=9,
                                  tabsetPanel(id='salary_tab',
                                              tabPanel('Yearly', plotlyOutput('salary_year', height='400px')),
                                              tabPanel('Monthly', plotlyOutput('salary_month', height='400px')),
                                              tabPanel('Weekly', plotlyOutput('salary_week', height='400px')),
                                              tabPanel('Daily', plotlyOutput('salary_day', height='400px')),
                                              tabPanel('Hourly', plotlyOutput('salary_hour', height='400px'))
                                  )
                           ),
                           column(width=3, dataTableOutput('salary_table'))
                         ),
                         fluidRow(column(uiOutput('market_headers'), width=12)),
                         fluidRow(
                           column(width=7, plotlyOutput('jobs_map', height='500px')),
                           column(width=5,
                                  plotlyOutput('education', height='250px'),
                                  plotlyOutput('top_hiring', height='250px')
                           )
                         )
                ),
                tabPanel('Dataset', column(dataTableOutput('datatable'), width=12)),
              )
    )
  )
)