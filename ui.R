library(shinydashboard)
library(shiny)
library(plotly)
library(DT)

dashboardPage(
  dashboardHeader(
    title="Indeed Data Jobs"
  ),
  
  dashboardSidebar(
    collapsed =T,
    sidebarMenu(
      menuItem("Dashboard", tabName="dashboard", icon=icon("dashboard", verify_fa=F)),
      menuItem("Table", tabName="datatable", icon=icon("table", verify_fa=F)),
      menuItem("Source code", icon = icon("file-code-o", verify_fa=F), href = "https://github.com/syvert-h?tab=repositories")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName="dashboard",
        fluidRow(
          column(
            width=3,
            selectInput(
              "country_dash",
              label="Select a country:",
              choices=c("Australia", "New Zealand"),
              selected="Australia",
              width=NULL
            )
          ),
          column(
            width=3,
            uiOutput("region_dash")
          ),
          column(
            width=3,
            textInput(
              "kw_dash",
              label="Keyword Search Job Title:",
              value="",
              width=NULL
            )
          ),
          column(
            width=1, style="margin-top: 25px; margin-left: 0px",
            actionButton(
              "go_filter",
              label="Apply"
            )
          ),
          column(
            width=2,
            htmlOutput("kw_dash_output")
          )
        ),
        fluidRow(
          column(width=6, h2("Skills Section")),
          column(width=6, h2("Job Type Section"))
        ),
        fluidRow(
          tabBox(
            id="skills_tab",
            title="In-demand Skills",
            width=6,
            tabPanel(
              "Data Tools",
              plotlyOutput("data_tools", height="360px")
            ),
            tabPanel(
              "Hard Skills",
              plotlyOutput("hard_skills", height="360px")
            ),
            tabPanel(
              "Soft Skills",
              plotlyOutput("soft_skills", height="360px")
            )
          ),
          box(
            width=6,
            plotlyOutput("jobtype_plot", height="260px"),
            plotlyOutput("remote_num_plot", height="140px")
          )
        ),
        fluidRow(
          column(width=12, h2("Salary Section"))
        ),
        fluidRow(
          tabBox(
            id="salary_tab",
            title="",
            width=8,
            tabPanel(
              "Yearly",
              plotlyOutput("salary_year")
            ),
            tabPanel(
              "Monthly",
              plotlyOutput("salary_month")
            ),
            tabPanel(
              "Weekly",
              plotlyOutput("salary_weekly")
            ),
            tabPanel(
              "Daily",
              plotlyOutput("salary_daily")
            ),
            tabPanel(
              "Hourly",
              plotlyOutput("salary_hourly")
            )
          ),
          column(
            width=4,
            DT::dataTableOutput("salary_table")
          )
        ),
        fluidRow(
          column(width=12, h2("Job Market Section"))
        ),
        fluidRow(
          column(
            width=8,
            box(
              width=NULL,
              plotlyOutput("jobs_map", height="700px")
            )
          ),
          column(
            width=4,
            fluidRow(
              box(
                width=NULL,
                plotlyOutput("education_pie", height="329px")
              )
            ),
            fluidRow(
              box(
                width=NULL,
                plotlyOutput("top_hiring_plot", height="329px")
              )
            )
          )
        )
      ),
      tabItem(tabName="datatable",
        fluidRow(
          column(
            width=3,
            selectInput(
              "country_table",
              label="Select a country:",
              choices=c("Australia", "New Zealand"),
              width=NULL,
              selected="Australia"
            )
          )
        ),
        fluidRow(
          column(
            width=12,
            DT::dataTableOutput("jobs_datatable")
          )
        )
      )
    )
  )
)