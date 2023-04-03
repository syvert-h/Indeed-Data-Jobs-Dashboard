# Return a histogram and boxplot of salary distributions
salary_plot = function(df) {
  req(input$go, input$salary_tab)
  salary_types = c("Yearly"="year","Monthly"="month","Weekly"="week","Daily"="day","Hourly"="hour")
  data = df %>%
    filter(!is.na(`salary`)) %>%
    filter(!is.na(`salary_type`)) %>%
    filter(`region` != '') %>%
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
  return(subplot(box, hist, nrows=2, shareX=T, heights = c(0.2, 0.8)))
}