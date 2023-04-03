# Return a horizontal bar chart from a column of comma separated strings (using plotly)

get_string_barplot = function(df, colname, xlabel='% of Jobs', ylabel='Skill', n=10) {
  # Split the text column into words, and count the frequency of each word
  data = df %>%
    separate_rows(!!sym(colname), sep = ",\\s*") %>%
    count(!!sym(colname), sort=T) %>%
    mutate(`percent` = round(n/nrow(df)*100, 2)) %>%
    filter(!!sym(colname) != "") %>% head(n)
  fig = plot_ly(data, x = ~`percent`, y = ~get(colname), type='bar', orientation='h') %>%
    layout(
      yaxis = list(
        categoryorder = 'total ascending',
        title = list(text = ylabel)
      ),
      xaxis = list(title = list(text = xlabel))
    )
  return(fig)
}