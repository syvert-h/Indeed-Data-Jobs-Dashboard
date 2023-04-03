# Returns a H2-heading
get_header = function(text, region, country) {
  title = sprintf('%s in %s%s', text,
                  ifelse(region == 'All', '', paste0(region, ', ')), country)
  return(h2(title))
}