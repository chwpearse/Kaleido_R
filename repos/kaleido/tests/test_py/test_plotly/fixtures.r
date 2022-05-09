# import plotly.graph_objects as go
# import plotly.express as px
# from pytest import fixture
# import pandas as pd

font = c("family" = "Monospace")

simple_figure <- function(){
  return(
    plot_ly(y = c(1, 3, 2),type = "scatter") %>% 
      layout(title = list(text = 'Title', font = font))
  )
}


gl_figure <- function(){
  return( 
    plot_ly(y = c(1, 3, 2), type = "scattergl") %>% 
      layout(title = list(text = "Title", font = font))
  )
}

mathjax_figure <- function(){
  return (
    plot_ly(y = c(1, 3, 2), type = "scatter") %>% 
      layout("title"= list("text"= "$\\pi^2$"), "font"= font)
  )
}

topojson_figure <- function(){
  df = gapminder::gapminder[gapminder::gapminder$year==2007,] #px.data.gapminder().query("year==2007")
  return (
    plot_ly(
      df,
      locations = "iso_alpha",
      color = ~lifeExp,
      z = ~country,
      type = "choropleth",
      # color_continuous_scale = px.colors.sequential.Plasma
      colorscale = 'Plasma'
    ) %>% 
      layout(font=font)
  )
}

mapbox_figure <- function(){
  # mapbox api not setup, skip for now
  return(plot_ly())
  us_cities = read.csv("https://raw.githubusercontent.com/plotly/datasets/master/us-cities-top-1k.csv")
  # import plotly.express as px
  
  fig <- plot_mapbox( #px.scatter_mapbox(
    us_cities, lat="lat", lon="lon", hover_name="City", hover_data = c("State", "Population"),
    color_discrete_sequence = "fuchsia", zoom=3, height=300
  )
  
  fig <- fig %>% layout(mapbox_style = "dark")
  fig <- fig %>% layout(margin = list("r" = 0, "t" = 0, "l" = 0, "b" = 0), font=font)
  return(fig)
}

all_figures <- function(){
  return(
    list(
      list(simple_figure(), 'simple'),
      list(gl_figure(), 'webgl'),
      list(mathjax_figure(), 'mathjax'),
      list(topojson_figure(), 'topojson'),
      # Comment until we get token worked out
      list(mapbox_figure(), 'mapbox')
    )
  )
}
all_formats = c('png', 'jpeg', 'webp', 'svg', 'pdf', 'eps')
