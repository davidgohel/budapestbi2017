library(ggiraph)
library(treemap)
library(tidyverse)

data(GNI2014)
treemap_ <- treemap(dtf = GNI2014,
        index=c("continent", "iso3"),
        vSize="population",
        draw = FALSE)$tm

continent_tmap <- treemap_[treemap_$level == 1, ]

country_layer <- treemap_[treemap_$level == 2, ]


country_layer <- as_tibble(country_layer) %>%
  mutate(continent = as.character(continent), iso3 = as.character(iso3) ) %>%
  tidyr::replace_na(replace = list(iso3 = "missing"))
library(ggrepel)

gg_obj <- ggplot(country_layer, aes( xmin = x0 + w, xmax = x0, ymin = y0 + h, ymax = y0, fill = color, data_id = iso3 ) ) +
  geom_rect_interactive( colour = "white") +
  geom_rect( data = continent_tmap, color = "black", fill = "transparent", lwd = 1 ) +
  geom_label_repel( data = continent_tmap, aes( x0 + w/2, y0 + h/2, label = continent ), fill = "white"  ) +
  theme_void() + guides(fill=FALSE)


shinyServer(function(input, output, session) {

  selected_state <- reactive({
    input$plot_selected
  })

  output$plot <- renderggiraph({
    ggiraph(code = print(gg_obj), selection_type = "multiple", zoom_max = 4,
            hover_css = "fill:#FF3333;stroke:black;cursor:pointer;",
            selected_css = "fill:#FF3333;stroke:black;")
  })

  observeEvent(input$reset, {
    session$sendCustomMessage(type = 'plot_set', message = character(0))
  })

  output$datatab <- renderTable({
    out <- country_layer[country_layer$iso3 %in% selected_state(), ]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    out
  })

})
