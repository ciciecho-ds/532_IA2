library(dash)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(purrr)
library(plotly)
library(ggthemes)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

#data
data <- read_csv('data/processed_df.csv')

#options
opt_dropdown_neighbourhood <- unique(data$Neighborhood) %>%
    map(function(col) list(label = col, value = col))
opt_dropdown_neighbourhood <- opt_dropdown_neighbourhood[-c(20, 24, 25)]

opt_radio_year <- list(list(label = '2017', value = 2017),
                       list(label = '2018', value = 2018),
                       list(label = '2019', value = 2019),
                       list(label = '2020', value = 2020),
                       list(label = '2021', value = 2021))
# summary card
card <- dbcCard(
    list(
        htmlH4("Total Number of Crimes", className = "card-title", style = list("marginLeft" = 50)),
        htmlDiv(id = "summary", style = list("color" = "#E33B18", "fontSize" = 25, "marginLeft" = 140))
    ),
    style = list("width" = "25rem", "marginLeft" = 20),
    body = TRUE,
    color = "light"
)

# filters card
filter_card <- dbcCard(
    list(
        # Dropdown for neighbourhood
        htmlH5("Neighbourhood", className="text-dark"),
        dccDropdown(id = "neighbourhood_input",
                    options = opt_dropdown_neighbourhood,
                    value = 'Fairview',
                    className="dropdown"),
        htmlBr(),
        htmlBr(),
        htmlBr(),

        # Radio button for year
        htmlH5("Year", className="text-dark"),
        dccRadioItems(id = "year_radio",
                    options = opt_radio_year,
                    value = 2021,
                    className="radiobutton",
                    labelStyle = list("display" = "in-block", "marginLeft" = 20)),
        htmlBr(),
        htmlBr(),
        htmlBr()
    ),
    style = list("width" = "25rem", "marginLeft" = 20),
    body = TRUE,
    color = "light"
)

# filter layout
filter_panel = list(
    htmlH2("Vancouver Crime Dashboard", style = list("marginLeft" = 20)),
    htmlBr(),
    htmlBr(),
    card,
    htmlBr(),
    htmlBr(),
    htmlH4("Filters", style = list("marginLeft" = 20)),
    filter_card,
    htmlBr()
)

# plots layout
plot_body = list(
    dccGraph("bar_plot")
)

# Page layout
page_layout <- htmlDiv(
    className="page_layout",
    children=list(
        dbcRow(htmlBr()),
        dbcRow(
            list(dbcCol(filter_panel, className = "panel", width = 3),
                 dbcCol(plot_body, className = "body"))
        )
    )
)

# Overall layout
app$layout(htmlDiv(id="main", className="app", children=page_layout))

# functions
app$callback(
    output("summary", "children"),
    list(input("neighbourhood_input", "value"),
    input("year_radio", "value")),
    function(neighbourhood, year) {
        data_summary <- data %>%
            filter(Neighborhood == neighbourhood, YEAR == year)
        nrow(data_summary)
    }
)

app$callback(
    output("bar_plot", 'figure'),
    list(input('neighbourhood_input', 'value'),
         input("year_radio", "value")),
         function(neighbourhood, year) {

             data <- data %>%
               filter(YEAR == year & Neighborhood == neighbourhood) %>%
               group_by(Type) %>%
               tally() %>%
               rename(Count = n) %>%
               arrange(desc(Count)) %>%
               head(8)

             bar_plot <- ggplot(data) +
               aes(x = reorder(Type, -Count),
                   y = Count,
                   fill = Type) +
               geom_bar(stat = "identity") +
               scale_fill_brewer(palette = 'OrRd') +
               labs(x = "Type of crime",
                    y = "Number of Crimes") +
               theme_classic() +
               theme(
                 axis.title = element_text(size = 20),
                 axis.text.y = element_text(size = 16),
                 axis.text.x = element_blank(),
                 legend.title = element_text(size = 16),
                 legend.text = element_text(size = 12)
               )

             ggplotly(bar_plot, tooltip = c("y", "fill"),
                      height = 400,
                      width = 800)

           }
)

app$run_server(host = '0.0.0.0')
