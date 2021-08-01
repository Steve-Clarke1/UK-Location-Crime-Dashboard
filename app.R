# Set Up -----
## Libraries -----
#packages <- c("shiny", "dplyr", "jsonlite", "plotly", "curl", "geodist", "shinyWidgets", "bs4Dash", "fresh", "shinycssloaders", "DT", "formattable", "leaflet")
#lapply(packages, library, character.only = TRUE)

library(shiny)
library(leaflet)
library(dplyr) 
library(jsonlite) 
library(plotly) 
library(curl) 
library(geodist) 
library(shinyWidgets) 
library(bs4Dash) 
library(fresh) 
library(shinycssloaders) 
library(DT) 
library(formattable)

## Global Options -----

options(spinner.color="#0dc5c1", spinner.type = 1)

theme <- create_theme(
    bs4dash_vars(
        navbar_light_bg = "#EBF5FB",
        main_footer_bg = "#EBF5FB",
        card_bg = "#EBF5FB"),
    # bs4dash_yiq(
    #     contrasted_threshold = 10,
    #     text_dark = "#FFF",
    #     text_light = "#272c30"
    # ),
    bs4dash_layout(
        main_bg = "#CCCCFF"
    ),
    bs4dash_sidebar_light(
        bg = "#EBF5FB",
        color = "#808B96",
        hover_color = "#FFF"
    ),
    bs4dash_status(
        primary = "#6495ED"#, danger = "#BF616A", light = "#272c30"
    ),
    # bs4dash_color(
    #     gray_900 = "#FFF", white = "#272c30"
    # ),
    bs4dash_layout(sidebar_width = "285px"),
    bs4dash_font(family_sans_serif = "Arial")#'Saira Stencil One', cursive
)

## User Function ----
# crime_per_month_value_card <- function(summary_tbl, crime_type, subtitle, colour, icon) {
#     renderValueBox({
#         crimeNum <- summary_tbl %>% 
#         ungroup() %>% 
#         filter(Crime_Type == crime_type) %>% 
#         pull("AvgNumPerMonth")
#     
#     valueBox(
#         value = h1(sprintf("%.1f", crimeNum)),
#         subtitle = subtitle,
#         color = colour,
#         icon = icon(icon)
#     )
#     })
# }

# Initial Data -----
dates <- fromJSON(rawToChar(curl_fetch_memory(url = "https://data.police.uk/api/crimes-street-dates")$content)) %>%
    select(date) %>%
    arrange() %>%
    head(24) %>%
    mutate(MonthsAgoFraction = (nrow(.)-1):0/(nrow(.)-1), YearGroup = c(rep(1, 12), rep(2, 12)))

crime_types <- fromJSON(rawToChar(curl_fetch_memory(url = paste0("https://data.police.uk/api/crime-categories?date=",dates$date[1]))$content)) %>%
    rename(Crime_Code = url, Crime_Type = name) %>%
    filter(Crime_Type != "All crime")

max_length_crime_type <- nchar(crime_types$Crime_Type)

# Application -----
shinyApp(
    ## UI -----
    ui = dashboardPage(
        freshTheme = theme,
        title = "Basic Dashboard",
        fullscreen = TRUE,
        ### Header -----
        header = dashboardHeader(
            title = dashboardBrand(
                title = "UK Crime Explorer",
                color = "primary",
                href = NULL,
                image = "PoliceHat-logo.png",
                
            ),
            skin = "light",
            status = "white",
            titleWidth = "500px",
            border = TRUE,
            sidebarIcon = icon("bars"),
            controlbarIcon = icon("th"),
            fixed = FALSE
        ),
        ### Sidebar -----
        sidebar = dashboardSidebar(
            skin = "light",
            status = "primary",
            elevation = 3,
            width = "500px",
            sidebarMenu(
                id = "tabs",
                h3("Navigation"),
                menuItem(
                    "Home",
                    tabName = "home",
                    icon = icon("home")
                ),
                menuItem(
                    "Crime Explorer",
                    tabName = "CrimeExplorer",
                    icon = icon("chart-line")
                ),
                menuItem(
                    "Location Comparer",
                    tabName = "LocationComparer",
                    icon = icon("map-marked-alt")
                ),
                menuItem(
                    "About",
                    tabName = "About",
                    icon = icon("id-card")
                ),
                br(),
                h3("Options"),
                selectInput(
                               inputId = "date_range_options", 
                               label = "Date Range",
                               choices = c("6 months", "1 Year", "2 Years"), #, "2 Years"
                               selected = c("2 Years")),
                selectInput(
                               inputId = "distance", 
                               label = "Distance Included", 
                               choices = c("1 Mile", "1/2 Mile", "1/4 Mile"), 
                               selected = c("1 Mile")),
                selectInput(
                               inputId = "map_style", 
                               label = "Map Style", 
                               choices = c("Simple", "Colour", "Terrain", "Watercolour"), 
                               selected = c("Colour")),
                pickerInput(
                               inputId = "crime_cat",
                               label = "Crime Categories", 
                               choices = crime_types$Crime_Type,
                               selected = crime_types$Crime_Type[!(crime_types$Crime_Type %in% c("Anti-social behaviour", "Other crime", "Other theft"))],
                               multiple = TRUE)
                )
            
        ),
        ### Footer -----
        footer = dashboardFooter(
            left = HTML(paste(sep = " &#160 &#160 ", strong("Created by Dr Steve Clarke"), '<a href="https://www.linkedin.com/in/steve-clarke2013"><img src="LI-Logo.png" title="LinkedIn Profile Link" width="81.92592592" height="20" /></a>', 
                              '<a href="https://github.com/Steve-Clarke1"><img src="Github-Logo.png" title="Github Profile Link" width="78.1512605" height="20" /></a>')),
            right = strong("Created 2021")
        ),
        ### Body -----
        body = dashboardBody(
            tabItems(
                tabItem(
                    #### Home -----
                    tabName = "home",
                    fluidRow(
                        column(width = 6, 
                               jumbotron(
                                   title = "Welcome to the UK Crime Explorer",
                                   lead = HTML("Local Crime Maps, Crime Statistics and Comparision Tool."),
                                   status = "primary",
                                   href = NULL,
                                   btnName = NULL),
                               jumbotron(
                                   title = "Location Comparer",
                                   lead = "Compare crime statistics between two seperate locations in the UK.",
                                   actionBttn(inputId = "JumboComparer", label = "Link", style = "bordered",
                                              color = "default",
                                              size = "lg",
                                              block = TRUE),
                                   status = "info",
                                   href = NULL,
                                   btnName = NULL)),
                        column(width = 6,
                               jumbotron(
                                   title = "Crime Explorer",
                                   lead = "Maps, Statistics and Graphs",
                                   actionBttn(inputId = "JumboExplorer", label = "Link", style = "bordered",color = "default",
                                              size = "lg",
                                              block = TRUE),
                                   status = "success",
                                   href = NULL,
                                   btnName = NULL),
                               jumbotron(
                                   title = "About",
                                   lead = "Learn more about the website, how it was created and the creator.",
                                   actionBttn(inputId = "JumboAbout", label = "Link", style = "bordered", color = "default",
                                              size = "lg",
                                              block = TRUE),
                                   status = "danger",
                                   href = NULL,
                                   btnName = NULL)))
                ),
                tabItem(
                    #### Crime Explorer -----
                    tabName = "CrimeExplorer",
                fluidRow(
                    box(
                    title = "Location Picker and Map",
                    width = 6,
                    status = "olive",
                    solidHeader = TRUE,
                    splitLayout(cellWidths = c("70%", "30%"),
                                textInput(inputId = "Address_main", label = NULL, value = "Truro, UK", width = "100%"),
                                actionButton(inputId = "Update_main", label = "Update")),
                    leafletOutput("crime_locs_rec_main_map", height = "470px") %>% withSpinner() #"446px"
                ),
                box(title = "Crime Summary", width = 6,
                    status = "olive",
                    solidHeader = TRUE,
                    formattableOutput("formatabletable")),
                box(title = "Year on Year Change in Number of Crimes", width = 6,
                    status = "olive",
                    solidHeader = TRUE,
                    fluidRow(
                        valueBoxOutput("violent_value_change", width = 4),
                        valueBoxOutput("burglary_value_change", width = 4),
                        valueBoxOutput("robbery_value_change", width = 4),
                        valueBoxOutput("drugs_value_change", width = 4),
                        valueBoxOutput("vehicle_value_change", width = 4),
                        valueBoxOutput("theftperson_value_change", width = 4)
                        # valueBoxOutput("weapons_value_change", width = 4),
                        # valueBoxOutput("Anti_social_value_change", width = 4),
                        # valueBoxOutput("Bicycle_theft_value_change", width = 4),
                        # valueBoxOutput("Other_theft_value_change", width = 4),
                        # valueBoxOutput("Public_order_value_change", width = 4),
                        # valueBoxOutput("Other_crime_value_change", width = 4),
                        # valueBoxOutput("Criminal_damage_arson_value_change", width = 4),
                        # valueBoxOutput("Shoplifting_value_change", width = 4)
                    )
                ),
                    box(title = "Crimes per Month", width = 6,
                        status = "olive",
                        solidHeader = TRUE,
                        fluidRow(valueBoxOutput("violent_value", width = 4),
                                 valueBoxOutput("burglary_value", width = 4),
                                 valueBoxOutput("robbery_value", width = 4),
                                 valueBoxOutput("drugs_value", width = 4),
                                 valueBoxOutput("vehicle_value", width = 4),
                                 valueBoxOutput("theftperson_value", width = 4)
                         #        valueBoxOutput("weapons_value", width = 4)
                        )
                    )),
                box(title = "Crime Timeline", width = 12,
                    status = "olive",
                    solidHeader = TRUE,
                    plotlyOutput("time", height = "500px") %>% withSpinner()
                ),
                box(title = "Crime Data", width = 12,
                    status = "olive",
                    solidHeader = TRUE,
                    DTOutput("table"))),
                tabItem(
                    #### Location Comparer -----
                    tabName = "LocationComparer",
                    fluidRow(
                        box(
                            title = strong("First Address and Map"),
                            width = 6,
                            status = "olive",
                            splitLayout(cellWidths = c("70%", "30%"),
                                        textInput(inputId = "Address1", label = NULL, value = "Truro, UK", width = "100%"),
                                        actionButton(inputId = "Update_Location1", label = "Update")),
                            leafletOutput("crime_locs_rec1_map") %>% withSpinner()
                        ),
                        box(
                            title = strong("Second Address and Map"),
                            width = 6,
                            status = "lightblue",
                            splitLayout(cellWidths = c("70%", "30%"),
                                        textInput(inputId = "Address2", label = NULL, value = "", width = "100%"),
                                        actionButton(inputId = "Update_Location2", label = "Update")),
                            leafletOutput("crime_locs_rec2_map") %>% withSpinner()
                        ),
                        box(title = strong("Comparing average crimes committed per month"),
                            width = 6,
                            collapsed = TRUE,
                            plotlyOutput("comp_bar", height = "800px")),
                        box(title = strong("More Stuff"),
                            collapsed = TRUE,
                            width = 6,
                            "Test")
                    )
                    
                ),
                tabItem(
                    #### About -----
                    tabName = "About",
                    fluidRow(userBox(
                        title = userDescription(
                            title = "DR Steve Clarke",
                            subtitle = "Data Scientist",
                            type = 2,
                            image = "me-Avatar.jpg",
                        ),
                        status = "warning",
                        HTML("I have a keen interest in coding and using data and IT to solve problems. <br>
                        Currently working at Highways England producing data standards (and associated contract documents), data/analytical pipelines, commercial analytics and cost modelling (including using machine learning techniques). <br>
                        I have a PhD obtained from the University of Nottingham on Railway Track Asset Management Modelling, for which I built a stocastic whole life cost model for a railway track <br>
                        <br>
                        This dashboard was developed to help improve and demonstate my skills. I hope it is useful for people."),
                        footer = p(HTML('<a href="https://www.linkedin.com/in/steve-clarke2013"><img src="LI-Logo.png" title="LinkedIn Profile Link" width="102.4074074" height="25" /></a>'), 
                                   HTML('<a href="https://github.com/Steve-Clarke1"><img src="Github-Logo.png" title="Github Profile Link" width="97.68907563" height="25" /></a>')),
                        collapsible = TRUE
                    ),
                    box(
                        title = strong("Guide"),
                        HTML("Left Toolbar includes navigation between the page as well as data and display options. <br>
                        Date Range: This sets how far back crime data should be included from. <br>
                        Distance Inclded: Only crimes which occured with the chosen distance from the location entered will be included. <br>
                        Map Style: Changes the style of the maps displayed on the dashboard.")
                    ),
                    box(
                        title = strong("Data"),
                        strong("Crime Data"),
                        br(),
                        HTML("Street-level crimes within 1 mile of a location (coordinates) <br>
                             Source: UK Police, <a href='https://data.police.uk/'>https://data.police.uk/</a> <br>
                        For more information about the data used please vist <a href='https://data.police.uk/about/'>https://data.police.uk/about/</a> <br>
                        Data used obtained using API (HTTP GET requests), more details at <a href='https://data.police.uk/docs/'>https://data.police.uk/docs/</a>
                        "),
                        br(),
                        br(),
                        strong("Location Data"),
                        br(),
                        HTML("As UK Crime API needed locations in Latitude and Longitude, it was required to convert addresses into coordinates. <br>
                             Source: Google Geocoding, <a href='https://developers.google.com/maps/documentation/geocoding/overview'>https://developers.google.com/maps/documentation/geocoding/overview</a> <br>
                        Data used obtained using API (HTTP GET requests)
                        ")
                    )
                    ))
            )
        )
    ),
    
    ## Server -----
    server = function(input, output, session) {
        
        ### Jumbotron Links -----
        observeEvent(input$JumboComparer, {
            updateTabItems(session, "tabs", "LocationComparer")
        })
        
        observeEvent(input$JumboAbout, {
            updateTabItems(session, "tabs", "About")
        })   
        
        observeEvent(input$JumboExplorer, {
            updateTabItems(session, "tabs", "CrimeExplorer")
        }) 
        
        ### Value Cards -----        
        output$violent_value <- renderValueBox({
            if ("Violence and sexual offences" %in% input$crime_cat) {
            crimeNum <- crime_locs_rec_main_summary_f1() %>% ungroup() %>% filter(Crime_Type == "Violence and sexual offences") %>% pull("AvgNumPerMonth")
            valueBox(
                value = h2(sprintf("%.1f", crimeNum)),
                subtitle = "Violent and Sexual",
                color = "danger",
                icon = icon("user-injured")
            )
            }
        })

        output$burglary_value <- renderValueBox({
            crimeNum <- crime_locs_rec_main_summary_f1() %>% ungroup() %>% filter(Crime_Type == "Burglary") %>% pull("AvgNumPerMonth")
            valueBox(
                value = h2(sprintf("%.1f", crimeNum)),
                subtitle = "Burglary",
                color = "orange",
                icon = icon("home")
            )
        })

        output$drugs_value <- renderValueBox({
            crimeNum <- crime_locs_rec_main_summary_f1() %>% ungroup() %>% filter(Crime_Type == "Drugs") %>% pull("AvgNumPerMonth")
            valueBox(
                value = h2(sprintf("%.1f", crimeNum)),
                subtitle = "Drugs",
                color = "secondary",
                icon = icon("syringe")
            )
        })

        output$vehicle_value <- renderValueBox({
            crimeNum <- crime_locs_rec_main_summary_f1() %>% ungroup() %>% filter(Crime_Type == "Vehicle crime") %>% pull("AvgNumPerMonth")
            valueBox(
                value = h2(sprintf("%.1f", crimeNum)),
                subtitle = "Vehicle crime",
                color = "warning",
                icon = icon("car-side")
            )
        })

        output$theftperson_value <- renderValueBox({
            crimeNum <- crime_locs_rec_main_summary_f1() %>% ungroup() %>% filter(Crime_Type == "Theft from the person") %>% pull("AvgNumPerMonth")
            valueBox(
                value = h2(sprintf("%.1f", crimeNum)),
                subtitle = "Theft from the person",
                color = "orange",
                icon = icon("people-arrows")
            )
        })

        output$robbery_value <- renderValueBox({
            crimeNum <- crime_locs_rec_main_summary_f1() %>% ungroup() %>% filter(Crime_Type == "Robbery") %>% pull("AvgNumPerMonth")
            valueBox(
                value = h2(sprintf("%.1f", crimeNum)),
                subtitle = "Robbery",
                color = "secondary",
                icon = icon("user-injured")
            )
        })

        # output$weapons_value <- renderValueBox({
        #     crimeNum <- crime_locs_rec_main_summary_f1() %>% ungroup() %>% filter(Crime_Type == "Possession of weapons") %>% pull("AvgNumPerMonth")
        #     valueBox(
        #         value = h2(sprintf("%.1f", crimeNum)),
        #         subtitle = "Possession of weapons",
        #         color = "warning",
        #         icon = icon("user-injured")
        #     )
        # })

        output$violent_value_change <- renderValueBox({
            crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Violence and sexual offences") %>% pull("YearonYearChange")
            valueBox(
                value = h2(sprintf("%+.1f%%", crimeNum*100)),
                subtitle = "Violent and Sexual",
                color = ifelse(crimeNum>0, "danger", "success"),
                icon = icon("user-injured")
            )
        })

        output$burglary_value_change <- renderValueBox({
            crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Burglary") %>% pull("YearonYearChange")
            valueBox(
                value = h2(sprintf("%+.1f%%", crimeNum*100)),
                subtitle = "Burglary",
                color = ifelse(crimeNum>0, "danger", "success"),
                icon = icon("home")
            )
        })
        
        output$drugs_value_change <- renderValueBox({
            crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Drugs") %>% pull("YearonYearChange")
            valueBox(
                value = h2(sprintf("%+.1f%%", crimeNum*100)),
                subtitle = "Drugs",
                color = ifelse(crimeNum>0, "danger", "success"),
                icon = icon("syringe")
            )
        })

        output$vehicle_value_change <- renderValueBox({
            crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Vehicle crime") %>% pull("YearonYearChange")
            valueBox(
                value = h2(sprintf("%+.1f%%", crimeNum*100)),
                subtitle = "Vehicle Crime",
                color = ifelse(crimeNum>0, "danger", "success"),
                icon = icon("car-side")
            )
        })
        
        output$theftperson_value_change <- renderValueBox({
            crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Theft from the person") %>% pull("YearonYearChange")
            valueBox(
                value = h2(sprintf("%+.1f%%", crimeNum*100)),
                subtitle = "Theft from the person",
                color = ifelse(crimeNum>0, "danger", "success"),
                icon = icon("people-arrows")
            )
        })  
        
        output$robbery_value_change <- renderValueBox({
            crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Robbery") %>% pull("YearonYearChange")
            valueBox(
                value = h2(sprintf("%+.1f%%", crimeNum*100)),
                subtitle = "Robbery",
                color = ifelse(crimeNum>0, "danger", "success"),
                icon = icon("user-injured")
            )
        })
        
        # output$weapons_value_change <- renderValueBox({
        #     crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Possession of weapons") %>% pull("YearonYearChange")
        #     valueBox(
        #         value = h2(sprintf("%+.1f%%", crimeNum*100)),
        #         subtitle = "Weapons",
        #         color = ifelse(crimeNum>0, "danger", "success"),
        #         icon = icon("user-injured")
        #     )
        # })
        # 
        # output$Anti_social_value_change <- renderValueBox({
        #     crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Anti-social behaviour") %>% pull("YearonYearChange")
        #     valueBox(
        #         value = h2(sprintf("%+.1f%%", crimeNum*100)),
        #         subtitle = "Anti-social behaviour",
        #         color = ifelse(crimeNum>0, "danger", "success"),
        #         icon = icon("user-injured")
        #     )
        # })
        
        # output$Bicycle_theft_value_change <- renderValueBox({
        #     crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Bicycle theft") %>% pull("YearonYearChange")
        #     valueBox(
        #         value = h2(sprintf("%+.1f%%", crimeNum*100)),
        #         subtitle = "Bicycle theft",
        #         color = ifelse(crimeNum>0, "danger", "success"),
        #         icon = icon("user-injured")
        #     )
        # })
        
        # output$Other_theft_value_change <- renderValueBox({
        #     crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Other theft") %>% pull("YearonYearChange")
        #     valueBox(
        #         value = h2(sprintf("%+.1f%%", crimeNum*100)),
        #         subtitle = "Other theft",
        #         color = ifelse(crimeNum>0, "danger", "success"),
        #         icon = icon("user-injured")
        #     )
        # })
        
        # output$Public_order_value_change <- renderValueBox({
        #     crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Public order") %>% pull("YearonYearChange")
        #     valueBox(
        #         value = h2(sprintf("%+.1f%%", crimeNum*100)),
        #         subtitle = "Public order",
        #         color = ifelse(crimeNum>0, "danger", "success"),
        #         icon = icon("user-injured")
        #     )
        # })
        # 
        # output$Other_crime_value_change <- renderValueBox({
        #     crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Other crime") %>% pull("YearonYearChange")
        #     valueBox(
        #         value = h2(sprintf("%+.1f%%", crimeNum*100)),
        #         subtitle = "Other crime",
        #         color = ifelse(crimeNum>0, "danger", "success"),
        #         icon = icon("user-injured")
        #     )
        # })

        # output$Criminal_damage_arson_value_change <- renderValueBox({
        #     crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Criminal damage and arson") %>% pull("YearonYearChange")
        #     valueBox(
        #         value = h2(sprintf("%+.1f%%", crimeNum*100)),
        #         subtitle = "Criminal damage and arson",
        #         color = ifelse(crimeNum>0, "danger", "success"),
        #         icon = icon("user-injured")
        #     )
        # })

        # output$Shoplifting_value_change <- renderValueBox({
        #     crimeNum <- crime_locs_rec_main_year_on_year_f1() %>% ungroup() %>% filter(Crime_Type == "Shoplifting") %>% pull("YearonYearChange")
        #     valueBox(
        #         value = h2(sprintf("%+.1f%%", crimeNum*100)),
        #         subtitle = "Shoplifting",
        #         color = ifelse(crimeNum>0, "danger", "success"),
        #         icon = icon("user-injured")
        #     )
        # })
        
        ### Inputs -----        
        incl_dates <- reactive({
            case_when(input$date_range_options == "6 months" ~ 6,
                      input$date_range_options == "1 Year" ~ 12, 
                      input$date_range_options == "2 Years" ~ 24)   
        })
        
        current_distance <- reactive({
            case_when(input$distance == "1 Mile" ~ 1,
                      input$distance == "1/2 Mile" ~ 0.5,
                      input$distance == "1/4 Mile" ~ 0.25)
        })
        
        current_style <- reactive({
            case_when(input$map_style == "Watercolour" ~ "Stamen.Watercolor", 
                      input$map_style == "Colour" ~ "OpenStreetMap.Mapnik", 
                      input$map_style == "Terrain" ~ "Stamen.Terrain", 
                      input$map_style == "Simple" ~ "Stamen.TonerLite")
        })
        
        observe({updateTextInput(session = session, inputId = "Address_main",
                        label = NULL,
                        value = address_main_location()$results$formatted_address)
        })
        
        observe({updateTextInput(session = session, inputId = "Address1",
                                  label = NULL,
                                  value = address_main_location()$results$formatted_address)
        })
        
        address_main_location_page <- eventReactive(input$Update_main | input$Update_Location1, {
            if (input$Update_Location1==0 & input$Update_main==0) {
                c("Truro, UK")
                } else {
            if (input$Update_Location1==1) {
                input$Address1
            } else {
                input$Address_main
            }
                }
        }, ignoreNULL = FALSE
        )
        
        ### APIs -----
        address_main_location <- reactive({
            fromJSON(rawToChar(curl_fetch_memory(url = paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",gsub(" ", "+", address_main_location_page()),"&key=AIzaSyCN3JprHGPe52rq2blCc2uuIbVf8psoAjo"))$content))
        })
        
        address2_location <- eventReactive(input$Update_Location2, {
            fromJSON(rawToChar(curl_fetch_memory(url = paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",gsub(" ", "+", input$Address2),"&key=AIzaSyCN3JprHGPe52rq2blCc2uuIbVf8psoAjo"))$content))
        }#, ignoreNULL = FALSE
        )

        crime_locs_rec_main <- reactive(
            {
                data <- list()
                pool_main <- new_pool(multiplex = TRUE, total_con = 30)
                success_main <- function(res){data <<- c(data, list(fromJSON(rawToChar(res$content))))}
                    for (lp_Date in 1:length(dates$date)) {
                                     curl_fetch_multi(paste0("https://data.police.uk/api/crimes-street/all-crime?lat=",address_main_location()$results$geometry$location$lat,"&lng=",address_main_location()$results$geometry$location$lng,"&date=",dates$date[lp_Date]), 
                                                      pool = pool_main, 
                                     done = success_main)
                    }
                
                multi_run(pool = pool_main)
                
                data <- data %>% 
                    bind_rows() %>% 
                    jsonlite::flatten() %>% 
                    left_join(dates, by = c("month" = "date")) %>% 
                    left_join(crime_types, by = c("category" = "Crime_Code")) %>%
                    rename(lat = location.latitude, lon = location.longitude) %>%
                    mutate(across(c("lon", "lat"), as.numeric)) %>%
                    mutate(distance = geodist(.[,c("lat","lon")], 
                                              data.frame(lat = address_main_location()$results$geometry$location$lat,
                                                         lon = address_main_location()$results$geometry$location$lng))/1000*0.621371) %>% 
                    mutate(colours = case_when(category == "anti-social-behaviour" ~ "#263238",
                                               category == "burglary" ~ "#C2185B",
                                               category == "criminal-damage-arson" ~ "#FF7043",
                                               category == "drugs" ~ "#9FA8DA",
                                               category == "other-theft" ~ "#E6EE9C",
                                               category == "possession-of-weapons" ~ "#673AB7",
                                               category == "public-order" ~ "#607D8B",
                                               category == "robbery" ~ "#F57F17",
                                               category == "shoplifting" ~ "#FFEB3B",
                                               category == "theft-from-the-person" ~ "#E57373",
                                               category == "vehicle-crime" ~ "#4A148C",
                                               category == "violent-crime" ~ "#B71C1C",
                                               category == "other-crime" ~ "#CFD8DC",
                                               category == "bicycle-theft" ~ "#81C784"))
            })
        
        crime_locs_rec2 <- reactive(
            {
                data2 <- list()
                pool2 <- new_pool(multiplex = TRUE, total_con = 20)
                success2 <- function(res){data2 <<- c(data2, list(fromJSON(rawToChar(res$content))))}
                for (lp_Date in 1:length(dates$date)) {
                    curl_fetch_multi(paste0("https://data.police.uk/api/crimes-street/all-crime?lat=",address2_location()$results$geometry$location$lat,"&lng=",address2_location()$results$geometry$location$lng,"&date=",dates$date[lp_Date]), 
                                     pool = pool2, 
                                     done = success2)
                }
                
                multi_run(pool = pool2)
                
                data2 <- data2 %>% 
                    bind_rows() %>% 
                    flatten() %>% 
                    left_join(dates, by = c("month" = "date")) %>% 
                    left_join(crime_types, by = c("category" = "Crime_Code")) %>%
                    rename(lat = location.latitude, lon = location.longitude) %>%
                    mutate(across(c("lon", "lat"), as.numeric)) %>%
                    mutate(distance = geodist(.[,c("lat","lon")], data.frame(lat = address2_location()$results$geometry$location$lat,lon = address2_location()$results$geometry$location$lng))/1000*0.621371) %>% 
                    mutate(colours = case_when(category == "anti-social-behaviour" ~ "#263238",
                                               category == "burglary" ~ "#C2185B",
                                               category == "criminal-damage-arson" ~ "#FF7043",
                                               category == "drugs" ~ "#9FA8DA",
                                               category == "other-theft" ~ "#E6EE9C",
                                               category == "possession-of-weapons" ~ "#673AB7",
                                               category == "public-order" ~ "#607D8B",
                                               category == "robbery" ~ "#F57F17",
                                               category == "shoplifting" ~ "#FFEB3B",
                                               category == "theft-from-the-person" ~ "#E57373",
                                               category == "vehicle-crime" ~ "#4A148C",
                                               category == "violent-crime" ~ "#B71C1C",
                                               category == "other-crime" ~ "#CFD8DC",
                                               category == "bicycle-theft" ~ "#81C784"))
            })
        
        ### Data Manipulation and Analysis -----        
        crime_locs_rec_main_f1 <- reactive({
            crime_locs_rec_main() %>% filter(month %in% dates$date[1:incl_dates()] &
                                             distance < current_distance())
            
        })

        crime_locs_rec_main_f2 <- reactive({
            crime_locs_rec_main_f1() %>% filter(Crime_Type %in% input$crime_cat)
        })
        
        crime_locs_rec_main_summary_f1 <- reactive({
            crime_locs_rec_main_f1() %>% 
                group_by(Crime_Type) %>% 
                summarise(NumOverPeriod = n(),
                          AvgNumPerMonth = NumOverPeriod/incl_dates(),
                          PcntofCrimes = NumOverPeriod/nrow(.))
        })

        crime_locs_rec_main_summary_f2 <- reactive({
            crime_locs_rec_main_f2() %>% 
                group_by(Crime_Type) %>% 
                summarise(NumOverPeriod = n(),
                          AvgNumPerMonth = NumOverPeriod/incl_dates(),
                          PcntofCrimes = NumOverPeriod/nrow(.))
        })
        
        crime_locs_rec_main_time_summary_f2 <- reactive({
            crime_locs_rec_main_f2() %>% 
                group_by(Crime_Type, month) %>% 
                summarise(Num = n())
        })
        
        crime_locs_rec_main_year_on_year_f1 <- reactive({
            crime_locs_rec_main() %>% filter(distance < current_distance()) %>%
                group_by(Crime_Type, YearGroup) %>% 
                summarise(Num = n()) %>%
                summarise(YearonYearChange = sum(ifelse(YearGroup == 1, Num, 0))/sum(ifelse(YearGroup == 2, Num, 0))-1)
        })

        crime_locs_rec_main_year_on_year_f2 <- reactive({
            crime_locs_rec_main() %>% filter(distance < current_distance() & Crime_Type %in% input$crime_cat) %>%
                group_by(Crime_Type, YearGroup) %>% 
                summarise(Num = n()) %>%
                summarise(YearonYearChange = sum(ifelse(YearGroup == 1, Num, 0))/sum(ifelse(YearGroup == 2, Num, 0))-1)
        })
        
        crime_locs_rec2_f1 <- reactive({
            crime_locs_rec2() %>% filter(month %in% dates$date[1:incl_dates()],
                                         distance < current_distance())
            
        })

        crime_locs_rec2_f2 <- reactive({
            crime_locs_rec2() %>% filter(Crime_Type %in% input$crime_cat)
        })


        crime_locs_rec2_summary_f2 <- reactive({
            crime_locs_rec2_f2() %>% 
                group_by(Crime_Type) %>% 
                summarise(NumOverPeriod = n(),
                          AvgNumPerMonth = NumOverPeriod/incl_dates(),
                          PcntofCrimes = NumOverPeriod/nrow(.))
        })
        
        crime_locs_summary_comp <- reactive({
            
            left_join(crime_locs_rec_main_summary_f2(), crime_locs_rec2_summary_f2(), by = "Crime_Type", suffix = c("_1", "_2"))
        })
        
        ### Graphs -----   
        output$crime_locs_rec_main_map <- renderLeaflet({
            leaflet() %>%
                addProviderTiles(providers[[current_style()]],
                                 options = providerTileOptions(noWrap = TRUE)
                ) %>%
                addMarkers(lat = address_main_location()$results$geometry$location$lat, lng = address_main_location()$results$geometry$location$lng) %>%
                addCircles(lat = address_main_location()$results$geometry$location$lat, lng = address_main_location()$results$geometry$location$lng, radius = current_distance()*1609.34, stroke = FALSE, fillOpacity = 0.15) %>%
                addCircleMarkers(lat = as.numeric(crime_locs_rec_main_f2()$lat),
                                 lng = as.numeric(crime_locs_rec_main_f2()$lon),
                                 clusterOptions = markerClusterOptions(),
                                 color = crime_locs_rec_main_f2()$colours,
                                 label = paste0(crime_locs_rec_main_f2()$month,": ",crime_locs_rec_main_f2()$category, " ", crime_locs_rec_main_f2()$location.street.name),
                                 popup = paste0("<i>",crime_locs_rec_main_f2()$month,"</i>","<br/>",
                                                "<b>", crime_locs_rec_main_f2()$category, "</b>", "<br/>",
                                                crime_locs_rec_main_f2()$location.street.name, "<br/>", 
                                                crime_locs_rec_main_f2()$outcome_status.category),
                                 fillOpacity = crime_locs_rec_main_f2()$MonthsAgoFraction)
        })

        output$crime_locs_rec1_map <- renderLeaflet({
            leaflet() %>%
                addProviderTiles(providers[[current_style()]],
                                 options = providerTileOptions(noWrap = TRUE)
                ) %>%
                addMarkers(lat = address_main_location()$results$geometry$location$lat, lng = address_main_location()$results$geometry$location$lng) %>%
                addCircles(lat = address_main_location()$results$geometry$location$lat, lng = address_main_location()$results$geometry$location$lng, radius = current_distance()*1609.34, stroke = FALSE, fillOpacity = 0.15) %>%
                addCircleMarkers(lat = as.numeric(crime_locs_rec_main_f2()$lat),
                                 lng = as.numeric(crime_locs_rec_main_f2()$lon),
                                 clusterOptions = markerClusterOptions(),
                                 color = crime_locs_rec_main_f2()$colours,
                                 label = paste0(crime_locs_rec_main_f2()$month,": ",crime_locs_rec_main_f2()$category, " ", crime_locs_rec_main_f2()$location.street.name),
                                 popup = paste0("<i>",crime_locs_rec_main_f2()$month,"</i>","<br/>",
                                                "<b>", crime_locs_rec_main_f2()$category, "</b>", "<br/>",
                                                crime_locs_rec_main_f2()$location.street.name, "<br/>", 
                                                crime_locs_rec_main_f2()$outcome_status.category),
                                 fillOpacity = crime_locs_rec_main_f2()$MonthsAgoFraction)
        })      

        output$crime_locs_rec2_map <- renderLeaflet({
            leaflet() %>%
                addProviderTiles(providers[[current_style()]],
                                 options = providerTileOptions(noWrap = TRUE)
                ) %>%
                addMarkers(lat = address2_location()$results$geometry$location$lat, lng = address2_location()$results$geometry$location$lng) %>%
                addCircles(lat = address2_location()$results$geometry$location$lat, lng = address2_location()$results$geometry$location$lng, radius = current_distance()*1609.34, stroke = FALSE, fillOpacity = 0.15) %>%
                addCircleMarkers(lat = as.numeric(crime_locs_rec2_f1()$lat),
                                 lng = as.numeric(crime_locs_rec2_f1()$lon),
                                 clusterOptions = markerClusterOptions(),
                                 color = crime_locs_rec2_f1()$colours,
                                 label = paste0(crime_locs_rec2_f1()$month,": ",crime_locs_rec2_f1()$Crime_Type, " ", crime_locs_rec2_f1()$location.street.name),
                                 popup = paste0("<i>",crime_locs_rec2_f1()$month,"</i>","<br/>",
                                                "<b>", crime_locs_rec2_f1()$Crime_Type, "</b>", "<br/>",
                                                crime_locs_rec2_f1()$location.street.name, "<br/>", 
                                                crime_locs_rec2_f1()$outcome_status.category),
                                 fillOpacity = crime_locs_rec2_f1()$MonthsAgoFraction)
        })
        
        output$comp_bar <- renderPlotly({
            crime_locs_summary_comp() %>% 
                mutate(across(c("AvgNumPerMonth_1", "AvgNumPerMonth_2"), round, 1)) %>%
                plot_ly(y = ~Crime_Type, x = ~AvgNumPerMonth_1, type = "bar", name = address_main_location()$results$formatted_address, text = ~AvgNumPerMonth_1, textposition = 'auto') %>%
                add_trace(y = ~Crime_Type, x = ~AvgNumPerMonth_2, name = address2_location()$results$formatted_address, text = ~AvgNumPerMonth_2, textposition = 'auto') %>% 
                layout(yaxis = list(title = "Crime Category", fixedrange = TRUE), 
                       xaxis = list(title = "Average Num of Crimes Per Month", fixedrange = TRUE), 
                       barmode = 'group', 
                       legend = list(orientation = 'h', xanchor = "left", x = 0, y = 1.05)) %>%
                config(displayModeBar = FALSE)
        })

        output$time <- renderPlotly({
            crime_locs_rec_main_time_summary_f2() %>%
                plot_ly(x = ~month, y = ~Num, color = ~Crime_Type, type = "scatter", mode = "lines") %>%
                layout(yaxis = list(title = "Number of Crimes in Month"), 
                       xaxis = list(title = ""),
                       legend = list(orientation = 'h', xanchor = "left", yanchor = "bottom", x = 0, y = 1.05))
            
        })

        ## Tables -----
        output$table <- renderDT({
            crime_locs_rec_main_f2() %>%
                select(`Month Occured` = month, 
                       `Crime Category` = category, 
                       `Outcome Status` = outcome_status.category,
                       `Location` = location.street.name) %>%
                mutate(across(where(is.character), factor))

            }, extensions = c('Scroller'),  #'Responsive'
            filter = list(
                position = 'top', clear = FALSE
            ),
            options = list(
                dom = 'Brti',
                fixedHeader = TRUE,
                deferRender = TRUE,
                scrollY = 600,
                scroller = TRUE
            ))
        
        output$formatabletable <-  renderFormattable({

            crime_locs_rec_main_summary_f2() %>%
                left_join(crime_locs_rec_main_year_on_year_f2(), by = "Crime_Type") %>%
                select(`Crime Type` = Crime_Type, `Pcnt of Crimes` = PcntofCrimes, `Avg Num Per Month` = AvgNumPerMonth, `Year on Year Change` = YearonYearChange) %>%
                mutate(`Pcnt of Crimes` = percent(`Pcnt of Crimes`, digits = 1),
                       `Avg Num Per Month` = digits(`Avg Num Per Month`, digits = 1),
                       `Year on Year Change` = percent(`Year on Year Change`, digits = 1)) %>%
                formattable(align = c("l", "r", "r", "r"),
                            list(`Pcnt of Crimes` = color_tile("white", "orange")),
                                 `Year on Year Change` = formatter("span", style = x ~ style(color = ifelse(x < 0, "red", "green"))))
                                 }
            )

    })