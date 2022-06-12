library(shiny)
suppressPackageStartupMessages(library(tidyverse)) 
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(gganimate))
suppressPackageStartupMessages(library(prophet))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(lubridate))

#setwd('C:\\R\\eda_rhine\\Shiny\\')

complete_df_orange_river <- readRDS("orange_river_all_stations.rds")
complete_df_vistula <- readRDS("vistula_all_stations.rds")

season_plot_average_flows <- function(df_data, how_many_years = 5, random_year = T, start_year = NULL){
    give_start <- function(vec_dates){
        min_Date <- vec_dates %>% min()
        return(c(lubridate::year(min_Date), lubridate::month(min_Date), lubridate::day(min_Date)))
    }
    
    give_end <- function(vec_dates){
        max_Date <- vec_dates %>% max()
        return(c(lubridate::year(max_Date), lubridate::month(max_Date), lubridate::day(max_Date)))
    }
    
    if(random_year){random_year_start <- sample(df_data$date %>% lubridate::year() %>% unique(),1)}else{random_year_start <- start_year}
    how_many_years <- how_many_years
    
    filtered_data <- df_data %>% 
        dplyr::filter(lubridate::year(date) %between% c(random_year_start, random_year_start + how_many_years)) %>%
        dplyr::select(date, discharge) %>%
        mutate(monthly = floor_date(date, "month"))
    
    flow_monthly <- ddply(filtered_data, "monthly", summarise, monthly_average_flow = mean(discharge))
    
    tmp_ts <- stats::ts(flow_monthly$monthly_average_flow, 
                        frequency = 12, 
                        start = give_start(flow_monthly$monthly)[-3], 
                        end = give_end(flow_monthly$monthly)[-3])
    
    seasonplot(tmp_ts, 12, year.labels=TRUE, main="Average flows") 
    abline(v = 6, col = 'red')
    abline(v = 8, col = 'red')
    abline(v = 1, col = 'blue')
    abline(v = 11, col = 'blue')
}

give_season <- function(current_date){
    case_when(lubridate::month(current_date) %in% c(12,1,2) ~ 'Winter',
              lubridate::month(current_date) %between% c(3,5)~ 'Spring',
              lubridate::month(current_date) %between% c(6,8)~ 'Summer',
              lubridate::month(current_date) %between% c(9,11)~ 'Fall')
}

daily_discharge_by_seasons_plot <- function(df_data){
    df_data %>%
        mutate(season = give_season(date)) %>%
        ggplot(aes(x = season, y = discharge, fill = season)) + 
        geom_boxplot() + 
        scale_y_log10() + 
        coord_flip()
}

are_distributions_daily_discharge_different_by_seasons <- function(df_data){
    df_data %>%
        mutate(season = give_season(date)) %>%
        kruskal.test(discharge ~ season, data = .)
}

monthly_discharge_by_seasons_plot <- function(df_data){
    
    filtered_data <- df_data %>% 
        dplyr::select(date, discharge) %>%
        mutate(monthly = floor_date(date, "month"))
    
    flow_monthly <- ddply(filtered_data, "monthly", summarise, monthly_average_flow = mean(discharge))
    
    flow_monthly %>%
        mutate(season = give_season(monthly)) %>%
        ggplot(aes(x = season, y = monthly_average_flow, fill = season)) + 
        geom_boxplot() + 
        scale_y_log10() + 
        coord_flip()
}

are_distributions_monthly_discharge_different_by_seasons <- function(df_data){
    
    filtered_data <- df_data %>% 
        dplyr::select(date, discharge) %>%
        mutate(monthly = floor_date(date, "month"))
    
    flow_monthly <- ddply(filtered_data, "monthly", summarise, monthly_average_flow = mean(discharge))
    
    
    flow_monthly %>%
        mutate(season = give_season(monthly)) %>%
        kruskal.test(monthly_average_flow ~ season, data = .)
}

daily_discharge_after_1990_plot <- function(df_data){
    df_data %>%
        mutate(after_1990 = ifelse(date > '1990-01-1',T,F)) %>%
        ggplot(aes(x = after_1990, y = discharge, fill = after_1990)) + 
        geom_boxplot() + 
        scale_y_log10() + 
        coord_flip()
}

are_distributions_daily_discharge_different_after_1990 <- function(df_data){
    df_data %>%
        mutate(after_1990 = ifelse(date > '1990-01-1',T,F)) %>%
        kruskal.test(discharge ~ after_1990, data = .)
}

monthly_discharge_after_1990_plot <- function(df_data){
    
    filtered_data<- df_data %>% 
        dplyr::select(date, discharge) %>%
        mutate(monthly = floor_date(date, "month"))
    
    flow_monthly <- ddply(filtered_data, "monthly", summarise, monthly_average_flow = mean(discharge))
    
    flow_monthly %>%
        mutate(after_1990 = ifelse(monthly > '1990-01-1',T,F)) %>%
        ggplot(aes(x = after_1990, y = monthly_average_flow, fill = after_1990)) + 
        geom_boxplot() + 
        scale_y_log10() + 
        coord_flip()
}

are_distributions_monthly_discharge_different_after_1990 <- function(df_data){
    
    filtered_data<- df_data %>% 
        dplyr::select(date, discharge) %>%
        mutate(monthly = floor_date(date, "month"))
    
    flow_monthly <- ddply(filtered_data, "monthly", summarise, monthly_average_flow = mean(discharge))
    
    flow_monthly %>%
        mutate(after_1990 = ifelse(monthly > '1990-01-1',T,F)) %>%
        kruskal.test(monthly_average_flow ~ after_1990, data = .)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("River Runoff"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("how_many_years",
                        "How many years you want to show on a seasonplot?",
                        min = 2,
                        max = 15,
                        value = 5),
            selectInput("river",
                        label = 'Choose a river:',
                        choices =  c("Orange river", 'Vistula')),
            uiOutput("station_id"),
            actionButton(inputId = 'recalculate', label = 'Recalculate values')
                
            )
        ,

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("season_plot", plotOutput('season_plot_months')), 
                tabPanel("daily_data_season", plotOutput('plot_daily_season'), verbatimTextOutput('kruskal_daily_season')),
                tabPanel("monthly_data_season", plotOutput('plot_monthly_season'), verbatimTextOutput('kruskal_monthly_season')),
                tabPanel("daily_data_after_1990", plotOutput('plot_daily_after_1990'), verbatimTextOutput('kruskal_daily_after_1990')),
                tabPanel("monthly_data_after_1990", plotOutput('plot_monthly_after_1990'), verbatimTextOutput('kruskal_monthly_after_1990'))
            )
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$station_id <- renderUI({
        
        if(input$river == 'Orange river'){
            selectInput('choose_station',
                        'Please, choose the station', 
                        choices = c(1159650L, 1159100L, 1159300L, 1159103L, 1159304L, 1159290L, 1159105L))
        }else{
            selectInput('choose_station',
                        'Please, choose the station', 
                        choices = c(6458450L, 6458010L))
        }
    })
    
    observeEvent(input$recalculate, {
        
        chosen_river <- reactive({
            if(input$river == 'Orange river'){
                complete_df_orange_river
            }else{
                complete_df_orange_vistula
            }
        })
        
        output$season_plot_months <- renderPlot({
            chosen_river() %>% 
                dplyr::filter(station == input$choose_station) %>%
                season_plot_average_flows(., how_many_years = (input$how_many_years-1))
        })
        
        output$plot_daily_season <- renderPlot({
            chosen_river() %>% 
                dplyr::filter(station == input$choose_station) %>%
                daily_discharge_by_seasons_plot()
        })
        
        output$kruskal_daily_season <- renderPrint({
            chosen_river() %>% 
                dplyr::filter(station == input$choose_station) %>% 
                are_distributions_daily_discharge_different_by_seasons()
        })
        
        output$plot_monthly_season <- renderPlot({
            chosen_river() %>% 
                dplyr::filter(station == input$choose_station) %>% 
                monthly_discharge_by_seasons_plot()
        })
        
        output$kruskal_monthly_season <- renderPrint({
            chosen_river() %>% 
                dplyr::filter(station == input$choose_station) %>%
                are_distributions_monthly_discharge_different_by_seasons()
        })
        
        output$plot_daily_after_1990 <- renderPlot({
            chosen_river() %>% 
                dplyr::filter(station == input$choose_station) %>%
                monthly_discharge_after_1990_plot()
        })
        
        output$kruskal_daily_after_1990 <- renderPrint({
            chosen_river() %>% 
                dplyr::filter(station == input$choose_station) %>%
                are_distributions_daily_discharge_different_after_1990()
        })
        
        output$plot_monthly_after_1990 <- renderPlot({
             chosen_river() %>% 
                dplyr::filter(station == input$choose_station) %>%
                daily_discharge_after_1990_plot()
        })
        
        output$kruskal_monthly_after_1990 <- renderPrint({
             chosen_river() %>% 
                dplyr::filter(station == input$choose_station) %>%
                are_distributions_monthly_discharge_different_after_1990()
        })
        
        
    })


}

# Run the application 
shinyApp(ui = ui, server = server)




#this is how our app looks like
#https://danek555.shinyapps.io/aika_eda_rivers/
