#
# This is the server logic of a Shiny web application to display my glucose levels. You can run the
# application by clicking 'Run App' above.
#
# To get started, you will need to upload a few CSV/Excel files.

library(shiny)

library(tidyverse)
library(lubridate)
library(ggthemes)

# one-time setup creates a few dataframes for glucose levels and activity

libre_raw <- readxl::read_excel("Librelink.xlsx")
libre_raw$`Meter Timestamp` <- lubridate::force_tz(libre_raw$`Meter Timestamp`, "US/Pacific")


activity_raw <- dplyr::full_join(readxl::read_excel("Rik Activity 2019.xlsx", sheet = "2018"),
                      readxl::read_excel("Rik Activity 2019.xlsx", sheet = "2019"))

activity_raw$Start <- lubridate::force_tz(activity_raw$Start, "US/Pacific")
activity_raw$End <- lubridate::force_tz(activity_raw$End, "US/Pacific")

glucose <- libre_raw %>% select(time = "Meter Timestamp",
                                scan = "Scan Glucose(mg/dL)",
                                hist = "Historic Glucose(mg/dL)",
                                strip = "Strip Glucose(mg/dL)",
                                food = "Notes")


glucose$value <- dplyr::if_else(is.na(glucose$scan),glucose$hist,glucose$scan)
glucose_raw <- glucose

theme_set(theme_stata())

# a handy ggplot object that draws a band through the "healthy" target zones across the width of any graph:
glucose_target_gg <-   geom_rect(aes(xmin=as.POSIXct(-Inf,  origin = "1970-01-01"),
                                     xmax=as.POSIXct(Inf,  origin= "1970-01-01"),
                                     ymin=100,ymax=140),
                                 alpha = 0.01, fill = "#CCCCCC",
                                 inherit.aes = FALSE)





# show glucose levels between start and end times
cgm_display <- function(start=lubridate::now()-lubridate::hours(18),
                        end=lubridate::now(),
                        activity_df=activity_raw,
                        glucose_df=glucose_raw) {
  ggplot(glucose_df ,aes(x=time,y=value)) + geom_line(size=2, color = "red")+
    geom_point(stat = "identity", aes(x=time,y=strip), color = "blue")+
    glucose_target_gg +
    geom_rect(data=activity_df %>% dplyr::filter(Activity == "Sleep") %>%
                select(xmin = Start,xmax = End) %>% cbind(ymin = -Inf, ymax = Inf),
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill="red",
              alpha=0.2,
              inherit.aes = FALSE) +
    geom_rect(data=activity_raw %>% dplyr::filter(Activity == "Exercise") %>%
                select(xmin = Start,xmax = End),
              aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
              fill="blue",
              alpha=0.2,
              inherit.aes = FALSE) +
    geom_vline(xintercept = activity_df %>%
                 dplyr::filter(Activity == "Event" & Comment == "awake") %>% select("Start") %>% unlist(),
               color = "green") +
    geom_vline(xintercept = activity_df %>%
                 dplyr::filter(Activity == "Food") %>% select("Start") %>% unlist(),
               color = "yellow")+
    geom_text(data = activity_df %>%
                dplyr::filter(Activity == "Food") %>% select("Start","Comment") ,
              aes(x=Start,y=50, angle=90, hjust = FALSE,  label = Comment),
              size = 6) +
    labs(title = "Glucose (mg/dL)", subtitle = start) +  theme(plot.title = element_text(size=22))+
    scale_x_datetime(limits = c(start,end))

}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$glucoseLevelsPlot <- renderPlot({

    glucose <- dplyr::filter(glucose, time >= input$date_range[1] & time <= input$date_range[2] + lubridate::hours(6))
    # activity <- dplyr::filter(activity_raw, Start >= input$date_range[1] &
    #                             Start <= input$date_range[2] + lubridate::hours(6))
    # activity$Activity <- factor(activity$Activity)
    #
    cgm_display(input$date_range[1],input$date_range[2],activity_raw,glucose_raw)

  })

})
