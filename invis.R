setwd("D:/r prog/InVis")
library(dplyr)
library(tidyr)
library(stringr)

library(highcharter)
library(shiny)
library(bslib)

data1 = read.csv("data_2021.csv")
data2 = read.csv("data_2020.csv")
data3 = read.csv("data_2019.csv")
databites = read.csv("bites.csv")

dataGood = mutate_all(data1, ~replace_na(.,0))
dataStates = dataGood %>% filter(Location.1 != "")
dataStates = dataStates %>% filter(!Reporting.Area %in% c("DISTRICT OF COLUMBIA","NEW YORK CITY","GUAM","NORTHERN MARIANA ISLANDS","AMERICAN SAMOA","PUERTO RICO","U.S. VIRGIN ISLANDS","VIRGIN ISLANDS"))
dataStates$Reporting.Area = str_to_title(dataStates$Reporting.Area)
q1sum = dataStates %>% group_by(state = Reporting.Area) %>% summarise(values = sum(Rabies..Animal..Current.week))
top10States = q1sum %>% top_n(10,values)

ui <- 
  navbarPage(
  title = "Rabies Case Analysis",
  theme = bs_theme(
    #bootswatch = "slate",
    bg = "#2C2B30", fg = "#D6D6D6",primary = "red"
    
  ),
  tabPanel("Home",
           fluidPage(
             h2("Rabies Case Data Analysis and Visualization Using Shiny"),
             p("This website is made using shiny to show visualizations and insights on rabies animal cases and animal bites. Our data-driven approach allows you to explore and understand trends, patterns, and risks related to these important public health issues."),
             h3("What is Rabies?"),
             p("Rabies is a viral disease that affects the nervous system of humans and other mammals. It is usually transmitted to humans through the bite of an infected animal, such as a dog, raccoon, bat, or fox. Once the virus enters the body, it travels along the nerves to the brain, where it can cause a range of symptoms, including fever, headache, muscle weakness, and convulsions.
If left untreated, rabies can lead to severe brain damage and death. However, with prompt medical attention, including a series of vaccinations and injections, it is possible to prevent the virus from causing serious illness. Rabies is a serious public health concern in many parts of the world, particularly in areas where vaccination and animal control programs are not widely implemented."),
             h4("The top navigation bar provides quick and easy access to all of the visualizations on our website, allowing you to explore and interact with the data in a variety of ways."),
             div(
               style = "display: grid;",
               div(
                 style = "grid-column-start: 1;",
                 p("Dataset: "),
                 tags$ul(
                   tags$li(a("NNDSS Rabies 2019", href = "https://data.cdc.gov/NNDSS/NNDSS-TABLE-1CC-Rabies-Animal-to-Rabies-Human/usyq-s7ip")),
                   tags$li(a("NNDSS Rabies 2020", href = "https://data.cdc.gov/NNDSS/NNDSS-TABLE-1CC-Rabies-Animal-to-Rabies-Human/6kf3-4udg")),
                   tags$li(a("NNDSS Rabies 2021", href = "https://data.cdc.gov/NNDSS/NNDSS-TABLE-1CC-Rabies-Animal-to-Rabies-Human/9976-4iqj")),
                   tags$li(a("Animal Bites", href = "https://www.kaggle.com/datasets/rtatman/animal-bites"))
                 )
               ),
               div(
                 style = "grid-column-start: 2;",
                 p("Charts: "),
                   tags$ul(
                     tags$li("Reported Animal Rabies Case Total Distribution 2019-2021"),
                     tags$li("Reported Animal Rabies Case Trend 2019-2021"),
                     tags$li("Reported Animal Rabies Case Distribution by area 2019-2021"),
                     tags$li("Animal Bite Cases Species Trend")
                   )
                 )
             )
             
           )
           ),
  tabPanel("Rabies Case Total Distribution",highchartOutput(outputId = "a")),
  tabPanel("Rabies Case Trend",
           sidebarLayout(
             sidebarPanel(
               radioButtons(
                 inputId = "q1year",
                 label = "Year",
                 choices = c("2019","2020","2021")
               ),
               radioButtons(
                 inputId = "q1state",
                 label = "Top 10 State with most cases",
                 choices = unique(top10States$state)
               )
             ),
             mainPanel(div(highchartOutput(outputId = "q1"), style = "border-radius: 50px; 
                           background: radial-gradient(circle, rgba(85,85,85,0.5) 0%, rgba(44,43,48,1) 100%);
                           "))
           )),
  tabPanel("Rabies Case Distribution by area", 
           sidebarLayout(
             sidebarPanel(
               radioButtons(
                 inputId = "q2year",
                 label = "Year",
                 choices = c("2019","2020","2021")
               ),
             ),
             mainPanel(div(highchartOutput(outputId = "q2"), style = "border-radius: 50px; 
                           background: radial-gradient(circle, rgba(85,85,85,0.5) 0%, rgba(44,43,48,1) 100%);
                           "))
           )),
  tabPanel("Rabies Bite Case", div(highchartOutput(outputId = "q3",), style = "border-radius: 50px; 
                           background: radial-gradient(circle, rgba(85,85,85,0.5) 0%, rgba(44,43,48,1) 100%);
                           "))
)

server <- function(input, output, session) {
  output$a <- renderHighchart({
    james <- bind_rows(data1,data2,data3)
    james <- subset(james, select = c(Reporting.Area, MMWR.Year, MMWR.Week, Rabies..Animal..Current.week, Location.1 ))
    dataGooda = mutate_all(james, ~replace_na(.,0))
    dataStatesa = dataGooda %>% filter(Location.1 != "")
    dataStatesa = dataStatesa %>% filter(!Reporting.Area %in% c("DISTRICT OF COLUMBIA","NEW YORK CITY","GUAM","NORTHERN MARIANA ISLANDS","AMERICAN SAMOA","PUERTO RICO","U.S. VIRGIN ISLANDS","VIRGIN ISLANDS"))
    dataStatesa$Reporting.Area = str_to_title(dataStatesa$Reporting.Area)
    asum <- dataStatesa %>% group_by(state = Reporting.Area) %>% summarise(values = sum(Rabies..Animal..Current.week))
    hca <- asum %>% filter(values > 10) %>% arrange(values) %>% hchart("bar", hcaes(x = state, y = values), name = "Cases",color = "red") %>%
      hc_yAxis(title = list(text = "States", style = list(color = "white")),labels = list(style = list(color = "white"))) %>%
      hc_xAxis(title = list(text = "Cases", style = list(color = "white")),labels = list(style = list(color = "white")))
  })
  output$q1 <- renderHighchart({
    dataUsed1 <- switch (input$q1year,
      "2019" = data3,
      "2020" = data2,
      "2021" = data1
    )
    dataGoodq1 = mutate_all(dataUsed1, ~replace_na(.,0))
    dataStatesq1 = dataGoodq1 %>% filter(Location.1 != "")
    dataStatesq1 = dataStatesq1 %>% filter(!Reporting.Area %in% c("DISTRICT OF COLUMBIA","NEW YORK CITY","GUAM","NORTHERN MARIANA ISLANDS","AMERICAN SAMOA","PUERTO RICO","U.S. VIRGIN ISLANDS","VIRGIN ISLANDS"))
    dataStatesq1$Reporting.Area = str_to_title(dataStatesq1$Reporting.Area)
    q1week = dataStatesq1 %>% group_by(state = Reporting.Area,week = MMWR.Week) %>% summarise(values = sum(Rabies..Animal..Current.week))
    q1getState = q1week %>% filter(state == input$q1state)
    hc1 = q1getState %>% hchart("areaspline", hcaes(x = week, y = values), color = "red",marker = list(enabled = FALSE),name = "Number of Rabies Case") %>%
      hc_yAxis(title = list(text = "Cases", style = list(color = "white")),labels = list(style = list(color = "white"))) %>%
      hc_xAxis(title = list(text = "Week", style = list(color = "white")),labels = list(style = list(color = "white")))
  })
  output$q2 <-renderHighchart({
    dataUsed2 <- switch (input$q2year,
          "2019" = data3,
          "2020" = data2,
          "2021" = data1
    )
    print("test")
    dataGoodq2 = mutate_all(dataUsed2, ~replace_na(.,0))
    dataStatesq2 = dataGoodq2 %>% filter(Location.1 != "")
    dataStatesq2 = dataStatesq2 %>% filter(!Reporting.Area %in% c("DISTRICT OF COLUMBIA","NEW YORK CITY","GUAM","NORTHERN MARIANA ISLANDS","AMERICAN SAMOA","PUERTO RICO","U.S. VIRGIN ISLANDS","VIRGIN ISLANDS"))
    dataStatesq2$Reporting.Area = str_to_title(dataStatesq2$Reporting.Area)
    q2sum = dataStatesq2 %>% group_by(Reporting.Area,MMWR.Week) %>% summarise(values = sum(Rabies..Animal..Current.week))
    q2sum = q2sum %>% group_by(state = Reporting.Area) %>% summarise(values = sum(values))
    hc2 = hcmap(
      map = "countries/us/us-all",
      data = q2sum,
      value = "values",
      joinBy = c("name", "state"),
      name = "Rabies Case",
      dataLabels = list(enabled = TRUE, format = "{point.name}")
    ) %>% hc_colorAxis(minColor = "white" , maxColor = "red")
  })
  output$q3 <- renderHighchart({
    databites$year <- as.numeric(str_extract(databites$bite_date,"\\d{4}"))
    q3 <- databites %>%  filter(year < 2023 & year > 2000)
    q3 <- q3 %>% group_by(Species = SpeciesIDDesc, year) %>% summarise(count=n()) %>% filter((Species != ""))
    hc3 = q3 %>% hchart("spline", hcaes(x = year, y  = count, group = Species), marker = list(enabled = FALSE)) %>% hc_colors(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")) %>%
      hc_yAxis(title = list(text = "Cases", style = list(color = "white")),labels = list(style = list(color = "white"))) %>%
      hc_xAxis(title = list(text = "Year", style = list(color = "white")),labels = list(style = list(color = "white"))) %>%
      hc_legend(itemStyle = list(color = "white"))
  })
}

shinyApp(ui, server)
