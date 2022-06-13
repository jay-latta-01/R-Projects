library(rvest)
library(xml2)
library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(stringr)
library(plotly)
nrg_page <- 'https://www.eia.gov/dnav/pet/pet_pri_gnd_a_epm0_pte_dpgal_w.htm'
nrg <- read_html(nrg_page)
dates1 <- nrg %>%
  # rvest::html_nodes("body") %>%
  xml2::xml_find_all("//th[@class='Series5']") %>%
  rvest::html_text()
dates_stamped <- mdy(dates1)
raw_table <- nrg %>%
  rvest::html_nodes("body") %>%
  xml2::xml_find_all("//table[@class='data1']") %>%
  rvest::html_table()
raw_df <- as.data.frame(raw_table)
raw_df <- raw_df[ -(1:7),-c(1:2, 10:41)]
raw_df <- na.omit(raw_df)
raw_df <- raw_df[raw_df$X4 != "",]
raw_df <- raw_df[1:9,]
names(raw_df) <- c("location", dates1)
row.names(raw_df) <- NULL
processed_df <- as.data.frame(t(raw_df))
regions <- c(t(processed_df))[1:9]
regions <- stringr::str_replace_all(regions, " ", "_")
regions <- stringr::str_replace_all(regions, "[(]", "")
regions <- stringr::str_replace_all(regions, "[)]", "")
names(processed_df) <- regions
processed_df <- processed_df[-1,]
rownames(processed_df) <- NULL
processed_df <- as.data.frame(apply(processed_df[,1:9], 2, function(x) as.numeric(x)))
processed_df['dates'] = dates_stamped
processed_df['week_num'] = seq(1,length(dates_stamped))
ui <- shinydashboard::dashboardPage(
  dashboardHeader(title = "Gas Prices By Region"),
  dashboardSidebar(),
  dashboardBody(
    box(plotlyOutput("gas_plot"), width = 8),
    box(selectInput("region", "Region:", regions, width = 250))
  )
)

server <- function(input, output){
  output$gas_plot <- renderPlotly({
    ggplotly(ggplot(processed_df, aes(x = dates , y = get(input$region), group = 1, text=paste("Date: ",
                                                                                               dates,
                                                                                               "\nPrice: ",
                                                                                               get(input$region)) )) +
               geom_point() + stat_smooth(method = "lm", se=FALSE, aes(text = paste("p = ", round(as.vector(lm(get(input$region)~week_num, data = processed_df)$coefficients)[1],3),
                                                                                    " + ",
                                                                                    round(as.vector(lm(get(input$region)~week_num, data = processed_df)$coefficients)[2],3),
                                                                                    "w"))) +
               xlab("Date") + ylab("Average Price ($)") + ylim(3,6), tooltip = "text")
  })
  
}

shinyApp(ui, server)