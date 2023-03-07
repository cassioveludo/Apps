
#####################
# loading libraries #
#####################
library(flexdashboard)
library(shinydashboard)
library(readxl)
library(stringr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(plotly)
library(scales)
library(DT)
library(wordcloud2)
library(ggplot2)
#library(wordcloud)
#library(kableExtra)
#library(reshape2)
#library(sf)
#library(tmap)
#library(foreign)



ui <- dashboardPage(skin = "purple",
        dashboardHeader(title = "Dashboard Vemse",
                        titleWidth = 230),
        
        dashboardSidebar(
                sidebarMenu(
                        menuItem(tags$b("Dados Gerais"),  tabName = "dashboard", icon = icon("dashboard")),
                        menuItem(tags$b("Adolescentes"), icon = icon("th"), tabName = "widgets"),
                        width = 200
                        )
                ),
        
        dashboardBody(
                tabItems(
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                        valueBoxOutput("pemse", width = 3),
                                        valueBoxOutput("tramit", width = 3),
                                        valueBoxOutput("arquiv", width = 3),
                                        valueBoxOutput("adol", width = 3)
                                        ),
                                
                                fluidRow(box(title = tags$b("Proporção entre meninas e meninos"), 
                                                            width = 6, height = "400px",  solidHeader = TRUE, 
                                                            status = "primary",
                                             plotOutput("prop.sexo", height = "340px" ))
                                        )
                                
                                         
                                        
                                ),
     
                                           
                        tabItem(tabName = "widgets",
                                h2("Widgets tab content"))
                        )
                )
        )
?box


server <- function(input, output) {
        
        # TOTAL DE PROCESSOS
        output$pemse <- renderValueBox({
                pemse = bd %>%
                        summarize(PEMSE.TRIBUNAL = sum(!is.na(PEMSE.TRIBUNAL)) + length(PEMSE.TRIBUNAL [PEMSE.TRIBUNAL == "-"]))
                
                valueBox(format(round(as.numeric(pemse)), nsmall=0, big.mark = ".", decimal.mark = "," ), 
                         "Total de Processos", color = "blue" )
                
        })
        
        # EM TRAMITAÇÃO (INCLUINDO OS SUSPENSOS)
        output$tramit <- renderValueBox({
                tramit = bd %>% drop_na (ARQ.TRAM) %>%
                        summarise(length = length(ARQ.TRAM [ARQ.TRAM == "Tramitação"]) + length(ARQ.TRAM [ARQ.TRAM == "Suspenso"]))
                
                valueBox(format(round(as.numeric(tramit)), nsmall=0, big.mark = ".", decimal.mark = "," ),
                         "Tramitação / Suspensos", color = "blue")
                
        })
        
        
        # ARQUIVADOS/DECLÍNIO
        output$arquiv <- renderValueBox({
               
                 arq = bd %>% drop_na (ARQ.TRAM) %>%
                        summarise(length = length(ARQ.TRAM [ARQ.TRAM == "Arquivado"]) + length(ARQ.TRAM [ARQ.TRAM == "Declínio"]))
               
                 valueBox(format(round(as.numeric(arq)), nsmall=0, big.mark = ".", decimal.mark = "," ),
                          "Arquivado/Declínio", color = "blue")
                
        })
        
       # TOTAL DE ADOLESCENTES
        output$adol <- renderValueBox({
                
                adol = bd %>% distinct(NOME, .keep_all = TRUE) %>% drop_na(NOME) %>%
                        summarise(count = n())
                valueBox(format(round(as.numeric(adol)), nsmall=0, big.mark = ".", decimal.mark = "," ),
                         "Total de Adolescentes", color = "blue")
                
        })
        
        
        #### PROPORÇÃO MENINOS X MENINAS ####
        
        output$prop.sexo <- renderPlot({
                
                bd %>% 
                        distinct(NOME, .keep_all = TRUE) %>% drop_na(SEXO) %>% 
                        group_by(SEXO) %>% count(SEXO) %>% summarise(y = sum(n))  %>% 
                        mutate(prop = percent(y/sum(y))) %>%
                        arrange(desc(prop)) %>%
                
                        ggplot(aes(x="", y=prop, fill=SEXO)) +
                        geom_bar(stat="identity", width=1, color="white") +
                        coord_polar("y", start=0) +
                        geom_text(aes(label = paste0(format(round(as.numeric(y)), nsmall=0, big.mark = ".", decimal.mark = "," ),
                                                    " (",prop, ")")), 
                                  position = position_stack(vjust = 0.5),  size=6, color = "black") +
                        
                        theme_void() + # remove background, grid, numeric labels
                        theme(legend.title=element_blank(),
                              legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=18)
                              )
                
                
        })    
        
        
        
        
}





# Run the application 
shinyApp(ui = ui, server = server)