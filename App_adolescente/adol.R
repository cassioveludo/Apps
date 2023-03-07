# App com infos sobrer adolescentes com processo na VEMSE/DF

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
#library(scales)
library(DT)
library(wordcloud2)
#library(wordcloud)
#library(kableExtra)
#library(reshape2)
#library(sf)
#library(tmap)
#library(foreign)

####################
# loading data set #
####################

#To run localy (puxando do servidor): ##### OneDrive #####
bd <- read_excel("../../../data/Banco de Dados Consolidado.xlsx", na = "-", sheet = "Banco de Dados", 
                 col_names = T, guess_max = 1048576) # 1048576 é o total máximo de linhas do excel

colnames(bd) <- str_replace_all(colnames(bd), "[[:punct:]]", " ")
colnames(bd) <- str_replace_all(names(bd), c(" " = ".", "/" = ".", "-" = ".", "," = "" ))
bd <- bd %>% rename("TIPO.DE.DECISAO" = 'TIPO.DE.DECISÃO\r\n..mérito.x.remissão.')


### Para estudos sobre Internação Provisória ###
ip <- bd %>%
        dplyr::select("CNJ.PEMSE", "NOME", "NOME.MAE", "DATA.APREENSÃO",
                      "DATA.DA.DECISÃO...INTERNAÇÃO.PROVISÓRIA", 
                      "DATA.LIBERAÇÃO","DATA.DA.DECISÃO...PRORROGAÇÃO.DO.PRAZO.DE.IP",
                      "CONTROLE.DO.PRAZO.DE.LIBERAÇÃO.IP",
                      "INTERNAÇÃO.PROVISÓRIA..dias.", "ARQ.TRAM", "DATA.DA.DECISÃO.SENTENÇA", 
                      "MEDIDA.APLICADA" ) %>%
        rename("Decisão.IP" = DATA.DA.DECISÃO...INTERNAÇÃO.PROVISÓRIA, "Data.Prorrogação" = DATA.DA.DECISÃO...PRORROGAÇÃO.DO.PRAZO.DE.IP,
               "Data.Liberação.Controle" = CONTROLE.DO.PRAZO.DE.LIBERAÇÃO.IP, "IP.dias"= INTERNAÇÃO.PROVISÓRIA..dias.) %>%
        filter(!is.na(Decisão.IP) | !is.na(IP.dias))

ip$DATA.LIBERAÇÃO <- strptime(ip$DATA.LIBERAÇÃO, "%Y-%m-%d")
ip$Decisão.IP <- strptime(ip$Decisão.IP, "%Y-%m-%d")
ip$DATA.DA.DECISÃO.SENTENÇA <- strptime(ip$DATA.DA.DECISÃO.SENTENÇA, "%Y-%m-%d")


# Internação provisória em tramitação
ip_tram <- ip %>% filter(ARQ.TRAM == "Tramitação", !is.na(Decisão.IP), is.na(DATA.LIBERAÇÃO)) 



##########
# APP UI #
##########
ui <- shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "Dashboard Vemse",
                                        titleWidth = 280),
        # shiny::uiOutput("logout_button")),
        shinydashboard::dashboardSidebar(tags$head(tags$style(HTML(".sidebar { position: fixed; width: 200px;}" ))),
                                         width = 230,
                                         
                                         div(h1("Filtros", style = "margin-left: 10px;")),
                                         
                                         selectizeInput('situ', tags$b("Situação Processual"), multiple = F, choices = NULL, 
                                                        selected= NULL, width = "230px"),
                                         
                                         selectizeInput('adol1', tags$b("Escolha o nome do adolescente"), multiple = F, choices = NULL, 
                                                        selected= NULL, width = "280px")
                                         
                                         #selectizeInput('ano', tags$b("Escolha o ano"), multiple = F, choices = NULL, 
                                         #               selected= NULL, width = "200px"),
                                         ),
        
        
        
       
                                                   
        
        
        shinydashboard::dashboardBody(
                
                #### tabsets ####
                shiny::tabsetPanel(id = "tabset",
                                   shiny::tabPanel(title = "Dados Gerais",
                                                   tabsetPanel(
                                                           #### geral ####
                                                           tabPanel("Geral", shiny::fluidRow(shinydashboard::box(width = 12,
                                                                                                                 valueBoxOutput("pemse", width = 3),
                                                                                                                 valueBoxOutput("total.stock", width = 3),
                                                                                                                 valueBoxOutput("total.fixa", width = 3),
                                                                                                                 DT::DTOutput("cart.geral")))),
                                                           
                                                           tabPanel("Alocação", shinydashboard::box(width = 13,
                                                                                                            fixedRow(
                                                                                                                    column(6,plotOutput("aloc")),
                                                                                                                    column(6,plotOutput("aloc2"))
                                                                                                                    )))
                                                           ))
                                   )))






# Define server logic required to draw a histogram
server <- function(input, output) {

#updateSelectizeInput(session, 'situ', choices = c("Arquivado", "Declínio","Suspenso", "Tramitação"),
 #                             selected= "Tramitação", server = TRUE) 

#observe({updateSelectizeInput(session, 'adol1', choices = bd$NOME[bd$ARQ.TRAM == input$situ], 
 #                            selected= "", server = TRUE)})        
        
        # ATUAL EM RENDA FIXA
        output$pemse <- renderValueBox({
               pemse = bd %>%
                       summarize(PEMSE.TRIBUNAL = sum(!is.na(PEMSE.TRIBUNAL)) + length(PEMSE.TRIBUNAL [PEMSE.TRIBUNAL == "-"]))
                
              valueBox(pemse, "sub")
                
                
       })
                   
        
        
}

# Run the application 
shinyApp(ui = ui, server = server)
