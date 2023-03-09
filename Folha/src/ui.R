##########
# APP UI #
##########
ui <- dashboardPage(
        dashboardHeader(title = CONST$APP_TITLE,
                        titleWidth = 350),
        
        dashboardSidebar(disable = T),
        
        dashboardBody(
                box(width = 13,
                    
                    selectizeInput('adol1', tags$b("Escolha o nome do adolescente"), multiple = F, choices = NULL, 
                                   selected= NULL, width = "280px"),
                    
                    
                    fluidRow(box(width = 12,
                                 valueBoxOutput("pemseTotal", width = 3),
                                 valueBoxOutput("pemseAtivo", width = 3),
                                 valueBoxOutput("anos21", width = 3),
                                 valueBoxOutput("unidade", width = 3),
                                 DT::DTOutput("folha"))
                             )
                    )
                )
        )
