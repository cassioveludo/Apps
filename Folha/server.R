
# Define server logic required to draw a histogram
server <- function(input, output, session) {
        

        observe({updateSelectizeInput(session, 'adol1', choices = bd$NOME, 
                                      selected= "", server = TRUE)})        
        
        # Total de Pemse do adolescente
        output$pemseTotal <- renderValueBox({
                pemse = bd %>% filter(NOME == input$adol1)%>% count ()
                #req(input$adol1)
                valueBox(pemse, "Total de PEMSE", color = "light-blue" )  
        })
        
        
        output$pemseAtivo <- renderValueBox({
                pemse = bd %>% filter(NOME == input$adol1 & ARQ.TRAM == "Tramitação")%>% count ()
                #req(input$adol1)
                valueBox(pemse, "Pemse Ativos", color = "light-blue" )  
        })
        
        output$anos21 <- renderValueBox({
                
                bd$DATA.PARA.COMPLETAR.21.ANOS <- format(as.Date(bd$DATA.PARA.COMPLETAR.21.ANOS), "%d/%m/%Y")  
                anos = bd %>% with(DATA.PARA.COMPLETAR.21.ANOS[NOME == input$adol1]) %>% head(1)
                
                #req(input$adol1)
                valueBox(if(identical(anos, character(0))){"NA"}
                         else {anos}, "21 anos", color = "light-blue" )  
        })
        
        
        output$unidade <-renderValueBox({
                atual = bd %>% 
                        with(UNIDADE.DE.CUMPRIMENTO.ATUAL[NOME == input$adol1 & ARQ.TRAM == "Tramitação"]) %>% head(1)

                #req(input$adol1)
                valueBox(ifelse(input$adol1 == " ", 0, 
                                ifelse(identical(atual, character(0)), "NA",
                                       atual)), 
                         "Unidade Atual", color = "light-blue" )  
        })        
        
  
        
        output$folha <- renderDataTable({
                
                bd$DATA.DA.DECISÃO.SENTENÇA <- as.Date(bd$DATA.DA.DECISÃO.SENTENÇA )
                bd$DATA.DA.EXTINÇÃO <- as.Date (as.numeric(bd$DATA.DA.EXTINÇÃO), origin = "1899-12-30") # windows excel origin
                
                x = bd %>% select("CNJ.PEMSE", "NOME", "SEXO","ARQ.TRAM", "TIPO.DE.DECISAO", "NOME.MAE", 
                              "MEDIDA.APLICADA", "DATA.DA.DECISÃO.SENTENÇA", "DATA.DA.EXTINÇÃO", "MOTIVO.DA.EXTINÇÃO") %>%
                        rename("Autos" = CNJ.PEMSE, "Situação" = ARQ.TRAM, "Mãe" = NOME.MAE, 
                               "Medida" = MEDIDA.APLICADA, "Data Decisão"="DATA.DA.DECISÃO.SENTENÇA", 
                               "Data Extinção" = DATA.DA.EXTINÇÃO,"Motivo Extinção" = MOTIVO.DA.EXTINÇÃO,
                               "Tipo" = TIPO.DE.DECISAO
                        ) %>%
                        filter(NOME == input$adol1)
              
                datatable (x, extensions = 'Buttons', 
                           options = list(lengthMenu = c(7, 10, 20), 
                                          pageLength = 7,
                                          dom = 'tB',
                                          buttons = c('print', 'excel', 'copy'),
                                          initComplete = JS(
                                                  "function(settings, json) {",
                                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                  "}")), 
                           class = 'display')
                
        })
        
}




