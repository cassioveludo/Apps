
# Define server logic required to draw a histogram
server <- function(input, output, session) {
        
        
        observe({updateSelectizeInput(session, 'adol1', choices = bd$NOME, 
                                      selected= "", server = TRUE)})        
        
        # Total de Pemse do adolescente
        output$pemseTotal <- renderValueBox({
                pemse = bd %>% filter(NOME == input$adol1)%>% count ()
                valueBox(pemse, "Total de PEMSE")  
        })
        
        
        output$pemseAtivo <- renderValueBox({
                pemse = bd %>% filter(NOME == input$adol1)%>% count ()
                valueBox(pemse, "Pemse Ativos") 
        })
        
        output$anos21 <- renderValueBox({
                
                bd$DATA.PARA.COMPLETAR.21.ANOS <- format(as.Date(bd$DATA.PARA.COMPLETAR.21.ANOS), "%d/%m/%Y")  
                anos = bd %>% with(DATA.PARA.COMPLETAR.21.ANOS[NOME == input$adol1]) %>% head(1)
                
                req(input$adol1)
                valueBox(if(input$adol1 == 0){"NA"}
                         else {anos}, "21 anos")
        })
        
        
        output$unidade <-renderValueBox({
                atual = bd %>% with(UNIDADE.DE.CUMPRIMENTO.ATUAL[NOME == input$adol1]) %>% head(1)
                
                req(input$adol1)
                
                valueBox(if(input$adol1 == 0){"NA"}
                         else {atual}, "Unidade Atual")
        })        
        
     
        
        
        output$folha <- renderDataTable({
                
                bd$DATA.DA.DECISÃO.SENTENÇA <- as.Date(bd$DATA.DA.DECISÃO.SENTENÇA )
                bd$DATA.DA.EXTINÇÃO <- as.Date (as.numeric(bd$DATA.DA.EXTINÇÃO), origin = "1899-12-30") # windows excel origin
                
                bd %>% select("CNJ.PEMSE", "NOME", "SEXO","ARQ.TRAM", "TIPO.DE.DECISAO", "NOME.MAE", 
                              "MEDIDA.APLICADA", "DATA.DA.DECISÃO.SENTENÇA", "DATA.DA.EXTINÇÃO", "MOTIVO.DA.EXTINÇÃO") %>%
                        rename("Autos" = CNJ.PEMSE, "Situação" = ARQ.TRAM, "Mãe" = NOME.MAE, 
                               "Medida" = MEDIDA.APLICADA, "Data Decisão"="DATA.DA.DECISÃO.SENTENÇA", 
                               "Data Extinção" = DATA.DA.EXTINÇÃO,"Motivo Extinção" = MOTIVO.DA.EXTINÇÃO,
                               "Tipo" = TIPO.DE.DECISAO
                        ) %>%
                        filter(NOME == input$adol1) %>% 
                        datatable (options = list(lengthMenu = c(7, 10, 20), pageLength = 7), class = 'display')
        })
        
}




