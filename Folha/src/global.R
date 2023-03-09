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
library(DT)
library(wordcloud2)
library(modules)
####################


# Constants
CONST <- use("constantes.R")


############
### data ###
############

bd <- read_excel("../../../data/Banco de Dados Consolidado.xlsx", na = "-", 
                 sheet = "Banco de Dados",
                 col_names = T, 
                 guess_max = 1048576) # 1048576 é o total máximo de linhas do excel


colnames(bd) <- str_replace_all(colnames(bd), "[[:punct:]]", " ")
colnames(bd) <- str_replace_all(names(bd), c(" " = ".", "/" = ".", "-" = ".", "," = "" ))
bd <- bd %>% rename("TIPO.DE.DECISAO" = 'TIPO.DE.DECISÃO\r\n..mérito.x.remissão.')
bd$SEXO <- sapply(bd$SEXO, function(v){if(is.character(v)) return(toupper(v)) else return(v)})

############
############


############
# OBJETOS #
############




# Modules
#metric_summary <- use("modules/metric_summary.R")
