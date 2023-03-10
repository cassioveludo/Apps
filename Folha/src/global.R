# Package names
packages <- c("flexdashboard", "shinydashboard", "readxl", "stringr", "dplyr", 
              "tidyverse", "lubridate", "shiny", "lubridate", "shinyWidgets", 
              "plotly", "DT", "wordcloud2", "modules")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
        install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# Constants
CONST <- use("constantes.R")


############
### data ###
############

bd <- read_excel("../../Banco de Dados Consolidado.xlsx", na = "-", 
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
