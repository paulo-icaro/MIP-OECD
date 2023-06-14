# ============================================ #
# === Input-Output Tables - OECD Countries === #
# ============================================ #

# -------------------------- #
# --- Autor: Paulo Icaro --- #
# -------------------------- #

# Link com a estrutura do dataset: https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/IOTS_2021
# Link onde os dados sao consultados: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/IOTS_2021
# Link para as matrizes no site da OCDE: https://stats.oecd.org/

# Obs:
# Aparentemente, diferente de outras bases, nao da para visualizar essa no navegador devido ao limite
# do tempo de conexao. Mas e sim possivel acessar os dados utilizando a funcao get_dataset



# ----------------- #
# --- Libraries --- #
# ----------------- #
library(OECD)                                                 # OECD API
library(openxlsx)
library(readxl)
library(tidyverse)
library(extrafont)                                            
path = 'C:/Users/paulo.costa/Downloads/OCDE/'                           # SDE
path = 'D:/Backup - Icaro/Documentos/'                                  # PC
path = 'C:/Users/Paulo/Documents/'                                      # Notebook
source('Repositorios/RAIS/Função - code_time.R', encoding = 'LATIN1')   # Função que contabilizar o tempo do code // Se precisar use setwd para mudar o path raiz



# ----------------------- #
# --- Data Extraction --- #
# ----------------------- #
# Na funcao get_dataset o argumento filter e uma lista para indicar as dimensoes do dataset que serao consultadas
# Obs: mudar a tipagem das colunas do dataframe (https://stackoverflow.com/questions/22772279/converting-multiple-columns-from-character-to-numeric-format-in-r)

options(timeout = 500)                                                                          # Aumentar o intervalo maximo de busca na URL do dataset
countries <- c('BRA')#, 'KOR')						                                                      # Variavel com os nomes dos paises

# Listas e colunas para armazenar dados tratados


db_sectors <- vector(mode = 'list', length = length(countries))
db_outputs <- vector(mode = 'list', length = length(countries))
db_sectors_coef <- vector(mode = 'list', length = length(countries))
db_sectors_matrix <- vector(mode = 'list', length = length(24))                                 # Lista que recebera as database dos setores
db_outputs_matrix <- vector(mode = 'list', length = length(24))                                 # Lista que recebera somente os dados dos outputs dos setores
db_sectors_coef_matrix <- vector(mode = 'list', length = length(24))                            # Lista que recebera os coeficientes da MIP


perc_change_oecd <- data.frame(matrix(nrow = 48600))                                            # Coluna que recebera as variacoes percentuais
colnames(perc_change_oecd) <- c('perc_change')                                                  # Nome da coluna das variacoes percentuais


# Colunas e Linhas cujas combinacoes serao desconsideradas
remove_col <- c('HFCE', 'NPISH', 'GGFC', 'GFCF', 'INVNT', 'CONS_ABR', 'CONS_NONRES', 'EXPO', 'IMPO')
remove_row <- c('TXS_IMP_FNL', 'TXS_INT_FNL', 'TTL_INT_FNL', 'VALU', 'OUTPUT')
dim_row <- read_excel(path = paste0(path, 'Repositorios/MIP-OECD/Dimensões.xlsx'), sheet = "linha", col_names=TRUE) %>% filter(!ROW %in% remove_row)
dim_col <- read_excel(path = paste0(path, 'Repositorios/MIP-OECD/Dimensões.xlsx'), sheet = "coluna", col_names=TRUE) %>% filter(!COL %in% remove_col)

# Extracao
start_time <- Sys.time()
for (c in 1:length(countries)){
  data_extraction <- get_dataset(dataset = "IOTS_2021",
                                 filter = list(c("TTL"), countries[c]),
                                 start_time = 1995,
                                 end_time = 2018)
  data_extraction[c('ObsValue', 'Time')] <- sapply(data_extraction[c('ObsValue', 'Time')], as.numeric)                                              # Mudanca da tipagem das colunas especificadas para numeric
  
  
  # Este loop filtra os dados intersetoriais e de output por ano, os transforma em matriz e os armazenas em uma lista
  # Desta maneira sera mais facil calcular os coeficientes tecnicos de cada matriz para cada ano
  for (t in 1:24){
    data_extraction_sectors <- data_extraction[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & !(ROW %in% remove_row) & (Time == 1994+t))         # Database Intersetorial // Remocao das combinacoes cujas variaveis nao serao de interesse
    data_extraction_outputs <- data_extraction[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & (ROW == "OUTPUT") & (Time == 1994+t))              # Database Outputs
    
    db_sectors_matrix[[t]] <- matrix(data = as.matrix(data_extraction_sectors[3]), nrow = 45, ncol = 45, dimnames = c(dim_row, dim_col))            # Database Intersetorial transformada em matriz e armazenada na lista
    db_outputs_matrix[[t]] <- matrix(data = as.matrix(data_extraction_outputs[3]), nrow = 1, ncol = 45, dimnames = c("Output", dim_col))            # Database Outputs transformada em matriz e armazenada na lista
    
    names(db_sectors_matrix)[t] <- 1994+t                                                                                                           # Cada elemento da lista Intersetorial recebera a data referente ao ano da matriz
    names(db_outputs_matrix)[t] <- 1994+t                                                                                                           # Cada elemento da lista Outputs recebera a data referente ao ano da matriz
    
    for (i in 1:45){
      if (i == 1){
        db_sectors_coef_matrix[[t]] <- db_sectors_matrix[[t]][1:45,i]/db_outputs_matrix[[t]][,i]
      }
      else{
        db_sectors_coef_matrix[[t]] <- cbind(db_sectors_coef_matrix[[t]], (db_sectors_matrix[[t]][1:45,i]/db_outputs_matrix[[t]][,i]))
        if (i == 45){
          colnames(db_sectors_coef_matrix[[t]]) <- t(dim_col)
        }
      }
    }
    
  }
  
  db_sectors[[c]] <- db_sectors_matrix                                                                                                              # Armazenamento da lista intersetorial com toda a serie temporal dentro da lista de países
  db_outputs[[c]] <- db_outputs_matrix                                                                                                              # Armazenamento da lista de outputs com toda a série temporal dentro da lista de países
  db_sectors_coef[[c]] <- db_sectors_coef_matrix                                                                                                    # Armazenamento da lista de coeficientes com toda a série temporal dentro da lista de países
  
  names(db_sectors)[c] <- countries[c]                                                                                                              # Cada lista intersetorial de cada pais recebera o nome do pais respectivo
  names(db_outputs)[c] <- countries[c]                                                                                                              # Cada lista de outputs de cada pais recebera o nome do pais respectivo
  names(db_sectors_coef) <- countries[c]                                                                                                            # Cada lista de coeficientes de cada pais recebera o nome do pais respectivo
  
  # Liberando memoria quando o ultimo pais for avaliado
  #if (c == length(countries)){rm(data_extraction, data_extraction_sectors, data_extraction_outputs, db_sectors_matrix, db_outputs_matrix, db_sectors_coef_matrix)}
}
end_time <- Sys.time()
code_time(start_time, end_time)                                                                                                                     # Cronometro


# --------------------- #
# --- Data Analysis --- #
# --------------------- #


# --- Percentage Changes --- #      
# Obs: ja foi incluido o salvamento dos dados analisados
#wb_perc_change  = createWorkbook(creator = 'pi')
for (p in 1:length(paises)){
  for (i in 2:48600){
    if(i%%24 != 1){
      database[[p]]$perc_change[i] <- (database[[p]]$ObsValue[i]/database[[p]]$ObsValue[i-1])-1
    }
    else {database[[p]]$perc_change[i] = NA}
  }
  #addWorksheet(wb = wb_perc_change, sheetName = paises[p])
  #writeData(wb = wb_perc_change, sheet = paises[p], x = database[[p]])
}
#saveWorkbook(wb = wb_perc_change, file = paste0(path, 'Variacoes_Percentuais.xlsx'), overwrite = TRUE)


# --- Plots --- #
dim_col <- unique(database[[1]][c(1)])
dim_row <- unique(database[[1]][c(4)])    


# Em aes o argumento color e empregado de maneira nao usual.
# Ele e utilizado para definir uma especie de id que sera associada a uma cor na funcao scale_color_manual

# font_import(): importa todas as fontes do sistema
# loadfonts(device = 'win'): ler o banco de dados de fontes importado e os registra junto ao R
# windowsFonts(): para ver todos os tipos de fontes agora disponiveis (por default o R so possui Times New Roman, Arial e Courier New)
# Para mais sobre o assunto, ver: https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
for (r in 1:2){
  for (c in 1:2){
    Plots <- ggplot() + 
      geom_line(data = database[[1]] %>% filter(COL %in% dim_col[c,1] & ROW %in% dim_row[r,1]), 
                aes(x = Time, y = ObsValue, color = 'Brazil'),
                linetype = 'dashed',
                size = .75) +
      geom_line(data = database[[2]] %>% filter(COL %in% dim_col[c,1] & ROW %in% dim_row[r,1]), 
                aes(x = Time, y = ObsValue, color = 'South Korea'),
                linetype = 'dashed',
                size = .75) +
      scale_color_manual(breaks = c('Brazil', 'South Korea'),
                         values = c('#45B39D', '#D35400'),
                         labels(NULL))+
      scale_x_continuous(breaks = seq(1995, 2018, 2)) +
      labs(title = 'Parameter Evolution',
           subtitle = paste0('From ', dim_row[r,1], ' to ', dim_col[c,1]),
           x = NULL,
           y = 'Parameter') +
      theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
            axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
            axis.text.x = element_text(angle = 45, margin = margin(t = 12), size = 15),          # Textos do eixo x 
            panel.background = element_rect(fill = '#F2F3F4')
      )
    
    ggsave(path = paste0(path, '/Plots'),
           filename = paste0('From ', dim_row[r,1], ' to ', dim_col[c,1], '.png'),
           width = 3000,
           height = 1300,
           units = 'px'
    )
  }
}



# --- Extracao das dimencoes (Linhas-Colunas) das matrizes --- #
#col_items <- unique(database[[1]][c(1)])
#row_items <- unique(database[[1]][c(5)])

# Workbook
#wb <- createWorkbook(creator = 'pi')
#addWorksheet(wb = wb, sheetName = 'coluna')
#addWorksheet(wb = wb, sheetName = 'linha')
#writeData(wb = wb, sheet = 'coluna' , x = unique(database[[1]][c(1)]))
#writeData(wb = wb, sheet = 'linha' , x = unique(database[[1]][c(5)]))
#saveWorkbook(wb = wb, file = paste0(path, '/Dimens?es.xlsx'), overwrite = TRUE)