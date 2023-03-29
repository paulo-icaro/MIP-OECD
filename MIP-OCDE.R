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
path = 'C:/Users/paulo.costa/Downloads/OCDE/'
path = 'D:/Backup - Icaro/Documentos/Repositórios/Projetos/MIP/MIP-OECD/'
path = 'C:/Users/Paulo/Documents/Repositórios/Projetos/MIP/MIP-OECD/'

# ----------------------- #
# --- Data Extraction --- #
# ----------------------- #
# Na funcao get_dataset o argumento filter e uma lista para indicar as dimensoes do dataset que serao consultadas
# Obs: mudar a tipagem das colunas do dataframe (https://stackoverflow.com/questions/22772279/converting-multiple-columns-from-character-to-numeric-format-in-r)

options(timeout = 500)                                                          # Aumentar o intervalo maximo de busca na URL do dataset
paises <- c('BRA', 'KOR')						                                            # Variavel com os nomes dos paises
database <- vector(mode = 'list', length = length(paises))                      # Lista que recebera as databases
perc_change_oecd <- data.frame(matrix(nrow = 48600))                            # Coluna que recebera as variacoes percentuais
colnames(perc_change_oecd) <- c('perc_change')                                  # Nome da nova coluna

# Colunas e Linnhas cujas combinacoes serao desconsideradas
remove_col <- c('HFCE', 'NPISH', 'GGFC', 'GFCF', 'INVNT', 'CONS_ABR', 'CONS_NONRES', 'EXPO', 'IMPO')
remove_row <- c('TXS_IMP_FNL', 'TXS_INT_FNL', 'TTL_INT_FNL', 'VALU', 'OUTPUT')


for (p in 1:length(paises)){
  data_extraction <- get_dataset(dataset = "IOTS_2021",
                                 filter = list(c("TTL"), paises[p]),
                                 start_time = 1995,
                                 end_time = 2018)
  
  database[[p]] <- data_extraction[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & !(ROW %in% remove_row))  # Remocao das combinacoes cujas variaveis nao serao de interesse
  database[[p]] <- database[[p]] %>% mutate(perc_change_oecd)                                                    # Filtragem para reduzir o tamanho do dataset
  database[[p]][c('ObsValue', 'Time')] <- sapply(database[[p]][c('ObsValue', 'Time')], as.numeric)            # Mudanca da tipagem das colunas especificadas para numeric
  #if (p == length(paises)){rm(data_extraction)}                                                               # Liberando memoria quando o ultimo pais for avaliado
}



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

for (p in 1:length(paises)){
  for (y in 1:45){
    for (x in 1:45){
      ggplot(data = database[[p]] %>% filter(COL %in% dim_col[x,1] & ROW %in% dim_row[y,1])) +
      geom_line(aes(x = Time, y = ObsValue)) +
      scale_x_continuous(breaks = seq(1995, 2018, 2)) +
      labs(title = paste0('From ', dim_col[x,1], ' to ', dim_row[y,1])) +
      theme_light()
      
    }
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
#saveWorkbook(wb = wb, file = paste0(path, '/Dimens?es.xlsx'), overwrite = TRUE)