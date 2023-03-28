# ============================================ #
# === Input-Output Tables - OECD Countries === #
# ============================================ #

# Link com a estrutura do dataset: https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/IOTS_2021
# Link onde os dados sao consultados: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/IOTS_2021

# Obs:
# Aparentemente, diferente de outras bases, nao da para visualizar essa no navegador devido ao limite
# do tempo de conexao. Mas e sim possivel acessar os dados utilizando a funcao get_dataset



# ---------------- #
# --- Packages --- #
# ---------------- #
install.packages('OECD')


# ----------------- #
# --- Libraries --- #
# ----------------- #
library(OECD)                                                 # OECD API
library(openxlsx)
library(readxl)
library(dplyr)
path = 'C:/Users/paulo.costa/Downloads/OCDE'
path = 'D:/Backup - Icaro/Documentos/Repositórios/Projetos/MIP/MIP-OECD'
path = 'C:/Users/Paulo/Documents/Repositórios/Projetos/MIP/MIP-OECD/'

# ----------------------- #
# --- Data Extraction --- #
# ----------------------- #
# Na funcao get_dataset o argumento filter e uma lista para indicar as dimensoes do dataset que serao consultadas

options(timeout = 180)                                                          # Aumentar o intervalo maximo de busca na URL do dataset
paises <- c('BRA', 'KOR')						                                            # Variavel com os nomes dos paises
database <- vector(mode = 'list', length = length(paises))                      # Lista que recebera as databases
dim_col <- read_excel(path= paste0(path, '/Dimensões.xlsx'), sheet = 'coluna', col_names = TRUE)
dim_row <- read_excel(path= paste0(path, '/Dimensões.xlsx'), sheet = 'linha', col_names = TRUE)
var_perc_oecd <- data.frame(matrix(nrow = 64800))                               # Coluna que recebera as variacoes percentuais
colnames(var_perc_oecd) <- c('var_perc')                                        # Nome da nova coluna



for (p in 1:length(paises)){
  data_extraction <- get_dataset(dataset = "IOTS_2021",
                                 filter = list(c("TTL"), paises[p]),
                                 start_time = 1995,
                                 end_time = 2018)
  
  database[[p]] <- data_extraction[c(1,2,3,5,7)] %>% mutate(var_perc_oecd)      # Filtragem para reduzir o tamanho do dataser

  #if (p == length(paises)){rm(data_extraction)}
}



# --------------------- #
# --- Data Analysis --- #
# --------------------- #

for (p in 1:length(paises)){
  for (j in 1:2700){
    for (i in 1:24){
      var_perc_oecd[[j*i]] <- (database[[p]]$ObsValue[i+1]/database[[p]]$ObsValue[i])-1
}}}



# --- Extracao das dimencoes (Linhas-Colunas) das matrizes --- #
#col_items <- unique(database[[1]][c(1)])
#row_items <- unique(database[[1]][c(5)])

# Workbook
#wb <- createWorkbook(creator = 'pi')
#addWorksheet(wb = wb, sheetName = 'coluna')
#addWorksheet(wb = wb, sheetName = 'linha')
#writeData(wb = wb, sheet = 'coluna' , x = unique(database[[1]][c(1)]))
#writeData(wb = wb, sheet = 'linha' , x = unique(database[[1]][c(5)]))
#saveWorkbook(wb = wb, file = paste0(path, '/Dimensões.xlsx'), overwrite = TRUE)