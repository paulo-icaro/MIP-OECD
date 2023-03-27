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
path = 'C:/Users/paulo.costa/Downloads/OCDE'
path = 'D:/Backup - Icaro/Documentos/Repositórios/Projetos/MIP/MIP-OECD'

# ----------------------- #
# --- Data Extraction --- #
# ----------------------- #
# Na funcao get_dataset o argumento filter e uma lista para indicar as dimensoes do dataset que serao consultadas

options(timeout = 180)                                        # Aumentar o intervalo maximo de busca na URL do dataset
paises <- c('BRA', 'KOR')						                          # Variavel com os nomes dos paises
dim_col <- read_excel(path= paste0(path, '/Dimensões.xlsx'), sheet = 'coluna', col_names = TRUE)
dim_row <- read_excel(path= paste0(path, '/Dimensões.xlsx'), sheet = 'linha', col_names = TRUE)

database <- vector(mode = 'list', length = length(paises))    # Lista que recebera as databases


for (p in 1:length(paises)){
  data_extraction <- get_dataset(dataset = "IOTS_2021",
                                 filter = list(c("TTL"), paises[i]),
                                 start_time = 1995,
                                 end_time = 2018)
  database[[i]] <- data_extraction
  rm(data_extraction)
}



# --------------------- #
# --- Data Analysis --- #
# --------------------- #

var_perc_oecd <- vector(mode = 'list', length = length(paises))

for (p in 1:length(paises)){
  for (i in 1:24)){
    var_perc_oecd[[]] <- (database[[p]]$ObsValue[i+1]/database[[p]]$ObsValue[i])-1
  }}



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