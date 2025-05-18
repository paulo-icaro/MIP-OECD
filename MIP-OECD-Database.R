# ============================================ #
# === INPUT-OUTPUT TABLES - OECD COUNTRIES === #
# ============================================ #

# --- Script by: Paulo Icaro --- #

# Link com a estrutura do dataset: https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/IOTS_2021
# Link onde os dados sao consultados: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/IOTS_2021
# Link para as matrizes no site da OCDE: https://stats.oecd.org/
# Link como commitar alteracoes direto no R: https://r-bio.github.io/intro-git-rstudio/
# Para multiplos comentarios: ctrl + shift + c

# Obs:
# Aparentemente, diferente de outras bases, nao da para visualizar essa no navegador devido ao limite
# do tempo de conexao. Mas e sim possivel acessar os dados utilizando a funcao get_dataset


# ==================== #
# === 1. Libraries === #
# ==================== #
library(OECD)                       # OECD API
library(openxlsx)                   # Escrever arquivos Excel

# --- Paths --- #
path = getwd()



# ========================== #
# === 2. Data Extraction === #
# ========================== #

# Na funcao get_dataset o argumento filter e uma lista para indicar as dimensoes do dataset que serao consultadas
# Obs: mudar a tipagem das colunas do dataframe (https://stackoverflow.com/questions/22772279/converting-multiple-columns-from-character-to-numeric-format-in-r)

# Aumentar o intervalo maximo de busca na URL do dataset
options(timeout = 500000)            
 
# Variavel com os nomes dos paises
# Lista com nomes dos paises por sigla: https://www.pucsp.br/~acomin/recursos/codpais.html
countries <- c('AUS', 'AUT', 'BEL', 'CAN', 'CHL', 'COL', 'CRI',
              'CZE', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC',
              'HUN', 'ISL', 'IRL', 'ISR', 'ITA', 'JPN', 'KOR',
              'LVA', 'LTU', 'LUX', 'MEX', 'NLD', 'NZL', 'NOR',
              'POL', 'PRT', 'SVK', 'SVN', 'ESP', 'SWE', 'CHE',
              'TUR', 'GBR', 'USA', 'ARG', 'BRA', 'BRN', 'BGR',
              'KHM', 'CHN', 'HRV', 'CYP', 'IND', 'IDN', 'HKG',
              'KAZ', 'LAO', 'MYS', 'MLT', 'MAR', 'MMR', 'PER',
              'PHL', 'ROU', 'RUS', 'SAU', 'SGP', 'ZAF', 'TWN',
              'THA', 'TUN', 'VNM')

# ---------------------- #
# --- 2.1 Extraction --- #
# ---------------------- #
wb <- createWorkbook(creator = 'pi')
for (c in 1:length(countries)){
     data_extraction <- get_dataset(dataset = "IOTS_2021", filter = list(c("TTL"), countries[c]), start_time = 1995, end_time = 2018)
     addWorksheet(wb = wb, sheetName = paste0(countries[c]))
     writeData(wb = wb, sheet = paste0(countries[c]), x = data_extraction)
   }
saveWorkbook(wb = wb, file = paste0(path, '/Dataset/', 'Database_IOTS_Countries_x.xlsx'), overwrite = TRUE)