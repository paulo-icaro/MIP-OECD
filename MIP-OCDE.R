# ============================================ #
# === Input-Output Tables - OECD Countries === #
# ============================================ #

# Link com a estrutura do dataset: https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/IOTS_2021
# Link onde os dados são consultados: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/IOTS_2021

# Obs:
# Aparentemente, diferente de outras bases, não dá para visualizar essa no navegador devido ao limite
# do tempo de conexão. Mas é sim possível acessar os dados utilizando a função get_dataset



# ---------------- #
# --- Packages --- #
# ---------------- #
install.packages('OECD')


# ----------------- #
# --- Libraries --- #
# ----------------- #
library(OECD)                                                 # OECD API


# ----------------------- #
# --- Data Extraction --- #
# ----------------------- #
# Na funcao get_dataset o argumento filter e uma lista para indicar as dimensoes do dataset que serao consultadas

options(timeout = 120)                                        # Aumentar o intervalo maximo de busca na URL do dataset
paises <- c('BRA', 'KOR')


database <- vector(mode = 'list', length = length(paises))    # Lista que receberá as databases


for (i in 1:length(paises)){
  data_extraction <- get_dataset(dataset = "IOTS_2021",
                                 filter = list(c("TTL"), paises[i]),
                                 start_time = 1995,
                                 end_time = 2018)
  database[[i]] <- data_extraction
  rm(data_extraction)
}


