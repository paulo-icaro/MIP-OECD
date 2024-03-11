# ======================================= #
# === Agriculture - Exports x Imports === #
# ======================================= #

# ================= #
# === 0. Coleta === #
# ================= #


# --- Autor: Paulo Icaro --- #





# ----------------- #
# --- Libraries --- #
# ----------------- #
library(readxl)
library(dplyr)
library(openxlsx)
path = getwd()


# ------------------- # 
# --- Dicionarios --- #
# ------------------- #

# Dicionario ISIC
dicio_isic = read_excel(path = paste0(path, '/Dataset/Dicionario_ISIC.xlsx'))
dicio_isic_agro = dicio_isic %>% filter(NO_ISIC_SECAO == 'Agropecuária')

# Dicionario ISIC x NCM
dicio_isic_ncm = read_excel(path = paste0(path, '/Dataset/Dicionario_NCM_ISIC.xlsx'))
dicio_isic_ncm_agro = dicio_isic_ncm %>% filter(CO_ISIC_CLASSE %in% dicio_isic_agro$CO_ISIC_CLASSE)





# --------------------- #
# --- Base de Dados --- #
# --------------------- #

# Exportacoes
expo = read.csv(file = paste0(path, '/Dataset/Exportacoes_Completas_(1997-2024).csv'), sep = ';')   # Base - Exportacoes
agro_expo = expo %>%  filter(CO_NCM %in% dicio_isic_ncm_agro$CO_NCM)                                # Base - Exportacoes - Agropecuaria
total_expo = expo[c(1,11)] %>% group_by(CO_ANO) %>% summarize(Total_Expo = sum(VL_FOB))             # Exporta?oes p/ Ano - Total
total_agro_expo = agro_expo[c(1,11)] %>% group_by(CO_ANO) %>% summarize(Total_Expo = sum(VL_FOB))   # Exportacoes p/ Ano - Agropecuaria
perc_expo_agro = total_agro_expo[c(2)]/total_expo[c(2)]                                         # Exportacoes (%) - Agropecuaria


# Importacoes
impo = read.csv(file = paste0(path, '/Dataset/Importacoes_Completas_(1997-2024).csv'), sep = ';')   # Base - Importacoes
agro_impo = impo %>%  filter(CO_NCM %in% dicio_isic_ncm_agro$CO_NCM)                                # Base - Importacoes - Agropecuaria
total_impo = impo[c(1,11)] %>% group_by(CO_ANO) %>% summarize(Total_Impo = sum(VL_FOB))             # Importacoes p/ Ano - Total
total_agro_impo = agro_impo[c(1,11)] %>% group_by(CO_ANO) %>% summarize(Total_Impo = sum(VL_FOB))   # Importacoes p/ Ano - Agropecuaria
perc_impo_agro = total_agro_impo[c(2)]/total_impo[c(2)]                                         # Importacoes (%) - Agropecuaria


# Tabela resumo
expo_impo = cbind(total_expo, total_agro_expo[c(2)], perc_expo_agro, total_impo[c(2)], total_agro_impo[c(2)], perc_impo_agro)
colnames(expo_impo) = c('Ano', 'Total Expo', 'Exp Agro', '% Expo', 'Total Impo', 'Imp Agro', '% Impo')
rownames(expo_impo) = expo_impo[,1]
expo_impo = expo_impo[,-1]


rm(dicio_isic, dicio_isic_agro, dicio_isic_ncm, expo, agro_expo, total_expo, total_agro_expo, perc_expo_agro, impo, agro_impo, total_impo, total_agro_impo, perc_impo_agro)


# Armazenando os resultados
wb = createWorkbook(creator = 'pi')
addWorksheet(wb = wb, sheetName = 'Expo_Impo')
writeData(wb = wb, sheet = 'Expo_Impo', x = expo_impo, rowNames = TRUE)
saveWorkbook(wb = wb, file = paste0(path, paste0('/Results/expo_impo_agriculture.xlsx')), overwrite = TRUE)
