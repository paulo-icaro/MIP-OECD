# ============================================ #
# === INPUT-OUTPUT TABLES - OECD COUNTRIES === #
# ============================================ #

# --- Script by: Paulo Icaro --- #


# ==================== #
# === 1. Libraries === #
# ==================== #
library(openxlsx)                 # Escrever arquivos Excel
library(readxl)                   # Ler arrquivos Excel
library(tidyverse)                # Mainpulacao de dados


# ----------------- #
# --- 1.1 Paths --- #
# ----------------- #
path = getwd()



# ================== #
# === 2 Database === #
# ================== #

# --- Listas e colunas para armazenafr dados tratados --- #
countries = c('BRA')


remove_col <- c('HFCE', 'NPISH', 'GGFC', 'GFCF', 'INVNT', 'CONS_ABR', 'CONS_NONRES', 'EXPO', 'IMPO')
remove_row <- c('TXS_IMP_FNL', 'TXS_INT_FNL', 'TTL_INT_FNL', 'VALU', 'OUTPUT')

dim_col_name <- read_excel(path = paste0(path, '/Dataset/Dimensoes.xlsx'), sheet = "coluna", col_names=TRUE, range = 'B1:B46')
dim_col_cod <- read_excel(path = paste0(path, '/Dataset/Dimensoes.xlsx'), sheet = "coluna", col_names=TRUE, range = 'A1:A46')

full_only_sectors_prod = NULL


for (c in length(countries)){
  database <- read_excel(path = paste0(path, '/Dataset/Database_IOTS_Countries.xlsx'), sheet = countries[c], col_names=TRUE)
  database[c('ObsValue', 'Time')] <- sapply(database[c('ObsValue', 'Time')], as.numeric)                                              # Mudanca da tipagem das colunas especificadas para numeric
  
  for(t in 1:24){
    sectors_prod = database[c(1,2,3,5,7)] %>% filter((COU == countries[c]) & !(ROW %in% remove_row) & (Time == 1994+t)) %>% group_by(ROW) %>% summarise(sector_prod = sum(ObsValue))
    inter_sectors_prod = database[c(1,2,3,5,7)] %>% filter((COU == countries[c]) & !(ROW %in% remove_row) & !(COL %in% remove_col) & (Time == 1994+t)) %>% group_by(ROW) %>% summarise(inter_sectors_prod = sum(ObsValue))
    only_sectors_perc = round(x = inter_sectors_prod[,2]/sectors_prod[,2], digits = 4)
    
    if(t == 1){full_only_sectors_prod = cbind(dim_col_cod, only_sectors_perc)}
    else{full_only_sectors_prod = cbind(full_only_sectors_prod, only_sectors_perc)}
  }
  
  colnames(full_only_sectors_prod) = c('Sector', 1995:2018)
}



# ================== #
# === 3. Savings === #
# ================== #
# wb = createWorkbook(creator = 'pi')
# addWorksheet(wb = wb, sheetName = 'participacao_produto_%')
# writeData(wb = wb, sheet = 'participacao_produto_%', x = full_only_sectors_prod, rowNames = FALSE)
# saveWorkbook(wb = wb , file = paste0(path, '/Results/Sheets/participacao_produto_%.xlsx'), overwrite = TRUE)



# ================ #
# === 4. Plots === #
# ================ #
w = t(full_only_sectors_prod[c(1, 6, 10, 11, 20, 25, 26, 36, 37, 38, 40, 41, 42),-1])
colnames(w) = t(dim_col_name[c(1, 6, 10, 11, 20, 25, 26, 36, 37, 38, 40, 41, 42),])
w = as.data.frame(w) %>% gather(key = 'Setor', value = 'value')
w = cbind(date = 1995:2018, w)

ggplot(data = w) + 
  geom_line(aes(x = date, y = value, color = Setor),
            linetype = 'dashed',
            linewidth = .5) +
  geom_line(aes(x = date, y = 0.5), color = 'black') + 
  geom_point(aes(x = date, y = value, color = Setor)) + 
  scale_x_continuous(breaks = seq(1995, 2018, 2)) +
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1, 1.1), labels = c('0%', '25%', '50%', '75%', '100%', '110%'), limits = c(0, 1.1)) + 
  #labs(title = paste0('Participação do Produto Setorial sobre o Produto Total do Setor')) +
  xlab(label = NULL) +
  ylab(label = NULL) +
  theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 14),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
        #axis.title.y = element_text(size = 14.5 , margin = margin(r = 15)),                   # Titulo do eixo y
        #axis.title.x = element_text(size = 14.5, margin = margin(t = 15)),                    # Titulo do eixo x
        axis.text.y = element_text(size = 11.5),                                              # Textos do eixo y
        axis.text.x = element_text(angle = 45, margin = margin(t = 12), size = 11.5),         # Textos do eixo x 
        panel.background = element_rect(fill = '#F2F3F4'),
        legend.position = 'top',
        legend.justification = 'left',
        legend.text = element_text(size = 11),
        legend.title = NULL,
        legend.title.position = 'top') +
  guides(colour = guide_legend(direction = "horizontal", ncol = 2, nrow = 7))                         # Quebrar o texto da legenda em n colunas por m colunas

ggsave(path = paste0('C:/Users/Paulo/Documents/Repositorios/MIP_OECD/Results/Plots/Produtos'),
       filename = paste0('Participacao_Produto_Setorial.png'),
       width = 3000,
       height = 2000,
       units = 'px'
)



# =================== #
# === 5. Cleasing === #
# =================== #
rm(database, countries, c, t, sectors_prod, inter_sectors_prod, only_sectors_perc, dim_col_cod, dim_row_cod)