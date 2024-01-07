# ============================================ #
# === Input-Output Tables - OECD Countries === #
# ============================================ #

# -------------------------- #
# --- Autor: Paulo Icaro --- #
# -------------------------- #

# Link com a estrutura do dataset: https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/IOTS_2021
# Link onde os dados sao consultados: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/IOTS_2021
# Link para as matrizes no site da OCDE: https://stats.oecd.org/
# Link como commitar alteracoes direto no R: https://r-bio.github.io/intro-git-rstudio/
# Para multiplos comentarios: ctrl + shift + c
# Blog bastante interesssante: https://viniciusavale.com/NEDUR/IP-R.html#7_%C3%ADndices_de_liga%C3%A7%C3%A3o

# Obs:
# Aparentemente, diferente de outras bases, nao da para visualizar essa no navegador devido ao limite
# do tempo de conexao. Mas e sim possivel acessar os dados utilizando a funcao get_dataset



# ----------------- #
# --- Libraries --- #
# ----------------- #
library(openxlsx)
library(readxl)
library(tidyverse)
library(extrafont) 
library(ggrepel)
library(plotly)
library(geomtextpath)


# --- Paths --- #
path = 'D:/Backup - Icaro/Documentos/Repositorios/'                     # PC
path = 'C:/Users/Paulo/Documents/Repositorios/'                         # Notebook
setwd(path)

# --- Funcao Cronometro --- #
source('Analises_Socioeconomicas/Scripts/Funcao - code_time.R', encoding = 'LATIN1')                # FunÃ§Ã£o que contabilizar o tempo do code // Se precisar use setwd para mudar o path raiz





# ---------------- #
# --- Database --- #
# ---------------- #

# --- Listas e colunas para armazenar dados tratados --- #
countries = c('BRA')

# Lista superiores (por pais)
db_sectors =
  db_outputs =
  db_sectors_coef =
  db_value_added =
  db_int_cons =
  db_household =
  db_investment =
  db_government =
  db_exports =
  db_imports =
  db_leontief =
  backward_linkages = 
  foward_linkages =
  backward_dispersion =
  foward_dispersion =
  pull_index =
  vector(mode = 'list', length = length(countries)) 


# Listas inferiores (por ano)
db_sectors_matrix =
  db_outputs_matrix =
  db_sectors_coef_matrix =
  db_value_added_matrix =
  db_int_cons_matrix = 
  db_household_matrix =
  db_investment_matrix = 
  db_government_matrix = 
  db_exports_matrix =
  db_imports_matrix =
  db_leontief_matrix =
  backward_linkages_matrix = 
  foward_linkages_matrix =
  backward_dispersion_matrix =
  foward_dispersion_matrix =
  pull_index_matrix =
  vector(mode = 'list', length = length(24))

#perc_change_oecd <- data.frame(matrix(nrow = 48600))                                            # Coluna que recebera as variacoes percentuais
#colnames(perc_change_oecd) <- c('perc_change')                                                  # Nome da coluna das variacoes percentuais


# Colunas e Linhas cujas combinacoes serao desconsideradas #
remove_col <- c('HFCE', 'NPISH', 'GGFC', 'GFCF', 'INVNT', 'CONS_ABR', 'CONS_NONRES', 'EXPO', 'IMPO')
remove_row <- c('TXS_IMP_FNL', 'TXS_INT_FNL', 'TTL_INT_FNL', 'VALU', 'OUTPUT')

dim_row_cod <- read_excel(path = paste0(path, 'MIP_OECD/Dimensões.xlsx'), sheet = "linha", col_names=TRUE, range = 'A1:A46')
dim_col_cod <- read_excel(path = paste0(path, 'MIP_OECD/Dimensões.xlsx'), sheet = "coluna", col_names=TRUE, range = 'A1:A46')

dim_row_name <- read_excel(path = paste0(path, 'MIP_OECD/Dimensões.xlsx'), sheet = "linha", col_names=TRUE, range = 'B1:B46') 
dim_col_name <- read_excel(path = paste0(path, 'MIP_OECD/Dimensões.xlsx'), sheet = "coluna", col_names=TRUE, range = 'B1:B46')
# dim_row_cod <- unique(database[[1]]) 
# dim_col_cod <- unique(database[[1]])


# Matriz Diagonal (I)
I = diag(x = 1, nrow = 45, ncol = 45)



# --- Preparacao da Database --- #

# Loop Principal - Filtragem por Pais #
start_time <- Sys.time()
for (c in length(countries)){
  database <- read_excel(path = paste0(path, 'MIP_OECD/Database_IOTS_Countries.xlsx'), sheet = countries[c], col_names=TRUE)
  database[c('ObsValue', 'Time')] <- sapply(database[c('ObsValue', 'Time')], as.numeric)                                              # Mudanca da tipagem das colunas especificadas para numeric
  
  # Loop Secundario - Filtragem por Ano #
  for (t in 1:24){
    
    # Separamento das bases

    database_sectors <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & !(ROW %in% remove_row) & (Time == 1994+t))       # Database Sectors // Remocao das combinacoes cujas variaveis nao serao de interesse
    database_outputs <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & (ROW == 'OUTPUT') & (Time == 1994+t))            # Database Outputs
    database_value_added <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & (ROW == 'VALU') & (Time == 1994+t))          # Database Added Value
    database_int_cons <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & (ROW == 'TTL_INT_FNL') & (Time == 1994+t))      # Database Intermediate Consumption
    database_household <- database[c(1,2,3,5,7)] %>% filter((COL == 'HFCE') & !(ROW %in% remove_row) & (Time == 1994+t))            # Database Households Consumption
    database_investment <- database[c(1,2,3,5,7)] %>% filter((COL == 'GFCF') & !(ROW %in% remove_row) & (Time == 1994+t))           # Database Investment
    database_government <- database[c(1,2,3,5,7)] %>% filter((COL == 'GGFC') & !(ROW %in% remove_row) & (Time == 1994+t))           # Database Government
    database_exports <- database[c(1,2,3,5,7)] %>% filter((COL == 'EXPO') & !(ROW %in% remove_row) & (Time == 1994+t))              # Database Exports
    database_imports <- database[c(1,2,3,5,7)] %>% filter((COL == 'IMPO') & !(ROW %in% remove_row) & (Time == 1994+t))              # Database Imports
    
    
    # Armazenamento das matrizes de dados temporais filtrados em listas
    db_sectors_matrix[[t]] <- matrix(data = as.matrix(database_sectors[3]), nrow = 45, ncol = 45, dimnames = c(dim_row_cod, dim_col_cod))                   # Matrix Sectors
    db_outputs_matrix[[t]] <- matrix(data = as.matrix(database_outputs[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Output"))                       # Matrix Outputs
    db_value_added_matrix[[t]] <- matrix(data = as.matrix(database_value_added[3]), nrow = 45 , ncol = 1, dimnames = c(dim_col_cod, 'Value Added'))         # Matrix Added Values
    db_int_cons_matrix[[t]] <- matrix(data = as.matrix(database_int_cons[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Intermediate Consumption"))   # Matrix Intermediate Consumption
    db_household_matrix[[t]] <- matrix(data = as.matrix(database_household[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Household"))                # Matrix Households Consumption
    db_investment_matrix[[t]] <- matrix(data = as.matrix(database_investment[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Investment"))             # Matrix Investment
    db_government_matrix[[t]] <- matrix(data = as.matrix(database_government[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Government"))             # Matrix Government
    db_exports_matrix[[t]] <- matrix(data = as.matrix(database_exports[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Exports"))                      # Matrix Exports
    db_imports_matrix[[t]] <- matrix(data = as.matrix(database_imports[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Imports"))                      # Matrix Imports

    database_sectors <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & !(ROW %in% remove_row) & (Time == 1994+t))         # Database Sectors // Remocao das combinacoes cujas variaveis nao serao de interesse
    database_outputs <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & (ROW == 'OUTPUT') & (Time == 1994+t))              # Database Outputs
    database_value_added <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & (ROW == 'VALU') & (Time == 1994+t))            # Database Added Value
    database_int_cons <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & !(ROW %in% remove_row) & (Time == 1994+t))        # Database Intermediate Consumption // Alternativa: (ROW == 'TTL_INT_FNL')
    database_household <- database[c(1,2,3,5,7)] %>% filter((COL == 'HFCE') & !(ROW %in% remove_row) & (Time == 1994+t))              # Database Households Consumption
    database_investment <- database[c(1,2,3,5,7)] %>% filter((COL == 'GFCF') & !(ROW %in% remove_row) & (Time == 1994+t))             # Database Investment
    database_government <- database[c(1,2,3,5,7)] %>% filter((COL == 'GGFC') & !(ROW %in% remove_row) & (Time == 1994+t))             # Database Government
    database_exports <- database[c(1,2,3,5,7)] %>% filter((COL == 'EXPO') & !(ROW %in% remove_row) & (Time == 1994+t))                # Database Exports
    database_imports <- database[c(1,2,3,5,7)] %>% filter((COL == 'IMPO') & !(ROW %in% remove_row) & (Time == 1994+t))                # Database Imports
    
    
    # Armazenamento das matrizes de dados temporais filtrados em listas
    db_sectors_matrix[[t]] <- matrix(data = as.matrix(database_sectors[3]), nrow = 45, ncol = 45, dimnames = c(dim_row_cod, dim_col_cod))                       # Matrix Sectors
    db_outputs_matrix[[t]] <- matrix(data = as.matrix(database_outputs[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Output"))                           # Matrix Outputs
    db_value_added_matrix[[t]] <- matrix(data = as.matrix(database_value_added[3]), nrow = 45 , ncol = 1, dimnames = c(dim_col_cod, 'Value Added'))             # Matrix Added Values
    db_int_cons_matrix[[t]] <- matrix(data = as.matrix(database_int_cons[3]), nrow = 45, ncol = 45)                                                             # Matrix Intermediate Consumption (1° Stage)
    db_int_cons_matrix[[t]] <- matrix(data = rowSums(x = db_int_cons_matrix[[t]]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Intermediate Consumption"))  # Matrix Intermediate Consumption (2° Stage)
    db_household_matrix[[t]] <- matrix(data = as.matrix(database_household[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Household"))                    # Matrix Households Consumption
    db_investment_matrix[[t]] <- matrix(data = as.matrix(database_investment[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Investment"))                 # Matrix Investment
    db_government_matrix[[t]] <- matrix(data = as.matrix(database_government[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Government"))                 # Matrix Government
    db_exports_matrix[[t]] <- matrix(data = as.matrix(database_exports[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Exports"))                          # Matrix Exports
    db_imports_matrix[[t]] <- matrix(data = as.matrix(database_imports[3]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, "Imports"))                          # Matrix Imports
    
    
    # Loop para calcular e armazenar os coeficientes
    for (w in 1:45){
      if (w == 1){
        db_sectors_coef_matrix[[t]] <- db_sectors_matrix[[t]][,w]/db_outputs_matrix[[t]][w,]
      }
      else{
        db_sectors_coef_matrix[[t]] <- cbind(db_sectors_coef_matrix[[t]], (db_sectors_matrix[[t]][,w]/db_outputs_matrix[[t]][w,]))
        if (w == 45){matrix(data = db_sectors_coef_matrix[[t]], nrow = 45, ncol = 45, dimnames = c(dim_row_cod, dim_col_cod))}
      }
    }
    
    
    
    # Matriz Leontief // (I - A)^(-1)
    db_leontief_matrix[[t]] = matrix(data = solve(I - db_sectors_coef_matrix[[t]]), nrow = 45, ncol = 45, dimnames = c(dim_row_cod, dim_col_cod))
    
    # Indice de Ligacao para Tras
    backward_linkages_matrix[[t]] = colMeans(x = db_leontief_matrix[[t]]) / mean(x = db_leontief_matrix[[t]])
    backward_linkages_matrix[[t]] = matrix(data = as.matrix(backward_linkages_matrix[[t]]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, 'Backward_Linkage'))
    
    # Indice de Ligacao para Frente
    foward_linkages_matrix[[t]] = rowMeans(x = db_leontief_matrix[[t]]) / mean(x = db_leontief_matrix[[t]])
    foward_linkages_matrix[[t]] = matrix(data = as.matrix(foward_linkages_matrix[[t]]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, 'Foward_Linkage'))
    
    # Indices de Dispersao
    for (w in 1:45){
      inner_piece_bd = sum((db_leontief_matrix[[t]][,w] - mean(x = db_leontief_matrix[[t]][,w]))^2)
      inner_piece_fd = sum((db_leontief_matrix[[t]][w,] - mean(x = db_leontief_matrix[[t]][w,]))^2)
      
      if (w == 1){
        backward_dispersion_matrix[[t]] = (inner_piece_bd/44)^(.5) / mean(x = db_leontief_matrix[[t]][,w])
        foward_dispersion_matrix[[t]] = (inner_piece_fd/44)^(.5) / mean(x = db_leontief_matrix[[t]][w,])
      }
      
      else{
        backward_dispersion_matrix[[t]] = cbind(backward_dispersion_matrix[[t]], (inner_piece_bd/44)^(.5) / mean(x = db_leontief_matrix[[t]][,w]))
        foward_dispersion_matrix[[t]] = cbind(foward_dispersion_matrix[[t]], (inner_piece_fd/44)^(.5) / mean(x = db_leontief_matrix[[t]][w,]))
        
        if (w == 45){
          backward_dispersion_matrix[[t]] = matrix(data = as.matrix(backward_dispersion_matrix[[t]]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, 'Backward_Dispersion'))
          foward_dispersion_matrix[[t]] = matrix(data = as.matrix(foward_dispersion_matrix[[t]]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, 'Foward_Dispersion'))
        }
      }
      
    }
    
    # Indices de Tracao
    max_backward_dispersion = max(backward_dispersion_matrix[[t]])
    max_foward_dispersion = max(backward_dispersion_matrix[[t]])
    
    ratio_backward = matrix(data = 1, nrow = 45, ncol = 1) - backward_dispersion_matrix[[t]]/max_backward_dispersion
    ratio_foward = matrix(data = 1, nrow = 45, ncol = 1) - foward_dispersion_matrix[[t]]/max_foward_dispersion
    
    pull_index_matrix[[t]] = backward_linkages_matrix[[t]] * ratio_backward + foward_linkages_matrix[[t]] * ratio_foward
    pull_index_matrix[[t]] = matrix(data = as.matrix(pull_index_matrix[[t]]), nrow = 45, ncol = 1, dimnames = c(dim_col_cod, 'Pull_Index'))
    
    
    # Renomeando os elementos da lista temporal. Cada elemento da lista recebera a data referente ao ano da matriz
    names(db_sectors_matrix)[t] =
      names(db_outputs_matrix)[t] =
      names(db_sectors_coef_matrix)[t] =
      names(db_value_added_matrix)[t] =
      names(db_int_cons_matrix)[t] =
      names(db_household_matrix)[t] =
      names(db_investment_matrix)[t] =
      names(db_government_matrix)[t] =
      names(db_exports_matrix)[t] =
      names(db_imports_matrix)[t] =
      names(db_leontief_matrix)[t] = 
      names(backward_linkages_matrix)[t] =
      names(foward_linkages_matrix)[t] =
      names(backward_dispersion_matrix)[t] =
      names(foward_dispersion_matrix)[t] =
      names(pull_index_matrix)[t] =
      (1994+t)
    
  }
  
  
  # Armazenamento das listas dentro da lista de paises. 
  db_sectors[[c]] = db_sectors_matrix
  db_outputs[[c]] = db_outputs_matrix
  db_sectors_coef[[c]] = db_sectors_coef_matrix
  db_value_added[[c]] = db_value_added_matrix
  db_int_cons[[c]] = db_int_cons_matrix
  db_household[[c]] = db_household_matrix
  db_investment[[c]] = db_investment_matrix
  db_government[[c]] = db_government_matrix
  db_exports[[c]] = db_exports_matrix
  db_imports[[c]] = db_imports_matrix
  db_leontief[[c]] = db_leontief_matrix
  backward_linkages[[c]] = backward_linkages_matrix
  foward_linkages[[c]] = foward_linkages_matrix
  backward_dispersion[[c]] = backward_dispersion_matrix
  foward_dispersion[[c]] = foward_dispersion_matrix
  pull_index[[c]] = pull_index_matrix
  
  
  # Renomeando os elementos da lista de paises. Cada lista de cada pais recebera o nome do pais respectivo
  names(db_sectors)[c] =
    names(db_outputs)[c] =
    names(db_sectors_coef)[c] =
    names(db_value_added)[c] =
    names(db_int_cons)[c] =
    names(db_household)[c] =
    names(db_investment)[c] =
    names(db_government)[c] =
    names(db_exports)[c] =
    names(db_imports)[c] =
    names(db_leontief)[c] =
    names(backward_linkages)[c] =
    names(foward_linkages)[c] =
    names(backward_dispersion)[c] =
    names(foward_dispersion)[c] =
    names(pull_index)[c] =
    countries[c]
  
  # Liberando memoria quando o ultimo pais for avaliado
  if (c == length(countries)){
    rm(database,
       
       database_sectors, database_outputs, database_value_added, database_int_cons, database_household,
       database_investment, database_government, database_exports, database_imports,
       
       db_sectors_matrix, db_outputs_matrix, db_sectors_coef_matrix, db_value_added_matrix, db_int_cons_matrix,
       db_household_matrix, db_investment_matrix, db_government_matrix, db_exports_matrix, db_imports_matrix,
       db_leontief_matrix, backward_linkages_matrix, foward_linkages_matrix, backward_dispersion_matrix, foward_dispersion_matrix,
       
       I,
       
       max_backward_dispersion, max_foward_dispersion, ratio_backward, ratio_foward, pull_index_matrix
       
    )
  }
  
}
end_time <- Sys.time()
code_time(start_time, end_time)     #Cronometro





# ------------------------------- #
# --- Data Analysis and Plots --- #
# ------------------------------- #


# --- Evolucao - Produto Agricola, Consumo Intermediario e Componentes da Demanda ---- #
#start_time = Sys.time()
for (c in 1:length(countries)){
  for (i in 1:45){
    for (t in 1:24){
      if (t == 1){
        output = db_outputs[[c]][[t]][i,1]
        int_cons = db_int_cons[[c]][[t]][i,1]
        household = db_household[[c]][[t]][i,1]
        investment = db_investment[[c]][[t]][i,1]
        government = db_government[[c]][[t]][i,1]
        exports = db_exports[[c]][[t]][i,1]
        imports = db_imports[[c]][[t]][i,1]
        
      }
      else{
        output = rbind(output, db_outputs[[c]][[t]][i,1])
        int_cons = rbind(int_cons, db_int_cons[[c]][[t]][i,1])
        household = rbind(household, db_household[[c]][[t]][i,1])
        investment = rbind(investment, db_investment[[c]][[t]][i,1])
        government = rbind(government, db_government[[c]][[t]][i,1])
        exports = rbind(exports, db_exports[[c]][[t]][i,1])
        imports = rbind(imports, db_imports[[c]][[t]][i,1])
      }
    }
    
    plot_variables_evolution =
      ggplot() +
      
      geom_line(data = as.data.frame(x = output), mapping = aes(x = 1995:2018, y = output, color = 'Produto'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = int_cons), mapping = aes(x = 1995:2018, y = int_cons, color = 'Cons. Intermediário'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = household), mapping = aes(x = 1995:2018, y = household, color = 'Cons. Famílias'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = investment), mapping = aes(x = 1995:2018, y = investment, color = 'Investimentos'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = government), mapping = aes(x = 1995:2018, y = government, color = 'Governo'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = exports), mapping = aes(x = 1995:2018, y = exports, color = 'Exportações'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = imports), mapping = aes(x = 1995:2018, y = imports, color = 'Importações'), linetype = 'dashed', linewidth = 0.7) +
      theme(title = element_text(family = 'Segoe UI', size = 14),
            text = element_text(family = 'Segoe UI', face = 'italic', size = 14),                 # Formatacao geral
            axis.title.y = element_text(size = 12.5 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 12.5, margin = margin(t = 15)),                    # Titulo do eixo x
            axis.text.x = element_text(angle = 30, margin = margin(t = 12), size = 12.5),         # Textos do eixo x 
            panel.background = element_rect(fill = '#F2F3F4')
      ) +
      scale_color_manual(breaks = c('Produto', 'Cons. Intermediário', 'Cons. Famílias', 'Investimentos', 'Governo', 'Exportações', 'Importações'),
                         values = c('#ff1a1a', '#5900cc', '#73e600', '#e63e00', '#333333', '#0035e6', '#24c8bf'),
                         labels = c('Produto', 'Cons. Intermediário', 'Cons. Famílias', 'Investimentos', 'Governo', 'Exportações', 'Importações'),
                         name = 'Variáveis') +
      scale_x_continuous(breaks = seq(1995, 2018, 2)) +
      scale_y_continuous(breaks = waiver(), n.breaks = 10) + 
      labs(title = 'Evolução - Variáveis Intermediárias e Finais - USD Milhões', subtitle = paste0('Setor: ', dim_row_name[i,1]), x = NULL, y = 'USD, Milhões')
    
    
    ggsave(filename = paste0('Evolução das Variáveis - Setor ', dim_row_cod[i, 1], '.png'),
           path = 'G:/Meu Drive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Plots/Evolucao_Variaveis',
           width = 3200,
           height = 1500,
           units = 'px'
    )
    
  }
  if (c == length(countries)){rm(output, int_cons, household, investment, government, exports, imports)}
}
#end_time = Sys.time()
#code_time(start_time, end_time)



# --- Ranking Setores --- #
ranking_matrix_output = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))
ranking_matrix_backward_linkages = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))
ranking_matrix_foward_linkages = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))
ranking_matrix_backward_dispersion = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))
ranking_matrix_foward_dispersion = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))
ranking_matrix_pull_index = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))

for (c in 1:length(countries)){
  for (t in 1:24){
    ranking_output_sectors = 46 - rank(x = db_outputs[[c]][[t]])                                  # Ranking por Produto
    ranking_backward_linkages = 46 - rank(x = backward_linkages[[c]][[t]])                        # Ranking Indice de Ligacao para Tras
    ranking_foward_linkages = 46 - rank(x = foward_linkages[[c]][[t]])                            # Ranking Indice de Ligacao para Frente
    ranking_backward_dispersion = rank(x = backward_dispersion[[c]][[t]])                         # Ranking Indice de Dispersao para Tras
    ranking_foward_dispersion = rank(x = foward_dispersion[[c]][[t]])                             # Ranking Indice de Dispersao para Frente
    ranking_pull_index = 46 - rank(x = pull_index[[c]][[t]])                                      # Ranking Indice de Tracao
    
    ranking_matrix_output[,t] = ranking_output_sectors
    ranking_matrix_backward_linkages[,t] = ranking_backward_linkages
    ranking_matrix_foward_linkages[,t] = ranking_foward_linkages
    ranking_matrix_backward_dispersion[,t] = ranking_backward_dispersion
    ranking_matrix_foward_dispersion[,t] = ranking_foward_dispersion
    ranking_matrix_pull_index[,t] = ranking_pull_index
    
  }
  
  if (c == length(countries)){
    ranking_list = list(ranking_matrix_output,
                        ranking_matrix_backward_linkages,
                        ranking_matrix_foward_linkages,
                        ranking_matrix_backward_dispersion,
                        ranking_matrix_foward_dispersion,
                        ranking_matrix_pull_index)
    
    rm(ranking_output_sectors, ranking_backward_linkages, ranking_foward_linkages,
       ranking_backward_dispersion, ranking_foward_dispersion, ranking_matrix_output,
       ranking_matrix_backward_linkages, ranking_matrix_foward_linkages,
       ranking_matrix_backward_dispersion, ranking_matrix_foward_dispersion, ranking_matrix_pull_index)
  }
}


wb = createWorkbook(creator = 'pi')
ranking_alias = c('Ranking_Outputs',
                  'Ranking_Backward_Linkages',
                  'Ranking_Foward_Linkages',
                  'Ranking_Backward_Dispersion',
                  'Ranking_Foward_Dispersion',
                  'Ranking_Pull_Index')
for (x in 1:length(ranking_list)){
  addWorksheet(wb = wb, sheetName = ranking_alias[x])
  writeData(wb = wb, sheet = ranking_alias[x], x = ranking_list[[x]], rowNames = TRUE)
}
saveWorkbook(wb = wb, file = paste0(path, 'MIP_OECD/Resultados/mip_rankings.xlsx'), overwrite = TRUE)



# --- Medias - Indice de Ligacao e Dispersao --- #
mean_bl = read_excel(path = 'MIP_OECD/Results/Análises.xlsx', sheet = 'Índices de Ligação', range = 'Z2:Z47')
mean_fl = read_excel(path = 'MIP_OECD/Results/Análises.xlsx', sheet = 'Índices de Ligação', range = 'Z52:Z97')

mean_linkages = cbind(mean_bl, mean_fl)
colnames(x = mean_linkages) = c('mean_bl', 'mean_fl')
rownames(x = mean_linkages) = as.matrix(dim_col_cod)

Plots_Linkages = ggplot(data = as.data.frame(x = mean_linkages), aes(x = mean_linkages[,1], y = mean_linkages[,2], label = rownames(x = mean_linkages))) +
  geom_point() + 
  geom_texthline(yintercept = 1, label = 'Indice de Ligação para Trás = 1', hjust = 0.02, vjust = -0.15) + 
  geom_textvline(xintercept = 1, label = 'Indice de Ligação para Frente = 1', hjust = 0.98, vjust = -0.15) +
  geom_label_repel(label.r = .2, min.segment.length = 0, fontface = 'italic', nudge_x = 0.03, nudge_y = 0.05) + 
  labs(title = 'Índices de Ligação (1995-2018)', 
       x = 'Índice de Ligação para Trás',
       y = 'Índice de Ligação para Frente') + 
  theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
        axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
        axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
        panel.background = element_rect(fill = '#F2F3F4')) +
  scale_x_continuous(breaks = seq(floor(min(mean_linkages[,1])), ceiling(max(mean_linkages[,1])), 0.1)) + 
  scale_y_continuous(breaks = seq(floor(min(mean_linkages[,2])), ceiling(max(mean_linkages[,2])), 0.5))



# --- Indices de Ligacao Puros --- #
Ajj = matrix(data = db_sectors_coef[[c]][[t]][1,1], nrow = 1, ncol = 1, dimnames = c(dim_row_cod[1,1], dim_col_cod[1,1]))
Arr = matrix(data = db_sectors_coef[[c]][[t]][-1,-1], nrow = 44, ncol = 44, dimnames = c(dim_row_cod[-1,1], dim_col_cod[-1,1]))
Arj = matrix(data = db_sectors_coef[[c]][[t]][2:45,1], nrow = 44, ncol = 1, dimnames = c(dim_row_cod[2:45,1], dim_col_cod[1,1]))
Ajr = matrix(data = db_sectors_coef[[t]][[t]][1,2:45], nrow = 1, ncol = 44, dimnames = c(dim_row_cod[1,1], dim_col_cod[2:45,1]))

delta_j = matrix(data = (1-Ajj)^(-1), nrow = 1, ncol = 1, dimnames = c(dim_row_cod[1,1], dim_col_cod[1,1]))
delta_r = matrix(data = solve(diag(x = 1, nrow = 44, ncol = 44) - Arr), nrow = 44, ncol = 44, dimnames = c(dim_row_cod[-1,1], dim_col_cod[-1,1]))

Yj = db_outputs[[c]][[t]][,i]
Yr = db_outputs[[c]][[t]][,-i]

PBL = delta_r %*% Arj %*% delta_j %*% Yj
PFL = delta_j %*% Ajr %*% delta_r %*% Yr






# --- Plots - Coeficientes --- #

# Em aes o argumento color e empregado de maneira nao usual.
# Ele e utilizado para definir uma especie de id que sera associada a uma cor na funcao scale_color_manual
# font_import(): importa todas as fontes do sistema
# loadfonts(device = 'win'): ler o banco de dados de fontes importado e os registra junto ao R
# windowsFonts(): para ver todos os tipos de fontes agora disponiveis (por default o R so possui Times New Roman, Arial e Courier New)
# Para mais sobre o assunto, ver: https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
start_time <- Sys.time()
for (c in length(countries)){                                                               # c para pais
  for (i in 1:45){                                                                           # i para linha
    for (j in 1:45){                                                                         # j para coluna
      for (t in 1:24){                                                                      # t para o ano
        if (t == 1) {
          w = db_sectors_coef[[c]][[t]][i:i,j:j]                                           # Gerar plot da serie de coeficientes
        } 
        else {
          w <- rbind(w, db_sectors_coef[[c]][[t]][i:i,j:j])
        }
      }
      Plots <- ggplot() + 
        
        # Caso deseje plotar para mais de um pais, basta copiar o trecho abaixo
        geom_line(data = as.data.frame(x = w),
                  #data = database[[c]] %>% filter(COL %in% dim_col_cod[c,1] & ROW %in% dim_row_cod[r,1]), 
                  aes(x = 1995:2018, y = w, color = 'Brazil'),
                  linetype = 'dashed',
                  size = .75) +
        geom_point(data = as.data.frame(x = w), x = 1995:2018, y = w) +
        scale_color_manual(breaks = c('Brazil'),#, 'South Korea'),
                           values = c('#45B39D'),#, '#D35400'),
                           labels(NULL)) +
        scale_x_continuous(breaks = seq(1995, 2018, 2)) +
        labs(title = 'Evolução do Coeficiente',
             subtitle = paste0(dim_row_cod[i,1], ' para ', dim_col_cod[j,1]),
             x = NULL,
             y = 'Coeficiente') +
        theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
              axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
              axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
              axis.text.x = element_text(angle = 45, margin = margin(t = 12), size = 15),         # Textos do eixo x 
              panel.background = element_rect(fill = '#F2F3F4')
        )
      
      ggsave(path = paste0('G:/Meu Drive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Plots/Coeficientes'),
             filename = paste0(dim_row_cod[i,1], ' para ', dim_col_cod[j,1], '.png'),
             width = 3000,
             height = 1300,
             units = 'px'
      )
    }
  }
}
end_time <- Sys.time()
code_time(start_time, end_time)





# --- Plots - Outputs --- #
start_time <- Sys.time()
for (c in length(countries)){                                                             # c para pais
  for (i in 1:45){                                                                        # j para coluna
    for (t in 1:24){                                                                      # t para o ano
      if (t == 1) {
        w = db_outputs[[c]][[t]][i,]                                         # Gerar plot da serie de valores da MIP // Obs: lembre que se esta extraindo valor de uma matriz e nao mais de um dataframe
      } 
      else {
        w = rbind(w, db_outputs[[c]][[t]][i,])
      }
    }
    Plots <- ggplot() + 
      
      # Caso deseje plotar para mais de um pais, basta copiar o trecho abaixo
      geom_line(data = as.data.frame(x = w),
                #data = database[[c]] %>% filter(COL %in% dim_col_cod[c,1] & ROW %in% dim_row_cod[r,1]), 
                aes(x = 1995:2018, y = w),
                linetype = 'dashed',
                size = .75) +
      geom_point(data = as.data.frame(x = w), x = 1995:2018, y = w) +
      scale_color_manual(breaks = NULL,
                         values = c('#45B39D'),#, '#D35400'),
                         labels(NULL)) +
      scale_x_continuous(breaks = seq(1995, 2018, 2)) +
      scale_y_continuous(breaks = waiver(), n.breaks = 10) + 
      labs(title = paste0('Evolução - Produto - USD Milhões'), subtitle = paste0('Setor: ', dim_row_name[i,1]), x = NULL, y = 'Produto') +
      theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
            axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
            axis.text.x = element_text(angle = 45, margin = margin(t = 12), size = 15),         # Textos do eixo x 
            panel.background = element_rect(fill = '#F2F3F4')
      )
    
    ggsave(path = paste0('G:/Meu Drive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Plots/Produtos'),
           filename = paste0('Evolucao_Produto_', dim_col_cod[i,1], '.png'),
           width = 3000,
           height = 1300,
           units = 'px'
    )
  }
}
end_time <- Sys.time()
code_time(start_time, end_time)





# --- Plots - Backward and Foward Linkages and Dispersion --- #
# https://www.statology.org/geom_vline-label/
start_time <- Sys.time() 
for(c in length(countries)){
  for(t in 1:24){
    
    linkages = cbind(backward_linkages[[c]][[t]], foward_linkages[[c]][[t]])
    Plots_Linkages = ggplot(data = as.data.frame(x = linkages), aes(x = linkages[,1], y = linkages[,2], label = rownames(linkages))) +
      geom_point() + 
      geom_texthline(yintercept = 1, label = 'Indice de Ligação para Trás = 1', hjust = 0.02, vjust = -0.15) + 
      geom_textvline(xintercept = 1, label = 'Indice de Ligação para Frente = 1', hjust = 0.98, vjust = -0.15) +
      geom_label_repel(label.r = .2, min.segment.length = 0, fontface = 'italic', nudge_x = 0.03, nudge_y = 0.05) + 
      labs(title = paste0('Índices de Ligação (', 1994+t, ')'), 
           x = 'Índice de Ligação para Trás',
           y = 'Índice de Ligação para Frente') + 
      theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
            axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
            panel.background = element_rect(fill = '#F2F3F4')) +
      scale_x_continuous(breaks = seq(floor(min(linkages[,1])), ceiling(max(linkages[,1])), 0.1)) + 
      scale_y_continuous(breaks = seq(floor(min(linkages[,2])), ceiling(max(linkages[,2])), 0.5))
    
    ggsave(plot = Plots_Linkages,
           path = paste0('G:/Meu Drive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Plots/Produtos'),
           filename = paste0('Indice_Ligacao (', 1994+t, ')', '.png'),
           width = 3200,
           height = 1500,
           units = 'px'
    )
    
    
    
    dispersion = cbind(backward_dispersion[[c]][[t]], foward_dispersion[[c]][[t]])
    Plots_Dispersion = ggplot(data = as.data.frame(x = dispersion), aes(x = dispersion[,1], y = dispersion[,2], label = rownames(dispersion))) +
      geom_point() + 
      geom_texthline(yintercept = 1, label = 'Indice de Dispersão para Trás = 1', hjust = 0.08, vjust = -0.15) + 
      geom_textvline(xintercept = 1, label = 'Indice de Dispersão para Frente = 1', hjust = 0.98, vjust = -0.15) +
      geom_label_repel(label.r = .2, min.segment.length = 0, fontface = 'italic', nudge_x = 0.03, nudge_y = 0.05) + 
      labs(title = paste0('Índices de Dispersão (', 1994+t, ')'), 
           x = 'Índice de Dispersão para Trás',
           y = 'Índice de Dispersão para Frente') + 
      theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
            axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
            panel.background = element_rect(fill = '#F2F3F4')) +
      scale_x_continuous(breaks = seq(floor(min(dispersion[,1])), ceiling(max(dispersion[,1])), 0.5)) + 
      scale_y_continuous(breaks = seq(floor(min(dispersion[,2])), ceiling(max(dispersion[,2])), 0.5))
    
    ggsave(plot = Plots_Dispersion,
           path = paste0('G:/Meu Drive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Plots/Produtos'),
           filename = paste0('Indice_Dispersao (', 1994+t, ')', '.png'),
           width = 3200,
           height = 1500,
           units = 'px'
    )
    
  }
}
end_time <- Sys.time()
code_time(start_time, end_time)






# --------------- #
# --- Savings --- #
# --------------- #

# AtenÃ§Ã£o !!! - Rode este code apenas se desejar salvar os resultados
alias <- c('mip_sectors',
           'mip_outputs',
           'mip_coef',
           'mip_value_added',
           'mip_leontief',
           'mip_backward_linkages',
           'mip_foward_linkages',
           'mip_backward_dispersion',
           'mip_foward_dispersion',
           'mip_pull_index'
)

db <- list(db_sectors,
           db_outputs,
           db_sectors_coef,
           db_value_added,
           db_leontief,
           backward_linkages,
           foward_linkages,
           backward_dispersion,
           foward_dispersion,
           pull_index
)

names(db) <- alias

for (c in countries){
  for (i in 1:length(alias)){
    wb <- createWorkbook(creator = 'pi')
    
    for (t in 1:24){
      addWorksheet(wb = wb, sheetName = paste0(alias[i], '_', (1994+t)))
      writeData(wb = wb, sheet = paste0(alias[i], '_', (1994+t)), x = db[[i]][[c]][[t]], rowNames = TRUE)
    }
    
    saveWorkbook(wb = wb, file = paste0(path, 'MIP_OECD/Resultados/', alias[i], '.xlsx'), overwrite = TRUE)
  }
}
