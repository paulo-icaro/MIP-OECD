# ============================================ #
# === Input-Output Tables - OECD Countries === #
# ============================================ #

# --- Autor: Paulo Icaro --- #


# Link com a estrutura do dataset: https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/IOTS_2021
# Link onde os dados sao consultados: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/IOTS_2021
# Link para as matrizes no site da OCDE: https://stats.oecd.org/
# Link como commitar alteracoes direto no R: https://r-bio.github.io/intro-git-rstudio/
# Para multiplos comentarios: ctrl + shift + c
# Blog bastante interesssante: https://viniciusavale.com/NEDUR/IP-R.html#7_%C3%ADndices_de_liga%C3%A7%C3%A3o



# -------------------- #
# --- 1. Libraries --- #
# -------------------- #
library(openxlsx)                 # Escrever arquivos Excel
library(readxl)                   # Ler arrquivos Excel
library(tidyverse)                # Mainpulacao de dados


# ----------------- #
# --- 1.1 Paths --- #
# ----------------- #
#path = 'C:/Users/Paulo/Documents/Repositorios/MIP_OECD/'         # PC/Notebook
path = getwd()



# ------------------ #
# --- 2 Database --- #
# ------------------ #

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

dim_row_cod <- read_excel(path = paste0(path, '/Dataset/Dimensoes.xlsx'), sheet = "linha", col_names=TRUE, range = 'A1:A46')
dim_col_cod <- read_excel(path = paste0(path, '/Dataset/Dimensoes.xlsx'), sheet = "coluna", col_names=TRUE, range = 'A1:A46')

dim_row_name <- read_excel(path = paste0(path, '/Dataset/Dimensoes.xlsx'), sheet = "linha", col_names=TRUE, range = 'B1:B46') 
dim_col_name <- read_excel(path = paste0(path, '/Dataset/Dimensoes.xlsx'), sheet = "coluna", col_names=TRUE, range = 'B1:B46')
# dim_row_cod <- unique(database[[1]]) 
# dim_col_cod <- unique(database[[1]])


# Matriz Diagonal (I)
I = diag(x = 1, nrow = 45, ncol = 45)



# -------------------------------------------------------------------------------------- #
# --- 2.1 Preparacao da Database e Calculo de Indices de Ligacao, Dispersao e Tracao --- #
# -------------------------------------------------------------------------------------- #

# Loop Principal - Filtragem por Pais #
for (c in length(countries)){
  database <- read_excel(path = paste0(path, '/Dataset/Database_IOTS_Countries.xlsx'), sheet = countries[c], col_names=TRUE)
  database[c('ObsValue', 'Time')] <- sapply(database[c('ObsValue', 'Time')], as.numeric)                                              # Mudanca da tipagem das colunas especificadas para numeric
  
  # Loop Secundario - Filtragem por Ano #
  for (t in 1:24){
    
    # Separamento das bases
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
  
}




# ----------------- #
# --- 3 Savings --- #
# ----------------- #
# Atencao !!! - Rode este code apenas se desejar salvar os resultados
alias <- c('sectors', 'outputs', 'sectors_coef', 'value_added', 'exports',
           'imports', 'int_cons', 'household', 'government', 'investment', 
           'leontief', 'backward_linkages', 'foward_linkages', 'backward_dispersion', 'foward_dispersion', 'pull_index')

db <- list(db_sectors, db_outputs, db_sectors_coef, db_value_added, db_exports,
           db_imports, db_int_cons, db_household, db_government, db_investment, 
           db_leontief, backward_linkages, foward_linkages, backward_dispersion, foward_dispersion, pull_index
           
)

names(db) <- alias

for (c in 1:length(countries)){
  for (a in 1:length(alias)){
    wb <- createWorkbook(creator = 'pi')
    
    for (t in 1:24){
      addWorksheet(wb = wb, sheetName = paste0(alias[a], '_', (1994+t)))
      writeData(wb = wb, sheet = paste0(alias[a], '_', (1994+t)), x = db[[a]][[c]][[t]], rowNames = TRUE)
    }
    
    saveWorkbook(wb = wb, file = paste0(path, '/Results/', alias[a], '_', countries[c], '.xlsx'), overwrite = TRUE)
  }
}




# ------------------------- #
# --- 4 Ranking Setores --- #
# ------------------------- #
ranking_matrix_outputs = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))
ranking_matrix_backward_linkages = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))
ranking_matrix_foward_linkages = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))
ranking_matrix_backward_dispersion = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))
ranking_matrix_foward_dispersion = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))
ranking_matrix_pull_index = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col_cod), 1995:2018))
ranking_list = vector(mode = 'list')


ranking_alias = c('Ranking_Outputs',
                  'Ranking_Backward_Linkages',
                  'Ranking_Foward_Linkages',
                  'Ranking_Backward_Dispersion',
                  'Ranking_Foward_Dispersion',
                  'Ranking_Pull_Index')

for (c in 1:length(countries)){
  for (t in 1:24){
    ranking_outputs_sectors = 46 - rank(x = db[['outputs']][[c]][[t]])                                  # Ranking por Produto
    ranking_backward_linkages = 46 - rank(x = db[['backward_linkages']][[c]][[t]])                        # Ranking Indice de Ligacao para Tras
    ranking_foward_linkages = 46 - rank(x = db[['foward_linkages']][[c]][[t]])                            # Ranking Indice de Ligacao para Frente
    ranking_backward_dispersion = rank(x = db[['backward_dispersion']][[c]][[t]])                         # Ranking Indice de Dispersao para Tras
    ranking_foward_dispersion = rank(x = db[['foward_dispersion']][[c]][[t]])                             # Ranking Indice de Dispersao para Frente
    ranking_pull_index = 46 - rank(x = db[['pull_index']][[c]][[t]])                                      # Ranking Indice de Tracao
    
    ranking_matrix_outputs[,t] = ranking_outputs_sectors
    ranking_matrix_backward_linkages[,t] = ranking_backward_linkages
    ranking_matrix_foward_linkages[,t] = ranking_foward_linkages
    ranking_matrix_backward_dispersion[,t] = ranking_backward_dispersion
    ranking_matrix_foward_dispersion[,t] = ranking_foward_dispersion
    ranking_matrix_pull_index[,t] = ranking_pull_index
  }
  
  
  ranking_list[[c]] = list(ranking_outputs = ranking_matrix_outputs,
                           ranking_backward_linkages = ranking_matrix_backward_linkages,
                           ranking_foward_linkages = ranking_matrix_foward_linkages,
                           ranking_backward_dispersion = ranking_matrix_backward_dispersion,
                           ranking_foward_dispersion = ranking_matrix_foward_dispersion,
                           ranking_pull_index = ranking_matrix_pull_index)
  
  names(ranking_list)[c] = countries[c]
  
  
  wb = createWorkbook(creator = 'pi')
  
  for (a in 1:length(ranking_alias)){
    addWorksheet(wb = wb, sheetName = paste0(ranking_alias[a], '_', countries[c]))
    writeData(wb = wb, sheet = paste0(ranking_alias[a], '_', countries[c]), x = ranking_list[[c]][[a]], rowNames = TRUE)
  }
  saveWorkbook(wb = wb, file = paste0(path, paste0('/Results/mip_rankings', '_', countries[c], '.xlsx')), overwrite = TRUE)
}





# Liberando memoria
rm(database,
   
   database_sectors, database_outputs, database_value_added, database_exports, database_imports,
   database_int_cons, database_household, database_government, database_investment,
   
   db_sectors_matrix, db_outputs_matrix, db_sectors_coef_matrix, db_value_added_matrix, db_int_cons_matrix,
   db_household_matrix, db_investment_matrix, db_government_matrix, db_exports_matrix, db_imports_matrix,
   db_leontief_matrix, backward_linkages_matrix, foward_linkages_matrix, backward_dispersion_matrix, foward_dispersion_matrix,
   
   db_sectors, db_outputs, db_sectors_coef, db_value_added, db_exports,
   db_imports, db_int_cons, db_household, db_government, db_investment, 
   db_leontief, backward_linkages, foward_linkages, backward_dispersion, foward_dispersion, pull_index,
   
   inner_piece_bd, inner_piece_fd,
   
   remove_col, remove_row,
   
   t, w, c,
   
   I,
   
   max_backward_dispersion, max_foward_dispersion, ratio_backward, ratio_foward, pull_index_matrix,
   
   ranking_outputs_sectors, ranking_backward_linkages, ranking_foward_linkages,
   ranking_backward_dispersion, ranking_foward_dispersion, ranking_matrix_outputs,
   ranking_matrix_backward_linkages, ranking_matrix_foward_linkages,
   ranking_matrix_backward_dispersion, ranking_matrix_foward_dispersion, ranking_matrix_pull_index,
   
   ranking_alias, alias
)





# --- Indices de Ligacao Puros --- #
# Ajj = matrix(data = db_sectors_coef[[c]][[t]][1,1], nrow = 1, ncol = 1, dimnames = c(dim_row_cod[1,1], dim_col_cod[1,1]))
# Arr = matrix(data = db_sectors_coef[[c]][[t]][-1,-1], nrow = 44, ncol = 44, dimnames = c(dim_row_cod[-1,1], dim_col_cod[-1,1]))
# Arj = matrix(data = db_sectors_coef[[c]][[t]][2:45,1], nrow = 44, ncol = 1, dimnames = c(dim_row_cod[2:45,1], dim_col_cod[1,1]))
# Ajr = matrix(data = db_sectors_coef[[t]][[t]][1,2:45], nrow = 1, ncol = 44, dimnames = c(dim_row_cod[1,1], dim_col_cod[2:45,1]))
# 
# delta_j = matrix(data = (1-Ajj)^(-1), nrow = 1, ncol = 1, dimnames = c(dim_row_cod[1,1], dim_col_cod[1,1]))
# delta_r = matrix(data = solve(diag(x = 1, nrow = 44, ncol = 44) - Arr), nrow = 44, ncol = 44, dimnames = c(dim_row_cod[-1,1], dim_col_cod[-1,1]))
# 
# Yj = db_outputs[[c]][[t]][,i]
# Yr = db_outputs[[c]][[t]][,-i]
# 
# PBL = delta_r %% Arj %% delta_j %*% Yj
# PFL = delta_j %% Ajr %% delta_r %*% Yr
