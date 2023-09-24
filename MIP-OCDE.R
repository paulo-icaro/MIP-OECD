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


# --- Paths --- #
path = 'D:/Backup - Icaro/Documentos/Repositorios/'                     # PC
path = 'C:/Users/Paulo/Documents/Repositorios/'                         # Notebook
setwd(path)

# --- Funcao Cronometro --- #
source('RAIS/Função - code_time.R', encoding = 'LATIN1')                # Função que contabilizar o tempo do code // Se precisar use setwd para mudar o path raiz





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
  vector(mode = 'list', length = length(24))

#perc_change_oecd <- data.frame(matrix(nrow = 48600))                                            # Coluna que recebera as variacoes percentuais
#colnames(perc_change_oecd) <- c('perc_change')                                                  # Nome da coluna das variacoes percentuais


# Colunas e Linhas cujas combinacoes serao desconsideradas #
remove_col <- c('HFCE', 'NPISH', 'GGFC', 'GFCF', 'INVNT', 'CONS_ABR', 'CONS_NONRES', 'EXPO', 'IMPO')
remove_row <- c('TXS_IMP_FNL', 'TXS_INT_FNL', 'TTL_INT_FNL', 'VALU', 'OUTPUT')
dim_row <- read_excel(path = paste0(path, 'MIP_OECD/Dimensões.xlsx'), sheet = "linha", col_names=TRUE) %>% filter(!ROW %in% remove_row)
dim_col <- read_excel(path = paste0(path, 'MIP_OECD/Dimensões.xlsx'), sheet = "coluna", col_names=TRUE) %>% filter(!COL %in% remove_col)
# dim_row <- unique(database[[1]]) 
# dim_col <- unique(database[[1]])


# Matriz Diagonal (I)
I = diag(x = 1, nrow = 45, ncol = 45)



# --- Preparacao da Database --- #

# Loop Principal - Filtragem por Pais #
start_time <- Sys.time()
for (c in 1:length(countries)){
  database <- read_excel(path = paste0(path, 'MIP_OECD/Database_IOTS_Countries.xlsx'), sheet = countries[c], col_names=TRUE)
  database[c('ObsValue', 'Time')] <- sapply(database[c('ObsValue', 'Time')], as.numeric)                                              # Mudanca da tipagem das colunas especificadas para numeric
  
  # Loop Secundario - Filtragem por Ano #
  for (t in 1:24){
    database_sectors <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & !(ROW %in% remove_row) & (Time == 1994+t))         # Database Sectors // Remocao das combinacoes cujas variaveis nao serao de interesse
    database_outputs <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & (ROW == 'OUTPUT') & (Time == 1994+t))              # Database Outputs
    database_value_added <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & (ROW == 'VALU') & (Time == 1994+t))            # Database Added Value
    database_int_cons <- database[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & (ROW == 'TTL_INT_FNL') & (Time == 1994+t))        # Database Intermediate Consumption
    database_household <- database[c(1,2,3,5,7)] %>% filter((COL == 'HFCE') & !(ROW %in% remove_row) & (Time == 1994+t))              # Database Households Consumption
    database_investment <- database[c(1,2,3,5,7)] %>% filter((COL == 'GFCF') & !(ROW %in% remove_row) & (Time == 1994+t))             # Database Investment
    database_government <- database[c(1,2,3,5,7)] %>% filter((COL == 'GGFC') & !(ROW %in% remove_row) & (Time == 1994+t))             # Database Government
    database_exports <- database[c(1,2,3,5,7)] %>% filter((COL == 'EXPO') & !(ROW %in% remove_row) & (Time == 1994+t))                # Database Exports
    database_imports <- database[c(1,2,3,5,7)] %>% filter((COL == 'IMPO') & !(ROW %in% remove_row) & (Time == 1994+t))                # Database Imports
    
    
    # Tratando possiveis valores nulos
    # for (k in 1:2025){
    #   if (database_sectors[3][k,1] == 0){ 
    #     database_sectors[3][k,1] <-  1/(10^10)
    #   }
    # }
    # 
    # for (k in 1:45){
    #   if (database_outputs[3][k,1] == 0){ 
    #     database_outputs[3][k,1] <-  1/(10^10)
    #   }
    # }
    
    
    # Armazenamento das matrizes de dados temporais filtrados em listas
    db_sectors_matrix[[t]] <- matrix(data = as.matrix(database_sectors[3]), nrow = 45, ncol = 45, dimnames = c(dim_row, dim_col))                       # Sectors
    db_outputs_matrix[[t]] <- matrix(data = as.matrix(database_outputs[3]), nrow = 1, ncol = 45, dimnames = c("Output", dim_col))                       # Outputs
    db_value_added_matrix[[t]] <- matrix(data = as.matrix(database_value_added[3]), nrow = 1 , ncol = 45, dimnames = c('Value Added', dim_col))         # Added Values
    db_int_cons_matrix[[t]] <- matrix(data = as.matrix(database_int_cons[3]), nrow = 1, ncol = 45, dimnames = c("Intermediate Consumption", dim_col))   # Intermediate Consumption
    db_household_matrix[[t]] <- matrix(data = as.matrix(database_household[3]), nrow = 1, ncol = 45, dimnames = c("Household", dim_row))                # Households Consumption
    db_investment_matrix[[t]] <- matrix(data = as.matrix(database_investment[3]), nrow = 1, ncol = 45, dimnames = c("Investment", dim_row))             # Investment
    db_government_matrix[[t]] <- matrix(data = as.matrix(database_government[3]), nrow = 1, ncol = 45, dimnames = c("Government", dim_row))             # Government
    db_exports_matrix[[t]] <- matrix(data = as.matrix(database_exports[3]), nrow = 1, ncol = 45, dimnames = c("Exports", dim_row))                      # Exports
    db_imports_matrix[[t]] <- matrix(data = as.matrix(database_imports[3]), nrow = 1, ncol = 45, dimnames = c("Imports", dim_row))                      # Imports
    
    
    # Loop para calcular e armazenar os coeficientes
    for (i in 1:45){
      if (i == 1){
        db_sectors_coef_matrix[[t]] <- db_sectors_matrix[[t]][1:45,i]/db_outputs_matrix[[t]][,i]
      }
      else{
        db_sectors_coef_matrix[[t]] <- cbind(db_sectors_coef_matrix[[t]], (db_sectors_matrix[[t]][1:45,i]/db_outputs_matrix[[t]][,i]))
        if (i == 45){colnames(db_sectors_coef_matrix[[t]]) <- t(dim_col)}
      }
    }
    
    
    
    # Matriz Leontief // (I - A)^(-1)
    db_leontief_matrix[[t]] = matrix(data = solve(I - db_sectors_coef_matrix[[t]]), nrow = 45, ncol = 45, dimnames = c(dim_row, dim_col))
    
    # Indice de Ligacao para Tras
    backward_linkages_matrix[[t]] = (colSums(x = db_leontief_matrix[[t]])/45) / mean(x = db_leontief_matrix[[t]])
    
    # Indice de Ligacao para Frente
    foward_linkages_matrix[[t]] = (rowSums(x = db_leontief_matrix[[t]])/45) / mean(x = db_leontief_matrix[[t]])
    
    
    
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
    countries[c]
  
  # Liberando memoria quando o ultimo pais for avaliado
  if (c == length(countries)){
    rm(database,

       database_sectors, database_outputs, database_value_added, database_int_cons, database_household,
       database_investment, database_government, database_exports, database_imports,
       
       db_sectors_matrix, db_outputs_matrix, db_sectors_coef_matrix, db_value_added_matrix, db_int_cons_matrix,
       db_household_matrix, db_investment_matrix, db_government_matrix, db_exports_matrix, db_imports_matrix,
       db_leontief_matrix, backward_linkages_matrix, foward_linkages_matrix,
       
       I
    )
  }
  
}
end_time <- Sys.time()
code_time(start_time, end_time)     #Cronometro





# ------------------------------- #
# --- Data Analysis and Plots --- #
# ------------------------------- #


# --- Evolucao - Produto Agricola, Consumo Intermediario e Componentes da Demanda ---- #
start_time = Sys.time()
for (c in 1:length(countries)){
  for (i in 1:45){
    for (t in 1:24){
      if (t == 1){
        output = db_outputs[[c]][[t]][1,i]
        int_cons = db_int_cons[[c]][[t]][1,i]
        household = db_household[[c]][[t]][1,i]
        investment = db_investment[[c]][[t]][1,i]
        government = db_government[[c]][[t]][1,i]
        exports = db_exports[[c]][[t]][1,i]
        imports = db_imports[[c]][[t]][1,i]
        
      }
      else{
        output = rbind(output, db_outputs[[c]][[t]][1,i])
        int_cons = rbind(int_cons, db_int_cons[[c]][[t]][1,i])
        household = rbind(household, db_household[[c]][[t]][1,i])
        investment = rbind(investment, db_investment[[c]][[t]][1,i])
        government = rbind(government, db_government[[c]][[t]][1,i])
        exports = rbind(exports, db_exports[[c]][[t]][1,i])
        imports = rbind(imports, db_imports[[c]][[t]][1,i])
      }
    }
    
    plot_variables_evolution =
      ggplot() +
      
      geom_line(data = as.data.frame(x = output), mapping = aes(x = 1995:2018, y = output, color = 'Output'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = int_cons), mapping = aes(x = 1995:2018, y = int_cons, color = 'Intermediate Cons.'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = household), mapping = aes(x = 1995:2018, y = household, color = 'Households Cons.'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = investment), mapping = aes(x = 1995:2018, y = investment, color = 'Investments'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = government), mapping = aes(x = 1995:2018, y = government, color = 'Government'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = exports), mapping = aes(x = 1995:2018, y = exports, color = 'Exports'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = imports), mapping = aes(x = 1995:2018, y = imports, color = 'Imports'), linetype = 'dashed', linewidth = 0.7) +
      theme(title = element_text(family = 'Segoe UI', size = 14),
            text = element_text(family = 'Segoe UI', face = 'italic', size = 14),                 # Formatacao geral
            axis.title.y = element_text(size = 12.5 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 12.5, margin = margin(t = 15)),                    # Titulo do eixo x
            axis.text.x = element_text(angle = 30, margin = margin(t = 12), size = 12.5),         # Textos do eixo x 
            panel.background = element_rect(fill = '#F2F3F4')
      ) +
      scale_color_manual(breaks = c('Output', 'Intermediate Cons.', 'Households Cons.', 'Investments', 'Government', 'Exports', 'Imports'),
                         values = c('#ff1a1a', '#5900cc', '#73e600', '#e63e00', '#333333', '#0035e6', '#24c8bf'),
                         labels = c('Output', 'Intermediate Cons.', 'Households Cons.', 'Investments', 'Government', 'Exports', 'Imports'),
                         name = 'Variables') +
      scale_x_continuous(breaks = seq(1996, 2018, 2)) +
      labs(title = 'Evolution - Intermediate and Final Variables', subtitle = paste0('Setor: ', dim_row[i,1]), x = NULL, y = 'US Dolar, Millions')
    
    
    ggsave(filename = paste0('Variables Evolution - Sector ', dim_row[i, 1], '.png'),
           path = paste0(path, 'MIP_OECD/Plots/Evolucao_Variaveis'),
           width = 3200,
           height = 1500,
           units = 'px'
    )
    
    ggsave(filename = paste0('Variables Evolution - Sector ', dim_row[i, 1], '.png'),
           path = 'G:/Meu Drive/Arquivos para estudo da UFC/Doutorado/1° Semestre/Economia Regional/Projeto/Plots/Evolucao_Variaveis',
           width = 3200,
           height = 1500,
           units = 'px'
    )
    
  }
  if (c == length(countries)){rm(output, int_cons, household, investment, government, exports, imports)}
}
end_time = Sys.time()
code_time(start_time, end_time)



# --- Ranking Setores por Produto --- #

ranking_matrix_output = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col), 1995:2018))
ranking_matrix_backward_linkages = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col), 1995:2018))
ranking_matrix_foward_linkages = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_col), 1995:2018))

for (c in 1:length(countries)){
  for (t in 1:24){
    ranking_output_sectors = 46 - rank(x = t(db_outputs[[c]][[t]]))
    ranking_backward_linkages = 46 - rank(x = t(backward_linkages[[c]][[t]]))
    ranking_foward_linkages = 46 - rank(x = t(foward_linkages[[c]][[t]]))
    
    ranking_matrix_output[,t] = ranking_output_sectors
    ranking_matrix_backward_linkages[,t] = ranking_backward_linkages
    ranking_matrix_backward_linkages[,t] = ranking_foward_linkages

  }
  
  if (c == length(countries)){rm(ranking_output_sectors, ranking_backward_linkages, ranking_foward_linkages)}
}




# --- Indices de Ligacao Puros --- #
Ajj = matrix(data = db_sectors_coef[[c]][[t]][1,1], nrow = 1, ncol = 1, dimnames = c(dim_row[1,1], dim_col[1,1]))
Arr = matrix(data = db_sectors_coef[[c]][[t]][-1,-1], nrow = 44, ncol = 44, dimnames = c(dim_row[-1,1], dim_col[-1,1]))
Arj = matrix(data = db_sectors_coef[[c]][[t]][2:45,1], nrow = 44, ncol = 1, dimnames = c(dim_row[2:45,1], dim_col[1,1]))
Ajr = matrix(data = db_sectors_coef[[t]][[t]][1,2:45], nrow = 1, ncol = 44, dimnames = c(dim_row[1,1], dim_col[2:45,1]))

delta_j = matrix(data = (1-Ajj)^(-1), nrow = 1, ncol = 1, dimnames = c(dim_row[1,1], dim_col[1,1]))
delta_r = matrix(data = solve(diag(x = 1, nrow = 44, ncol = 44) - Arr), nrow = 44, ncol = 44, dimnames = c(dim_row[-1,1], dim_col[-1,1]))

Yj = db_outputs[[c]][[t]][,i]
Yr = db_outputs[[c]][[t]][,-i]

PBL = delta_r %*% Arj %*% delta_j %*% Yj
PFL = delta_j %*% Ajr %*% delta_r %*% Yr


# --- Autovalores --- #
eigenvalues <- vector(mode = 'list', length = length(countries))                                                                                    # Lista para armazenar os autovalores ao longo dos anos para cada pais

# Atencao: na analise realizada caso o autovalor seja um numero complexo a parte imaginaria e descartada
for (c in length(countries)){
  for (t in 1:24){
    if (t == 1){
      eigenvalues[[c]] <- Re(eigen(db_sectors_coef[[c]][[t]])$values)
    }
    
    else{
      eigenvalues[[c]] <- cbind(eigenvalues[[c]], Re(eigen(db_sectors_coef[[c]][[t]])$values))
      if(t == 24){colnames(eigenvalues[[c]]) <- (1994 + 1:24)}
    }
  }
  names(eigenvalues)[c] <- countries[c]
}

# --- Plots - Eigenvalues --- #
# for (c in length(countries)){
#   for (i in 1:45){
#     w = matrix(data = eigenvalues[[c]][i,], nrow = 24, ncol = 1)
#     Plots <- ggplot() +
#       geom_line(data = as.data.frame(x = w), aes(x = 1995:2018, y = w, color = 'Brazil'), linetype = 'dashed', linewidth = .75) +
#       geom_point(data = as.data.frame(x = w), x = 1995:2018, y = w) +
#       scale_color_manual(breaks = c('Brazil'), values = c('#45B39D'), labels(NULL)) +
#       scale_x_continuous(breaks = seq(1995, 2018, 2)) +
#       labs(title = paste0('Time Evolution: Eigenvalue #', i), x = NULL, y = 'Parameter') +
#       theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),           # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
#             axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
#             axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
#             axis.text.x = element_text(angle = 45, margin = margin(t = 12), size = 15),         # Textos do eixo x 
#             panel.background = element_rect(fill = '#F2F3F4')
#       )
#     
#     ggsave(path = paste0(path, 'MIP-OECD/Plots/Autovalores'), filename = paste0('Eigenvalue #', i, '.png'), width = 3000, height = 1300, units = 'px')
#     ggsave(path = 'G:/Meu Drive/Arquivos para estudo da UFC/Doutorado/1° Semestre/Economia Regional/Projeto/Plots/Autovalores', filename = paste0('Eigenvalue #', i, '.png'), width = 3000, height = 1300, units = 'px')
#     
#   }
# }



# --- PCA Analysis --- #
for (c in 1:length(countries)){
  for (t in 1:24){
    pca <- prcomp(x = db_sectors[[c]][[t]])
    pca_scores <- pca$x
    pca_variance_explained <- (pca$sdev^2) / sum(pca$sdev^2)
    pca_loadings <- pca$rotation
    
    
    pca_scores_df <- as.data.frame(pca_scores[c(-1), c(1,2)])
    pca_variance_explained_df <- as.data.frame(pca_variance_explained)
    pca_loadings_df <- as.data.frame(pca_loadings)
    rm(pca_scores, pca_variance_explained, pca_loadings)
    
    pca_variance_explained_plot <- 
      ggplot() +
      geom_col(data = pca_variance_explained_df, aes(y = pca_variance_explained*100 , x = 1:45)) + 
      labs(title = 'Explained variance by each PC', x = 'Principal Component', y = '%')
    
    ggsave(path = paste0(path, 'MIP-OECD/Plots/Componentes Principais/Variânca Explicada'),
           filename = paste0('Variancia_Explicada_PCA_', countries[c], '_', 1994+t,'.png'),
           width = 3000,
           height = 1300,
           units = 'px'
    )
    
    
    # --- Plots - PCA Analysis --- #
    # pca_biplot <-
    #   ggplot() +
    #   geom_point(data = pca_scores_df, aes(x = PC1, y = PC2), colour = '#002BFF') +
    #   geom_segment(data = pca_loadings_df*10000, aes(x = 0, y = 0, xend = PC1, yend = PC2), colour = '#000000') +
    #   #geom_label_repel(aes(label = rownames(pca_scores_df), x = pca_scores_df$PC1, y = pca_scores_df$PC2), box.padding = 0.35, point.padding = 0.75, segment.color = 'grey50') +
    #   geom_text_repel(aes(label = rownames(pca_scores_df), x = pca_scores_df$PC1, y = pca_scores_df$PC2), nudge_x = 0.6, nudge_y = 0.6, colour = '#002BFF') +                                                 # Use este comando para caso deseje usar o ggplotly        
    #   geom_text_repel(aes(label = rownames(pca_loadings_df), x = (pca_loadings_df*10000)$PC1, y = (pca_loadings_df*10000)$PC2), nudge_x = 0.6, nudge_y = 0.6, colour = '#000000') +                           # Use este comando para caso deseje usar o ggplotly        
    #   scale_y_continuous(name = 'Scores (PC2)', sec.axis = sec_axis(trans = ~.*(0.0001), name = 'Loadings (PC2)')) + 
    #   scale_x_continuous(name = 'Scores (PC1)', sec.axis = sec_axis(trans = ~.*(0.0001), name = 'Loadings (PC1)'))
    # 
    # ggsave(path = paste0(path, 'MIP-OECD/Plots/Componentes Principais/Biplot'),
    #        filename = paste0('Biplot_PCA_Zoomed_', countries[c], '_', 1994+t,'.png'),
    #        width = 3000,
    #        height = 1500,
    #        units = 'px'
    # )
    
    
    
    #Graficos dinamicos
    #ggplotly(pca_variance_explained_plot)
    #ggplotly(pca_biplot)
    #pca_biplot
  }
}


# Gráfico de correlação entre as variáveis originais e os componentes principais
correlation <- cor(db_sectors[[1]][[1]], pca_scores)
plot(correlation, xlab = "Variáveis Originais", ylab = "Componentes Principais", main = "Análise de Componentes Principais - Correlação")



# --- Percentage Changes --- #      
# Obs: ja foi incluido o salvamento dos dados analisados.
#wb_perc_change  = createWorkbook(creator = 'pi')
for (c in length(countries)){
  for (i in 2:48600){
    if(i%%24 != 1){
      database[[c]]$perc_change[i] <- (database[[c]]$ObsValue[i]/database[[c]]$ObsValue[i-1])-1
    }
    else {database[[c]]$perc_change[i] = NA}
  }
  #addWorksheet(wb = wb_perc_change, sheetName = paises[p])
  #writeData(wb = wb_perc_change, sheet = paises[p], x = database[[p]])
}
#saveWorkbook(wb = wb_perc_change, file = paste0(path, 'Variacoes_Percentuais.xlsx'), overwrite = TRUE)



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
        
        # Caso deseje plotar para mais de um país, basta copiar o trecho abaixo
        geom_line(data = as.data.frame(x = w),
                  #data = database[[c]] %>% filter(COL %in% dim_col[c,1] & ROW %in% dim_row[r,1]), 
                  aes(x = 1995:2018, y = w, color = 'Brazil'),
                  linetype = 'dashed',
                  size = .75) +
        geom_point(data = as.data.frame(x = w), x = 1995:2018, y = w) +
        scale_color_manual(breaks = c('Brazil'),#, 'South Korea'),
                           values = c('#45B39D'),#, '#D35400'),
                           labels(NULL)) +
        scale_x_continuous(breaks = seq(1995, 2018, 2)) +
        labs(title = 'Coeficient Evolution',
             subtitle = paste0('From ', dim_row[i,1], ' to ', dim_col[j,1]),
             x = NULL,
             y = 'Coeficient') +
        theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
              axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
              axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
              axis.text.x = element_text(angle = 45, margin = margin(t = 12), size = 15),         # Textos do eixo x 
              panel.background = element_rect(fill = '#F2F3F4')
        )
      
      ggsave(path = paste0(path, 'MIP-OECD/Plots/Coeficientes'),
             filename = paste0('From ', dim_row[i,1], ' to ', dim_col[j,1], '.png'),
             width = 3000,
             height = 1300,
             units = 'px'
      )
      
      ggsave(path = paste0('G:/Meu Drive/Arquivos para estudo da UFC/Doutorado/1° Semestre/Economia Regional/Projeto/Plots/Coeficientes'),
             filename = paste0('From ', dim_row[i,1], ' to ', dim_col[j,1], '.png'),
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
  for (j in 1:45){                                                                        # j para coluna
    for (t in 1:24){                                                                      # t para o ano
      if (t == 1) {
        w = db_outputs[[c]][[t]][,j]                                         # Gerar plot da serie de valores da MIP // Obs: lembre que se esta extraindo valor de uma matriz e nao mais de um dataframe
      } 
      else {
        w = rbind(w, db_outputs[[c]][[t]][,j])
      }
    }
    Plots <- ggplot() + 
      
      # Caso deseje plotar para mais de um país, basta copiar o trecho abaixo
      geom_line(data = as.data.frame(x = w),
                #data = database[[c]] %>% filter(COL %in% dim_col[c,1] & ROW %in% dim_row[r,1]), 
                aes(x = 1995:2018, y = w),
                linetype = 'dashed',
                size = .75) +
      geom_point(data = as.data.frame(x = w), x = 1995:2018, y = w) +
      scale_color_manual(breaks = NULL,
                         values = c('#45B39D'),#, '#D35400'),
                         labels(NULL)) +
      scale_x_continuous(breaks = seq(1995, 2018, 2)) +
      labs(title = paste0('Output Evolution (', dim_col[j,1], ')'),
           #subtitle = paste0('From ', dim_row[i,1], ' to ', dim_col[j,1]),
           x = NULL,
           y = 'Output') +
      theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
            axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
            axis.text.x = element_text(angle = 45, margin = margin(t = 12), size = 15),         # Textos do eixo x 
            panel.background = element_rect(fill = '#F2F3F4')
      )
    
    ggsave(path = paste0(path, 'MIP-OECD/Plots/Produtos'),                                      # Não esqueça de mudar o path ao salvar
           filename = paste0('Output Evolution (', dim_col[j,1], ')', '.png'),
           width = 3000,
           height = 1300,
           units = 'px'
    )
    
    ggsave(path = paste0('G:/Meu Drive/Arquivos para estudo da UFC/Doutorado/1° Semestre/Economia Regional/Projeto/Plots/Produtos'),
           filename = paste0('Output Evolution (', dim_col[j,1], ')', '.png'),
           width = 3000,
           height = 1300,
           units = 'px'
    )
  }
}
end_time <- Sys.time()
code_time(start_time, end_time)





# --------------- #
# --- Savings --- #
# --------------- #

# Atenção !!! - Rode este code apenas se desejar salvar os resultados
alias <- c('mip_sectors', 'mip_outputs', 'mip_coef', 'mip_eigenvalues')#, 'mip_value_addeds', 'mip_final_demand')
db <- list(db_sectors, db_outputs, db_sectors_coef, eigenvalues)#, db_value_added, db_final_demand)
names(db) <- alias

for (c in countries){
  for (i in 1:length(alias)){
    wb <- createWorkbook(creator = 'pi')
    
    if (i != 4){
      for (t in 1:24){
        addWorksheet(wb = wb, sheetName = paste0(alias[i], '_', (1994+t)))
        writeData(wb = wb, sheet = paste0(alias[i], '_', (1994+t)), x = db[[i]][[c]][[t]], rowNames = TRUE)
      }
    }
    
    if (i == 4){
      addWorksheet(wb = wb, sheetName = paste0(alias[i], '_1995_2018'))
      writeData(wb = wb, sheet = paste0(alias[i], '_1995_2018'), x = db[[i]][[c]], rowNames = TRUE)
    }
    
    saveWorkbook(wb = wb, file = paste0(path, 'MIP-OECD/', alias[i], '.xlsx'), overwrite = TRUE)
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