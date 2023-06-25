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
library(OECD)                                                 # OECD API
library(openxlsx)
library(readxl)
library(tidyverse)
library(extrafont) 
library(ggrepel)
library(plotly)
path = 'C:/Users/paulo.costa/Downloads/OCDE/'                           # SDE
path = 'D:/Backup - Icaro/Documentos/Repositorios/'                     # PC
path = 'C:/Users/Paulo/Documents/Repositorios/'                         # Notebook
setwd(path)
source('RAIS/Função - code_time.R', encoding = 'LATIN1')                # Função que contabilizar o tempo do code // Se precisar use setwd para mudar o path raiz




# ----------------------- #
# --- Data Extraction --- #
# ----------------------- #

# Na funcao get_dataset o argumento filter e uma lista para indicar as dimensoes do dataset que serao consultadas
# Obs: mudar a tipagem das colunas do dataframe (https://stackoverflow.com/questions/22772279/converting-multiple-columns-from-character-to-numeric-format-in-r)

  options(timeout = 500)                                                                          # Aumentar o intervalo maximo de busca na URL do dataset
  countries <- c('BRA')#, 'KOR')						                                                      # Variavel com os nomes dos paises



# --- Listas e colunas para armazenar dados tratados --- #
  # Lista superiores (por pais)
  db_sectors <- vector(mode = 'list', length = length(countries))
  db_outputs <- vector(mode = 'list', length = length(countries))
  db_sectors_coef <- vector(mode = 'list', length = length(countries))
  #db_added_value <- vector(mode = 'list', length = length(countries))
  #db_final_demand <- vector(mode = 'list', length = length(countries))

  # Listas inferiores (por ano)
  db_sectors_matrix <- vector(mode = 'list', length = length(24))                                 # Lista que recebera as database dos setores
  db_outputs_matrix <- vector(mode = 'list', length = length(24))                                 # Lista que recebera somente os dados dos outputs dos setores
  db_sectors_coef_matrix <- vector(mode = 'list', length = length(24))                            # Lista que recebera os coeficientes da MIP
  #db_added_value_matrix <- vector(mode = 'list', length = length(24))
  #db_final_demand_matrix <- vector(mode = 'list', length = length(24))
  
  #perc_change_oecd <- data.frame(matrix(nrow = 48600))                                            # Coluna que recebera as variacoes percentuais
  #colnames(perc_change_oecd) <- c('perc_change')                                                  # Nome da coluna das variacoes percentuais


  # Colunas e Linhas cujas combinacoes serao desconsideradas #
  remove_col <- c('HFCE', 'NPISH', 'GGFC', 'GFCF', 'INVNT', 'CONS_ABR', 'CONS_NONRES', 'EXPO', 'IMPO')
  remove_row <- c('TXS_IMP_FNL', 'TXS_INT_FNL', 'TTL_INT_FNL', 'VALU', 'OUTPUT')
  dim_row <- read_excel(path = paste0(path, 'MIP-OECD/Dimensões.xlsx'), sheet = "linha", col_names=TRUE) %>% filter(!ROW %in% remove_row)
  dim_col <- read_excel(path = paste0(path, 'MIP-OECD/Dimensões.xlsx'), sheet = "coluna", col_names=TRUE) %>% filter(!COL %in% remove_col)

  

# --- Extracao --- #
  #start_time <- Sys.time()
  for (c in length(countries)){
    data_extraction <- get_dataset(dataset = "IOTS_2021", filter = list(c("TTL"), countries[c]), start_time = 1995, end_time = 2018)
    data_extraction[c('ObsValue', 'Time')] <- sapply(data_extraction[c('ObsValue', 'Time')], as.numeric)                                              # Mudanca da tipagem das colunas especificadas para numeric
  

  
    # Este loop filtra os dados intersetoriais e de output por ano, os transforma em matriz e os armazenas em uma lista
    # Desta maneira sera mais facil calcular os coeficientes tecnicos de cada matriz para cada ano
    # Tambem adicionei um tratamento para valores nulos. Aparentemente não ira alterar muita coisa
    for (t in 1:24){
      data_extraction_sectors <- data_extraction[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & !(ROW %in% remove_row) & (Time == 1994+t))         # Database Sectors // Remocao das combinacoes cujas variaveis nao serao de interesse
      data_extraction_outputs <- data_extraction[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & (ROW == "OUTPUT") & (Time == 1994+t))              # Database Outputs
      #data_extraction_added_value <- data_extraction[c(1,2,3,5,7)] %>% filter(!(COL %in% remove_col) & (ROW %in% remove_row) & (Time == 1994+t))     # Database Added Value
      #data_extraction_final_demand <- data_extraction[c(1,2,3,5,7)] %>% filter((COL %in% remove_col) & !(ROW %in% remove_row) & (Time == 1994+t))    # Database Final Demand
    
      
      # Tratando possíveis valores nulos
      # for (k in 1:2025){
      #   if (data_extraction_sectors[3][k,1] == 0){ 
      #     data_extraction_sectors[3][k,1] <-  1/(10^10)
      #   }
      # }
      # 
      # for (k in 1:45){
      #   if (data_extraction_outputs[3][k,1] == 0){ 
      #     data_extraction_outputs[3][k,1] <-  1/(10^10)
      #   }
      # }
    
      
      # Gerando as matrizes
      db_sectors_matrix[[t]] <- matrix(data = as.matrix(data_extraction_sectors[3]), nrow = 45, ncol = 45, dimnames = c(dim_row, dim_col))            # Lista de matrizes: Sectors
      db_outputs_matrix[[t]] <- matrix(data = as.matrix(data_extraction_outputs[3]), nrow = 1, ncol = 45, dimnames = c("Output", dim_col))            # Lista de matrizes: Outputs
      #db_added_value_matrix[[t]] <- matrix(data = as.matrix(data_extraction_added_value[3]), nrow = 5 , ncol = 45)                                   # Lista de matrizes: Added Values
      #db_final_demand_matrix[[t]] <- matrix(data = as.matrix(data_extraction_final_demand[3]), nrow = 45 , ncol = 9)                                 # Lista de matrizes: Final Demand
    
    
      for (i in 1:45){
        if (i == 1){
          db_sectors_coef_matrix[[t]] <- db_sectors_matrix[[t]][1:45,i]/db_outputs_matrix[[t]][,i]
        }
        else{
          db_sectors_coef_matrix[[t]] <- cbind(db_sectors_coef_matrix[[t]], (db_sectors_matrix[[t]][1:45,i]/db_outputs_matrix[[t]][,i]))
          if (i == 45){colnames(db_sectors_coef_matrix[[t]]) <- t(dim_col)}
        }
      }
    
      names(db_sectors_matrix)[t] <- 1994+t                                                                                                           # Cada elemento da lista Intersetorial recebera a data referente ao ano da matriz
      names(db_outputs_matrix)[t] <- 1994+t                                                                                                           # Cada elemento da lista Outputs recebera a data referente ao ano da matriz
      names(db_sectors_coef_matrix)[t] <- 1994+t
      #names(db_added_value_matrix)[t] <- 1994+t
      #names(db_final_demand_matrix)[t] <- 1994+t
    
    }
  
    db_sectors[[c]] <- db_sectors_matrix                                                                                                              # Armazenamento da lista intersetorial com toda a serie temporal dentro da lista de países
    db_outputs[[c]] <- db_outputs_matrix                                                                                                              # Armazenamento da lista de outputs com toda a série temporal dentro da lista de países
    db_sectors_coef[[c]] <- db_sectors_coef_matrix                                                                                                    # Armazenamento da lista de coeficientes com toda a série temporal dentro da lista de países
    #db_added_value[[c]] <- db_added_value_matrix
    #db_final_demand[[c]] <- db_final_demand_matrix
  
    names(db_sectors)[c] <- countries[c]                                                                                                              # Cada lista intersetorial de cada pais recebera o nome do pais respectivo
    names(db_outputs)[c] <- countries[c]                                                                                                              # Cada lista de outputs de cada pais recebera o nome do pais respectivo
    names(db_sectors_coef)[c] <- countries[c]                                                                                                         # Cada lista de coeficientes de cada pais recebera o nome do pais respectivo
    #names(db_added_value)[c] <- countries[c]
    #names(db_final_demand)[c] <- countries[c]
  
    # Liberando memoria quando o ultimo pais for avaliado
    if (c == length(countries)){rm(data_extraction, data_extraction_sectors, data_extraction_outputs, db_sectors_matrix, db_outputs_matrix, db_sectors_coef_matrix)}
  }
  #end_time <- Sys.time()
  #code_time(start_time, end_time)                                                                                                                    # Cronometro





# --------------------- #
# --- Data Analysis --- #
# --------------------- #
  

  
# --- Eigenvalues --- #
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
  
  for (c in length(countries)){
    for (i in 1:45){
      w = matrix(data = eigenvalues[[c]][i,], nrow = 24, ncol = 1)
      Plots <- ggplot() +
              geom_line(data = as.data.frame(x = w), aes(x = 1995:2018, y = w, color = 'Brazil'), linetype = 'dashed', linewidth = .75) +
              geom_point(data = as.data.frame(x = w), x = 1995:2018, y = w) +
              scale_color_manual(breaks = c('Brazil'), values = c('#45B39D'), labels(NULL)) +
              scale_x_continuous(breaks = seq(1995, 2018, 2)) +
              labs(title = paste0('Time Evolution: Eigenvalue #', i), x = NULL, y = 'Parameter') +
              theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),           # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
                axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
                axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
                axis.text.x = element_text(angle = 45, margin = margin(t = 12), size = 15),         # Textos do eixo x 
                panel.background = element_rect(fill = '#F2F3F4')
              )
      
              ggsave(path = paste0(path, 'MIP-OECD/Plots/Autovalores'), filename = paste0('Eigenvalue #', i, '.png'), width = 3000, height = 1300, units = 'px')
              ggsave(path = 'G:/Meu Drive/Arquivos para estudo da UFC/Doutorado/1° Semestre/Economia Regional/Projeto/Plots/Autovalores', filename = paste0('Eigenvalue #', i, '.png'), width = 3000, height = 1300, units = 'px')
              
    }
  }



# --- PCA Analysis --- #
  pca <- prcomp(x = db_sectors[[1]][[1]])
  pca_scores <- pca$x
  pca_variance_explained <- (pca$sdev^2) / sum(pca$sdev^2)
  pca_loadings <- pca$rotation
  
  
  pca_scores_df <- as.data.frame(pca_scores)#[c(-6, -10, -11), c(1,2)])
  pca_variance_explained_df <- as.data.frame(pca_variance_explained)
  pca_loadings_df <- as.data.frame(pca_loadings)
  
  pca_variance_explained_plot <- 
    ggplot() +
    geom_col(data = pca_variance_explained_df, aes(y = pca_variance_explained*100 , x = 1:45)) + 
    labs(title = 'Explained variance by each PC', x = 'Principal Component', y = '%')
  pca_variance_explained_plot
  
  pca_biplot <-
    ggplot() +
    geom_point(data = pca_scores_df*0.0001, aes(x = PC1, y = PC2)) +
    #geom_label_repel(aes(label = rownames(pca_scores_df), x = pca_scores_df$PC1, y = pca_scores_df$PC2), box.padding = 0.35, point.padding = 0.75, segment.color = 'grey50') +
    geom_text(aes(label = rownames(pca_scores_df), x = (pca_scores_df*0.0001)$PC1, y = (pca_scores_df*0.0001)$PC2), nudge_x = 0.05, nudge_y = 0.05) +                                                            # Use este comando para caso deseje usar o ggplotly        
    geom_segment(data = pca_loadings_df, aes(x = 0, y = 0, xend = PC1, yend = PC2))
    #geom_abline(intercept = 0, slope = pca_loadings_df$PC2/pca_loadings_df$PC1) +
    #scale_y_continuous(sec.axis = sec_axis(~.*(0.0001), name = 'Eixo y secundário')) + 
    #scale_x_continuous(sec.axis = sec_axis(~.*(0.0001), name = 'Eixo x secundário'))
  #Graficos dinamicos
  #ggplotly(pca_variance_explained_plot)
  #ggplotly(pca_biplot)
  pca_biplot
  
  plot(pca_scores[, 1], type = "l", xlab = "Tempo", ylab = "Score", main = "Análise de Componentes Principais - Componente Principal 1")
  for (i in 2:ncol(pca_scores)) {
    lines(pca_scores[, i], col = i)
  }
  
  ggplotly() + 
  biplot(pca, scale = 0)
  
  
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


  
# --- Savings --- #
# Atenção !!! - Rode este code apenas se desejar salvar os resultados
  alias <- c('mip_sectors', 'mip_outputs', 'mip_coef', 'mip_eigenvalues')#, 'mip_added_values', 'mip_final_demand')
  db <- list(db_sectors, db_outputs, db_sectors_coef, eigenvalues, db_added_value, db_final_demand)
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



# --- Plots --- #
dim_col <- unique(database[[1]][c(1)])
dim_row <- unique(database[[1]][c(4)])    

# Em aes o argumento color e empregado de maneira nao usual.
# Ele e utilizado para definir uma especie de id que sera associada a uma cor na funcao scale_color_manual

# font_import(): importa todas as fontes do sistema
# loadfonts(device = 'win'): ler o banco de dados de fontes importado e os registra junto ao R
# windowsFonts(): para ver todos os tipos de fontes agora disponiveis (por default o R so possui Times New Roman, Arial e Courier New)
# Para mais sobre o assunto, ver: https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
  for (c in length(countries)){
    for (i in 1:5){
      for (j in 1:5){
        for (t in 1:24){if (t == 1) {w = db_sectors_coef[[c]][[t]][i:i,j:j]} else {w <- rbind(w, db_sectors_coef[[c]][[t]][i:i,j:j])}}
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
          labs(title = 'Parameter Evolution',
              subtitle = paste0('From ', dim_row[i,1], ' to ', dim_col[j,1]),
              x = NULL,
              y = 'Parameter') +
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
      
        ggsave(path = paste0('G:/Meu Drive/Arquivos para estudo da UFC/Doutorado/1° Semestre/Economia Regional/Projeto/Plots'),
               filename = paste0('From ', dim_row[i,1], ' to ', dim_col[j,1], '.png'),
               width = 3000,
               height = 1300,
              units = 'px'
        )
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
#saveWorkbook(wb = wb, file = paste0(path, '/Dimens?es.xlsx'), overwrite = TRUE)