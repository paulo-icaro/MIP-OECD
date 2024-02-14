# ============================================ #
# === Input-Output Tables - OECD Countries === #
# ============================================ #

# ================ #
# === 2. Plots === #
# ================ #


# --- Autor: Paulo Icaro --- #





# ----------------- #
# --- Libraries --- #
# ----------------- #
library(tidyverse)
library(extrafont) 
library(ggrepel)
library(plotly)
library(geomtextpath)
library(gganimate)
library(gifski)

# --- Funcao Percentual --- #
source('Funcao-Percentual.R')



# ------------------------------- #
# --- Data Analysis and Plots --- #
# ------------------------------- #

# --- Execucao do script MIP-OCDE-Analysis --- #
source(file = paste0(getwd(),'/MIP-OCDE-Analysis.R'))

# --- Evolucao - Variaveis Intermediarias e Finais ---- #
for (c in 1:length(countries)){
  for (i in 1:45){
    for (t in 1:24){
      if (t == 1){
        output = db[[2]][[c]][[t]][i,1]
        exports = db[[5]][[c]][[t]][i,1]
        imports = db[[6]][[c]][[t]][i,1]
        int_cons = db[[7]][[c]][[t]][i,1]
        household = db[[8]][[c]][[t]][i,1]
        government = db[[9]][[c]][[t]][i,1]
        investment = db[[10]][[c]][[t]][i,1]
      }
      else{
        output = rbind(output, db[[2]][[c]][[t]][i,1])
        exports = rbind(exports, db[[5]][[c]][[t]][i,1])
        imports = rbind(imports, db[[6]][[c]][[t]][i,1])
        int_cons = rbind(int_cons, db[[7]][[c]][[t]][i,1])
        household = rbind(household, db[[8]][[c]][[t]][i,1])
        government = rbind(government, db[[9]][[c]][[t]][i,1])
        investment = rbind(investment, db[[10]][[c]][[t]][i,1])
        
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
      theme(title = element_text(family = 'Segoe UI', size = 16),
            text = element_text(family = 'Segoe UI', face = 'italic', size = 15),               # Formatacao geral
            axis.title.y = element_text(size = 15 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 15, margin = margin(t = 15)),                    # Titulo do eixo x
            axis.text.y = element_text(size = 13),                                              # Textos do eixo y
            axis.text.x = element_text(angle = 30, margin = margin(t = 12), size = 13),         # Textos do eixo x 
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
           path = 'C:/Users/paulo/OneDrive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Resultados/Evolucao_Variaveis',
           width = 3000,
           height = 1800,
           units = 'px'
    )
    
  }
  
  if (c == length(countries)){rm(output, int_cons, household, investment, government, exports, imports)}
}





# --- Plots - Coeficientes --- #

# Em aes o argumento color e empregado de maneira nao usual.
# Ele e utilizado para definir uma especie de id que sera associada a uma cor na funcao scale_color_manual
# font_import(): importa todas as fontes do sistema
# loadfonts(device = 'win'): ler o banco de dados de fontes importado e os registra junto ao R
# windowsFonts(): para ver todos os tipos de fontes agora disponiveis (por default o R so possui Times New Roman, Arial e Courier New)
# Para mais sobre o assunto, ver: https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
start_time <- Sys.time()
for (c in length(countries)){                                                               # c para pais
  for (i in 1:45){                                                                          # i para linha
    for (j in 1:45){                                                                        # j para coluna
      for (t in 1:24){                                                                      # t para o ano
        if (t == 1) {
          w = db[[3]][[c]][[t]][i:i,j:j]                                                    # Gerar plot da serie de coeficientes
        } 
        else {
          w <- rbind(w, db[[3]][[c]][[t]][i:i,j:j])
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
      
      ggsave(path = paste0('C:/Users/paulo/OneDrive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Resultados/Coeficientes'),
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
for (c in length(countries)){                                                             # c para pais
  for (i in 1:45){                                                                        # j para coluna
    for (t in 1:24){                                                                      # t para o ano
      if (t == 1) {
        w = db[[2]][[c]][[t]][i,]                                         # Gerar plot da serie de valores da MIP // Obs: lembre que se esta extraindo valor de uma matriz e nao mais de um dataframe
      } 
      else {
        w = rbind(w, db[[2]][[c]][[t]][i,])
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
            axis.title.y = element_text(size = 15 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 15, margin = margin(t = 15)),                    # Titulo do eixo x
            axis.text.y = element_text(size = 13),                                              # Textos do eixo y
            axis.text.x = element_text(angle = 45, margin = margin(t = 12), size = 13),         # Textos do eixo x 
            panel.background = element_rect(fill = '#F2F3F4')
      )
    
    ggsave(path = paste0('C:/Users/paulo/OneDrive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Resultados/Produtos'),
           filename = paste0('Evolucao_Produto_', dim_col_cod[i,1], '.png'),
           width = 3000,
           height = 1800,
           units = 'px'
    )
  }
}





# --- Plots - Backward and Foward Linkages and Dispersion --- #
# https://www.statology.org/geom_vline-label/
for(c in length(countries)){
  for(t in 1:24){
    
    linkages = cbind(db[[12]][[c]][[t]], db[[13]][[c]][[t]])
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
           path = paste0('C:/Users/paulo/OneDrive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Resultados/Indice_Ligacao'),
           filename = paste0('Indice_Ligacao (', 1994+t, ')', '.png'),
           width = 4500,
           height = 2500,
           units = 'px'
    )
    
    
    
    dispersion = cbind(db[[14]][[c]][[t]], db[[15]][[c]][[t]])
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
           path = paste0('C:/Users/paulo/OneDrive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Resultados/Indice_Dispersao'),
           filename = paste0('Indice_Dispersao (', 1994+t, ')', '.png'),
           width = 4500,
           height = 2500,
           units = 'px'
    )
    
  }
}





# --- Medias - Indice de Ligacao --- #
mean_bl = read_excel(path = 'MIP_OECD/Results/Análises.xlsx', sheet = 'Índices de Ligação', range = 'Z2:Z47')
mean_fl = read_excel(path = 'MIP_OECD/Results/Análises.xlsx', sheet = 'Índices de Ligação', range = 'Z52:Z97')

mean_linkages = cbind(mean_bl, mean_fl)
colnames(x = mean_linkages) = c('mean_bl', 'mean_fl')
rownames(x = mean_linkages) = as.matrix(dim_col_cod)

Plots_Linkages = ggplot(data = as.data.frame(x = mean_linkages), aes(x = mean_linkages[,1], y = mean_linkages[,2], label = rownames(x = mean_linkages))) +
  geom_point() + 
  geom_texthline(size = 6.2, yintercept = 1, label = 'Índice de Ligação para Trás = 1', hjust = 0.05, vjust = -0.15) + 
  geom_textvline(size = 6.2, xintercept = 1, label = 'Índice de Ligação para Frente = 1', hjust = 0.98, vjust = -0.15) +
  geom_label_repel(size = 6.2, label.r = .2, min.segment.length = 0, fontface = 'italic', nudge_x = 0.03, nudge_y = 0.05) + 
  labs(#title = 'Índices de Ligação (1995-2018)', 
    x = 'Índice de Ligação para Trás',
    y = 'Índice de Ligação para Frente') + 
  theme(title = element_text(family = 'Segoe UI', face = 'italic', size = 19),
        text = element_text(family = 'Segoe UI', face = 'italic', size = 20),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
        axis.title.y = element_text(size = 20 , margin = margin(r = 15)),                   # Titulo do eixo y
        axis.title.x = element_text(size = 20, margin = margin(t = 15)),                    # Titulo do eixo x
        panel.background = element_rect(fill = '#F2F3F4')) +
  scale_x_continuous(breaks = seq(floor(min(mean_linkages[,1])), ceiling(max(mean_linkages[,1])), 0.1)) + 
  scale_y_continuous(breaks = seq(floor(min(mean_linkages[,2])), ceiling(max(mean_linkages[,2])), 0.5))


ggsave(filename = 'Indices_Ligacao (1995-2018).png',
       path = 'C:/Users/paulo/OneDrive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Resultados/Indice_Ligacao',
       width = 4600,
       height = 3400,
       units = 'px'
)





# --- Medias - Indice de Dispersão --- #
mean_bd = read_excel(path = 'MIP_OECD/Results/Análises.xlsx', sheet = 'Índices de Dispersão', range = 'Z2:Z47')
mean_fd = read_excel(path = 'MIP_OECD/Results/Análises.xlsx', sheet = 'Índices de Dispersão', range = 'Z52:Z97')

mean_dispersion = cbind(mean_bd, mean_fd)
colnames(x = mean_dispersion) = c('mean_bd', 'mean_fd')
rownames(x = mean_dispersion) = as.matrix(dim_col_cod)

Plots_Dispersion = ggplot(data = as.data.frame(x = mean_dispersion), aes(x = mean_dispersion[,1], y = mean_dispersion[,2], label = rownames(x = mean_dispersion))) +
  geom_point() + 
  geom_texthline(size = 6.2, yintercept = 1, label = 'Índice de Dispersão para Trás = 1', hjust = 0.08, vjust = -0.15) + 
  geom_textvline(size = 6.2, xintercept = 1, label = 'Índice de Dispersão para Frente = 1', hjust = 0.98, vjust = -0.15) +
  geom_label_repel(size = 6.2, label.r = .2, min.segment.length = 0, fontface = 'italic', nudge_x = 0.03, nudge_y = 0.05) + 
  labs(#title = 'Índices de Dispersão (1995-2018)', 
    x = 'Índice de Dispersão para Trás',
    y = 'Índice de Dispersão para Frente') + 
  theme(title = element_text(family = 'Segoe UI', face = 'italic', size = 19),
        text = element_text(family = 'Segoe UI', face = 'italic', size = 20),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
        axis.title.y = element_text(size = 20 , margin = margin(r = 15)),                   # Titulo do eixo y
        axis.title.x = element_text(size = 20, margin = margin(t = 15)),                    # Titulo do eixo x
        panel.background = element_rect(fill = '#F2F3F4')) +
  scale_x_continuous(breaks = seq(floor(min(mean_dispersion[,1])), ceiling(max(mean_dispersion[,1])), 0.5)) + 
  scale_y_continuous(breaks = seq(floor(min(mean_dispersion[,2])), ceiling(max(mean_dispersion[,2])), 0.5))


ggsave(filename = 'Indice_Dispersao (1995-2018).png',
       path = 'C:/Users/paulo/OneDrive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agrícola Brasileiro (1995-2018)/Resultados/Indice_Dispersao',
       width = 4600,
       height = 3400,
       units = 'px'
)




# --- Top 5 - Setores Associados a Agricultura --- #
agr_dmd_perc = agr_oft_perc = NULL

for (c in 1:length(countries)){
  for (t in 1:24){
    agr_dmd_perc = cbind(agr_dmd_perc, as.matrix(db[[3]][[c]][[t]][,1])/colSums(as.matrix(db[[3]][[c]][[t]][,1])))
    agr_oft_perc = cbind(agr_oft_perc, as.matrix(db[[3]][[c]][[t]][1,])/colSums(as.matrix(db[[3]][[c]][[t]][1,])))
  }
  colnames(x = agr_dmd_perc) = colnames(x = agr_oft_perc) = 1995:2018
  rownames(x = agr_dmd_perc) = rownames(x = agr_oft_perc) = as.matrix(dim_row_name)
}


agr_perc = list(agr_dmd_perc, agr_oft_perc)
names(agr_perc) = c('agr_dmd_perc', 'agr_oft_perc')

for (x in 1:length(agr_perc)){
  ranking_database_plot = rownames_to_column(as.data.frame(agr_perc[[x]]), 'Setor') %>%
    gather(key = 'Ano', value = 'Valores', -Setor) %>%
    group_by(Ano) %>%
    slice_max(Valores, n = 5) %>%
    mutate(Ranking = rank(-Valores)) %>%
    mutate(`Participacao %` = percentual(Valores))
    #filter(Ano == 1995)

  Ranking_Plots = 
    ggplot(data = ranking_database_plot, aes(x = Ranking, group = Setor)) +
    geom_tile(aes(y = Valores/2, height = Valores, width = 0.45, fill = as.factor(Setor)), alpha = 0.8) +    # 
    scale_fill_manual(values = c('#1C18EA', '#EA181B', '#18EA84', '#6418EA', '#18EAE0', '#EA5118', '#1a3c47', '#666666'), labels(NULL)) +
    geom_text(aes(y = (1/1000)*Valores, label = Setor, hjust = 1.05, vjust = 0), size = 6.8) + 
    geom_text(aes(y = 1.01*Valores, label = `Participacao %`, hjust = 0), size = 6.8) + 
    scale_y_continuous(labels = c('0%', '5%', '10%', '15%', '20%', '25%', '30%', '35%', '40%', '45%', '50%', '55%', '60%', '75%', '100%'), breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.75, 1), n.breaks = 8) + #scales::comma) +
    scale_x_reverse() +
    coord_flip(expand = FALSE, clip = 'off') +
    theme(
      plot.margin = margin(t = 3, b = 5, r = 5, l = 20, unit = 'cm'),
      title = element_text(family = 'Segoe UI', face = 'italic', size = 23, colour = 'black'),
      plot.caption = element_text(size = 20),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(family = 'SEGOE UI', size = 20),
      panel.grid.major.x = element_line(colour = 'gray', linewidth = 0.1),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      legend.position = 'none'
    ) +
    transition_states(states = Ano, transition_length = 18, state_length = 18) +
    labs(title = 'Top 5 - Setores mais associados à agricultura', subtitle = if(x == 1){'Setor Agrícola Demandante'} else {'Setor Agrícola Ofertante'}, caption = '{closest_state}') +
    view_follow(fixed_x = TRUE)


  # Para renderizar vídeos em MP4, e preciso instalar o ffmpeg no computador. Link para download: https://www.gyan.dev/ffmpeg/builds/
  # A pasta deve se extraida e levada ao program_files do pc. Apos isso, deve ser especificado o path da pasta bin nas variaveis ambiente
  # da maquina. Para mais detalhes, ver: https://www.youtube.com/watch?v=WDCJzPfWx6o
  # Obs: a especificacao do path e crucial. Para checar se a instalacao funcionou, digite Sys.which('ffmpeg') no console do R
  plot_race_chart = animate(plot = Ranking_Plots,
                            fps = 30,
                            width = 1500,
                            height = 1200,
                            duration = 40,
                            #renderer = 
                            device = 'png',
                            #renderer = gifski_renderer('plot.gif')
                            renderer = ffmpeg_renderer()
                            )

  anim_save(paste0(names(agr_perc[x]), '.mp4'), animation = plot_race_chart)
}
