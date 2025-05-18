# ============================================ #
# === INPUT-OUTPUT TABLES - OECD COUNTRIES === #
# ============================================ #

# --- Script by: Paulo Icaro --- #


# ==================== #
# === 1. Libraries === #
# ==================== #
library(readxl)
library(tidyverse)
library(extrafont) 
library(ggrepel)
library(plotly)
library(geomtextpath)
library(gganimate)
library(gifski)
library(scales)

# --- Funcao Percentual --- #
source('Funcao-Percentual.R')



# ========================== #
# === 2. Analysis Script === #
# ========================== #
source(file = paste0(getwd(),'/MIP-OECD-Analysis.R'))     # Executing the MIP-OCDE-Analysis script



# ================================== #
# === 3. Data Analysis and Plots === #
# ================================== #

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
      geom_line(data = as.data.frame(x = int_cons), mapping = aes(x = 1995:2018, y = int_cons, color = 'Cons. Intermediario'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = household), mapping = aes(x = 1995:2018, y = household, color = 'Cons. Familias'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = investment), mapping = aes(x = 1995:2018, y = investment, color = 'Investimentos'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = government), mapping = aes(x = 1995:2018, y = government, color = 'Governo'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = exports), mapping = aes(x = 1995:2018, y = exports, color = 'Exportacoes'), linetype = 'dashed', linewidth = 0.7) +
      geom_line(data = as.data.frame(x = imports), mapping = aes(x = 1995:2018, y = imports, color = 'Importacoes'), linetype = 'dashed', linewidth = 0.7) +
      theme(title = element_text(family = 'Segoe UI', size = 16),
            text = element_text(family = 'Segoe UI', face = 'italic', size = 15),               # Formatacao geral
            axis.title.y = element_text(size = 15 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 15, margin = margin(t = 15)),                    # Titulo do eixo x
            axis.text.y = element_text(size = 13),                                              # Textos do eixo y
            axis.text.x = element_text(angle = 30, margin = margin(t = 12), size = 13),         # Textos do eixo x 
            panel.background = element_rect(fill = '#F2F3F4')
      ) +
      scale_color_manual(breaks = c('Produto', 'Cons. Intermediário', 'Cons. Familias', 'Investimentos', 'Governo', 'Exportacoes', 'Importacoes'),
                         values = c('#ff1a1a', '#5900cc', '#73e600', '#e63e00', '#333333', '#0035e6', '#24c8bf'),
                         labels = c('Produto', 'Cons. Intermediário', 'Cons. Familias', 'Investimentos', 'Governo', 'Exportacoes', 'Importacoes'),
                         name = 'Variaveis') +
      scale_x_continuous(breaks = seq(1995, 2018, 2)) +
      scale_y_continuous(breaks = waiver(), n.breaks = 10) + 
      labs(title = 'Evolução - Variaveis Intermediarias e Finais - USD Milhoes', subtitle = paste0('Setor: ', dim_row_name[i,1]), x = NULL, y = 'USD, Milhoes')
    
    
    ggsave(filename = paste0('Evolucao das Variaveis - Setor ', dim_row_cod[i, 1], '.png'),
           path = 'C:/Users/Paulo/Documents/Repositorios/MIP_OECD/Results/Plots/Evolucao_Variaveis',
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
      
      ggsave(path = paste0('C:/Users/Paulo/Documents/Repositorios/MIP_OECD/Results/Plots/Coeficientes'),
             filename = paste0(dim_row_cod[i,1], ' para ', dim_col_cod[j,1], '.png'),
             width = 3000,
             height = 1300,
             units = 'px'
      )
    }
  }
}






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
      labs(title = paste0('Evolução - Produto - USD Milhoes'), subtitle = paste0('Setor: ', dim_row_name[i,1]), x = NULL, y = 'Produto') +
      theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
            axis.title.y = element_text(size = 15 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 15, margin = margin(t = 15)),                    # Titulo do eixo x
            axis.text.y = element_text(size = 13),                                              # Textos do eixo y
            axis.text.x = element_text(angle = 45, margin = margin(t = 12), size = 13),         # Textos do eixo x 
            panel.background = element_rect(fill = '#F2F3F4')
      )
    
    ggsave(path = paste0('C:/Users/Paulo/Documents/Repositorios/MIP_OECD/Results/Plots/Produtos'),
           filename = paste0('Evolucao_Produto_', dim_col_cod[i,1], '.png'),
           width = 3000,
           height = 1800,
           units = 'px'
    )
  }
}





# --- Plots - Top-10 Outputs --- #
for (c in length(countries)){                                                             # c para pais
    for (t in 1:24){                                                                      # t para o ano
      if (t == 1) {
        w = db[[2]][[c]][[t]][c(1, 6, 10, 11, 20, 25, 26, 36, 37, 38, 40, 41, 42),]                              # Gerar plot da serie de valores da MIP // Obs: lembre que se esta extraindo valor de uma matriz e nao mais de um dataframe
      } 
      else {
        w = rbind(w, db[[2]][[c]][[t]][c(1, 6, 10, 11, 20, 25, 26, 36, 37, 38, 40, 41, 42),])
      }
    }
  
    
    colnames(w) = as.matrix(dim_col_name[c(1, 6, 10, 11, 20, 25, 26, 36, 37, 38, 40, 41, 42),])
    w = as.data.frame(w) %>%  gather(key = "Setor", value = "value")
    w = cbind(date = 1995:2018, w)
    
    
    Plots <- ggplot(data =  w) + 
      
      # Caso deseje plotar para mais de um pais, basta copiar o trecho abaixo
      geom_line(
                aes(x = date, y = value, color = Setor),
                linetype = 'dashed',
                linewidth = .5) +
      geom_point(aes(x = date, y = value, color = Setor)) + 
      scale_x_continuous(breaks = seq(1995, 2018, 2)) +
      scale_y_continuous(breaks = c(50000, 100000, 200000, 300000, 400000, 450000), labels = c('50.000', '100.000', '200.000', '300.000', '400.000', '450.000')) + 
      labs(title = paste0('Evolução: Top-10 Setores com maior produto (USD Milhões)')) +
      xlab(label = NULL) +
      ylab(label = NULL) +
      theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 14.5),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
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
           filename = 'Evolucao_Top-10_Produto.png',
           width = 3000,
           height = 2000,
           units = 'px'
    )
}





# --- Plots - Backward and Foward Linkages and Dispersion --- #
# https://www.statology.org/geom_vline-label/
for(c in length(countries)){
  for(t in 1:24){
    
    linkages = cbind(db[[12]][[c]][[t]], db[[13]][[c]][[t]])
    Plots_Linkages = ggplot(data = as.data.frame(x = linkages), aes(x = linkages[,1], y = linkages[,2], label = rownames(linkages))) +
      geom_point() + 
      geom_texthline(yintercept = 1, label = 'Indice de Ligacao para Tras = 1', hjust = 0.02, vjust = -0.15) + 
      geom_textvline(xintercept = 1, label = 'Indice de Ligacao para Frente = 1', hjust = 0.98, vjust = -0.15) +
      geom_label_repel(label.r = .2, min.segment.length = 0, fontface = 'italic', nudge_x = 0.03, nudge_y = 0.05) + 
      labs(title = paste0('Indices de Ligacao (', 1994+t, ')'), 
           x = 'Indice de Ligacao para Tras',
           y = 'Indice de Ligacao para Frente') + 
      theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
            axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
            panel.background = element_rect(fill = '#F2F3F4')) +
      scale_x_continuous(breaks = seq(floor(min(linkages[,1])), ceiling(max(linkages[,1])), 0.1)) + 
      scale_y_continuous(breaks = seq(floor(min(linkages[,2])), ceiling(max(linkages[,2])), 0.5))
    
    ggsave(plot = Plots_Linkages,
           path = paste0('C:/Users/Paulo/Documents/Repositorios/MIP_OECD/Results/Plots/Indice_Ligacao'),
           filename = paste0('Indice_Ligacao (', 1994+t, ')', '.png'),
           width = 4500,
           height = 2500,
           units = 'px'
    )
    
    
    
    dispersion = cbind(db[[14]][[c]][[t]], db[[15]][[c]][[t]])
    Plots_Dispersion = ggplot(data = as.data.frame(x = dispersion), aes(x = dispersion[,1], y = dispersion[,2], label = rownames(dispersion))) +
      geom_point() + 
      geom_texthline(yintercept = 1, label = 'Indice de Dispersao para Tras = 1', hjust = 0.08, vjust = -0.15) + 
      geom_textvline(xintercept = 1, label = 'Indice de Dispersao para Frente = 1', hjust = 0.98, vjust = -0.15) +
      geom_label_repel(label.r = .2, min.segment.length = 0, fontface = 'italic', nudge_x = 0.03, nudge_y = 0.05) + 
      labs(title = paste0('Indices de Dispersao (', 1994+t, ')'), 
           x = 'Indice de Dispersao para Tras',
           y = 'Indice de Dispersao para Frente') + 
      theme(text = element_text(family = 'Segoe UI', face = 'italic', size = 16),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
            axis.title.y = element_text(size = 16 , margin = margin(r = 15)),                   # Titulo do eixo y
            axis.title.x = element_text(size = 16, margin = margin(t = 15)),                    # Titulo do eixo x
            panel.background = element_rect(fill = '#F2F3F4')) +
      scale_x_continuous(breaks = seq(floor(min(dispersion[,1])), ceiling(max(dispersion[,1])), 0.5)) + 
      scale_y_continuous(breaks = seq(floor(min(dispersion[,2])), ceiling(max(dispersion[,2])), 0.5))
    
    ggsave(plot = Plots_Dispersion,
           path = paste0('C:/Users/Paulo/Documents/Repositorios/MIP_OECD/Results/Plots/Indice_Dispersao'),
           filename = paste0('Indice_Dispersao (', 1994+t, ')', '.png'),
           width = 4500,
           height = 2500,
           units = 'px'
    )
    
  }
}





# --- Medias - Indice de Ligacao --- #
mean_bl = read_excel(path = paste0(getwd(), '/Results/Analises.xlsx'), sheet = 'Indices de Ligacao', range = 'Z2:Z47')
mean_fl = read_excel(path = paste0(getwd(), '/Results/Analises.xlsx'), sheet = 'Indices de Ligacao', range = 'Z52:Z97')

mean_linkages = cbind(mean_bl, mean_fl)
colnames(x = mean_linkages) = c('mean_bl', 'mean_fl')
rownames(x = mean_linkages) = as.matrix(dim_col_name)

Plots_Linkages = ggplot(data = as.data.frame(x = mean_linkages), aes(x = mean_linkages[,1], y = mean_linkages[,2], label = rownames(x = mean_linkages))) +
  geom_point() + 
  geom_texthline(size = 6.2, yintercept = 1, label = 'Indice de Ligacao para Tras = 1', hjust = 0.05, vjust = -0.15) + 
  geom_textvline(size = 6.2, xintercept = 1, label = 'Indice de Ligacao para Frente = 1', hjust = 0.98, vjust = -0.15) +
  geom_label_repel(data = mean_linkages[1,],
                   aes(x = mean_linkages[1,][,1], y = mean_linkages[1,][,2], label = rownames(x = mean_linkages[1,])),
                   size = 6.2, label.r = .2, min.segment.length = 0, fontface = 'italic', nudge_x = 0.03, nudge_y = 0.05) + 
  labs(#title = 'Indices de Ligacao (1995-2018)', 
    x = 'Indice de Ligacao para Tras',
    y = 'Indice de Ligacao para Frente') + 
  theme(title = element_text(family = 'Segoe UI', face = 'italic', size = 19),
        text = element_text(family = 'Segoe UI', face = 'italic', size = 20),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
        axis.title.y = element_text(size = 20 , margin = margin(r = 15)),                   # Titulo do eixo y
        axis.title.x = element_text(size = 20, margin = margin(t = 15)),                    # Titulo do eixo x
        panel.background = element_rect(fill = '#F2F3F4')) +
  scale_x_continuous(breaks = seq(floor(min(mean_linkages[,1])), ceiling(max(mean_linkages[,1])), 0.1)) + 
  scale_y_continuous(breaks = seq(floor(min(mean_linkages[,2])), ceiling(max(mean_linkages[,2])), 0.5))


ggsave(filename = 'Indices_Ligacao (1995-2018).png',
       path = 'G:Meu Drive//Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agricola Brasileiro (1995-2018)/Resultados/Indice_Ligacao',
       width = 4600,
       height = 3400,
       units = 'px'
)





# --- Medias - Indice de Dispersao --- #
mean_bd = read_excel(path = paste0(getwd(), '/Results/Analises.xlsx'), sheet = 'Indice de Dispersao', range = 'z2:Z47')
mean_fd = read_excel(path = paste0(getwd(), '/Results/Analises.xlsx'), sheet = 'Indice de Dispersao', range = 'z52:Z97')#[c(1,26)]
# mean_bd = mean_bd %>% mutate(Ranking = rank(Média))
# colnames(mean_bd) = c('Setor', 'Média', 'Ranking')
# top5_bd = rbind(mean_bd %>% slice_min(order_by = Média, n = 5),
#                 mean_bd %>% filter(Setor == 'D01T02'),
#                 mean_bd %>% slice_max(order_by = Média, n = 5)
#                 )

mean_dispersion = cbind(mean_bd, mean_fd)
colnames(x = mean_dispersion) = c('mean_bd', 'mean_fd')
rownames(x = mean_dispersion) = as.matrix(dim_col_name)



Plots_Dispersion = ggplot(data = as.data.frame(x = mean_dispersion), aes(x = mean_dispersion[,1], y = mean_dispersion[,2])) +
  geom_point() + 
  geom_texthline(size = 6.2, yintercept = 1, label = 'Indice de Dispersao para Tras = 1', hjust = 0.08, vjust = -0.15) + 
  geom_textvline(size = 6.2, xintercept = 1, label = 'Indice de Dispersao para Frente = 1', hjust = 0.98, vjust = -0.15) +
  geom_label_repel(data = mean_dispersion[1,],
                   aes(x = mean_dispersion[1,][,1], y = mean_dispersion[1,][,2], label = rownames(x = mean_dispersion[1,])),
                   size = 6.2, label.r = .2, min.segment.length = 0, fontface = 'italic', nudge_x = 0.03, nudge_y = 0.05) + 
  labs(#title = 'Indices de Dispersao (1995-2018)', 
    x = 'Indice de Dispersao para Tras',
    y = 'Indice de Dispersao para Frente') + 
  theme(title = element_text(family = 'Segoe UI', face = 'italic', size = 19),
        text = element_text(family = 'Segoe UI', face = 'italic', size = 20),               # Essa formatacao e geral para todos os tipos de texto. Formatacoes especificas sao feitas abaixo. Estas superam a formatacao geral.
        axis.title.y = element_text(size = 20 , margin = margin(r = 15)),                   # Titulo do eixo y
        axis.title.x = element_text(size = 20, margin = margin(t = 15)),                    # Titulo do eixo x
        panel.background = element_rect(fill = '#F2F3F4')) +
  scale_x_continuous(breaks = seq(floor(min(mean_dispersion[,1])), ceiling(max(mean_dispersion[,1])), 0.5)) + 
  scale_y_continuous(breaks = seq(floor(min(mean_dispersion[,2])), ceiling(max(mean_dispersion[,2])), 0.5))


ggsave(filename = 'Indice_Dispersao (1995-2018).png',
       path = 'G:Meu Drive//Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agricola Brasileiro (1995-2018)/Resultados/Indice_Dispersao',
       width = 4600,
       height = 3400,
       units = 'px'
)




# --- Top 5 - Setores Associados a Agricultura --- #
# Exemplo para basear: https://www.r-bloggers.com/2020/01/how-to-create-bar-race-animation-charts-in-r/

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

# Tenho a impressao que o adicionar espaco antes do texto percentual formatado melhora na exibicao dos valores durante a transicao temporal
for (x in 1:length(agr_perc)){
  ranking_database_plot = rownames_to_column(as.data.frame(agr_perc[[x]]), 'Setor') %>%
    gather(key = 'Ano', value = 'Valores', -Setor) %>%
    group_by(Ano) %>%
    slice_max(Valores, n = 5) %>%
    mutate(Ranking = rank(-Valores)) %>%
    mutate(`Participacao %` = paste0(' ', percentual(Valores)))                 # Tambem e possivel utilizar a funcao label_percent da library scales
    #filter(Ano == 1995)
  
  Ranking_Plots = 
    ggplot(data = ranking_database_plot, aes(x = Ranking, group = Setor)) +
    geom_tile(aes(y = Valores/2, height = Valores, width = 0.45, fill = as.factor(Setor)), alpha = 0.8) +    # 
    scale_fill_manual(values = c('#1C18EA', '#EA181B', '#18EA84', '#6418EA', '#18EAE0', '#EA5118', '#1a3c47', '#666666'), labels(NULL)) +
    geom_text(aes(y = (1/1000)*Valores, label = Setor, hjust = 1.05, vjust = 0), size = 8.5) + 
    geom_text(aes(y = 1.01*Valores, label = `Participacao %`, hjust = 0), size = 9) + 
    scale_y_continuous(labels = label_percent(), breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.75, 1), n.breaks = 8) + #scales::comma) +
    scale_x_reverse() +
    coord_flip(expand = FALSE, clip = 'off') +
    theme(
      plot.margin = margin(t = 5, b = 7, r = 5, l = 25, unit = 'cm'),
      title = element_text(family = 'Segoe UI', face = 'italic', size = 27, colour = 'black'),
      plot.subtitle = element_text(family = 'Segoe UI', face = 'italic', size = 24, colour = 'black'),
      #plot.caption = element_text(size = 20),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(family = 'Segoe UI', size = 25),
      panel.grid.major.x = element_line(colour = 'gray', linewidth = 0.2),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      legend.position = 'none'
    ) +
    transition_states(states = Ano, transition_length = 18, state_length = 10) +
    labs(title = 'Top 5 - Setores associados a agricultura ({closest_state})', subtitle = if(x == 1){'Setor Agricola Demandante'} else {'Setor Agricola Ofertante'}) +
    view_follow(fixed_x = TRUE)
  
  
  # Para renderizar vídeos em MP4, e preciso instalar o ffmpeg no computador. Link para download: https://www.gyan.dev/ffmpeg/builds/
  # A pasta deve se extraida e levada ao program_files do pc. Apos isso, deve ser especificado o path da pasta bin nas variaveis ambiente
  # da maquina. Para mais detalhes, ver: https://www.youtube.com/watch?v=WDCJzPfWx6o
  # Obs: a especificacao do path e crucial. Para checar se a instalacao funcionou, digite Sys.which('ffmpeg') no console do R
  plot_race_chart = animate(plot = Ranking_Plots,
                            fps = 30,
                            width = 2000,
                            height = 1700,
                            duration = 35,
                            device = 'png',
                            #renderer = gifski_renderer('plot.gif')
                            renderer = ffmpeg_renderer()
  )
  
  anim_save(paste0('G:Meu Drive/Arquivos para estudo da UFC/Doutorado/Tese/Análise Insumo-Produto do Setor Agricola Brasileiro (1995-2018)/Resultados/Top-5  - Setores associados a agricultura/', names(agr_perc[x]), '.mp4'), animation = plot_race_chart)
}
