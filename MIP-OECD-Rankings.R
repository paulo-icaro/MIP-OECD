# ============================================ #
# === Input-Output Tables - OECD Countries === #
# ============================================ #

# --- Autor: Paulo Icaro --- #



# ------------------------ #
# --- Previous Scripts --- #
# ------------------------ #
source(file = paste0(getwd(),'/MIP-OCDE-Analysis.R'))     # --- Execucao do script MIP-OCDE-Analysis --- #



# -------------------------- #
# --- 1. Ranking Setores --- #
# -------------------------- #
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
    ranking_outputs_sectors = 46 - rank(x = db[['outputs']][[c]][[t]])                                    # Ranking por Produto
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
rm(ranking_outputs_sectors, ranking_backward_linkages, ranking_foward_linkages,
   ranking_backward_dispersion, ranking_foward_dispersion, ranking_matrix_outputs,
   ranking_matrix_backward_linkages, ranking_matrix_foward_linkages,
   ranking_matrix_backward_dispersion, ranking_matrix_foward_dispersion, ranking_matrix_pull_index,
   ranking_alias)