# ============================================ #
# === INPUT-OUTPUT TABLES - OECD COUNTRIES === #
# ============================================ #

# --- Script by: Paulo Icaro --- #



# -------------------- #
# --- 1. Libraries --- #
# -------------------- #
library(strucchange)



# --------------------------- #
# --- 2. Previous Scripts --- #
# --------------------------- #
source(file = paste0(getwd(),'/MIP-OECD-Analysis.R'))     # --- Run the MIP-OCDE-Analysis script --- #



# ------------------------------------- #
# --- 3. Breaking Test - Bai-Perron --- #
# ------------------------------------- #

# --- 3.1 - Breaking Test on Techinal Coeficients --- #
bp_test_results = vector(mode = 'list')                                                     # List for breaking points test results
check_breakpoints = vector('list')                                                         # List for visual analysis of the results
check_matrix = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_row_cod), 1995:2018))  # Store matrix of breaking point of sectors i againt all the j sectors
counter = 0                                                                                 # Counter of all i x j technical coeficients combinations

for (c in length(countries)){                                                               # c for country
  for (i in 1:45){                                                                          # i for row
    for (j in 1:45){                                                                        # j for column
      counter = counter + 1
      
      # Gathering the coefficients series
      for (t in 1:24){                                                                      # t for year
        if (t == 1) {w = db[['sectors_coef']][[c]][[t]][i:i,j:j]} 
        else {w <- rbind(w, db[['sectors_coef']][[c]][[t]][i:i,j:j])}
      }
 
      # Coeficient time series
      ts_w = ts(data = w, frequency = 1, start = 1995)
  
      # Bai-Perron test
      bp_test_results[[counter]] <- breakpoints(ts_w ~ 1)                                   # Breaking points test
      #summary(bp_test)                                                                     # Summarizing the results
      names(bp_test_results)[counter] = paste0(dim_row_cod[i,],' to ', dim_col_cod[j,])     # Renaming the storage list
      check_matrix[j, ] = 1995:2018 %in% (1994 + bp_test_results[[counter]]$breakpoints)    # Matrix for visual checking the breaking points
    }
    
    check_matrix = ifelse(check_matrix == FALSE, "", '*')                                   # Replacing FALSE for empty cell and TRUE for *
    check_breakpoints[[i]] = check_matrix                                                   # Storing the result
    names(check_breakpoints)[i] = dim_row_cod[i,]
    check_matrix = matrix(data = NA, nrow = 45, ncol = 24, dimnames = list(t(dim_row_cod), 1995:2018))  # Reseting the matrix
  }
}



# -------------------------- #
# --- 4. Storing Results --- #
# -------------------------- #
wb = createWorkbook(creator = 'pi')
for(i in 1:45){
  addWorksheet(wb = wb, sheetName = t(dim_row_cod)[i])
  writeData(wb = wb, sheet = t(dim_row_cod)[i], x = check_breakpoints[[i]], rowNames = TRUE, colNames = TRUE)
}
saveWorkbook(wb = wb, file = 'Results/Sheets/breakpoints_check_coef.xlsx', overwrite = TRUE)