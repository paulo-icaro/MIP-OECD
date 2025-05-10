# ========================================================== #
# ========== Input-Output Tables - OECD Countries ========== #
# ========================================================== #
# ============== 4. Structural Breaks Test ================= #
# ========================================================== #

# --- Autor: Paulo Icaro --- #



# -------------------- #
# --- 1. Libraries --- #
# -------------------- #
library(strucchange)



# --------------------------- #
# --- 2. Previous Scripts --- #
# --------------------------- #
source(file = paste0(getwd(),'/MIP-OECD-Analysis.R'))     # --- Execucao do script MIP-OCDE-Analysis --- #



# ------------------------------------- #
# --- 3. Breaking Test - Bai-Perron --- #
# ------------------------------------- #

# --- 3.1 - Breaking Test on Techinal Coeficients --- #
bp_test_results = vector(mode = 'list')
counter = 0

for (c in length(countries)){                                                               # c para pais
  for (i in 1:45){                                                                          # i para linha
    for (j in 1:45){                                                                        # j para coluna
      counter = counter + 1
      
      # Gathering the Coeficients Series
      for (t in 1:24){                                                                      # t para o ano
        if (t == 1) {w = db[['sectors_coef']][[c]][[t]][i:i,j:j]} 
        else {w <- rbind(w, db[['sectors_coef']][[c]][[t]][i:i,j:j])}
      }
 
      # Coeficient Time Series
      ts_w = ts(data = w, frequency = 1, start = 1995)
  
      # Bai-Perron Test
      bp_test_results[[counter]] <- breakpoints(ts_w ~ 1)
      #summary(bp_test)
      names(bp_test_results)[counter] = paste0(dim_row_cod[i,],' to ', dim_col_cod[j,])
    }
  }
}





# Load the example dataset
data("Nile")

# Plotting the data
plot(Nile, main = "Annual Flow of the Nile River", ylab = "Flow", xlab = "Year")

# Chow Test
nile_pre <- window(Nile, end = 1898)
nile_post <- window(Nile, start = 1899)
chow_test <- sctest(Nile ~ 1, type = "Chow", point = 28)
print(chow_test)

# Bai-Perron Test
bp_test <- breakpoints(Nile ~ 1)
summary(bp_test)

# Plotting the breakpoints
plot(bp_test)
lines(bp_test, col = "red")