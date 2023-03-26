# ============================================ #
# === Input-Output Tables - OECD Countries === #
# ============================================ #

# ---------------- #
# --- Packages --- #
# ---------------- #
install.packages('OECD')


# ----------------- #
# --- Libraries --- #
# ----------------- #
library(OECD)           # OECD API


# ----------------------- #
# --- Data Extraction --- #
# ----------------------- #
# options(timeout = 120)                          # Aumentar o intervalo máximo de busca na URL do dataset
databse <- get_dataset("IOTS_2021",
            filter = list(c("TTL"), c('BRA')),
            start_time = 1998, end_time = 1998)

