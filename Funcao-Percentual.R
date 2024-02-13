# -------------------------------------- #
# --- Funcao - Formatacao Percentual --- #
# -------------------------------------- #

percentual = function(Valor){
  perc = paste0(round(x = Valor, digits = 4)*100, '%')
  return(perc)
}

