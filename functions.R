library("dplyr")

calculate_profit <- function(cutoff, probabilities, true_classes){
	sum((probabilities > cutoff) * ifelse(true_classes == 1, 11700, -300))
}

format_money <- function(value) {
	paste0("$", formatC(value, format = "f", digits = 0, big.mark = ".", decimal.mark = ','))
}

calculate_present_features <- function(df) {
	df <- mutate(df,
			  mv_cuenta_estado2 = pmax(Master_cuenta_estado, Visa_cuenta_estado, na.rm = TRUE),
			  mv_marca_atraso = pmax(Master_marca_atraso, Visa_marca_atraso, na.rm = TRUE),
			  mv_mfinanciacion_limite = rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE),
			  mv_Fvencimiento = pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE),
			  mv_Finiciomora = pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE),
			  mv_msaldototal = rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE),
			  mv_msaldopesos = rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE),
			  mv_msaldodolares = rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE),
			  mv_mconsumospesos = rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE),
			  mv_mconsumosdolares = rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE),
			  mv_mlimitecompra = rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE),
			  mv_madelantopesos = rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE),
			  mv_madelantodolares = rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE),
			  mv_fultimo_cierre = pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE),
			  mv_mpagado = rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE),
			  mv_mpagospesos = rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE),
			  mv_mpagosdolares = rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE),
			  mv_fechaalta = pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE),
			  mv_mconsumototal = rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE),
			  mv_tconsumos = rowSums(cbind(Master_tconsumos, Visa_tconsumos), na.rm = TRUE),
			  mv_tadelantosefectivo = rowSums(cbind(Master_tadelantosefectivo, Visa_tadelantosefectivo), na.rm = TRUE),
			  mv_mpagominimo = rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE),
			  mvr_Master_mlimitecompra = Master_mlimitecompra / mv_mlimitecompra,
			  mvr_Visa_mlimitecompra = Visa_mlimitecompra / mv_mlimitecompra,
			  mvr_msaldototal = mv_msaldototal / mv_mlimitecompra,
			  mvr_msaldopesos = mv_msaldopesos / mv_mlimitecompra,
			  mvr_msaldopesos2 = mv_msaldopesos / mv_msaldototal,
			  mvr_msaldodolares = mv_msaldodolares / mv_mlimitecompra,
			  mvr_msaldodolares2 = mv_msaldodolares / mv_msaldototal,
			  mvr_mconsumospesos = mv_mconsumospesos / mv_mlimitecompra,
			  mvr_mconsumosdolares = mv_mconsumosdolares / mv_mlimitecompra,
			  mvr_madelantopesos = mv_madelantopesos / mv_mlimitecompra,
			  mvr_madelantodolares = mv_madelantodolares / mv_mlimitecompra,
			  mvr_mpagado = mv_mpagado / mv_mlimitecompra,
			  mvr_mpagospesos = mv_mpagospesos / mv_mlimitecompra,
			  mvr_mpagosdolares = mv_mpagosdolares / mv_mlimitecompra,
			  mvr_mconsumototal = mv_mconsumototal / mv_mlimitecompra,
			  mvr_mpagominimo = mv_mpagominimo / mv_mlimitecompra
		)
}
