library("dplyr")
source("config.R")

calculate_profit <- function(cutoff, probabilities, true_classes){
	sum((probabilities > cutoff) * ifelse(true_classes == 1, 11700, -300))
}

format_money <- function(value) {
	paste0("$", formatC(value, format = "f", digits = 0, big.mark = ".", decimal.mark = ','))
}

final_preprocess <- function(df, useless_columns, is_train = TRUE) {
	df[, (useless_columns) := NULL]
	if (is_train) {
		#mejorar
		df[, target := ifelse(df$clase_ternaria == 'CONTINUA', 0, 1)]
	} else {
		#mejorar
		df[, target := ifelse(df$clase_ternaria == 'BAJA+2', 1, 0)]
	}	
	df[, clase_ternaria := NULL]
}

calculate_backward_months <- function(period, quantity) {

	counter <- 0
	periods <- c()
	while (counter <= quantity) {

		yearr <- substr(period, 1, 4)
		monthh <- substr(period, 5,6)

		if (monthh == "00") {
			monthh <- 12
			yearr <- as.integer(yearr) - 1
		}

		period <- as.integer(paste0(yearr, monthh))
		periods <- c(periods, period)

		counter <- counter + 1
		period <- period - 1
	}


	return(rev(periods))
}




load_dataset <- function(periods,
                         remove_columns = c(),
                         avoid_columns_for_tendency = c(),
                         categorical_features = c(),
                         remove_columns_final = c(),
                         growth_polinomio = TRUE,
                         acceleration_polinomio = TRUE,
                         growth_dif_finitas = TRUE,
                         acceleration_dif_finitas = TRUE,
                         tendency_andreu = TRUE,
                         derived = FALSE,
                         max_min_etc = FALSE) {
	backward <- 3 #3 meses pa tras
	final_data <- NULL	

	for (period in periods) {
		needed_periods <- period
		if (growth_polinomio || acceleration_polinomio || tendency_andreu || growth_dif_finitas || acceleration_dif_finitas) {
			needed_periods <- calculate_backward_months(period, backward)
		}

		data <- do.call(rbind, lapply(needed_periods, function(period1) fread(paste0(CONFIG$DATASETS_PATH, period1, '_dias.txt'), header = TRUE, sep = "\t")))

		if(length(remove_columns) > 0){
		  data[, (remove_columns) := NULL]  
		}
		
		
		if (derived) {
			data <- as.data.table(calculate_present_features(data))
		}		
		
		data[, cliente_edad := cut(data$cliente_edad, c(-Inf, 18, 30, 50, Inf))] 
		
		data_period <- data[foto_mes == period]

		if (growth_polinomio || acceleration_polinomio || tendency_andreu || growth_dif_finitas || acceleration_dif_finitas) {
		  needed_periods <- rev(needed_periods)
		  
		  count_by_cliente <- data[, .(count = .N), by = numero_de_cliente]
		  clientes_incompletos <- count_by_cliente[count < backward + 1, numero_de_cliente]
		  
		  clientes_a_completar <- data[numero_de_cliente %in% clientes_incompletos & foto_mes == period, numero_de_cliente]
		  clientes_a_borrar <- setdiff(clientes_incompletos, clientes_a_completar)
		  data <- data[!(numero_de_cliente %in% clientes_a_borrar)]
		  
		  
		  for(i in 2:length(needed_periods)) { #el mes actual no lo proceso
		    clientes_to_add <- setdiff(data_period$numero_de_cliente, data[foto_mes == needed_periods[i]]$numero_de_cliente)
		    dt_clientes_agregados = data.table(numero_de_cliente = clientes_to_add, foto_mes = needed_periods[i])
		    data <- rbindlist(list(data, dt_clientes_agregados), use.names = TRUE, fill = TRUE)
		    dt_clientes_agregados <- NULL
		  }
		  
		  #super imrpotante!!!
		  #data <- data[order(numero_de_cliente, foto_mes)]
		  setorder(data, numero_de_cliente, foto_mes)
		  
		  #super impornte!!
		  #data_period <- data_period[order(numero_de_cliente)]
		  setorder(data_period, numero_de_cliente)
		  
		  
		  period0 <- needed_periods[1]
		  period1 <- needed_periods[2]
		  period2 <- needed_periods[3]
		  period3 <- needed_periods[4]
		  

			tendency_columns <- setdiff(colnames(data), avoid_columns_for_tendency)
		  
		  y0 <- data[foto_mes == period3, ..tendency_columns]
		  y0[is.na(y0)] <- 0
		  #y0[is.infinite(y0)] <- 0
		  #y0[is.nan(y0)] <- 0
		  y1 <- data[foto_mes == period2, ..tendency_columns]
		  y1[is.na(y1)] <- 0
		  #y1[is.infinite(y1)] <- 0
		  #y1[is.nan(y1)] <- 0
		  y2 <- data[foto_mes == period1, ..tendency_columns]
		  y2[is.na(y2)] <- 0
		  #y2[is.infinite(y2)] <- 0
		  #y2[is.nan(y2)] <- 0
		  y3 <- data[foto_mes == period0, ..tendency_columns]
		  y3[is.na(y3)] <- 0
		  #y3[is.infinite(y3)] <- 0
		  #y3[is.nan(y3)] <- 0

		  
		  if(max_min_etc){
		    df_max <- pmax(y0, y1, y2, y3, na.rm = T)
		    df_max <- df_max / y0
		    df_min <- pmin(y0, y1, y2, y3, na.rm = T)
		    df_min <- df_min / y0
		    colnames(df_max) <- paste0(colnames(df_max), "__MAX")
		    colnames(df_min) <- paste0(colnames(df_min), "__MIN")
		    
		    data_period[, colnames(df_max) := as.list(df_max)]
		    data_period[, colnames(df_min) := as.list(df_min)]
		  }


		  if(growth_polinomio){
  			crecimiento_df_poli <- ((-2 * y0 + 9 * y1 - 18 * y2 + 11 * y3) / 6 * y0) #newton
  
  			colnames(crecimiento_df_poli) <- paste0(colnames(crecimiento_df_poli), "__CREC_POLI")
  			#crecimiento_df <- scale(crecimiento_df)
  			crecimiento_df_poli <- as.data.frame(crecimiento_df_poli)
  		    #crecimiento_df <- data.frame(lapply(crecimiento_df, function(col) { round(scales::rescale(-col, to = c(-100, 100)), 2) }))
  			
  		    data_period[, colnames(crecimiento_df_poli) := as.list(crecimiento_df_poli)]
  		    #VER LOS NAAA
		  }
		  
		  if(growth_dif_finitas){
		    crecimiento_df_difini <- (y2 - 4 * y1 + 3 * y0) / 2 * y0  #diferencia finitas regresivas
		    
		    colnames(crecimiento_df_difini) <- paste0(colnames(crecimiento_df_difini), "__CREC_DIFINI")
		    #crecimiento_df <- scale(crecimiento_df)
		    crecimiento_df_difini <- as.data.frame(crecimiento_df_difini)
		    #crecimiento_df <- data.frame(lapply(crecimiento_df, function(col) { round(scales::rescale(-col, to = c(-100, 100)), 2) }))
		    
		    data_period[, colnames(crecimiento_df_difini) := as.list(crecimiento_df_difini)]
		  }
		  
		  if(acceleration_polinomio){
			    aceleracion_df_poli <- (-y0 + 4 * y1 - 5 * y2 + 2 * y3) / y0 #newton
		      colnames(aceleracion_df_poli) <- paste0(colnames(aceleracion_df_poli), "__ACEL_POLI")
			    #aceleracion_df <- scale(aceleracion_df)
		      aceleracion_df_poli <- as.data.frame(aceleracion_df_poli)
		      #aceleracion_df <- data.frame(lapply(aceleracion_df, function(col) { round(scales::rescale(-col, to = c(-100, 100)), 2) }))
		    
		      data_period[, colnames(aceleracion_df_poli) := as.list(aceleracion_df_poli)]
		  }

		  if(acceleration_dif_finitas){
		      aceleracion_df_difini <- (-y3 + 4 * y2 - 5 * y1 + 2 * y0) / y0 #diferencia finitas regresivas
		      colnames(aceleracion_df_difini) <- paste0(colnames(aceleracion_df_difini), "__ACEL_DIFINI")
		      #aceleracion_df <- scale(aceleracion_df)
		      aceleracion_df_difini <- as.data.frame(aceleracion_df_difini)
		      #aceleracion_df <- data.frame(lapply(aceleracion_df, function(col) { round(scales::rescale(-col, to = c(-100, 100)), 2) }))
		    
		      data_period[, colnames(aceleracion_df_difini) := as.list(aceleracion_df_difini)]
		  }		  
		  
			if (tendency_andreu) {
				andreu_df <- (-(y3 - y2) / y2)
				colnames(andreu_df) <- paste0(colnames(andreu_df), "__ANDREU")
				andreu_df <- as.data.frame(andreu_df)
				data_period[, colnames(andreu_df) := as.list(andreu_df)]
			}

		}
		
		if(is.null(final_data)){
			final_data <- data_period
		} else {
			final_data <- rbindlist(list(final_data, data_period))
		}
		
		invisible(gc())
	}


	if (length(remove_columns_final) > 0) {
		final_data[, (remove_columns_final) := NULL]
	}


	if (length(categorical_features) > 0) {
		final_data[, (categorical_features) := lapply(.SD, as.factor), .SDcols = categorical_features]
	}


	return(final_data)
}
#aa <- load_dataset(c(201802))

calculate_present_features <- function(df) {
	return(mutate(df,
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
		mvr_mpagominimo = mv_mpagominimo / mv_mlimitecompra,

	z_prom_pagodeservicios = ifelse(tpagodeservicios == 0 || is.na(tpagodeservicios), 0, mpagodeservicios / tpagodeservicios),
	z_prom_pagomiscuentas = ifelse(tpagomiscuentas == 0 || is.na(tpagomiscuentas), 0, mpagomiscuentas / tpagomiscuentas),
	z_pagodeserviciosypagomiscuentas = rowSums(cbind(mpagodeservicios, mpagomiscuentas), na.rm = TRUE),
	z_prom_debitos = ifelse(tcuenta_debitos_automaticos == 0 || is.na(tcuenta_debitos_automaticos), 0, mcuenta_debitos_automaticos / tcuenta_debitos_automaticos),
	z_ratio_debitos_sueldo = mcuenta_debitos_automaticos / mplan_sueldo,

	z_prom_sueldoporempresa = mplan_sueldo / tplan_sueldo,
	z_sueldototal = rowSums(cbind(mplan_sueldo, mplan_sueldo_manual), na.rm = TRUE),
	z_ratio_limitestarjetas_sueldo = z_sueldototal / mv_mlimitecompra,

	z_ratio_rentabilidad = mrentabilidad / mrentabilidad_annual,

	z_total_prestamos = rowSums(cbind(mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios), na.rm = TRUE),
	z_prom_prestamos = z_total_prestamos / rowSums(cbind(cprestamos_personales, cprestamos_prendarios, cprestamos_hipotecarios), na.rm = TRUE),

	z_prom_prestamos_personales = mprestamos_personales / cprestamos_personales,
	z_prom_prestamos_prendarios = mprestamos_prendarios / cprestamos_prendarios,
	z_prom_prestamos_hipotecarios = mprestamos_hipotecarios / cprestamos_hipotecarios,


	z_prom_transaccionesdebito = ifelse(ttarjeta_debito == 0 || is.na(ttarjeta_debito), 0, ctarjeta_debito_transacciones / ttarjeta_debito),

	z_total_transaccionescchb = rowSums(cbind(ccallcenter_transacciones, chomebanking_transacciones), na.rm = TRUE)
	))
}
