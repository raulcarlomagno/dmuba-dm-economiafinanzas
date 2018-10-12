library(DBI)

dbdir <- "D:/dsbank"
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)


dbSendQuery(con, "CREATE SCHEMA dataset")

createTable <- "CREATE TABLE dataset.periodos (
	     numero_de_cliente INTEGER,
	     foto_mes INTEGER,
	     marketing_activo_ultimos90dias INTEGER,
	     cliente_vip INTEGER,
	     internet INTEGER,
	     cliente_edad INTEGER,
	     cliente_antiguedad INTEGER,
	     mrentabilidad DOUBLE,
	     mrentabilidad_annual DOUBLE,
	     mcomisiones DOUBLE,
	     mactivos_margen DOUBLE,
	     mpasivos_margen DOUBLE,
	     marketing_coss_selling INTEGER,
	     tpaquete1 INTEGER,
	     tpaquete2 INTEGER,
	     tpaquete3 INTEGER,
	     tpaquete4 INTEGER,
	     tpaquete5 INTEGER,
	     tpaquete6 INTEGER,
	     tpaquete7 INTEGER,
	     tpaquete8 INTEGER,
	     tpaquete9 INTEGER,
	     tcuentas INTEGER,
	     tcuenta_corriente INTEGER,
	     mcuenta_corriente_Nopaquete DOUBLE,
	     mcuenta_corriente_Paquete DOUBLE,
	     mcuenta_corriente_dolares INTEGER,
	     tcaja_ahorro INTEGER,
	     mcaja_ahorro_Paquete DOUBLE,
	     mcaja_ahorro_Nopaquete DOUBLE,
	     mcaja_ahorro_dolares DOUBLE,
	     mdescubierto_preacordado DOUBLE,
	     mcuentas_saldo DOUBLE,
	     ttarjeta_debito INTEGER,
	     ctarjeta_debito_transacciones INTEGER,
	     mautoservicio DOUBLE,
	     ttarjeta_visa INTEGER,
	     ctarjeta_visa_transacciones INTEGER,
	     mtarjeta_visa_consumo DOUBLE,
	     ttarjeta_master INTEGER,
	     ctarjeta_master_transacciones INTEGER,
	     mtarjeta_master_consumo DOUBLE,
	     cprestamos_personales INTEGER,
	     mprestamos_personales DOUBLE,
	     cprestamos_prendarios INTEGER,
	     mprestamos_prendarios DOUBLE,
	     cprestamos_hipotecarios INTEGER,
	     mprestamos_hipotecarios DOUBLE,
	     tplazo_fijo INTEGER,
	     mplazo_fijo_dolares DOUBLE,
	     mplazo_fijo_pesos DOUBLE,
	     tfondos_comunes_inversion INTEGER,
	     mfondos_comunes_inversion_pesos DOUBLE,
	     mfondos_comunes_inversion_dolares DOUBLE,
	     ttitulos INTEGER,
	     mtitulos DOUBLE,
	     tseguro_vida_mercado_abierto INTEGER,
	     tseguro_auto INTEGER,
	     tseguro_vivienda INTEGER,
	     tseguro_accidentes_personales INTEGER,
	     tcaja_seguridad INTEGER,
	     mbonos_corporativos INTEGER,
	     mmonedas_extranjeras INTEGER,
	     minversiones_otras INTEGER,
	     tplan_sueldo INTEGER,
	     mplan_sueldo DOUBLE,
	     mplan_sueldo_manual DOUBLE,
	     cplan_sueldo_transaccion INTEGER,
	     tcuenta_debitos_automaticos INTEGER,
	     mcuenta_debitos_automaticos DOUBLE,
	     ttarjeta_visa_debitos_automaticos INTEGER,
	     mttarjeta_visa_debitos_automaticos DOUBLE,
	     ttarjeta_master_debitos_automaticos INTEGER,
	     mttarjeta_master_debitos_automaticos DOUBLE,
	     tpagodeservicios INTEGER,
	     mpagodeservicios DOUBLE,
	     tpagomiscuentas INTEGER,
	     mpagomiscuentas DOUBLE,
	     ccajeros_propios_descuentos INTEGER,
	     mcajeros_propios_descuentos DOUBLE,
	     ctarjeta_visa_descuentos INTEGER,
	     mtarjeta_visa_descuentos DOUBLE,
	     ctarjeta_master_descuentos INTEGER,
	     mtarjeta_master_descuentos DOUBLE,
	     ccuenta_descuentos INTEGER,
	     mcuenta_descuentos INTEGER,
	     ccomisiones_mantenimiento INTEGER,
	     mcomisiones_mantenimiento DOUBLE,
	     ccomisiones_otras INTEGER,
	     mcomisiones_otras DOUBLE,
	     tcambio_monedas INTEGER,
	     ccambio_monedas_compra INTEGER,
	     mcambio_monedas_compra DOUBLE,
	     ccambio_monedas_venta INTEGER,
	     mcambio_monedas_venta DOUBLE,
	     ctransferencias_recibidas INTEGER,
	     mtransferencias_recibidas DOUBLE,
	     ctransferencias_emitidas INTEGER,
	     mtransferencias_emitidas DOUBLE,
	     cextraccion_autoservicio INTEGER,
	     mextraccion_autoservicio DOUBLE,
	     ccheques_depositados INTEGER,
	     mcheques_depositados DOUBLE,
	     ccheques_emitidos INTEGER,
	     mcheques_emitidos DOUBLE,
	     ccheques_depositados_rechazados INTEGER,
	     mcheques_depositados_rechazados DOUBLE,
	     ccheques_emitidos_rechazados INTEGER,
	     mcheques_emitidos_rechazados DOUBLE,
	     tcallcenter INTEGER,
	     ccallcenter_transacciones INTEGER,
	     thomebanking INTEGER,
	     chomebanking_transacciones INTEGER,
	     tautoservicio INTEGER,
	     cautoservicio_transacciones INTEGER,
	     tcajas INTEGER,
	     tcajas_consultas INTEGER,
	     tcajas_depositos INTEGER,
	     tcajas_extracciones INTEGER,
	     tcajas_otras INTEGER,
	     ccajeros_propio_transacciones INTEGER,
	     mcajeros_propio DOUBLE,
	     ccajeros_ajenos_transacciones INTEGER,
	     mcajeros_ajenos DOUBLE,
	     tmovimientos_ultimos90dias INTEGER,
	     Master_marca_atraso INTEGER,
	     Master_cuenta_estado INTEGER,
	     Master_mfinanciacion_limite DOUBLE,
	     Master_Fvencimiento INTEGER,
	     Master_Finiciomora INTEGER,
	     Master_msaldototal DOUBLE,
	     Master_msaldopesos DOUBLE,
	     Master_msaldodolares DOUBLE,
	     Master_mconsumospesos DOUBLE,
	     Master_mconsumosdolares DOUBLE,
	     Master_mlimitecompra DOUBLE,
	     Master_madelantopesos DOUBLE,
	     Master_madelantodolares DOUBLE,
	     Master_fultimo_cierre INTEGER,
	     Master_mpagado DOUBLE,
	     Master_mpagospesos DOUBLE,
	     Master_mpagosdolares DOUBLE,
	     Master_fechaalta INTEGER,
	     Master_mconsumototal DOUBLE,
	     Master_tconsumos INTEGER,
	     Master_tadelantosefectivo INTEGER,
	     Master_mpagominimo DOUBLE,
	     Visa_marca_atraso INTEGER,
	     Visa_cuenta_estado INTEGER,
	     Visa_mfinanciacion_limite DOUBLE,
	     Visa_Fvencimiento INTEGER,
	     Visa_Finiciomora INTEGER,
	     Visa_msaldototal DOUBLE,
	     Visa_msaldopesos DOUBLE,
	     Visa_msaldodolares DOUBLE,
	     Visa_mconsumospesos DOUBLE,
	     Visa_mconsumosdolares DOUBLE,
	     Visa_mlimitecompra DOUBLE,
	     Visa_madelantopesos DOUBLE,
	     Visa_madelantodolares DOUBLE,
	     Visa_fultimo_cierre INTEGER,
	     Visa_mpagado DOUBLE,
	     Visa_mpagospesos DOUBLE,
	     Visa_mpagosdolares DOUBLE,
	     Visa_fechaalta INTEGER,
	     Visa_mconsumototal DOUBLE,
	     Visa_tconsumos INTEGER,
	     Visa_tadelantosefectivo INTEGER,
	     Visa_mpagominimo DOUBLE,
	     clase_ternaria VARCHAR(16));"

dbSendQuery(con, createTable)

periodos <- c(201601:201612, 201701:201712, 201801:201806)

for (periodo in periodos) {
	print(paste("inserting", periodo))
	dbSendQuery(con, paste0("COPY OFFSET 2 INTO dataset.periodos FROM 'D:\\Dropbox\\UBA2018\\datasets\\dias\\", periodo,"_dias.txt' USING DELIMITERS '\t', '\n' NULL AS '';"))
}

#dbWriteTable(con, "periodos", "D:\\Dropbox\\UBA2018\\datasets\\dias\\201804_dias.txt", sep = "\t") #, eol = "\r\n")


y <- dbGetQuery(con, "SELECT * FROM dataset.periodos where foto_mes = 201804")

dbSendQuery(con, "UPDATE dataset.periodos SET clase_ternaria = REPLACE(clase_ternaria, '\"', '')")

dbSendQuery(con, "CREATE INDEX idx_numero_de_cliente ON dataset.periodos (numero_de_cliente)")
dbSendQuery(con, "CREATE ORDERED INDEX idx_foto_mes_ordered ON dataset.periodos (foto_mes)")
dbSendQuery(con, "CREATE IMPRINTS INDEX idx_foto_mes_imprints ON dataset.periodos (foto_mes)")




dbBegin(con)



dbCommit(con)

b <- dbGetQuery(con, "SELECT AVG(mplan_sueldo) FROM dataset.periodos")

y <- dbGetQuery(con, "SELECT * FROM dataset.periodos where foto_mes = 201804")
y <- dbGetQuery(con, "SELECT clase_ternaria, REPLACE(clase_ternaria, '\"', '') FROM periodos where foto_mes = 201804")



x <- dbReadTable(con, "periodos")

aa <- "select
(select avg(p1.mplan_sueldo) from dataset.periodos p1 where p1.numero_de_cliente = p.numero_de_cliente and p1.foto_date < p.foto_date AND p1.foto_date > p.foto_date + INTERVAL '-6' MONTH) as prom,
	(select max(p1.mplan_sueldo) from dataset.periodos p1 where p1.numero_de_cliente = p.numero_de_cliente and p1.foto_date < p.foto_date AND p1.foto_date > p.foto_date + INTERVAL '-6' MONTH) as maxi,
	(select STDDEV_POP(p1.mplan_sueldo) from dataset.periodos p1 where p1.numero_de_cliente = p.numero_de_cliente and p1.foto_date < p.foto_date AND p1.foto_date > p.foto_date + INTERVAL '-6' MONTH) as stddev2,
	p.numero_de_cliente, p.foto_mes
from dataset.periodos p where p.foto_mes in (201804, 201803)"

system.time(y3 <- dbGetQuery(con, aa))

q3 <- "select
(select avg(p1.mplan_sueldo) from dataset.periodos p1 where p1.numero_de_cliente = p.numero_de_cliente and p1.foto_mes < p.foto_mes AND p1.foto_mes > date_to_str(str_to_date(substr(p.foto_mes, 1, 4) || '-' || substr(p.foto_mes, 5, 2) || '-15', '%Y-%m-%d') + INTERVAL '-6' month, '%Y%m')) as prom,
	(select max(p1.mplan_sueldo) from dataset.periodos p1 where p1.numero_de_cliente = p.numero_de_cliente and p1.foto_mes < p.foto_mes AND p1.foto_mes > date_to_str(str_to_date(substr(p.foto_mes, 1, 4) || '-' || substr(p.foto_mes, 5, 2) || '-15', '%Y-%m-%d') + INTERVAL '-6' month, '%Y%m')) as maxi,
	p.numero_de_cliente, p.foto_mes
from dataset.periodos p where p.foto_mes in (201804, 201803)"
y3 <- dbGetQuery(con, q3)


q <- "select 
(select avg(p1.mplan_sueldo) from dataset.periodos p1 where p1.numero_de_cliente = p.numero_de_cliente and p1.foto_mes < p.foto_mes AND p1.foto_mes > 201709) as prom, 
(select max(p1.mplan_sueldo) from dataset.periodos p1 where p1.numero_de_cliente = p.numero_de_cliente and p1.foto_mes < p.foto_mes AND p1.foto_mes > 201709) as maxi,
p.numero_de_cliente, p.foto_mes
from dataset.periodos p where p.foto_mes in (201804, 201803)
order by p.numero_de_cliente asc"

q2 <- "select p.* from dataset.periodos p where p.foto_mes in (201804, 201803)"

y <- dbGetQuery(con, q)
y2 <- dbGetQuery(con, q2)

z <- cbind(y,y2)


dbDisconnect(con, shutdown = TRUE)
con <- NULL


