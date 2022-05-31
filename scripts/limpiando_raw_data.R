# cargando datos crudos

seguridad_raw_data <- read.csv(unzip("raw_data/CS_SEGURIDAD_ALIMENTARIA.csv.csv.zip"))

seguridad_raw_data <- readRDS("raw_data/seguridad_alimentaria_datos_crudos.rds")

# cambiando nombres a las columnas

colnames(seguridad_raw_data) <- c("upm", "vivienda", "hogar", "renglon", "p1", "p2",
                         "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10",
                         "p11", "p12", "p13", "p14", "p15", "p16", "edad",
                         "sexo", "entidad", "dominio", "region", "estrato_diseno",
                         "upm_diseno", "estrato_sociodemografico", "factor")

# modificar sexo

seguridad_raw_data$sexo <- as.factor(seguridad_raw_data$sexo)
seguridad_raw_data$sexo <- as.character(seguridad_raw_data$sexo)

seguridad_raw_data$sexo <- revalue(seguridad_raw_data$sexo,
                          c("1" = "hombre", "2" = "mujer"))

# modificando la entidad

seguridad_raw_data$entidad <- as.factor(seguridad_raw_data$entidad)
seguridad_raw_data$entidad <- as.character(seguridad_raw_data$entidad)

seguridad_raw_data$entidad <- revalue(seguridad_raw_data$entidad,
                             c("1"= "Aguascalientes", "2"="Baja California Norte",
                               "3"="Baja California Sur", "4"="Campeche",
                               "5"="Coahuila", "6"="Colima", "7"="Chiapas",
                               "8"= "Chihuahua", "9"="CdMx", "10"="Durango",
                               "11"="Guanajuato", "12"="Guerrero", "13"="Hidalgo",
                               "14"="Jalisco", "15"="Edo. México","16"= "Michoacán",
                               "17"="Morelos", "18"="Nayarit", "19"= "Nuevo León",
                               "20"="Oaxaca", "21"= "Puebla", "22"="Querétaro",
                               "23"="Quintana Roo", "24"="San Luis Potosí",
                               "25"= "Sinaloa", "26"="Sonora", "27"="Tabasco",
                               "28"="Tamaulipas","29"="Tlaxcala", "30"="Veracruz",
                               "31"="Yucatán", "32"="Zacatecas"))

# modificando el dominio

seguridad_raw_data$dominio <- as.factor(seguridad_raw_data$dominio)
seguridad_raw_data$dominio <- as.character(seguridad_raw_data$dominio)

seguridad_raw_data$dominio <- revalue(seguridad_raw_data$dominio,
                             c("1" = "urbano", "2" = "rural"))

# modificando la region

seguridad_raw_data$region <- as.factor(seguridad_raw_data$region)
seguridad_raw_data$region <- as.character(seguridad_raw_data$region)

seguridad_raw_data$region <- revalue(seguridad_raw_data$region,
                            c("1" = "norte", "2" = "centro",
                              "3" = "cdmx", "4" = "sur"))

# modificando respuestas p1

seguridad_raw_data$p1 <- as.factor(seguridad_raw_data$p1)
seguridad_raw_data$p1 <- as.character(seguridad_raw_data$p1)

seguridad_raw_data$p1 <- revalue(seguridad_raw_data$p1,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))

# modificando respuestas p2

seguridad_raw_data$p2 <- as.factor(seguridad_raw_data$p2)
seguridad_raw_data$p2 <- as.character(seguridad_raw_data$p2)

seguridad_raw_data$p2 <- revalue(seguridad_raw_data$p2,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))

# modificando respuestas p3

seguridad_raw_data$p3 <- as.factor(seguridad_raw_data$p3)
seguridad_raw_data$p3 <- as.character(seguridad_raw_data$p3)

seguridad_raw_data$p3 <- revalue(seguridad_raw_data$p3,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))

# modificando respuestas p4

seguridad_raw_data$p4 <- as.factor(seguridad_raw_data$p4)
seguridad_raw_data$p4 <- as.character(seguridad_raw_data$p4)

seguridad_raw_data$p4 <- revalue(seguridad_raw_data$p4,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))

# modificando respuestas p5

seguridad_raw_data$p5 <- as.factor(seguridad_raw_data$p5)
seguridad_raw_data$p5 <- as.character(seguridad_raw_data$p5)

seguridad_raw_data$p5 <- revalue(seguridad_raw_data$p5,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))

# modificando respuestas p6

seguridad_raw_data$p6 <- as.factor(seguridad_raw_data$p6)
seguridad_raw_data$p6 <- as.character(seguridad_raw_data$p6)

seguridad_raw_data$p6 <- revalue(seguridad_raw_data$p6,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))

# modificando respuestas p7

seguridad_raw_data$p7 <- as.factor(seguridad_raw_data$p7)
seguridad_raw_data$p7 <- as.character(seguridad_raw_data$p7)

seguridad_raw_data$p7 <- revalue(seguridad_raw_data$p7,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))

# modifiando respuestas p8

seguridad_raw_data$p8 <- as.factor(seguridad_raw_data$p8)
seguridad_raw_data$p8 <- as.character(seguridad_raw_data$p8)

seguridad_raw_data$p8 <- revalue(seguridad_raw_data$p8,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))

# modificando respuestas p9

seguridad_raw_data$p9 <- as.factor(seguridad_raw_data$p9)
seguridad_raw_data$p9 <- as.character(seguridad_raw_data$p9)

seguridad_raw_data$p9 <- revalue(seguridad_raw_data$p9,
                                 c("1" = "si", "2"="no"))

# modificando respuesta p10

seguridad_raw_data$p10 <- as.factor(seguridad_raw_data$p10)
seguridad_raw_data$p10 <- as.character(seguridad_raw_data$p10)

seguridad_raw_data$p10 <- revalue(seguridad_raw_data$p10,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))


# modificando respuesta p11

seguridad_raw_data$p11 <- as.factor(seguridad_raw_data$p11)
seguridad_raw_data$p11 <- as.character(seguridad_raw_data$p11)

seguridad_raw_data$p11 <- revalue(seguridad_raw_data$p11,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))

# modificando respuestas p12

seguridad_raw_data$p12 <- as.factor(seguridad_raw_data$p12)
seguridad_raw_data$p12 <- as.character(seguridad_raw_data$p12)

seguridad_raw_data$p12 <- revalue(seguridad_raw_data$p12,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde"))

# modificando respuestas p13

seguridad_raw_data$p13 <- as.factor(seguridad_raw_data$p13)
seguridad_raw_data$p13 <- as.character(seguridad_raw_data$p13)

seguridad_raw_data$p13 <- revalue(seguridad_raw_data$p13,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))


# modificando respuestas p14

seguridad_raw_data$p14 <- as.factor(seguridad_raw_data$p14)
seguridad_raw_data$p14 <- as.character(seguridad_raw_data$p14)

seguridad_raw_data$p14 <- revalue(seguridad_raw_data$p14,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde"))

# modificando respuestas p15

seguridad_raw_data$p15 <- as.factor(seguridad_raw_data$p15)
seguridad_raw_data$p15 <- as.character(seguridad_raw_data$p15)

seguridad_raw_data$p15 <- revalue(seguridad_raw_data$p15,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde", "9" = "no sabe"))

# modificando respuestas p16

seguridad_raw_data$p16 <- as.factor(seguridad_raw_data$p16)
seguridad_raw_data$p16 <- as.character(seguridad_raw_data$p16)

seguridad_raw_data$p16 <- revalue(seguridad_raw_data$p16,
                                 c("1" = "si", "2"="no",
                                   "8" = "no responde"))

# guardando datos crudos

write_rds(seguridad_raw_data, "raw_data/seguridad_alimentaria_datos_crudos.rds")

# dividiendo data set de adultos y dataset de adultos/jovenes

seguridad_raw_data$p9 <- as.factor(seguridad_raw_data$p9)
seguridad_raw_data$p9 <- as.character(seguridad_raw_data$p9)

seguridad <-  seguridad_raw_data %>%
  mutate(tipo = case_when(p9 == "2" ~ "adultos",
                          p9 == "1" ~ "familia"))

write_rds(seguridad, "clean_data/seguridad_alimentaria_datos_limpios.rds")

# paso para conocer el numero de personas cono menores de edad
seguridad %>%
  select(tipo) %>%
  group_by(tipo) %>%
  count()

solo_adultos <- seguridad %>%
  filter(tipo == "adultos")

solo_adultos <- solo_adultos %>%
  select(upm, vivienda, hogar, renglon, p1, p2, p3, p4, p5, p6, p7, p8, p9,
         edad, sexo, entidad, dominio, region, estrato_diseno, upm_diseno,
         estrato_sociodemografico, factor)

solo_familias <- seguridad %>%
  filter(tipo == "familia")

solo_familias <- solo_familias %>%
  select(-tipo)
