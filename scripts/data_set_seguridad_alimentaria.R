# limpiando data set de adultos y familias

adultos_limpia <- solo_adultos %>%
  filter(p1 == 1 | p1 == 2,
         p2 == 1 | p2 == 2,
         p3 == 1 | p3 == 2,
         p4 == 1 | p4 == 2,
         p5 == 1 | p5 == 2,
         p6 == 1 | p6 == 2,
         p7 == 1 | p7 == 2,
         p8 == 1 | p8 == 2,
         p9 == 1 | p9 == 2)

# modificando respuestas p1

adultos_limpia$p1 <- as.factor(adultos_limpia$p1)
adultos_limpia$p1 <- as.character(adultos_limpia$p1)

adultos_limpia$p1 <- revalue(adultos_limpia$p1,
                     c("2"="0"))

# modificando respuestas p2

adultos_limpia$p2 <- as.factor(adultos_limpia$p2)
adultos_limpia$p2 <- as.character(adultos_limpia$p2)

adultos_limpia$p2 <- revalue(adultos_limpia$p2,
                     c("2"="0"))

# modificando respuestas p3

adultos_limpia$p3 <- as.factor(adultos_limpia$p3)
adultos_limpia$p3 <- as.character(adultos_limpia$p3)

adultos_limpia$p3 <- revalue(adultos_limpia$p3,
                     c("2"="0"))

# modificando respuestas p4

adultos_limpia$p4 <- as.factor(adultos_limpia$p4)
adultos_limpia$p4 <- as.character(adultos_limpia$p4)

adultos_limpia$p4 <- revalue(adultos_limpia$p4,
                     c("2"="0"))

# modificando respuestas p5

adultos_limpia$p5 <- as.factor(adultos_limpia$p5)
adultos_limpia$p5 <- as.character(adultos_limpia$p5)

adultos_limpia$p5 <- revalue(adultos_limpia$p5,
                     c("2"="0"))

# modificando respuestas p6

adultos_limpia$p6 <- as.factor(adultos_limpia$p6)
adultos_limpia$p6 <- as.character(adultos_limpia$p6)

adultos_limpia$p6 <- revalue(adultos_limpia$p6,
                     c("2"="0"))

# modificando respuestas p7

adultos_limpia$p7 <- as.factor(adultos_limpia$p7)
adultos_limpia$p7 <- as.character(adultos_limpia$p7)

adultos_limpia$p7 <- revalue(adultos_limpia$p7,
                     c("2"="0"))

# modifiando respuestas p8

adultos_limpia$p8 <- as.factor(adultos_limpia$p8)
adultos_limpia$p8 <- as.character(adultos_limpia$p8)

adultos_limpia$p8 <- revalue(adultos_limpia$p8,
                     c("2"="0"))

# convirtiendo a numero de nuevo

adultos_limpia$p1 <- as.numeric(adultos_limpia$p1)
adultos_limpia$p2 <- as.numeric(adultos_limpia$p2)
adultos_limpia$p3 <- as.numeric(adultos_limpia$p3)
adultos_limpia$p4 <- as.numeric(adultos_limpia$p4)
adultos_limpia$p5 <- as.numeric(adultos_limpia$p5)
adultos_limpia$p6 <- as.numeric(adultos_limpia$p6)
adultos_limpia$p7 <- as.numeric(adultos_limpia$p7)
adultos_limpia$p8 <- as.numeric(adultos_limpia$p8)

adultos_limpia$suma <- rowSums(adultos_limpia[5:12])

# agregando columna de seguridad e inseguridad

adultos_limpia <- adultos_limpia %>%
  mutate(alimentaria = case_when(suma == 0 ~ "seguridad_alimentaria",
                                 suma == 1 | suma == 2 | suma == 3 ~ "inseguridad_leve",
                                 suma == 4 | suma == 5 | suma == 6 ~ "inseguridad_moderada",
                                 suma == 7 | suma == 8 ~ "inseguridad_severa"))

# como graficar seguridad e inseguridad alimentaria
adultos_limpia %>%
  select(alimentaria) %>%
  group_by(alimentaria) %>%
  count() %>%
  mutate(porcentaje = freq/19178) %>%
  ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
  geom_col()+
  ylab("")+
  xlab("")+
  ylim(0,1)+
  theme_grey()+
  theme(legend.position="none")


###
###
###

# limpiando dataset de familias

familias_limpia <- solo_familias %>%
  select(-p9) %>%
  filter(p1 == 1 | p1 == 2,
         p2 == 1 | p2 == 2,
         p3 == 1 | p3 == 2,
         p4 == 1 | p4 == 2,
         p5 == 1 | p5 == 2,
         p6 == 1 | p6 == 2,
         p7 == 1 | p7 == 2,
         p8 == 1 | p8 == 2,
         p10 == 1 | p10 == 2,
         p11 == 1 | p11 == 2,
         p12 == 1 | p12 == 2,
         p13 == 1 | p13 == 2,
         p14 == 1 | p14 == 2,
         p15 == 1 | p15 == 2,
         p16 == 1 | p16 == 2)

# modificando respuestas p1

familias_limpia$p1 <- as.factor(familias_limpia$p1)
familias_limpia$p1 <- as.character(familias_limpia$p1)

familias_limpia$p1 <- revalue(familias_limpia$p1,
                             c("2"="0"))

# modificando respuestas p2

familias_limpia$p2 <- as.factor(familias_limpia$p2)
familias_limpia$p2 <- as.character(familias_limpia$p2)

familias_limpia$p2 <- revalue(familias_limpia$p2,
                             c("2"="0"))

# modificando respuestas p3

familias_limpia$p3 <- as.factor(familias_limpia$p3)
familias_limpia$p3 <- as.character(familias_limpia$p3)

familias_limpia$p3 <- revalue(familias_limpia$p3,
                             c("2"="0"))

# modificando respuestas p4

familias_limpia$p4 <- as.factor(familias_limpia$p4)
familias_limpia$p4 <- as.character(familias_limpia$p4)

familias_limpia$p4 <- revalue(familias_limpia$p4,
                             c("2"="0"))

# modificando respuestas p5

familias_limpia$p5 <- as.factor(familias_limpia$p5)
familias_limpia$p5 <- as.character(familias_limpia$p5)

familias_limpia$p5 <- revalue(familias_limpia$p5,
                             c("2"="0"))

# modificando respuestas p6

familias_limpia$p6 <- as.factor(familias_limpia$p6)
familias_limpia$p6 <- as.character(familias_limpia$p6)

familias_limpia$p6 <- revalue(familias_limpia$p6,
                             c("2"="0"))

# modificando respuestas p7

familias_limpia$p7 <- as.factor(familias_limpia$p7)
familias_limpia$p7 <- as.character(familias_limpia$p7)

familias_limpia$p7 <- revalue(familias_limpia$p7,
                             c("2"="0"))

# modifiando respuestas p8

familias_limpia$p8 <- as.factor(familias_limpia$p8)
familias_limpia$p8 <- as.character(familias_limpia$p8)

familias_limpia$p8 <- revalue(familias_limpia$p8,
                             c("2"="0"))

# modifiando respuestas p10

familias_limpia$p10 <- as.factor(familias_limpia$p10)
familias_limpia$p10 <- as.character(familias_limpia$p10)

familias_limpia$p10 <- revalue(familias_limpia$p10,
                              c("2"="0"))


# modifiando respuestas p11

familias_limpia$p11 <- as.factor(familias_limpia$p11)
familias_limpia$p11 <- as.character(familias_limpia$p11)

familias_limpia$p11 <- revalue(familias_limpia$p11,
                              c("2"="0"))

# modifiando respuestas p12

familias_limpia$p12 <- as.factor(familias_limpia$p12)
familias_limpia$p12 <- as.character(familias_limpia$p12)

familias_limpia$p12 <- revalue(familias_limpia$p12,
                              c("2"="0"))

# modifiando respuestas p13

familias_limpia$p13 <- as.factor(familias_limpia$p13)
familias_limpia$p13 <- as.character(familias_limpia$p13)

familias_limpia$p13 <- revalue(familias_limpia$p13,
                              c("2"="0"))


# modifiando respuestas p14

familias_limpia$p14 <- as.factor(familias_limpia$p14)
familias_limpia$p14 <- as.character(familias_limpia$p14)

familias_limpia$p14 <- revalue(familias_limpia$p14,
                              c("2"="0"))

# modifiando respuestas p15

familias_limpia$p15 <- as.factor(familias_limpia$p15)
familias_limpia$p15 <- as.character(familias_limpia$p15)

familias_limpia$p15 <- revalue(familias_limpia$p15,
                              c("2"="0"))

# modifiando respuestas p16

familias_limpia$p16 <- as.factor(familias_limpia$p16)
familias_limpia$p16 <- as.character(familias_limpia$p16)

familias_limpia$p16 <- revalue(familias_limpia$p16,
                              c("2"="0"))


# convirtiendo a numero de nuevo

familias_limpia$p1 <- as.numeric(familias_limpia$p1)
familias_limpia$p2 <- as.numeric(familias_limpia$p2)
familias_limpia$p3 <- as.numeric(familias_limpia$p3)
familias_limpia$p4 <- as.numeric(familias_limpia$p4)
familias_limpia$p5 <- as.numeric(familias_limpia$p5)
familias_limpia$p6 <- as.numeric(familias_limpia$p6)
familias_limpia$p7 <- as.numeric(familias_limpia$p7)
familias_limpia$p8 <- as.numeric(familias_limpia$p8)
familias_limpia$p10 <- as.numeric(familias_limpia$p10)
familias_limpia$p11 <- as.numeric(familias_limpia$p11)
familias_limpia$p12 <- as.numeric(familias_limpia$p12)
familias_limpia$p13 <- as.numeric(familias_limpia$p13)
familias_limpia$p14 <- as.numeric(familias_limpia$p14)
familias_limpia$p15 <- as.numeric(familias_limpia$p15)
familias_limpia$p16 <- as.numeric(familias_limpia$p16)

familias_limpia$suma <- rowSums(familias_limpia[5:19])

# agregando columna de seguridad e inseguridad

familias_limpia <- familias_limpia %>%
  mutate(alimentaria = case_when(suma == 0 ~ "seguridad_alimentaria",
                                 suma == 1 | suma == 2 | suma == 3 | suma == 4 | suma == 5 ~ "inseguridad_leve",
                                 suma == 6 | suma == 7 | suma == 8 | suma == 9 | suma == 10 ~ "inseguridad_moderada",
                                 suma == 11 | suma == 12 | suma == 13 | suma == 14 | suma == 15 ~ "inseguridad_severa"))

# como graficar seguridad e inseguridad alimentaria
familias_limpia %>%
  select(alimentaria) %>%
  group_by(alimentaria) %>%
  count() %>%
  mutate(porcentaje = freq/24973) %>%
  ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
  geom_col()+
  ylab("")+
  xlab("")+
  ylim(0,1)+
  theme_grey()+
  theme(legend.position="none")


