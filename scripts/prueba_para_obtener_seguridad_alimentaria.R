prueba <- solo_adultos %>%
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

prueba$p1 <- as.factor(prueba$p1)
prueba$p1 <- as.character(prueba$p1)

prueba$p1 <- revalue(prueba$p1,
                     c("2"="0"))

# modificando respuestas p2

prueba$p2 <- as.factor(prueba$p2)
prueba$p2 <- as.character(prueba$p2)

prueba$p2 <- revalue(prueba$p2,
                     c("2"="0"))

# modificando respuestas p3

prueba$p3 <- as.factor(prueba$p3)
prueba$p3 <- as.character(prueba$p3)

prueba$p3 <- revalue(prueba$p3,
                     c("2"="0"))

# modificando respuestas p4

prueba$p4 <- as.factor(prueba$p4)
prueba$p4 <- as.character(prueba$p4)

prueba$p4 <- revalue(prueba$p4,
                     c("2"="0"))

# modificando respuestas p5

prueba$p5 <- as.factor(prueba$p5)
prueba$p5 <- as.character(prueba$p5)

prueba$p5 <- revalue(prueba$p5,
                     c("2"="0"))

# modificando respuestas p6

prueba$p6 <- as.factor(prueba$p6)
prueba$p6 <- as.character(prueba$p6)

prueba$p6 <- revalue(prueba$p6,
                     c("2"="0"))

# modificando respuestas p7

prueba$p7 <- as.factor(prueba$p7)
prueba$p7 <- as.character(prueba$p7)

prueba$p7 <- revalue(prueba$p7,
                     c("2"="0"))

# modifiando respuestas p8

prueba$p8 <- as.factor(prueba$p8)
prueba$p8 <- as.character(prueba$p8)

prueba$p8 <- revalue(prueba$p8,
                     c("2"="0"))

# convirtiendo a numero de nuevo

prueba$p1 <- as.numeric(prueba$p1)
prueba$p2 <- as.numeric(prueba$p2)
prueba$p3 <- as.numeric(prueba$p3)
prueba$p4 <- as.numeric(prueba$p4)
prueba$p5 <- as.numeric(prueba$p5)
prueba$p6 <- as.numeric(prueba$p6)
prueba$p7 <- as.numeric(prueba$p7)
prueba$p8 <- as.numeric(prueba$p8)


prueba$suma <- rowSums(prueba[5:12])
