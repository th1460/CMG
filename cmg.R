require(readxl)
require(dplyr)

dcm1 <- read_xlsx("CMG.xlsx", sheet = "CM1")
dcm2a <- read_xlsx("CMG.xlsx", sheet = "CM2A")
dcm2b <- read_xlsx("CMG.xlsx", sheet = "CM2B")
dcm3 <- read_xlsx("CMG.xlsx", sheet = "CM3")
dcm4 <- read_xlsx("CMG.xlsx", sheet = "CM4")
dcm5 <- read_xlsx("CMG.xlsx", sheet = "CM5")

cmg <- function(porte, relevo, classe, padrao, pavimento, k, d, et,
                peca, pecb, pecc, pecd, pece, pecf, pecg){
  
  cm1 <- dcm1 %>% filter(PORTE == porte) %>% pull(CUSTO)
  
  cm2a <- dcm2a %>% filter(PORTE == porte) %>% pull(CUSTO)
  
  cm2b <- dcm2b %>% filter(RELEVO == relevo,
                           CLASSE == classe) %>% pull(CUSTO)
  
  cm3 <- dcm3 %>% filter(PORTE == porte, 
                         PADRAO == padrao) %>% pull(CUSTO)
  
  cm4 <- dcm4 %>% filter(RELEVO == relevo,
                         CLASSE == classe) %>% pull(CUSTO)
  
  cm5a <- dcm5 %>% filter(RELEVO == relevo, 
                          CLASSE == classe,
                          PAVIMENTO == "A") %>% pull(CUSTO)
  cm5b <- dcm5 %>% filter(RELEVO == relevo, 
                          CLASSE == classe,
                          PAVIMENTO == "B") %>% pull(CUSTO)
  cm5c <- dcm5 %>% filter(RELEVO == relevo, 
                          CLASSE == classe,
                          PAVIMENTO == "C") %>% pull(CUSTO)
  cm5d <- dcm5 %>% filter(RELEVO == relevo, 
                          CLASSE == classe,
                          PAVIMENTO == "D") %>% pull(CUSTO)
  cm5e <- dcm5 %>% filter(RELEVO == relevo, 
                          CLASSE == classe,
                          PAVIMENTO == "E") %>% pull(CUSTO)
  cm5f <- dcm5 %>% filter(RELEVO == relevo, 
                          CLASSE == classe,
                          PAVIMENTO == "F") %>% pull(CUSTO)
  cm5g <- dcm5 %>% filter(RELEVO == relevo, 
                          CLASSE == classe,
                          PAVIMENTO == "G") %>% pull(CUSTO)
  
  eca <- peca * et
  ecb <- pecb * et
  ecc <- pecc * et
  ecd <- pecd * et
  ece <- pece * et
  ecf <- pecf * et
  ecg <- pecg * et
  
  parcial <- 
    list(CM1 = cm1 * k,
         CM2 = cm2a * d + cm2b * et,
         CM3 = cm3,
         CM4 = cm4 * et,
         CM5 = cm5a * eca + cm5b * ecb + cm5c * ecc + cm5d * ecd + cm5e * ece + cm5f * ecf + cm5g * ecg)
  
  CMG <- 
    parcial[[1]] + parcial[[2]] + parcial[[3]] + parcial[[4]] + parcial[[5]]
  
  return(list(Parcial = parcial, CMG = CMG))
  
}

