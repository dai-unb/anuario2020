#-------------------------------------------------------------------------------------------
#------------------------------ S E T U P --------------------------------------------------
#-------------------------------------------------------------------------------------------
setwd("C:/anuario2020")
source("C:/anuario2020/scripts/00_execute-me.R")
library(dplyr)
library(tidyr)
library(tidyverse)

Arquivo41 <- readRDS("dados_identificados/Arquivo41.RDS")
Arquivo42 <- readRDS("dados_identificados/Arquivo42.RDS")
load("labels/Label_Unidades.RData")

### tabela referência de unidades e cursos
Nomes <- Arquivo42 %>% 
  count(Curso, Unidade) %>% 
  select(-n)

### define o ano do CENSO
ano_base <- 2019
sem1 <- 12019
sem2 <- 22019

#*******************************************************************************************
#************************************* T A B E L A S ***************************************
#*******************************************************************************************
#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.03 Forma de Ingresso ------------------------------
#-------------------------------------------------------------------------------------------
### ingressantes do 1o semestre
Ing1 <- Arquivo42 %>% 
  filter(Sem_Ingresso==sem1 & Sem_Ref==1) %>% 
  group_by(Curso) %>% 
  summarise(`Vestibular 1 Sem`=sum(Ing_Vest), 
            `PAS 1 Sem`=sum(Ing_Pas), 
            `Convênio PEC 1 Sem`=sum(Ing_PECG),
            `ENEM 1 Sem`=sum(Ing_Enem), 
            `Programas Especiais 1`=sum(Ing_Esp),
            `Transferência Facultativa 1 Sem`=sum(Ing_Rem),
            `Transferência Obrigatória 1 Sem`=sum(Ing_Trans))

### ingressantes do 2o semestre
Ing2 <- Arquivo42 %>% 
  filter(Sem_Ingresso==sem2 & Sem_Ref==2) %>% 
  group_by(Curso) %>%  
  summarise(`Vestibular 2 Sem`=sum(Ing_Vest), 
            `PAS 2 Sem`=sum(Ing_Pas), 
            `Convênio PEC 2 Sem`=sum(Ing_PECG),
            `ENEM 2 Sem`=sum(Ing_Enem), 
            `Programas Especiais 2`=sum(Ing_Esp),
            `Transferência Facultativa 2 Sem`=sum(Ing_Rem),
            `Transferência Obrigatória 2 Sem`=sum(Ing_Trans))

### junta as informações numa tabela só (Adicionar a Unidade à tabela)
t1 <- right_join(Nomes, Ing1)
t2 <- right_join(Nomes, Ing2)
Tabela2.03 <- full_join(t1, t2)
Tabela2.03[is.na(Tabela2.03)] <- 0
rm(Ing1, Ing2, t1, t2)

### cria tabela de totais
totais <- Tabela2.03 %>% 
  ungroup() %>% 
  select(-Curso) %>% 
  group_by(Unidade) %>% 
  summarise_all(sum, na.rm = T) %>% 
  left_join(Label_Unidades)  # adiciona coluna com a descrição das unidades

### junta a tabela com os totais, e ordena
Tabela2.03$Unidade <- sapply(Tabela2.03$Unidade, function(x) paste(x, "-", collapse=""))

Tabela2.03 <- bind_rows(Tabela2.03,totais) %>% 
  arrange(Unidade, Curso)

### nomeia as linhas, para aplicar o rowSums
row.names(Tabela2.03) <- Tabela2.03$Curso; Tabela2.03$Curso <- NULL; Tabela2.03$Unidade <- NULL

Tabela2.03$`Total Geral` <- rowSums(Tabela2.03, na.rm=T)
#Tabela2.03$Total <- rowSums(Tabela2.03[,c("Vestibular 1 Sem", "Vestibular 2 Sem")])

### arruma as colunas e insere a linha de total
Tabela2.03 <- Tabela2.03 %>% 
  ungroup() %>% 
  select(`Vestibular 1 Sem`, `Vestibular 2 Sem`,
         `PAS 1 Sem`, `PAS 2 Sem`, 
         `ENEM 1 Sem`, `ENEM 2 Sem`, 
         `Transferência Obrigatória 1 Sem`, `Transferência Obrigatória 2 Sem`, 
         `Transferência Facultativa 1 Sem`, `Transferência Facultativa 2 Sem`,
         `Programas Especiais 1`, `Programas Especiais 2`, 
         `Convênio PEC 1 Sem`, `Convênio PEC 2 Sem`, 
         `Total Geral`)

Tabela2.03["Total Geral",] <- colSums(Tabela2.03, na.rm=T)/2
#Tabela2.03[is.na(Tabela2.03)] <- 0
nomes <- rownames(Tabela2.03)

Tabela2.03 <- Tabela2.03 %>% 
  mutate_each(list(format(., big.mark = ".")))

rownames(Tabela2.03) <- nomes

save(Tabela2.03, file = "dados_graduacao/Tabela2.03.RData")

rm(totais, nomes, Tabela2.03)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.04 Ingressantes e Formados ------------------------
#-------------------------------------------------------------------------------------------
### ingressantes do 1o e 2o semestres
Ing1 <- Arquivo42 %>% 
  filter(Sem_Ingresso == sem1 & Sem_Ref==1) %>%
  group_by(Curso) %>% 
  summarise(`Ingressantes 1° Sem` = sum(Ing_Vest, 
                                        Ing_Pas, 
                                        Ing_PECG, 
                                        Ing_Enem, 
                                        Ing_Esp,
                                        Ing_Rem,
                                        Ing_Trans, 
                                        na.rm=T))

Ing2 <- Arquivo42 %>% 
  filter(Sem_Ingresso == sem2 & Sem_Ref==2) %>%
  group_by(Curso) %>% 
  summarise(`Ingressantes 2° Sem`=sum(Ing_Vest, 
                                      Ing_Pas, 
                                      Ing_PECG, 
                                      Ing_Enem,
                                      Ing_Esp,
                                      Ing_Rem,
                                      Ing_Trans, 
                                      na.rm=T))

### formados do 1o e 2o semestres
For1 <- Arquivo42 %>% 
  filter(Sem_Ref==1 & Vinculo=="Formado") %>% 
  group_by(Curso) %>% 
  summarise(`Formados 1° Sem`=n())

For2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Vinculo=="Formado") %>% 
  group_by(Curso) %>% 
  summarise(`Formados 2° Sem`=n())

### tabela geral de cursos e unidades
Nomes <- Arquivo42 %>% 
  group_by(Curso, Unidade) %>% 
  tally() %>% 
  select(-n)

### junta as informações numa tabela só
t1 <- right_join(Nomes, Ing1)
t2 <- right_join(Nomes, Ing2)
t3 <- right_join(Nomes, For1)
t4 <- right_join(Nomes, For2)
Tabela2.04 <- full_join(t1, t2) %>% 
  full_join(t3) %>% 
  full_join(t4)
#Transforma NA em 0
Tabela2.04[is.na(Tabela2.04)] <- 0

rm(t1, t2, t3, t4)

### cria tabela de totais
totais <- Tabela2.04 %>% 
  ungroup() %>% 
  select(-Curso) %>% 
  group_by(Unidade) %>% 
  summarise_all(sum, na.rm = T)

### junta a descrição das unidades
totais <- left_join(totais, Label_Unidades)

### junta a tabela com os totais por unidade
Tabela2.04$Unidade <- sapply(Tabela2.04$Unidade, function(x) paste(x, "-", collapse=""))
Tabela2.04 <- bind_rows(Tabela2.04,totais) %>% 
  arrange(Unidade, Curso)

### nomeia as linhas para aplicar rowSums
row.names(Tabela2.04) <- Tabela2.04$Curso; Tabela2.04$Curso <- NULL; Tabela2.04$Unidade <- NULL
Tabela2.04$`Total Ingressantes` <- rowSums(Tabela2.04[,c("Ingressantes 1° Sem", "Ingressantes 2° Sem")])
Tabela2.04$`Total Formados` <- rowSums(Tabela2.04[,c("Formados 1° Sem", "Formados 2° Sem")])
Tabela2.04["Total Geral",] <- colSums(Tabela2.04, na.rm=T)/2

### finaliza a tabela
Tabela2.04 <- Tabela2.04 %>% 
  ungroup() %>% 
  select(`Ingressantes 1° Sem`,`Ingressantes 2° Sem`,`Total Ingressantes`,
         `Formados 1° Sem`,`Formados 2° Sem`,`Total Formados`); rm(For1,For2,Ing1,Ing2, totais)

#Tabela2.04[is.na(Tabela2.04)] <- 0

nomes <- rownames(Tabela2.04)

Tabela2.04 <- map_df(Tabela2.04, ~ format(., big.mark = "."))

rownames(Tabela2.04) <- nomes

### cria tabela por unidade
For2019 <- Arquivo42 %>% 
  filter((Sem_Ref==1 | Sem_Ref==2) & Vinculo=="Formado") %>%
  group_by(Unidade) %>% 
  summarise(`Formados`=n())

Ing2019 <- Arquivo42 %>% 
  group_by(Unidade) %>% 
  filter((Sem_Ingresso==sem1 & Sem_Ref==1) | (Sem_Ingresso==sem2 & Sem_Ref==2)) %>% 
  summarise(`Ingressantes`=sum(Ing_Vest, 
                               Ing_Pas, 
                               Ing_PECG, 
                               Ing_Enem, 
                               Ing_Esp,
                               Ing_Rem, 
                               Ing_Trans, 
                               na.rm=T))

Tabela2.04.2 <- full_join(Ing2019, For2019)

save(Tabela2.04, file = "dados_graduacao/Tabela2.04.RData")
save(Tabela2.04.2, file = "dados_graduacao/Tabela2.04.2.RData")

rm(Ing2019, For2019, nomes, Tabela2.04, Tabela2.04.2)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.05 Ingressantes por Sexo e Faixa Etária -----------
#-------------------------------------------------------------------------------------------

Ing1 <- Arquivo42 %>% 
  filter(Sem_Ingresso==sem1 & Sem_Ref==1)
Ing2 <- Arquivo42 %>% 
  filter(Sem_Ingresso==sem2 & Sem_Ref==2)

temp1 <- inner_join(Arquivo41, Ing1, "ID_Inep")
temp2 <- inner_join(Arquivo41, Ing2, "ID_Inep")

Tabela2.05 <- rbind(temp1,temp2)

rm(temp1, temp2, Ing1, Ing2)

Tabela2.05$Nascimento <- sapply(Tabela2.05$Nascimento, 
                                function(x) ifelse(nchar(x)<8, paste0(paste0(rep(0,8-nchar(x)),collapse=""), x), x))

### a idade deve ser do ANO DE REFERÊNCIA
Tabela2.05$Idade <- ano_base - as.numeric(substr(Tabela2.05$Nascimento, 5, 8)) - as.numeric(substr(Tabela2.05$Nascimento, 3, 4))/12

Tabela2.05$`Faixa Etária`[Tabela2.05$Idade<=18] <- "Até 18 anos"
Tabela2.05$`Faixa Etária`[Tabela2.05$Idade>18 & Tabela2.05$Idade<=24] <- "De 19 a 24 anos"
Tabela2.05$`Faixa Etária`[Tabela2.05$Idade>24 & Tabela2.05$Idade<=29] <- "De 25 a 29 anos"
Tabela2.05$`Faixa Etária`[Tabela2.05$Idade>29 & Tabela2.05$Idade<=34] <- "De 30 a 34 anos"
Tabela2.05$`Faixa Etária`[Tabela2.05$Idade>34 & Tabela2.05$Idade<=39] <- "De 35 a 39 anos"
Tabela2.05$`Faixa Etária`[Tabela2.05$Idade>39 & Tabela2.05$Idade<=44] <- "De 40 a 44 anos"
Tabela2.05$`Faixa Etária`[Tabela2.05$Idade>44] <- "De 45 anos ou mais"

Tabela2.05 <- Tabela2.05 %>% 
  group_by(`Faixa Etária`, Sexo) %>% 
  tally() %>% 
  spread(Sexo, n) %>% 
  janitor::adorn_totals(where = c("row", "col"))

Tabela2.05$`% Feminino` <- paste(round(Tabela2.05$Feminino/Tabela2.05$Total*100,1), "%", sep="")
Tabela2.05$`% Masculino` <- paste(round(Tabela2.05$Masculino/Tabela2.05$Total*100,1), "%", sep="")
Tabela2.05$`% Total` <- paste(round(Tabela2.05$Total/Tabela2.05$Total[8]*100,1), "%", sep="")

rownames(Tabela2.05) <- Tabela2.05$`Faixa Etária`

nomes <- rownames(Tabela2.05)

Tabela2.05 <- Tabela2.05 %>% 
  select(Feminino, `% Feminino`, Masculino, `% Masculino`, Total, `% Total`)

Tabela2.05 <- Tabela2.05 %>% 
  mutate_at(vars(Feminino, Masculino, Total),
            funs(format(., big.mark = ".")))

rownames(Tabela2.05) <- nomes

save(Tabela2.05, file = "dados_graduacao/Tabela2.05.RData")

rm(nomes, Tabela2.05)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.06 Alunos Regulares por Turno - EXCLUI EAD --------
#-------------------------------------------------------------------------------------------
### Alunos regulares: exclui transferidos e falecidos
M1 <- Arquivo42 %>% 
  filter(Turno!="" & Sem_Ref==1 & Vinculo!="Transferido" & Vinculo != "Falecido") %>%  # EAD está como vazio ""
  group_by(Unidade, Turno) %>% 
  tally() %>% 
  spread(Turno, n)

M1[is.na(M1)] <- 0
M1$Total <- rowSums(M1[,c("Integral", "Noturno")], na.rm=T)

M2 <- Arquivo42 %>% 
  filter(Turno!="" & Sem_Ref==2 & Vinculo!="Transferido" & Vinculo != "Falecido") %>%
  group_by(Unidade, Turno) %>% 
  tally() %>% 
  spread(Turno, n)

M2[is.na(M2)] <- 0
M2$Total <- rowSums(M2[,c("Integral", "Noturno")], na.rm=T)

Tabela2.06 <- left_join(M1, M2, by = "Unidade") %>% 
  janitor::adorn_totals()

rownames(Tabela2.06) <- Tabela2.06$Unidade

nomes <- rownames(Tabela2.06)

#Tabela2.06[is.na(Tabela2.06)] <- 0

Tabela2.06 <- map_df(Tabela2.06, ~ format(., big.mark = "."))

rownames(Tabela2.06) <- nomes

Tabela2.06 <- select(Tabela2.06, -Unidade)

save(Tabela2.06, file = "dados_graduacao/Tabela2.06.RData")

rm(M1,M2, nomes, Tabela2.06)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.07 Alunos Regulares Ativos por Sexo ---------------
#-------------------------------------------------------------------------------------------

### alunos ativos: apenas CURSANDO e FORMADO
matriculados1 <- Arquivo42 %>% 
  filter(Sem_Ref==1 & Vinculo %in% c("Cursando", "Formado"))

M1 <- inner_join(Arquivo41, matriculados1, "ID_Inep") %>% 
  group_by(Unidade, Sexo) %>% 
  tally() %>% 
  spread(Sexo, n) %>% 
  janitor::adorn_totals(c("row", "col"))

matriculados2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Vinculo %in% c("Cursando", "Formado"))

M2 <- inner_join(Arquivo41, matriculados2, "ID_Inep") %>% 
  group_by(Unidade, Sexo) %>% 
  tally() %>% 
  spread(Sexo, n) %>% 
  janitor::adorn_totals(c("row", "col"))

Tabela2.07 <- left_join(M1, M2, by = "Unidade")

rownames(Tabela2.07) <- Tabela2.07$Unidade

nomes <- rownames(Tabela2.07)

Tabela2.07 <- map_df(Tabela2.07, ~ format(., big.mark = "."))
rownames(Tabela2.07) <- nomes

Tabela2.07 <- select(Tabela2.07, -Unidade)

save(Tabela2.07, file = "dados_graduacao/Tabela2.07.RData")

rm(M1, M2, matriculados1, matriculados2, nomes, Tabela2.07)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.08 Alunos Regulares Ativos e Trancados ------------
#-------------------------------------------------------------------------------------------

matriculados1 <- Arquivo42 %>% 
  filter(Sem_Ref==1 & Vinculo %in% c("Cursando", "Formado"))

M1 <- inner_join(Arquivo41, matriculados1, "ID_Inep") %>% 
  group_by(Curso, Sexo) %>% 
  tally() %>% 
  spread(Sexo, n)

names(M1)[2:3] <- c("Fem Ativo 1 Sem", "Masc Ativo 1 Sem")

matriculados2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Vinculo %in% c("Cursando", "Formado"))

M2 <- inner_join(Arquivo41, matriculados2, "ID_Inep") %>% 
  group_by(Curso, Sexo) %>% 
  tally() %>% 
  spread(Sexo, n)

names(M2)[2:3] <- c("Fem Ativo 2 Sem", "Masc Ativo 2 Sem")

trancados1 <- Arquivo42 %>% 
  filter(Sem_Ref==1 & Vinculo=="Trancado")

T1 <- inner_join(Arquivo41, trancados1, "ID_Inep") %>% 
  group_by(Curso, Sexo) %>% 
  tally() %>% 
  spread(Sexo, n)

names(T1)[2:3] <- c("Fem Tranc 1 Sem", "Masc Tranc 1 Sem")

trancados2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Vinculo=="Trancado")

T2 <- inner_join(Arquivo41, trancados2, "ID_Inep") %>% 
  group_by(Curso, Sexo) %>% 
  tally() %>% 
  spread(Sexo, n)

names(T2)[2:3] <- c("Fem Tranc 2 Sem", "Masc Tranc 2 Sem")

t1 <- left_join(Nomes, M1)
t2 <- left_join(Nomes, M2)
t3 <- left_join(Nomes, T1)
t4 <- left_join(Nomes, T2)

Tabela2.08 <- cbind.data.frame(t1, t2[c(3, 4)], t3[c(3, 4)], t4[c(3, 4)])

Tabela2.08[is.na(Tabela2.08)] <- 0

totais <- Tabela2.08 %>% 
  group_by(Unidade) %>%
  select(-Curso) %>% 
  summarise_all(sum, na.rm = T) %>% 
  left_join(Label_Unidades)

Tabela2.08$Unidade <- sapply(Tabela2.08$Unidade, function(x) paste(x, "-", collapse=""))
Tabela2.08 <- bind_rows(Tabela2.08,totais) %>% arrange(Unidade)

row.names(Tabela2.08) <- Tabela2.08$Curso
Tabela2.08$Curso <- NULL
Tabela2.08$Unidade <- NULL

Tabela2.08$`Total Fem 1 Sem` <- rowSums(Tabela2.08[,c("Fem Ativo 1 Sem", "Fem Tranc 1 Sem")], na.rm = TRUE)
Tabela2.08$`Total Masc 1 Sem` <- rowSums(Tabela2.08[,c("Masc Ativo 1 Sem", "Masc Tranc 1 Sem")], na.rm = TRUE)
Tabela2.08$`Total Fem 2 Sem` <- rowSums(Tabela2.08[,c("Fem Ativo 2 Sem", "Fem Tranc 2 Sem")], na.rm = TRUE)
Tabela2.08$`Total Masc 2 Sem` <- rowSums(Tabela2.08[,c("Masc Ativo 2 Sem", "Masc Tranc 2 Sem")], na.rm = TRUE)
Tabela2.08["Total Geral", ] <- colSums(Tabela2.08, na.rm = TRUE)/2

#Tabela2.08[is.na(Tabela2.08)] <- 0

nomes <- rownames(Tabela2.08)
Tabela2.08 <- map_df(Tabela2.08, ~ format(., big.mark = "."))
rownames(Tabela2.08) <- nomes

save(Tabela2.08, file = "dados_graduacao/Tabela2.08.RData")

rm(t1, t2, t3, t4, T1, T2, M1, M2, matriculados1, matriculados2, trancados1, trancados2, totais, nomes, Tabela2.08)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.09 Alunos Regulares por Sexo e Faixa Etária -------
#-------------------------------------------------------------------------------------------

matriculados2 <- Arquivo42 %>% 
  filter(Sem_Ref == 2 & Vinculo != "Transferido" & Vinculo != "Falecido")

Tabela2.09 <- inner_join(Arquivo41, matriculados2, "ID_Inep")

Tabela2.09$Nascimento <- sapply(Tabela2.09$Nascimento, 
                                function(x) ifelse(nchar(x)<8, paste0(paste0(rep(0,8-nchar(x)),collapse=""), x), x))
Tabela2.09$Idade <- ano_base - as.numeric(substr(Tabela2.09$Nascimento, 5, 8)) - as.numeric(substr(Tabela2.09$Nascimento, 3, 4))/12

Tabela2.09$`Faixa Etária`[Tabela2.09$Idade<=18] <- "Até 18 anos"
Tabela2.09$`Faixa Etária`[Tabela2.09$Idade>18 & Tabela2.09$Idade<=24] <- "De 19 a 24 anos"
Tabela2.09$`Faixa Etária`[Tabela2.09$Idade>24 & Tabela2.09$Idade<=29] <- "De 25 a 29 anos"
Tabela2.09$`Faixa Etária`[Tabela2.09$Idade>29 & Tabela2.09$Idade<=34] <- "De 30 a 34 anos"
Tabela2.09$`Faixa Etária`[Tabela2.09$Idade>34 & Tabela2.09$Idade<=39] <- "De 35 a 39 anos"
Tabela2.09$`Faixa Etária`[Tabela2.09$Idade>39 & Tabela2.09$Idade<=44] <- "De 40 a 44 anos"
Tabela2.09$`Faixa Etária`[Tabela2.09$Idade>44] <- "De 45 anos ou mais"

Tabela2.09 <- Tabela2.09 %>% 
  group_by(`Faixa Etária`, Sexo) %>% 
  tally() %>% 
  spread(Sexo, n) %>% 
  janitor::adorn_totals(c("row", "col"))

Tabela2.09$`% Feminino` <- paste(round(Tabela2.09$Feminino/Tabela2.09$Total*100,1), "%", sep="")
Tabela2.09$`% Masculino` <- paste(round(Tabela2.09$Masculino/Tabela2.09$Total*100,1), "%", sep="")
Tabela2.09$`% Total` <- paste(round(Tabela2.09$Total/Tabela2.09$Total[8]*100,1), "%", sep="")

rownames(Tabela2.09) <- Tabela2.09$`Faixa Etária`

Tabela2.09 <- Tabela2.09 %>% 
  select(Feminino, `% Feminino`, Masculino, `% Masculino`, Total, `% Total`)
# Tabela2.09[] <- lapply(Tabela2.09, gsub, pattern = ".", replacement = ",", fixed = TRUE)

nomes <- rownames(Tabela2.09)

Tabela2.09 <- Tabela2.09 %>% 
  mutate_at(vars(Feminino, Masculino, Total),
            funs(format(., big.mark = ".")))

rownames(Tabela2.09) <- nomes

rm(matriculados2, nomes)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.11 Forma de saida dos alunos ----------------------
#-------------------------------------------------------------------------------------------

# load("O:/DAI/SIGRA/Extracao 12062019/Completo.RData")
load("dados_identificados/Completo14042020.RData")

Desl_sigra_1 <- Completo %>% 
  filter(`Nivel Curso` == "Graduacao" & ((`Ano Saida Opcao`== ano_base & `Semestre Saida Opcao` <= 1) |
                                           `Ano Saida Opcao`== (ano_base - 1) & `Semestre Saida Opcao` == 2))
Desl_sigra_1 <- within(Desl_sigra_1, {
  `For. Saida Opcao`[`For. Saida Opcao`=="Formatura" | `For. Saida Opcao`=="Formatura Anterior a 1/88"] <- "Formado-Graduação"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Formatura Pos-Graduacao" | `For. Saida Opcao`=="Formatura com Especializacao" |
                       `For. Saida Opcao`=="Desligamento com Especializacao"] <- "Outros 1"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Deslig - Nao Cumpriu condicao" | `For. Saida Opcao`=="Desligamento Jubilamento" |
                       `For. Saida Opcao`=="Desligamento Rendimento Academico" |
                       `For. Saida Opcao`=="Rep 3 vezes na mesma Disc Obrig"] <- "Desligado-Falta de Rendimento 1"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Desligamento Voluntario"] <- "Desligamento Voluntário 1"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Falecimento" | `For. Saida Opcao`=="Desligamento Forca de Convenio" | 
                       `For. Saida Opcao`=="Desligamento Falta Documentacao" | `For. Saida Opcao`=="Anulacao de Registro" | 
                       `For. Saida Opcao`=="Mudanca de Habilitacao" | `For. Saida Opcao`=="Transferencia" |
                       `For. Saida Opcao`=="Desligamento Decisao Judicial" | `For. Saida Opcao`=="Novo Vestibular" |
                       `For. Saida Opcao`=="Expulsao Disciplinar" |`For. Saida Opcao`=="Mudanca de Curso" |
                       `For. Saida Opcao`== "Vestibular p/outra Habilitacao" | `For. Saida Opcao`== "SISU" |
                       `For. Saida Opcao`== "Mudança de Turno" | `For. Saida Opcao`== "Outros"] <- "Outros 1"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Desligamento - Abandono"] <- "Abandono 1"
})


Desl_sigra_1 <- Desl_sigra_1 %>% 
  filter(`For. Saida Opcao`!="Formado-Graduação" & `For. Saida Opcao`!="Está Cursando") %>%
  select(MatricAluno, `For. Saida Opcao`) %>% 
  distinct(MatricAluno, .keep_all = T)

M1 <- Arquivo42 %>% filter(Sem_Ref==1)
M1 <- inner_join(Arquivo41, M1, "ID_Inep")

Reg_1 <- M1 %>% filter(Vinculo!="Desvinculado")
Reg_1$`For. Saida Opcao` <- "Registrado_1"

Desl_censo_1 <- M1 %>% filter(Vinculo=="Desvinculado")

Desligados_1 <- inner_join(Desl_censo_1, Desl_sigra_1, by = "MatricAluno")
Desligados_1.1 <- anti_join(Desl_censo_1, Desl_sigra_1, by = "MatricAluno") %>%
  mutate(`For. Saida Opcao` = "Outros 1")

Desligados_1 <- rbind(Desligados_1, Desligados_1.1)

Final_1 <- bind_rows(Desligados_1, Reg_1)
Final_1 <- Final_1 %>%  
  group_by(Curso, `For. Saida Opcao`) %>% 
  tally() %>% 
  spread(`For. Saida Opcao`, n)

rm(M1, Desligados_1, Desligados_1.1, Reg_1, Desl_censo_1, Desl_sigra_1)

### 2o semestre
Desl_sigra_2 <- Completo %>% 
  filter(`Nivel Curso`=="Graduacao" & (`Ano Ingresso Opcao` <= ano_base & (`Ano Saida Opcao` > ano_base |
                                                                             (`Ano Saida Opcao` == ano_base & 
                                                                                `Semestre Saida Opcao` == 2))))
Desl_sigra_2 <- within(Desl_sigra_2, {
  `For. Saida Opcao`[`For. Saida Opcao`=="Formatura" | `For. Saida Opcao`=="Formatura Anterior a 1/88"] <- "Formado-Graduação"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Formatura Pos-Graduacao" | `For. Saida Opcao`=="Formatura com Especializacao" |
                       `For. Saida Opcao`=="Desligamento com Especializacao"] <- "Outros 2"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Deslig - Nao Cumpriu condicao" | `For. Saida Opcao`=="Desligamento Jubilamento" |
                       `For. Saida Opcao`=="Desligamento Rendimento Academico" |
                       `For. Saida Opcao`=="Rep 3 vezes na mesma Disc Obrig"] <- "Desligado-Falta de Rendimento 2"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Desligamento Voluntario"] <- "Desligamento Voluntário 2"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Mudanca de Turno"] <- "Registrado_2"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Falecimento" | `For. Saida Opcao`=="Desligamento Forca de Convenio" | 
                       `For. Saida Opcao`=="Desligamento Falta Documentacao" | `For. Saida Opcao`=="Anulacao de Registro" | 
                       `For. Saida Opcao`=="Mudanca de Habilitacao" | `For. Saida Opcao`=="Transferencia" |
                       `For. Saida Opcao`=="Desligamento Decisao Judicial" | `For. Saida Opcao`=="Novo Vestibular" |
                       `For. Saida Opcao`=="Expulsao Disciplinar" |`For. Saida Opcao`=="Mudanca de Curso" |
                       `For. Saida Opcao`== "Vestibular p/outra Habilitacao" | `For. Saida Opcao`== "SISU" |
                       `For. Saida Opcao`== "Mudança de Turno"] <- "Outros 2"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Desligamento - Abandono"] <- "Abandono 2"
})

Desl_sigra_2 <- Desl_sigra_2 %>% 
  filter(`For. Saida Opcao`!="Formado-Graduação" & `For. Saida Opcao`!="Está Cursando") %>%
  select(MatricAluno, `For. Saida Opcao`) %>% 
  distinct(MatricAluno, .keep_all = T)

M1 <- Arquivo42 %>% filter(Sem_Ref==2)
M1 <- inner_join(Arquivo41, M1, "ID_Inep")

Reg_2 <- M1 %>% filter(Vinculo!="Desvinculado")
Reg_2$`For. Saida Opcao` <- "Registrado_2"

Desl_censo_2 <- M1 %>% filter(Vinculo=="Desvinculado")

Desligados_2 <- inner_join(Desl_censo_2, Desl_sigra_2, c("MatricAluno"))
Desligados_2.1 <- anti_join(Desl_censo_2, Desl_sigra_2, c("MatricAluno")) %>% mutate(`For. Saida Opcao`="Outros 2")
Desligados_2 <- rbind(Desligados_2, Desligados_2.1)

Final_2 <- bind_rows(Desligados_2, Reg_2)
Final_2 <- Final_2 %>%  
  group_by(Curso, `For. Saida Opcao`) %>% 
  tally() %>% 
  spread(`For. Saida Opcao`, n)

rm(M1, Desligados_2, Desligados_2.1, Reg_2, Desl_censo_2, Desl_sigra_2, Completo)

Tabela2.11 <- left_join(Nomes, Final_1, by = "Curso") %>% 
  left_join(Final_2, by = "Curso")

rm(Final_1, Final_2)

totais <- Tabela2.11 %>% 
  ungroup() %>% 
  select(-Curso) %>% 
  group_by(Unidade) %>% 
  summarise_all(sum, na.rm = T) %>% 
  left_join(Label_Unidades)

Tabela2.11$Unidade <- sapply(Tabela2.11$Unidade, function(x) paste(x, "-", collapse=""))
Tabela2.11 <- bind_rows(Tabela2.11,totais) %>% arrange(Unidade, Curso)

row.names(Tabela2.11) <- Tabela2.11$Curso
Tabela2.11$Curso <- NULL
Tabela2.11$Unidade <- NULL

Tabela2.11 <- Tabela2.11 %>% 
  ungroup() %>% 
  select(Registrado_1, Registrado_2, `Abandono 1`, `Abandono 2`, `Desligamento Voluntário 1`, 
         `Desligamento Voluntário 2`, `Desligado-Falta de Rendimento 1`, `Desligado-Falta de Rendimento 2`,
         `Outros 1`, `Outros 2`)

Tabela2.11$`Movimentacao 1` <- rowSums(Tabela2.11[,c(3,5,7,9)], na.rm = T)
Tabela2.11$`Movimentacao 2` <- rowSums(Tabela2.11[,c(4,6,8,10)], na.rm = T)
Tabela2.11["Total Geral", ] <- colSums(Tabela2.11, na.rm = T)/2
Tabela2.11[is.na(Tabela2.11)] <- 0

nomes <- rownames(Tabela2.11)

Tabela2.11 <- map_df(Tabela2.11, ~ format(.x, big.mark = "."))

rownames(Tabela2.11) <- nomes

rm(totais, nomes)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.XX Raça Cor ---------------------------------------
#-------------------------------------------------------------------------------------------

M2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Vinculo != "Transferido" & Vinculo != "Falecido")
M2 <- inner_join(Arquivo41, M2, "ID_Inep")
M2 <- M2 %>% group_by(Curso, Raca) %>% 
  tally() %>% 
  spread(Raca, n)

Tabela2.XX <- left_join(Nomes, M2)

totais <- Tabela2.XX %>% 
  group_by(Unidade) %>% 
  select(-Curso) %>% 
  summarise_all(sum, na.rm = T) %>% 
  left_join(Label_Unidades)

Tabela2.XX$Unidade <- sapply(Tabela2.XX$Unidade, function(x) paste(x, "-", collapse=""))
Tabela2.XX <- bind_rows(Tabela2.XX,totais) %>% arrange(Unidade, Curso)
row.names(Tabela2.XX) <- Tabela2.XX$Curso
Tabela2.XX$Curso <- NULL
Tabela2.XX$Unidade <- NULL

Tabela2.XX <- Tabela2.XX %>% 
  ungroup() %>% 
  select(Amarela, Branca, Indígena, Parda, Preta, `Aluno não quis declarar cor/raça`, `Não dispõe de informação`)

Tabela2.XX["Total Geral", ] <- colSums(Tabela2.XX, na.rm=T)/2
Tabela2.XX[is.na(Tabela2.XX)] <- 0

nomes <- rownames(Tabela2.XX)

Tabela2.XX <- map_df(Tabela2.XX, ~ format(., big.mark = "."))

rownames(Tabela2.XX) <- nomes

rm(M2, totais, nomes)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.XX2 Raça/Cor e Sexo -------------------------------
#-------------------------------------------------------------------------------------------

Tabela2.XX2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Vinculo != "Transferido" & Vinculo != "Falecido")
Tabela2.XX2 <- inner_join(Arquivo41, Tabela2.XX2, "ID_Inep")
Tabela2.XX2 <- Tabela2.XX2 %>% 
  group_by(Raca, Sexo) %>% 
  tally() %>% 
  spread(Sexo, n) %>% 
  janitor::adorn_totals(c("row", "col"))

Tabela2.XX2$`% Feminino` <- paste(round(Tabela2.XX2$Feminino/Tabela2.XX2$Total*100,1), "%", sep="")
Tabela2.XX2$`% Masculino` <- paste(round(Tabela2.XX2$Masculino/Tabela2.XX2$Total*100,1), "%", sep="")
Tabela2.XX2$`% Total` <- paste(round(Tabela2.XX2$Total/Tabela2.XX2$Total[8]*100,1), "%", sep="")

rownames(Tabela2.XX2) <- Tabela2.XX2$Raca

Tabela2.XX2 <- Tabela2.XX2 %>% select(Feminino, `% Feminino`, Masculino, `% Masculino`, Total, `% Total`)

nomes <- rownames(Tabela2.XX2)

Tabela2.XX2 <- Tabela2.XX2 %>% 
  mutate_at(vars(Feminino, Masculino, Total),
            funs(format(., big.mark = ".")))

rownames(Tabela2.XX2) <- nomes

rm(nomes)

#----------------------------------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.10 Alunos Regulares Ativos, Matrículas e Aprovados em Disciplinas ------------
#----------------------------------------------------------------------------------------------------------------------
### mudar o arquivo do HE - Histórico Escolar

HE0 <- read_fwf("dados_identificados/HE20190.txt", 
                fwf_widths(c(9,4,1,6,2,3,2,3), col_names=c("MatricAluno","Ano HE","Semestre HE", 
                                                           "CodDisc", "Turma","Creditos","Mencao","Frequencia")), 
                locale = locale(encoding = "latin1"))

HE1 <- read_fwf("dados_identificados/HE20191.txt", 
                fwf_widths(c(9,4,1,6,2,3,2,3), col_names=c("MatricAluno","Ano HE","Semestre HE", 
                                                           "CodDisc", "Turma","Creditos","Mencao","Frequencia")), 
                locale = locale(encoding = "latin1"))

HE1 <- rbind(HE0, HE1) %>% select(MatricAluno, Mencao_1=Mencao)
rm(HE0)

HE2 <- read_fwf("dados_identificados/HE20192.txt", 
                fwf_widths(c(9,4,1,6,2,3,2,3), col_names=c("MatricAluno","Ano HE","Semestre HE", 
                                                           "CodDisc", "Turma","Creditos","Mencao","Frequencia")), 
                locale = locale(encoding = "latin1")) %>% 
  select(MatricAluno, Mencao_2=Mencao)


HE1 <- within(HE1, {
  Mencao_1[Mencao_1=="AP"] <- "AP 1"
  Mencao_1[Mencao_1=="CC"] <- "AP 1"
  Mencao_1[Mencao_1=="MM"] <- "AP 1"
  Mencao_1[Mencao_1=="MS"] <- "AP 1"
  Mencao_1[Mencao_1=="SS"] <- "AP 1"
  
  Mencao_1[Mencao_1=="SR"] <- "RP 1"
  Mencao_1[Mencao_1=="II"] <- "RP 1"
  Mencao_1[Mencao_1=="MI"] <- "RP 1"
  Mencao_1[Mencao_1=="TR"] <- "RP 1"
  Mencao_1[Mencao_1=="TJ"] <- "RP 1"
  Mencao_1[Mencao_1=="DP"] <- "RP 1"
})
HE2 <- within(HE2, {
  Mencao_2[Mencao_2=="AP"] <- "AP 2"
  Mencao_2[Mencao_2=="CC"] <- "AP 2"
  Mencao_2[Mencao_2=="MM"] <- "AP 2"
  Mencao_2[Mencao_2=="MS"] <- "AP 2"
  Mencao_2[Mencao_2=="SS"] <- "AP 2"
  
  Mencao_2[Mencao_2=="SR"] <- "RP 2"
  Mencao_2[Mencao_2=="II"] <- "RP 2"
  Mencao_2[Mencao_2=="MI"] <- "RP 2"
  Mencao_2[Mencao_2=="TR"] <- "RP 2"
  Mencao_2[Mencao_2=="TJ"] <- "RP 2"
  Mencao_2[Mencao_2=="DP"] <- "RP 2"
})


M1 <- Arquivo42 %>% filter(Sem_Ref==1 & Vinculo %in% c("Cursando", "Formado"))
M1 <- inner_join(Arquivo41, M1, "ID_Inep")
AA1 <- M1 %>% group_by(Curso) %>% summarise(`Alunos Ativos 1`=n())

M1 <- inner_join(M1, HE1, "MatricAluno")
MD1 <- M1 %>% group_by(Curso) %>% summarise(`Matriculados em Disciplinas 1`=n())
AP1 <- M1 %>% group_by(Curso, Mencao_1) %>% 
  tally() %>% 
  spread(Mencao_1, n)

M2 <- Arquivo42 %>% filter(Sem_Ref==2 & Vinculo %in% c("Cursando", "Formado"))
M2 <- inner_join(Arquivo41, M2, "ID_Inep")
AA2 <- M2 %>% 
  group_by(Curso) %>% 
  summarise(`Alunos Ativos 2`=n())

M2 <- inner_join(M2, HE2, "MatricAluno")
MD2 <- M2 %>% 
  group_by(Curso) %>% 
  summarise(`Matriculados em Disciplinas 2`=n())
AP2 <- M2 %>% 
  group_by(Curso, Mencao_2) %>% 
  tally() %>% 
  spread(Mencao_2, n)

Tabela2.10 <- left_join(Nomes, AA1) %>% 
  left_join(AA2) %>% 
  left_join(MD1) %>% 
  left_join(MD2) %>% 
  left_join(AP1) %>% 
  left_join(AP2)

totais <- Tabela2.10 %>% 
  group_by(Unidade) %>% 
  select(-Curso) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela2.10$Unidade <- sapply(Tabela2.10$Unidade, function(x) paste(x, "-", collapse=""))
Tabela2.10 <- bind_rows(Tabela2.10,totais) %>% arrange(Unidade, Curso)
row.names(Tabela2.10) <- Tabela2.10$Curso
Tabela2.10$Curso <- NULL
Tabela2.10$Unidade <- NULL

Tabela2.10 <- Tabela2.10 %>% 
  ungroup() %>% 
  select(`Alunos Ativos 1`, `Alunos Ativos 2`, 
         `Matriculados em Disciplinas 1`, `Matriculados em Disciplinas 2`,
         `AP 1`, `AP 2`, `RP 1`, `RP 2`)

Tabela2.10["Total Geral", ] <- colSums(Tabela2.10, na.rm=T)/2

Tabela2.10$`RP 1` <- Tabela2.10$`AP 1`/(Tabela2.10$`RP 1` + Tabela2.10$`AP 1`)
Tabela2.10$`RP 1` <- paste(round(Tabela2.10$`RP 1`*100,1), "%", sep="")
Tabela2.10$`RP 2` <- Tabela2.10$`AP 2`/(Tabela2.10$`RP 2` + Tabela2.10$`AP 2`)
Tabela2.10$`RP 2` <- paste(round(Tabela2.10$`RP 2`*100,1), "%", sep="")

nomes <- rownames(Tabela2.10)

Tabela2.10 <- map_df(Tabela2.10, ~ format(., big.mark = "."))

rownames(Tabela2.10) <- nomes

rm(AA1, AA2, AP1, AP2, HE1, HE2, M1, M2, MD1, MD2, totais, nomes)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.12 Alunos Regulares Portadores de Necessidades ----
#-------------------------------------------------------------------------------------------

Tabela2.12 <- Arquivo42 %>% filter(Sem_Ref==2 & Vinculo != "Transferido" & Vinculo != "Falecido")
Tabela2.12 <- inner_join(Arquivo41, Tabela2.12, "ID_Inep")

Tabela2.12 <- Tabela2.12 %>% 
  group_by(Curso) %>%
  summarise(Auditiva=sum(Def_Aud, Surdez, Surdocegueira, na.rm=T), 
            Visual=sum(Cegueira, Baixa_Vis, Surdocegueira, na.rm=T),
            `Física`=sum(Def_Fis, na.rm=T), 
            `Altas Habilidades`=sum(Super_Dot, na.rm=T), 
            `Dislexia e TDAH`=sum(Def_Int,na.rm=T),
            `Outras`=sum(Def_Mult, Autismo, Sind_Asper, Sindr_Rett, Trans_Des, na.rm=T)) %>% 
  left_join(Nomes)

totais <- Tabela2.12 %>% 
  group_by(Unidade) %>% 
  select(-Curso) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela2.12$Unidade <- paste0(Tabela2.12$Unidade, "-")

Tabela2.12 <- bind_rows(Tabela2.12, totais) %>% 
  arrange(Unidade, Curso) %>% 
  janitor::adorn_totals(c("row", "col"))

rownames(Tabela2.12) <- Tabela2.12$Curso

Tabela2.12$Curso <- NULL
Tabela2.12$Unidade <- NULL

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.14 Países -----------------------------------------
#-------------------------------------------------------------------------------------------

Label_Pais <- rio::import("Tabela_Pais.xlsx", skip = 1) %>% 
  rename(Pais = Código, Pais_Nome = Nome)

Tabela2.14 <- Arquivo42 %>% filter(Sem_Ref==2 & Vinculo != "Transferido" & Vinculo != "Falecido")
Tabela2.14 <- inner_join(Arquivo41, Tabela2.14, "ID_Inep")
Tabela2.14 <- Tabela2.14 %>% 
  filter(Pais!="BRA") %>% 
  mutate(Pais = as.character(Pais)) %>% 
  left_join(Label_Pais) %>% 
  rename(Pais_codigo = Pais, Pais = Pais_Nome)

Tabela2.14 <- within(Tabela2.14, {
  Continente <- NA
  
  Continente[Pais=="África do Sul" | Pais=="Argélia" | Pais=="Angola" | Pais=="Benim" | Pais=="Burkina Faso" | 
               Pais=="Cabo Verde" | Pais=="Camarões" | Pais=="Ángola" | Pais=="Congo" | Pais == "Líbia" |
               Pais=="Costa do Marfim" | Pais=="Gana" | Pais=="Guiné" | Pais=="Guiné-Bissau"| 
               Pais=="Mali" | Pais=="Marrocos" | Pais=="Moçambique" |Pais=="Nigéria"| Pais=="Quénia"| 
               Pais == "República Democrática do Congo" | Pais == "República do Congo" | 
               Pais == "Senegal" | Pais=="Síria" | Pais=="Sudão" | Pais=="Togo" | Pais=="Zimbábue"] <- "ÁFRICA"
  
  Continente[Pais=="Argentina" | Pais=="Bolívia" | Pais=="Chile" | Pais=="Colômbia" | Pais=="Canadá" |
               Pais=="Costa Rica" | Pais=="Cuba" | Pais=="El Salvador" | Pais=="Equador" |
               Pais=="Estados Unidos" | Pais=="Guatemala" | Pais=="Guiana" | Pais=="Haiti" | Pais=="Honduras" | 
               Pais=="Jamaica" | Pais=="México" | Pais=="Nicarágua" | Pais=="Paraguai" | Pais=="Peru" |
               Pais=="Porto Rico" | Pais=="República Dominicana" | Pais=="Suriname" | 
               Pais=="Trinidad e Tobago" | Pais=="Uruguai" | Pais=="Venezuela"] <- "AMÉRICA"
  
  Continente[Pais=="Arábia Saudita" | Pais=="China" | Pais=="Coreia do Norte" |
               Pais=="Coreia do Sul" | Pais=="Emirados Árabes Unidos" | Pais=="Índia" | Pais=="Iraque" | 
               Pais=="Irã" | Pais=="Japão" | Pais=="Jordânia" | Pais=="Kazaquistão" | Pais=="Malásia" |
               Pais=="Paquistão" | Pais=="Rússia" | Pais=="Timor Leste" | Pais=="Vietnã"] <- "ÁSIA"
  
  Continente[Pais=="Albânia" | Pais=="Alemanha" | Pais=="Armênia" | Pais=="Áustria" | Pais=="Bélgica" | Pais=="Bulgária" |
               Pais=="Espanha" | Pais=="Finlândia" | Pais=="França" | Pais=="Grécia" | Pais=="Holanda" |
               Pais=="Hungria" | Pais=="Itália" | Pais=="Mônaco" | Pais=="Portugal" | Pais=="Polônia" |
               Pais=="Suíça" | Pais=="Reino Unido" | Pais=="Ucrânia" | Pais == "Dinamarca" |
               Pais=="Bielorrússia"] <- "EUROPA"
  
  Continente[Pais=="Ilhas Fiji"] <- "OCEANIA"
  
})

Tabela2.14.2 <- Tabela2.14 %>% 
  count(Pais) %>% 
  arrange(desc(n)) %>% 
  head(10)

Tabela2.14 <- Tabela2.14 %>% 
  group_by(Unidade, Pais, Continente) %>% 
  tally() %>% 
  spread(Unidade, n)

# VERIFICAR ANTES SE PRECISA
# NESSE CASO SIM POIS TEMOS UM APÁTRIDA
Tabela2.14 <- drop_na(Tabela2.14, Continente)

totais <- Tabela2.14 %>% 
  group_by(Continente) %>%
  select(-Pais) %>% 
  summarise_all(sum, na.rm = TRUE)

totais$Pais <- c("Total do Continente: África", "Total do Continente: América", "Total do Continente: Ásia",
                 "Total do Continente: Europa")

totais$Continente <- sapply(totais$Continente, function(x) paste(x, "-", collapse=""))
Tabela2.14 <- bind_rows(Tabela2.14,totais) %>% arrange(Continente, Pais)
row.names(Tabela2.14) <- Tabela2.14$Pais; Tabela2.14$Pais <- NULL; Tabela2.14$Continente <- NULL

Tabela2.14$Total <- rowSums(Tabela2.14, na.rm=T)
Tabela2.14["Total Geral", ] <- colSums(Tabela2.14, na.rm=T)/2
Tabela2.14[is.na(Tabela2.14)] <- 0

Tabela2.14 <- as.data.frame(Tabela2.14)
Tabela2.14.2 <- as.data.frame(Tabela2.14.2)

rm(Label_Pais, totais)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.21 Evolução Ingressantes --------------------------
#-------------------------------------------------------------------------------------------

# Atualizar o CSV que está na pasta
# com os dados do último anuário
Tabela2.21 <- read.csv2("EvoIng.csv", check.names=F, stringsAsFactors = FALSE)
Tabela2.21 <- cbind(Tabela2.21[1], 
                    map_df(Tabela2.21[-1], ~ as.double(str_remove(string = .x, pattern = "\\."))))

Ing <- Arquivo42 %>% 
  group_by(Curso) %>% 
  filter((Sem_Ingresso==sem1 & Sem_Ref==1) | 
           (Sem_Ingresso==sem2 & Sem_Ref==2)) %>% 
  summarise(`2018` = sum(Ing_Vest, Ing_Pas, Ing_PECG, Ing_Enem, Ing_Esp, Ing_Rem, Ing_Trans, 
                         na.rm=T)) %>% 
  left_join(Nomes)

totais <- Ing %>% 
  group_by(Unidade) %>% 
  select(-Curso) %>% 
  summarise(`2018`=sum(`2018`)) %>% 
  left_join(Label_Unidades)

Ing$Unidade <- sapply(Ing$Unidade, function(x) paste(x, "-", collapse=""))
Ing <- bind_rows(Ing,totais) %>% arrange(Unidade, Curso)
row.names(Ing) <- Ing$Curso
Ing$Curso <- NULL
Ing$Unidade <- NULL
Ing["Total Geral",] <- colSums(Ing, na.rm=T)/2

Ing$Curso <- row.names(Ing)
Ing <- Ing %>% select("Unidade Acadêmica / Curso / Habilitação" = Curso, `2018`)
Tabela2.21 <- left_join(Tabela2.21, Ing)

### formatação

nomes <- Tabela2.21$`Unidade Acadêmica / Curso / Habilitação`

Tabela2.21[is.na(Tabela2.21)] <- 0

Tabela2.21 <- map_df(Tabela2.21[-1], ~ format(.x, big.mark = "."))
Tabela2.21 <- map_df(Tabela2.21, ~ str_replace(.x, "NA", "-"))

rownames(Tabela2.21) <- nomes

rm(Ing, totais, nomes)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.22 Evolução Alunos Registrados --------------------
#-------------------------------------------------------------------------------------------

# Atualizar o CSV que está na pasta
# com os dados do último anuário
Tabela2.22 <- read.csv2("EvoReg.csv", check.names=F, stringsAsFactors = FALSE)
Tabela2.22 <- cbind(Tabela2.22[1], 
                    map_df(Tabela2.22[-1], ~ as.double(str_remove(string = .x, pattern = "\\."))))

Reg <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Vinculo != "Transferido" & Vinculo != "Falecido") %>% 
  group_by(Curso) %>% 
  summarise(`2018`=n()) %>% 
  left_join(Nomes)

totais <- Reg %>% 
  group_by(Unidade) %>% 
  select(-Curso) %>% 
  summarise(`2018`=sum(`2018`)) %>% 
  left_join(Label_Unidades)

Reg$Unidade <- sapply(Reg$Unidade, function(x) paste(x, "-", collapse=""))
Reg <- bind_rows(Reg,totais) %>% arrange(Unidade, Curso)
row.names(Reg) <- Reg$Curso
Reg$Curso <- NULL
Reg$Unidade <- NULL
Reg["Total Geral",] <- colSums(Reg, na.rm=T)/2

Reg$Curso <- row.names(Reg)
Reg <- Reg %>% select("Unidade Acadêmica / Curso / Habilitação" = Curso, `2018`)
Tabela2.22 <- left_join(Tabela2.22, Reg)

### formatação
nomes <- Tabela2.22$`Unidade Acadêmica / Curso / Habilitação`

Tabela2.22[is.na(Tabela2.22)] <- 0

Tabela2.22 <- map_df(Tabela2.22[-1], ~ format(.x, big.mark = "."))
Tabela2.22 <- map_df(Tabela2.22, ~ str_replace(.x, "NA", "-"))

rownames(Tabela2.22) <- nomes

rm(Reg, totais, nomes)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.23 Evolução Formados ------------------------------
#-------------------------------------------------------------------------------------------

# Atualizar o CSV que está na pasta
# com os dados do último anuário
Tabela2.23 <- read.csv2("EvoFor.csv", check.names=F)
Tabela2.23 <- cbind(Tabela2.23[1], 
                    map_df(Tabela2.23[-1], ~ as.double(str_remove(string = .x, pattern = "\\."))))

For <- Arquivo42 %>% 
  filter((Sem_Ref==1 | Sem_Ref==2) & Vinculo=="Formado") %>% 
  group_by(Curso) %>% 
  summarise(`2018`=n()) %>% 
  left_join(Nomes)

totais <- For %>% 
  group_by(Unidade) %>% 
  select(-Curso) %>% 
  summarise(`2018`=sum(`2018`)) %>% 
  left_join(Label_Unidades)

For$Unidade <- sapply(For$Unidade, function(x) paste(x, "-", collapse=""))
For <- bind_rows(For,totais) %>% arrange(Unidade, Curso)
row.names(For) <- For$Curso
For$Curso <- NULL
For$Unidade <- NULL
For["Total Geral",] <- colSums(For, na.rm=T)/2

For$Curso <- row.names(For)
For <- For %>% select("Unidade Acadêmica / Curso / Habilitação" = Curso, `2018`)
Tabela2.23 <- left_join(Tabela2.23, For)

### formatação
nomes <- Tabela2.23$`Unidade Acadêmica / Curso / Habilitação`

Tabela2.23[is.na(Tabela2.23)] <- 0

Tabela2.23 <- map_df(Tabela2.23, ~ format(.x, big.mark = "."))
Tabela2.23 <- map_df(Tabela2.23[-1], ~ str_replace(.x, "NA", "-"))

rownames(Tabela2.23) <- nomes

rm(For, totais)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.27 Pesquisa ---------------------------------------
#-------------------------------------------------------------------------------------------

Pesquisa_1 <- Arquivo42 %>% 
  filter(Sem_Ref==1 & Pesquisa==1) %>% 
  distinct(.keep_all=T) %>% 
  group_by(Curso) %>% 
  summarise(Rem_1 = sum(Bolsa_Pesquisa), `Não Rem. 1` = n()) %>% 
  mutate(`Não Rem. 1` = `Não Rem. 1` - Rem_1)

Pesquisa_2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Pesquisa==1) %>% 
  distinct(.keep_all=T) %>% 
  group_by(Curso) %>% 
  summarise(Rem_2 = sum(Bolsa_Pesquisa), `Não Rem. 2` = n()) %>% 
  mutate(`Não Rem. 2` = `Não Rem. 2` - Rem_2)

Tabela2.27 <- left_join(Nomes, Pesquisa_1) %>% 
  left_join(Pesquisa_2)

totais <- Tabela2.27 %>% 
  group_by(Unidade) %>% 
  select(-Curso) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela2.27$Unidade <- sapply(Tabela2.27$Unidade, function(x) paste(x, "-", collapse=""))
Tabela2.27 <- bind_rows(Tabela2.27,totais) %>% arrange(Unidade, Curso)
rownames(Tabela2.27) <- Tabela2.27$Curso
Tabela2.27$Curso <- NULL
Tabela2.27$Unidade <- NULL

Tabela2.27["Total Geral",] <- colSums(Tabela2.27, na.rm=T)/2

cursos <- rownames(Tabela2.27)

### formatação

Tabela2.27[is.na(Tabela2.27)] <- 0

Tabela2.27 <- map_df(Tabela2.27, ~ format(.x, big.mark = "."))
Tabela2.27 <- map_df(Tabela2.27, ~ str_replace(.x, "NA", "-"))

rownames(Tabela2.27) <- cursos

rm(Pesquisa_1, Pesquisa_2, totais)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.28 Extensão ---------------------------------------
#-------------------------------------------------------------------------------------------

Extensao_1 <- Arquivo42 %>% 
  filter(Sem_Ref==1 & Extensao==1) %>% 
  distinct(.keep_all=T) %>% 
  group_by(Curso) %>% 
  summarise(Rem_1=sum(Bolsa_Extensao), `Não Rem. 1`=n()) %>% 
  mutate(`Não Rem. 1`=`Não Rem. 1`-Rem_1)

Extensao_2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Extensao==1) %>% 
  distinct(.keep_all=T) %>% 
  group_by(Curso) %>% 
  summarise(Rem_2=sum(Bolsa_Extensao), `Não Rem. 2`=n()) %>% 
  mutate(`Não Rem. 2`=`Não Rem. 2`-Rem_2)

Tabela2.28 <- left_join(Nomes, Extensao_1) %>% 
  left_join(Extensao_2)

totais <- Tabela2.28 %>% 
  group_by(Unidade) %>% 
  select(-Curso) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela2.28$Unidade <- sapply(Tabela2.28$Unidade, function(x) paste(x, "-", collapse=""))
Tabela2.28 <- bind_rows(Tabela2.28,totais) %>% arrange(Unidade, Curso)
row.names(Tabela2.28) <- Tabela2.28$Curso; Tabela2.28$Curso <- NULL; Tabela2.28$Unidade <- NULL

Tabela2.28["Total Geral",] <- colSums(Tabela2.28, na.rm=T)/2

cursos <- rownames(Tabela2.28)

### formatação

Tabela2.28[is.na(Tabela2.28)] <- 0

Tabela2.28 <- map_df(Tabela2.28, ~ format(.x, big.mark = "."))
Tabela2.28 <- map_df(Tabela2.28, ~ str_replace(.x, "NA", "-"))

rownames(Tabela2.28) <- cursos

rm(Extensao_1, Extensao_2, totais)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.29 Monitoria --------------------------------------
#-------------------------------------------------------------------------------------------

Monitoria_1 <- Arquivo42 %>% 
  filter(Sem_Ref==1 & Monitoria==1) %>% 
  distinct(.keep_all=T) %>% 
  group_by(Curso) %>% 
  summarise(Rem_1=sum(Mon_Remun), `Não Rem. 1`=n()) %>% 
  mutate(`Não Rem. 1`=`Não Rem. 1`-Rem_1)

Monitoria_2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Monitoria==1) %>% 
  distinct(.keep_all=T) %>% 
  group_by(Curso) %>% 
  summarise(Rem_2=sum(Mon_Remun), `Não Rem. 2`=n()) %>% 
  mutate(`Não Rem. 2`=`Não Rem. 2`-Rem_2)

Tabela2.29 <- left_join(Nomes, Monitoria_1) %>% 
  left_join(Monitoria_2)

totais <- Tabela2.29 %>% 
  group_by(Unidade) %>% 
  select(-Curso) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela2.29$Unidade <- sapply(Tabela2.29$Unidade, function(x) paste(x, "-", collapse=""))
Tabela2.29 <- bind_rows(Tabela2.29,totais) %>% arrange(Unidade, Curso)
row.names(Tabela2.29) <- Tabela2.29$Curso; Tabela2.29$Curso <- NULL; Tabela2.29$Unidade <- NULL

Tabela2.29["Total Geral",] <- colSums(Tabela2.29, na.rm=T)/2

cursos <- rownames(Tabela2.29)

### formatação

Tabela2.29[is.na(Tabela2.29)] <- 0

Tabela2.29 <- map_df(Tabela2.29, ~ format(.x, big.mark = "."))
Tabela2.29 <- map_df(Tabela2.29, ~ str_replace(.x, "NA", "-"))

rownames(Tabela2.29) <- cursos

rm(Monitoria_1, Monitoria_2, totais)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela 2.30 EnO - Estágio não obrigatório ------------------
#-------------------------------------------------------------------------------------------

EnO_1 <- Arquivo42 %>% 
  filter(Sem_Ref==1 & EnO==1) %>% 
  distinct(.keep_all=T) %>% 
  group_by(Curso) %>% 
  summarise(Rem_1=n())

EnO_2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & EnO==1) %>% 
  distinct(.keep_all=T) %>% 
  group_by(Curso) %>% 
  summarise(Rem_2=n())

Tabela2.30 <- left_join(Nomes, EnO_1) %>% 
  left_join(EnO_2)

totais <- Tabela2.30 %>% 
  group_by(Unidade) %>% 
  select(-Curso) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela2.30$Unidade <- sapply(Tabela2.30$Unidade, function(x) paste(x, "-", collapse=""))
Tabela2.30 <- bind_rows(Tabela2.30,totais) %>% arrange(Unidade, Curso)
row.names(Tabela2.30) <- Tabela2.30$Curso; Tabela2.30$Curso <- NULL; Tabela2.30$Unidade <- NULL

Tabela2.30["Total Geral",] <- colSums(Tabela2.30, na.rm=T)/2

cursos <- rownames(Tabela2.30)

### formatação

Tabela2.30[is.na(Tabela2.30)] <- 0

Tabela2.30 <- map_df(Tabela2.30, ~ format(.x, big.mark = "."))
Tabela2.30 <- map_df(Tabela2.30, ~ str_replace(.x, "NA", "-"))

rownames(Tabela2.30) <- cursos

rm(EnO_1, EnO_2, totais)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela COTA ingresso ---------------------------------------
#-------------------------------------------------------------------------------------------

M2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Vinculo != "Transferido" & Vinculo != "Falecido")
M2 <- inner_join(Arquivo41, M2, "ID_Inep")
M2 <- M2 %>% 
  select(Curso, starts_with("Cota")) %>% 
  group_by(Curso) %>% 
  summarise_all(sum, na.rm = TRUE)

Tabela2.COTA <- left_join(Nomes, M2)

totais <- Tabela2.COTA %>% 
  group_by(Unidade) %>% 
  select(-Curso) %>% 
  summarise_all(sum, na.rm = T) %>% 
  left_join(Label_Unidades)

Tabela2.COTA$Unidade <- sapply(Tabela2.COTA$Unidade, function(x) paste(x, "-", collapse=""))
Tabela2.COTA <- bind_rows(Tabela2.COTA,totais) %>% arrange(Unidade, Curso)
row.names(Tabela2.COTA) <- Tabela2.COTA$Curso
Tabela2.COTA$Curso <- NULL
Tabela2.COTA$Unidade <- NULL

Tabela2.COTA <- Tabela2.COTA %>% 
  ungroup() %>% 
  select("Étnica" = Cota_Etnica,
         "Pessoa com Deficiência" = Cota_Deficientes,
         "Escola Pública" = Cota_Escola,
         "Social / Renda Familiar" = Cota_Social,
         Total = Cotas)

Tabela2.COTA["Total Geral", ] <- colSums(Tabela2.COTA, na.rm=T)/2

### formatação
nomes <- rownames(Tabela2.COTA)

Tabela2.COTA <- map_df(Tabela2.COTA, ~ format(.x, big.mark = "."))
Tabela2.COTA <- map_df(Tabela2.COTA, ~ str_replace(.x, "NA", "-"))

rownames(Tabela2.COTA) <- nomes

rm(M2, totais)

#-------------------------------------------------------------------------------------------
#------------------------------ Tabela COTA ingresso e Sexo --------------------------------
#-------------------------------------------------------------------------------------------

Tabela2.COTA2 <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Vinculo != "Transferido" & Vinculo != "Falecido")
Tabela2.COTA2 <- inner_join(Arquivo41, Tabela2.COTA2, "ID_Inep")

Tabela2.COTA2 <- Tabela2.COTA2 %>% 
  select(Sexo,
         "Étnica" = Cota_Etnica,
         "Pessoa com Deficiência" = Cota_Deficientes,
         "Escola Pública" = Cota_Escola,
         "Social / Renda Familiar" = Cota_Social,
         Total = Cotas) %>% 
  group_by(Sexo) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  janitor::adorn_totals() %>% 
  t()

Tabela2.COTA2 <- Tabela2.COTA2[-1,]
Cotas <- rownames(Tabela2.COTA2)
Tabela2.COTA2 <- as_tibble(Tabela2.COTA2)
colnames(Tabela2.COTA2) <- c("Feminino", "Masculino", "Total")
Tabela2.COTA2 <- cbind(Cotas, map_df(Tabela2.COTA2, as.double))

Tabela2.COTA2$`% Feminino` <- paste(round(Tabela2.COTA2$Feminino/Tabela2.COTA2$Total*100,1), "%", sep="")
Tabela2.COTA2$`% Masculino` <- paste(round(Tabela2.COTA2$Masculino/Tabela2.COTA2$Total*100,1), "%", sep="")
Tabela2.COTA2$`% Total` <- paste(round(Tabela2.COTA2$Total/Tabela2.COTA2$Total[nrow(Tabela2.COTA2)]*100,1), "%", sep="")

Tabela2.COTA2 <- Tabela2.COTA2 %>% 
  mutate(Feminino = format(Feminino, big.mark = "."),
         Masculino = format(Masculino, big.mark = "."),
         Total = format(Total, big.mark = ".")) %>% 
  select(Cotas,
         Feminino, `% Feminino`,
         Masculino, `% Masculino`,
         Total, `% Total`)

rownames(Tabela2.COTA2) <- Tabela2.COTA2$Cotas
Tabela2.COTA2$Cotas <- NULL

rm(Cotas)

#*******************************************************************************************
#************************************* E X C E L *******************************************
#*******************************************************************************************
#-------------------------------------------------------------------------------------------
#------------------------------ S E T U P --------------------------------------------------
#-------------------------------------------------------------------------------------------
# REVER TODA ESSA PARTE
# ACHO QUE NÃO PRECISAMOS MAIS

library(openxlsx)
library(xlsx)

wb <- createWorkbook()

# Carregar dados das Tabelas
# Forma de Ingresso
load("dados_graduacao/Tabela2.03.RData")

# Ingressantes e Formados
# Tabela por Curso
load("dados_graduacao/Tabela2.04.RData")
# Tabela por Unidade
load("dados_graduacao/Tabela2.04.2.RData")

# Ingressantes por Sexo e Faixa Etária
load("dados_graduacao/Tabela2.05.RData")

# Alunos Regulares por Turno - EXCLUI EAD
load("dados_graduacao/Tabela2.06.RData")

# Alunos Regulares Ativos por Sexo
load("dados_graduacao/Tabela2.07.RData")

# Alunos Regulares Ativos e Trancados
load("dados_graduacao/Tabela2.08.RData")

#------------------------------ Tabela 2.03 --------------------------------
sheet <- createSheet(wb, sheetName="Tabela 2.03")
addDataFrame(Tabela2.03, sheet, row.names = T, startRow=6, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 3, 1, create=TRUE) 
x <- c("Tabela 2.03: Ingresso nos cursos de graduação pelo vestibular, PAS, ENEM e outras vias, por Unidade Acadêmica, curso e habilitação, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE)
x <- c("Unidade Acadêmica / Curso / Habilitação", "Ingressantes")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("Vestibular", NA, "PAS", NA, "ENEM",NA, "Transferência Obrigatória", NA,
       "Transferência Facultativa", NA,"Programas Especiais", NA,	"Convênio PEC", NA,"Total Geral")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 6, 5, 1, 60, create=TRUE)
x <- c("1° Sem.","2° Sem.", "1° Sem.","2° Sem.","1° Sem.","2° Sem.", "1° Sem.","2° Sem.", "1° Sem.","2° Sem.", "1° Sem.", 
       "2° Sem.", "1° Sem.","2° Sem.")
CB.setRowData(cb, x, 1, F)


#Tabela2.04
sheet <- createSheet(wb, sheetName="Tabela 2.04")
addDataFrame(Tabela2.04, sheet, row.names = T, startRow=5, startColumn = 4)
addDataFrame(Tabela2.04.2, sheet, row.names = T, startRow=5, startColumn = 13)
cb <- CellBlock(sheet, 2, 4, 1, 1, create=TRUE) 
x <- c("Tabela 2.04: Ingresso de alunos (vestibular, PAS, ENEM e outras vias) e número de formados nos cursos de graduação, por Unidade Acadêmica, curso e habilitação, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação", "Ingressantes", NA,NA,"Formados")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 5, 1, 9, create=TRUE)
x <- c("1° Sem.","2° Sem.", "Total", "1° Sem.","2° Sem.", "Total", NA,NA, "Unidade")
CB.setRowData(cb, x, 1, F)



#Tabela 2.05
sheet <- createSheet(wb, sheetName="Tabela 2.05")
addDataFrame(Tabela2.05, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.05: Ingresso de alunos (vestibular, PAS, ENEM e outras vias) nos cursos de graduação, por sexo e faixa etária, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Faixa Etária", "Feminino", NA,"Masculino", NA, "Total")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("Freq.","%", "Freq.","%","Freq.","%")
CB.setRowData(cb, x, 1, F)


#Tabela 2.06
sheet <- createSheet(wb, sheetName="Tabela 2.06")
addDataFrame(Tabela2.06[, -1], sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.06: Alunos regulares registrados nos cursos de graduação (exclui EAD), por Unidade Acadêmica e turno, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Unidade Acadêmica", "1° Semestre", NA,NA, "2° Semestre")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 6, 4, 75, 1, create=TRUE) 
x <- c("Centro de Excelência em Turismo - CET","Faculdade de Comunicação - FAC",
       "Faculdade de Economia, Administração e Contabilidade - FACE",
       "Faculdade de Arquitetura e Urbanismo - FAU","Faculdade de Agronomia e Medicina Veterinária - FAV",
       "Faculdade UnB Ceilândia - FCE","Faculdade de Ciência da Informação - FCI","Faculdade de Direito - FD",
       "Faculdade de Educação - FE","Faculdade de Educação Física - FEF","Faculdade UnB Gama - FGA",
       "Faculdade de Medicina - FM","Faculdade de Ciências da Saúde - FS","Faculdade de Tecnologia - FT",
       "Faculdade UnB Planaltina - FUP","Instituto de Ciências Biológicas - IB","Instituto de Ciências Sociais - ICS",
       "Instituto de Artes - IdA","Instituto de Ciências Exatas - IE","Instituto de Física - IF","Instituto de Geociências - IG",
       "Instituto de Ciências Humanas - IH","Instituto de Letras - IL","Instituto de Psicologia - IP",
       "Instituto de Ciência Política - IPOL", "Instituto de Química - IQ","Instituto de Relações Internacionais - IREL", "Total")
CB.setColData(cb, x, 1, F)


#Tabela 2.07
sheet <- createSheet(wb, sheetName="Tabela 2.07")
addDataFrame(Tabela2.07[, -1], sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.07: Alunos regulares ativos registrados nos cursos de graduação, por Unidade Acadêmica e sexo, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Unidade Acadêmica", "1° Semestre", NA,NA, "2° Semestre")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 6, 4, 75, 1, create=TRUE) 
x <- c("Centro de Excelência em Turismo - CET","Faculdade de Comunicação - FAC",
       "Faculdade de Economia, Administração e Contabilidade - FACE",
       "Faculdade de Arquitetura e Urbanismo - FAU","Faculdade de Agronomia e Medicina Veterinária - FAV",
       "Faculdade UnB Ceilândia - FCE","Faculdade de Ciência da Informação - FCI","Faculdade de Direito - FD",
       "Faculdade de Educação - FE","Faculdade de Educação Física - FEF","Faculdade UnB Gama - FGA",
       "Faculdade de Medicina - FM","Faculdade de Ciências da Saúde - FS","Faculdade de Tecnologia - FT",
       "Faculdade UnB Planaltina - FUP","Instituto de Ciências Biológicas - IB","Instituto de Ciências Sociais - ICS",
       "Instituto de Artes - IdA","Instituto de Ciências Exatas - IE","Instituto de Física - IF","Instituto de Geociências - IG",
       "Instituto de Ciências Humanas - IH","Instituto de Letras - IL","Instituto de Psicologia - IP",
       "Instituto de Ciência Política - IPOL", "Instituto de Química - IQ","Instituto de Relações Internacionais - IREL", "Total")
CB.setColData(cb, x, 1, F)


#Tabela2.08
sheet <- createSheet(wb, sheetName="Tabela 2.08")
addDataFrame(Tabela2.08, sheet, row.names = T, startRow=6, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.08: Alunos regulares registrados ativos e com trancamento geral de matrícula nos cursos de graduação, por Unidade Acadêmica, curso, habilitação e sexo, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação", "Ativos", NA,NA,NA, "Com Trancamento Geral de Matrícula", NA,NA,NA, "Total")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("1° Sem.", NA, "2° Sem.", NA, "1° Sem.", NA, "2° Sem.", NA, "1° Sem.", NA, "2° Sem.")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 6, 5, 1, 60, create=TRUE)
x <- c("Fem.","Masc.", "Fem.","Masc.","Fem.","Masc.","Fem.","Masc.","Fem.","Masc.","Fem.","Masc.")
CB.setRowData(cb, x, 1, F)


#Tabela 2.09
sheet <- createSheet(wb, sheetName="Tabela 2.09")
addDataFrame(Tabela2.09, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.09: Alunos regulares registrados nos cursos de graduação, por sexo e faixa etária, UnB, 2o Semestre de 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Faixa Etária", "Feminino", NA,"Masculino", NA, "Total")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("Freq.","%", "Freq.","%","Freq.","%")
CB.setRowData(cb, x, 1, F)


#Tabela 2.11
sheet <- createSheet(wb, sheetName="Tabela 2.11")
addDataFrame(Tabela2.11, sheet, row.names = T, startRow=7, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.11: Movimentação dos alunos nos cursos de graduação, por Unidade Acadêmica, curso e habilitação, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 4, 1, 60, create=TRUE)
x <- c("Unidade Acadêmica / Curso / Habilitação", NA,NA,NA,NA,NA, "Alunos / Desligamentos")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 6, 5, 1, 60, create=TRUE)
x <- c("Alunos Regulares Registrados",NA, "Abandono",NA, "Desligamento Voluntário",NA, "Desligamento por Falta de Rendimento",NA,
       "Outros*",NA, "Total de Movimentação")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 7, 5, 1, 60, create=TRUE)
x <- c("1° Sem.","2° Sem.", "1° Sem.", "2° Sem.","1° Sem.", "2° Sem.","1° Sem.", "2° Sem.","1° Sem.", "2° Sem.", "1° Sem.","2° Sem.")
CB.setRowData(cb, x, 1, F)


#Tabela 2.XX
sheet <- createSheet(wb, sheetName="Tabela 2.XX")
addDataFrame(Tabela2.XX, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.XX: Alunos regulares registrados nos cursos de graduação, por Unidade Acadêmica e raça/cor, UnB, 2o Semestre de 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação", "Raça/Cor")
CB.setRowData(cb, x, 1, F)


#Tabela 2.XX2
sheet <- createSheet(wb, sheetName="Tabela 2.XX2")
addDataFrame(Tabela2.XX2, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.XX2: Alunos regulares registrados nos cursos de graduação, por raça/cor e sexo, UnB, 2o Semestre de 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Cor/Raça", "Feminino", NA,"Masculino", NA, "Total")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("Freq.","%", "Freq.","%","Freq.","%")
CB.setRowData(cb, x, 1, F)


#Tabela 2.COTA
sheet <- createSheet(wb, sheetName="Tabela 2.COTA")
addDataFrame(Tabela2.COTA, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.XX: Alunos regulares registrados nos cursos de graduação, por Unidade Acadêmica e cota de ingresso, UnB, 2o Semestre de 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação", "Cota de ingresso")
CB.setRowData(cb, x, 1, F)


#Tabela 2.COTA2
sheet <- createSheet(wb, sheetName="Tabela 2.COTA2")
addDataFrame(Tabela2.COTA2, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.COTA2: Alunos regulares registrados nos cursos de graduação, por cota de ingresso e sexo, UnB, 2o Semestre de 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Cota de ingresso", "Feminino", NA,"Masculino", NA, "Total")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("Freq.","%", "Freq.","%","Freq.","%")
CB.setRowData(cb, x, 1, F)


#Tabela 2.10
sheet <- createSheet(wb, sheetName="Tabela 2.10")
addDataFrame(Tabela2.10, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.10: Alunos regulares ativos, matrículas e aprovados em disciplinas, nos cursos de graduação, por Unidade Acadêmica, curso e habilitação, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação", "Alunos Ativos", NA, "Matrículas em Disciplinas", NA, "Aprovados em Disciplinas", NA, 
       "% Aprovados/Disciplina")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("1° Sem.","2° Sem.", "1° Sem.", "2° Sem.","1° Sem.", "2° Sem.","1° Sem.", "2° Sem.")
CB.setRowData(cb, x, 1, F)


#Tabela 2.12
sheet <- createSheet(wb, sheetName="Tabela 2.12")
addDataFrame(Tabela2.12, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.12: Alunos regulares Portadores de Necessidades Especiais registrados nos cursos da UnB, 2o Semestre de 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Unidade acadêmica / Curso / Habilitação", "Área de Necessidade Especial")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 184, 4, 1, 60, create=TRUE) 
x <- c("Outros: deficiência múltipla, autismo, síndrome de Asperger, síndrome de Rett e transtorno desintegrativo da infância")
CB.setRowData(cb, x, 1, F)


#Tabela 2.14
sheet <- createSheet(wb, sheetName="Tabela 2.14")
addDataFrame(Tabela2.14, sheet, row.names = T, startRow=5, startColumn = 5)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 2.14: Alunos estrangeiros regulares registrados nos cursos de graduação, por Continente, País e Unidade Acadêmica, UnB, 2o Semestre de 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Continente", "País", "Unidade Acadêmica")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 6, 4, 75, 1, create=TRUE)
### precisa rever o comando abaixo toda vez
x <- c("ÁFRICA", rep(NA,21), "AMÉRICA", rep(NA,22), "ÁSIA", rep(NA,11), "EUROPA", rep(NA,16))
CB.setColData(cb, x, 1, F)
addDataFrame(Tabela2.14.2, sheet, row.names = F, startRow=26, startColumn = 38)


#Tabela 2.21
sheet <- createSheet(wb, sheetName="Tabela 2.21")
addDataFrame(Tabela2.21, sheet, row.names = T, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 1, create=TRUE) 
x <- c("Tabela 2.21 - Evolução do ingresso de alunos (vestibular, PAS, ENEM e outras vias) nos cursos de graduação, por Unidade Acadêmica e curso, UnB, 2011 a 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 1, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação")
CB.setRowData(cb, x, 1, F)
#Tem que arrumar os dois últimos


#Tabela 2.22
sheet <- createSheet(wb, sheetName="Tabela 2.22")
addDataFrame(Tabela2.22, sheet, row.names = T, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 1, create=TRUE) 
x <- c("Tabela 2.22 - Evolução do número de alunos regulares registrados nos cursos de graduação, por Unidade Acadêmica e Curso, UnB, 2011 a 2018 (saldo do 2o semestre)")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 1, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação")
CB.setRowData(cb, x, 1, F)
#Tem que arrumar os dois últimos


#Tabela 2.23
sheet <- createSheet(wb, sheetName="Tabela 2.23")
addDataFrame(Tabela2.23, sheet, row.names = T, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 1, create=TRUE) 
x <- c("Tabela 2.23 - Evolução do número de alunos formados nos cursos de graduação, por Unidade Acadêmica e Curso, UnB, 2011 a 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 1, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação")
CB.setRowData(cb, x, 1, F)
#Tem que arrumar os dois últimos


#Tabela 2.27
sheet <- createSheet(wb, sheetName="Tabela 2.27")
addDataFrame(Tabela2.27, sheet, row.names = T, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 1, create=TRUE) 
x <- c("Tabela 27: Bolsas Acadêmicas de Pesquisa1 Concedidas a Alunos de Graduação, por Unidade Acadêmica, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 5, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação", "1° Rem", "1° Não Rem", "2° Rem", "2° Não Rem")
CB.setRowData(cb, x, 1, F)


#Tabela 2.28
sheet <- createSheet(wb, sheetName="Tabela 2.28")
addDataFrame(Tabela2.28, sheet, row.names = T, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 1, create=TRUE) 
x <- c("Tabela 28: Bolsas Acadêmicas de Extensão Concedidas a Alunos de Graduação, por Unidade Acadêmica, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 5, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação", "1° Rem", "1° Não Rem", "2° Rem", "2° Não Rem")
CB.setRowData(cb, x, 1, F)


#Tabela 2.29
sheet <- createSheet(wb, sheetName="Tabela 2.29")
addDataFrame(Tabela2.29, sheet, row.names = T, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 1, create=TRUE) 
x <- c("Tabela 29: Bolsas Acadêmicas de Monitoria Concedidas a Alunos de Graduação, por Unidade Acadêmica, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 5, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação", "1° Rem", "1° Não Rem", "2° Rem", "2° Não Rem")
CB.setRowData(cb, x, 1, F)


#Tabela 2.30
sheet <- createSheet(wb, sheetName="Tabela 2.30")
addDataFrame(Tabela2.30, sheet, row.names = T, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 1, create=TRUE) 
x <- c("Tabela 30: Estágio Não Obrigatatório dos Alunos de Graduação, por Unidade Acadêmica, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 5, create=TRUE) 
x <- c("Unidade Acadêmica / Curso / Habilitação", "1° Sem", "2° Sem")
CB.setRowData(cb, x, 1, F)

### salvar o excel
### não esquecer de atualizar o nome do arquivo
saveWorkbook(wb, "dados_graduacao/Tabelas Anuário 2019 Graduação.xlsx") 

rm(wb,cb,sheet,x)


