# Setup -------------------------------------------------------------------

options(OutDec = ",")

library(tidyverse)
library(lubridate)
library(data.table)
library(dataframes2xls)

load("dados_identificados/Completo14042020.RData")
load("labels/Label_Unidades_pos.RData")

### define ano do CENSO

anobase <- 2019

Doc <- Completo %>% 
  filter(`Ano Ingresso Opcao` <= anobase,
         `Ano Saida Opcao` >= anobase,
         `Nivel` == "Doutorado",
         `Forma Saida` != "Anulacao de Registro")

### tabela de referência dos cursos de doutorado

Nomes <- Doc %>% 
  count(`Nome Curso`, Unidade) %>% 
  select(-n)

# Tabela 3.23 Ingressantes e Formados por Sexo -------------------------------------------------------------

Ing1 <- Doc %>% filter(`Ano Ingresso Opcao`== anobase & `Semestre Ingresso Opcao`==1) %>%
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n, fill = 0) %>% 
  rename(`Ing Feminino 1`=Feminino, `Ing Masculino 1`=Masculino)

Ing2 <- Doc %>% filter(`Ano Ingresso Opcao`== anobase & `Semestre Ingresso Opcao`==2) %>%
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n, fill = 0) %>% 
  rename(`Ing Feminino 2`=Feminino, `Ing Masculino 2`=Masculino)

For1 <- Doc %>% filter(`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`<=1) %>%
  filter(`For. Saida Opcao` == "Formatura Pos-Graduacao") %>%  
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n, fill = 0) %>% 
  rename(`For. Feminino 1`=Feminino, `For. Masculino 1`=Masculino)

For2 <- Doc %>% filter(`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`==2) %>%
  filter(`For. Saida Opcao` == "Formatura Pos-Graduacao") %>%  
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n, fill = 0) %>% 
  rename(`For. Feminino 2`=Feminino, `For. Masculino 2`=Masculino)

# Unir tabelas
Tabela3.23 <- left_join(Nomes, Ing1) %>% 
  left_join(Ing2) %>% 
  left_join(For1) %>% 
  left_join(For2)

Tabela3.23[is.na(Tabela3.23)] <- 0

totais <- Tabela3.23 %>% 
  group_by(Unidade) %>% 
  select(-`Nome Curso`) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela3.23$Unidade <- sapply(Tabela3.23$Unidade, function(x) paste(x, "-", collapse=""))

Tabela3.23 <- rbind(Tabela3.23, totais) %>% 
  arrange(Unidade, `Nome Curso`)      

row.names(Tabela3.23) <- Tabela3.23$`Nome Curso`; Tabela3.23$`Nome Curso` <- NULL; Tabela3.23$Unidade <- NULL

Tabela3.23$`Total Ing 1` <- rowSums(Tabela3.23[,c("Ing Feminino 1","Ing Masculino 1")])
Tabela3.23$`Total Ing 2` <- rowSums(Tabela3.23[,c("Ing Feminino 2","Ing Masculino 2")])
Tabela3.23$`Total Ing` <- rowSums(Tabela3.23[,c("Total Ing 1","Total Ing 2")])
Tabela3.23$`Total For. 1` <- rowSums(Tabela3.23[,c("For. Feminino 1","For. Masculino 1")])
Tabela3.23$`Total For. 2` <- rowSums(Tabela3.23[,c("For. Feminino 2","For. Masculino 2")])
Tabela3.23$`Total For.` <- rowSums(Tabela3.23[,c("Total For. 1","Total For. 2")])


Tabela3.23 <- Tabela3.23 %>% select(`Ing Feminino 1`, `Ing Masculino 1`, `Total Ing 1`, `Ing Feminino 2`, `Ing Masculino 2`, `Total Ing 2`,
                                    `Total Ing`, `For. Feminino 1`, `For. Masculino 1`, `Total For. 1`, `For. Feminino 2`, `For. Masculino 2`,
                                    `Total For. 2`, `Total For.`)

Tabela3.23["Total Geral",] <- colSums(Tabela3.23, na.rm = TRUE)/2

Ing <- Doc %>% filter(`Ano Ingresso Opcao`== anobase) %>% 
  group_by(Unidade) %>% 
  summarise(Ingressantes=n()) %>% 
  arrange(desc(Ingressantes))

For <- Doc %>% filter(`Ano Saida Opcao`== anobase & `For. Saida Opcao` == "Formatura Pos-Graduacao") %>% 
  group_by(Unidade) %>% 
  summarise(Concluintes=n())

Tabela3.23.2 <- full_join(Ing, For)
Tabela3.23.2[is.na(Tabela3.23.2)] <- 0


rm(Ing, Ing1, Ing2, For, For1, For2, totais)

# Tabela 3.24 Ingressantes e concluintes por sexo e faixa etária -------------------------------------------------------------

Ing <- Doc %>% filter(`Ano Ingresso Opcao`== anobase)

# verificar data para o cálculo da idade

Ing <- Ing %>% mutate(`Faixa Etária` = ifelse(Idade > 18 & Idade <= 24, "De 19 a 24 anos",
                                              ifelse(Idade > 24 & Idade <= 29, "De 25 a 29 anos",
                                                     ifelse(Idade > 29 & Idade <= 34, "De 30 a 34 anos",
                                                            ifelse(Idade > 34 & Idade <= 39, "De 35 a 39 anos",
                                                                   ifelse(Idade > 39 & Idade <= 44, "De 40 a 44 anos", "De 45 anos ou mais"))))))

Ing <-  Ing %>% count(`Faixa Etária`, Sexo) %>% 
  spread(Sexo, n) %>% 
  janitor::adorn_totals(c("row", "col"))

Ing$`% Feminino` <- paste(round(Ing$Feminino/Ing$Total*100,1), "%", sep = "")
Ing$`% Masculino` <- paste(round(Ing$Masculino/Ing$Total*100,1), "%", sep = "")

For <-  Doc %>%  filter(`Ano Saida Opcao` == anobase & `For. Saida Opcao` == "Formatura Pos-Graduacao")

For$Idade <- difftime(as.Date("2018-01-01"), For$Nascimento)/365

For <- For %>% mutate(`Faixa Etária` = ifelse(Idade > 18 & Idade <= 24, "De 19 a 24 anos",
                                              ifelse(Idade > 24 & Idade <= 29, "De 25 a 29 anos",
                                                     ifelse(Idade > 29 & Idade <= 34, "De 30 a 34 anos",
                                                            ifelse(Idade > 34 & Idade <= 39, "De 35 a 39 anos",
                                                                   ifelse(Idade > 39 & Idade <= 44, "De 40 a 44 anos", "De 45 anos ou mais"))))))

For <- For %>% count(`Faixa Etária`, Sexo) %>% 
  spread(Sexo, n) %>% 
  janitor::adorn_totals(c("row", "col"))

For$`% Feminino` <- paste(round(For$Feminino/For$Total*100,1), "%", sep = "")
For$`% Masculino` <- paste(round(For$Masculino/For$Total*100,1), "%", sep = "")

Tabela3.24 <- full_join(Ing, For, by = "Faixa Etária")

Tabela3.24[is.na(Tabela3.24)] <- 0
Tabela3.24$`% Masculino.x`[Tabela3.24$`% Masculino.x` == "NA%"] <- 0
Tabela3.24$`% Feminino.x`[Tabela3.24$`% Feminino.x` == "NA%"] <- 0
Tabela3.24$`% Masculino.y`[Tabela3.24$`% Masculino.y` == "NA%"] <- 0
Tabela3.24$`% Feminino.y`[Tabela3.24$`% Feminino.y` == "NA%"] <- 0

Tabela3.24 <- Tabela3.24 %>% select("Faixa Etária","Feminino.x","% Feminino.x","Masculino.x","% Masculino.x","Total.x","Feminino.y","% Feminino.y","Masculino.y","% Masculino.y", "Total.y")

rownames(Tabela3.24) <- Tabela3.24$`Faixa Etária`
Tabela3.24$`Faixa Etária` <-  NULL

rm(For, Ing)


# Tabela 3.25 Alunos Regulares Ativos, Matrículas e Aprovados em Disc --------

# MUDAR NOME DAS VARIAVEIS PARA HE0 HE1 HE2

HE20180 <- read_fwf("dados_identificados/he20190.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                    col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20181", "Frequencias")), 
                    locale = locale(encoding = "latin1")) %>% distinct(MatricAluno, .keep_all = T)

HE20181 <- read_fwf("dados_identificados/he20191.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                    col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20181", "Frequencias")), 
                    locale = locale(encoding = "latin1")) %>% distinct(MatricAluno, .keep_all = T)

HE20182 <- read_fwf("dados_identificados/he20192.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                    col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20182", "Frequencias")), 
                    locale = locale(encoding = "latin1")) %>% distinct(MatricAluno, .keep_all = T)


HE20181 <- rbind(HE20181, HE20180)

HE20181 <- within(HE20181, {
  
  Mencao_20181[Mencao_20181=="AP"] <- "AP 1"
  Mencao_20181[Mencao_20181=="CC"] <- "AP 1"
  Mencao_20181[Mencao_20181=="MM"] <- "AP 1"
  Mencao_20181[Mencao_20181=="MS"] <- "AP 1"
  Mencao_20181[Mencao_20181=="SS"] <- "AP 1"
  
  Mencao_20181[Mencao_20181=="SR"] <- "RP 1"
  Mencao_20181[Mencao_20181=="II"] <- "RP 1"
  Mencao_20181[Mencao_20181=="MI"] <- "RP 1"
  Mencao_20181[Mencao_20181=="TR"] <- "RP 1"
  Mencao_20181[Mencao_20181=="TJ"] <- "RP 1"
  Mencao_20181[Mencao_20181=="DP"] <- "RP 1"
  Mencao_20181[Mencao_20181=="RP"] <- "RP 1"
})

HE20182 <- within(HE20182, {
  
  Mencao_20182[Mencao_20182=="AP"] <- "AP 2"
  Mencao_20182[Mencao_20182=="CC"] <- "AP 2"
  Mencao_20182[Mencao_20182=="MM"] <- "AP 2"
  Mencao_20182[Mencao_20182=="MS"] <- "AP 2"
  Mencao_20182[Mencao_20182=="SS"] <- "AP 2"
  
  Mencao_20182[Mencao_20182=="SR"] <- "RP 2"
  Mencao_20182[Mencao_20182=="II"] <- "RP 2"
  Mencao_20182[Mencao_20182=="MI"] <- "RP 2"
  Mencao_20182[Mencao_20182=="TR"] <- "RP 2"
  Mencao_20182[Mencao_20182=="TJ"] <- "RP 2"
  Mencao_20182[Mencao_20182=="DP"] <- "RP 2"
  Mencao_20182[Mencao_20182=="RP"] <- "RP 2"
})

M1 <- Doc %>% filter(is.na(Trancados_1)) %>% filter(`Ano Ingresso Opcao`< anobase | (`Ano Ingresso Opcao`== anobase & `Semestre Ingresso Opcao`<=1) & `Ano Saida Opcao`>= anobase)
AA1 <- M1 %>% group_by(`Nome Curso`) %>% summarize(`Alunos Ativos 1` = n())
M1 <- inner_join(M1, HE20181, by = "MatricAluno")
MD1 <- M1 %>% group_by(`Nome Curso`) %>% summarise(`Matriculados em Disciplinas 1`=n())
AP1 <- M1 %>% group_by(`Nome Curso`, Mencao_20181) %>% summarize(n=n()) %>% spread(Mencao_20181, n, fill = 0)

M2 <- Doc %>% filter(is.na(Trancados_2)) %>% filter(`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`> anobase | (`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`==2)))
AA2 <- M2 %>% group_by(`Nome Curso`) %>% summarize(`Alunos Ativos 2` = n())
M2 <- inner_join(M2, HE20182, by = "MatricAluno")
MD2 <- M2 %>% group_by(`Nome Curso`) %>% summarise(`Matriculados em Disciplinas 2`=n())
AP2 <- M2 %>% group_by(`Nome Curso`, Mencao_20182) %>% summarize(n=n()) %>% spread(Mencao_20182, n, fill = 0)

Tabela3.25 <- left_join(Nomes, AA1) %>% 
  left_join(AA2) %>% 
  left_join(AP1) %>% 
  left_join(AP2) %>% 
  left_join(MD1) %>% 
  left_join(MD2)

Totais <- Tabela3.25 %>% group_by(`Unidade`) %>% 
  select(-`Nome Curso`) %>%
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela3.25$Unidade <- sapply(Tabela3.25$Unidade, function(x) paste(x, "-", collapse=""))
Tabela3.25 <- rbind(Tabela3.25, Totais) %>% arrange(`Unidade`,`Nome Curso`)

rownames(Tabela3.25) <- Tabela3.25$`Nome Curso`

Tabela3.25$Unidade <- NULL
Tabela3.25$`Nome Curso` <- NULL

Tabela3.25["Total Geral",] <- colSums(Tabela3.25, na.rm = T)

Tabela3.25$`% Aprovado/Matriculado 1` <- paste(round(Tabela3.25$`AP 1`/Tabela3.25$`Matriculados em Disciplinas 1`*100,1),"%", sep = "")
Tabela3.25$`% Aprovado/Matriculado 2` <- paste(round(Tabela3.25$`AP 2`/Tabela3.25$`Matriculados em Disciplinas 2`*100,1),"%", sep = "")

Tabela3.25$`% Reprovado/Matriculado 1` <- paste(round(Tabela3.25$`RP 1`/Tabela3.25$`Matriculados em Disciplinas 1`*100,1),"%", sep = "")
Tabela3.25$`% Reprovado/Matriculado 2` <- paste(round(Tabela3.25$`RP 2`/Tabela3.25$`Matriculados em Disciplinas 2`*100,1),"%", sep = "")

Tabela3.25[is.na(Tabela3.25)] <- 0
Tabela3.25$`% Aprovado/Matriculado 1`[Tabela3.25$`% Aprovado/Matriculado 1`== "NA%"] <- 0
Tabela3.25$`% Aprovado/Matriculado 2`[Tabela3.25$`% Aprovado/Matriculado 2`== "NA%"] <- 0
Tabela3.25$`% Aprovado/Matriculado 1`[Tabela3.25$`% Aprovado/Matriculado 1`== "NaN%"] <- 0
Tabela3.25$`% Aprovado/Matriculado 2`[Tabela3.25$`% Aprovado/Matriculado 2`== "NaN%"] <- 0
Tabela3.25$`% Reprovado/Matriculado 1`[Tabela3.25$`% Reprovado/Matriculado 1`== "NA%"] <- 0
Tabela3.25$`% Reprovado/Matriculado 2`[Tabela3.25$`% Reprovado/Matriculado 2`== "NA%"] <- 0
Tabela3.25$`% Reprovado/Matriculado 1`[Tabela3.25$`% Reprovado/Matriculado 1`== "NaN%"] <- 0
Tabela3.25$`% Reprovado/Matriculado 2`[Tabela3.25$`% Reprovado/Matriculado 2`== "NaN%"] <- 0

Tabela3.25 <- Tabela3.25 %>% select(1,2,3,5,4,6,7,8,9,10,11,12)

rm(HE20180, HE20181, HE20182, M1, AA1, MD1, AP1, M2, AA2, MD2, AP2, Totais)

# Tabela 3.26 Alunos Regulares por Sexo e Faixa Etária --------------------------------

Tabela3.26 <- Doc %>% filter(`Ano Ingresso Opcao`<=2018 & (`Ano Saida Opcao`>2018 | (`Ano Saida Opcao`==2018 & `Semestre Saida Opcao`==2)))

Tabela3.26$Idade <- difftime(as.Date("2018-01-01"), Tabela3.26$Nascimento)/365

Tabela3.26 <- Tabela3.26 %>% mutate(`Faixa Etária` = ifelse(Idade > 18 & Idade <= 24, "De 19 a 24 anos",
                                                            ifelse(Idade > 24 & Idade <= 29, "De 25 a 29 anos",
                                                                   ifelse(Idade > 29 & Idade <= 34, "De 30 a 34 anos",
                                                                          ifelse(Idade > 34 & Idade <= 39, "De 35 a 39 anos",
                                                                                 ifelse(Idade > 39 & Idade <= 44, "De 40 a 44 anos", "De 45 anos ou mais"))))))


Tabela3.26 <- Tabela3.26 %>% group_by(`Faixa Etária`, Sexo) %>% summarise(n=n())
md <- melt(Tabela3.26, id=c("Faixa Etária", "Sexo"))
Tabela3.26 <- dcast(md, `Faixa Etária` ~ Sexo)

rownames(Tabela3.26) <- Tabela3.26$`Faixa Etária`
Tabela3.26 <- Tabela3.26 %>% select(-`Faixa Etária`)

Tabela3.26["Total", ] <- colSums(Tabela3.26, na.rm=T)
Tabela3.26$Total <- rowSums(Tabela3.26)

Tabela3.26$`% Feminino` <- paste(round(Tabela3.26$Feminino/Tabela3.26$Total*100,1), "%", sep="")
Tabela3.26$`% Masculino` <- paste(round(Tabela3.26$Masculino/Tabela3.26$Total*100,1), "%", sep="")

Tabela3.26 <- Tabela3.26 %>% select(Feminino, `% Feminino`, Masculino, `% Masculino`, Total)

rownames(Tabela3.26)[6] <- "45 anos ou mais"

rm(md)

# Tabela 3.27 Alunos Regulares Registrados por Sexo --------------------------------

M1 <- Doc %>% filter(`Ano Ingresso Opcao`< anobase | (`Ano Ingresso Opcao` == anobase & `Semestre Ingresso Opcao` <= 1) & `Ano Saida Opcao` > anobase) %>% group_by(`Nome Curso`, Sexo) %>% summarize(n=n())
md <- melt(M1, id=c("Nome Curso", "Sexo"))
M1 <- dcast(md, `Nome Curso` ~ `Sexo`)
M1.1 <- M1 %>% select(`Nome Curso`)
M1$`Nome Curso` <- NULL
M1 <- M1 %>% mutate(Total = rowSums(M1, na.rm=TRUE))
M1 <- cbind(M1.1, M1)
names(M1)[2:4] <- c("Feminino 1", "Masculino 1", "Total 1")
M1 <- left_join(M1, Nomes)
M1.Unidade <- M1 %>% select(-`Nome Curso`) %>% group_by(Unidade) %>% summarize_all(sum, na.rm=TRUE) %>% left_join(Label_Unidades)
M1 <- rbind(M1.Unidade, M1) %>%  arrange(Unidade, `Nome Curso`)
M1 <- M1 %>% select(-1) %>% select(4, 1, 2, 3)


M2 <- Doc %>% filter(`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>% 
  group_by(`Nome Curso`, Sexo) %>% summarise(n=n())
md <- melt(M2, id=c("Nome Curso", "Sexo"))
M2 <- dcast(md, `Nome Curso` ~ `Sexo`)
M2.2 <- M2 %>% select(`Nome Curso`)
M2$`Nome Curso` <- NULL
M2 <- M2 %>% mutate(Total = rowSums(M2, na.rm=TRUE))
M2 <- cbind(M2.2, M2)
names(M2)[2:4] <- c("Feminino 2", "Masculino 2", "Total 2")
M2 <- left_join(M2, Nomes)
M2.Unidade <- M2 %>% select(-`Nome Curso`) %>% group_by(Unidade) %>% summarize_all(sum, na.rm=TRUE) %>% left_join(Label_Unidades)
M2 <- rbind(M2.Unidade, M2) %>%  arrange(Unidade, `Nome Curso`)
M2 <- M2 %>% select(-1) %>% select(4, 1, 2, 3)


Tabela3.27 <- left_join(M1, M2) 

Tabela3.27 <- Tabela3.27[c(1,2,3,4,6,5,10,7,8,9,12,11,15,13,14,16,18,17,20,19,22,21,24,23,26,25,28,27,29,34,30,31,32,33,35,36,37,38,44,39,40,41,42,43,45,46,47,48,50,49,58,51,52,53,54,55,56,57,59,60,63,61,62,64,66,65,68,67,69,71,70,74,72,73,78,75,76,77,79,80,81,82,83,85,84,86,87,88,90,89,92,91,93,94,95,96,97), ]

row.names(Tabela3.27) <- Tabela3.27$`Nome Curso`

Tabela3.27$`Nome Curso` <- NULL

"Total Geral" <-  colSums(Tabela3.27, na.rm = TRUE)/2

Tabela3.27 <- rbind(Tabela3.27, `Total Geral`)

row.names(Tabela3.27)[98] <- "Total Geral"

rm(M1, M1.1, M1.Unidade, M2, M2.2, M2.Unidade, md)

Tabela3.27.2 <- Doc %>% filter(`Ano Ingresso Opcao` <= anobase & `Ano Saida Opcao` >= anobase) %>% group_by(Unidade) %>% summarise(Registrados=n()) %>% arrange(desc(Registrados))

row.names(Tabela3.27.2 ) <- Tabela3.27.2$Unidade
Tabela3.27.2$Unidade <- NULL

# Tabela 3.28 Alunos Ativos e Trancados por Sexo -------------------------------------------------------------

# PARECE REPETIDO - REVER

HE20180 <- read_fwf("X:/CIG/SIGRA/30072019/HE20180.txt", 
                    fwf_widths(c(9,4,1,6,2,3,2,3), col_names=c("MatricAluno","Ano HE","Semestre HE", 
                                                               "CodDisc", "Turma","Creditos","Mencao","Frequencia")), 
                    locale = locale(encoding = "latin1"))

HE20181 <- read_fwf("X:/CIG/SIGRA/30072019/HE20181.txt", 
                    fwf_widths(c(9,4,1,6,2,3,2,3), col_names=c("MatricAluno","Ano HE","Semestre HE", 
                                                               "CodDisc", "Turma","Creditos","Mencao","Frequencia")), 
                    locale = locale(encoding = "latin1"))

HE20181 <- rbind(HE20180, HE20181) %>% select(MatricAluno, Mencao_20181=Mencao); rm(HE20180)


HE20182 <- read_fwf("X:/CIG/SIGRA/30072019/HE20182.txt", 
                    fwf_widths(c(9,4,1,6,2,3,2,3), col_names=c("MatricAluno","Ano HE","Semestre HE", 
                                                               "CodDisc", "Turma","Creditos","Mencao","Frequencia")), 
                    locale = locale(encoding = "latin1")) %>% select(MatricAluno, Mencao_20182=Mencao)


HE20181 <- within(HE20181, {
  Mencao_20181[Mencao_20181=="AP"] <- "AP 1"
  Mencao_20181[Mencao_20181=="CC"] <- "AP 1"
  Mencao_20181[Mencao_20181=="MM"] <- "AP 1"
  Mencao_20181[Mencao_20181=="MS"] <- "AP 1"
  Mencao_20181[Mencao_20181=="SS"] <- "AP 1"
  
  Mencao_20181[Mencao_20181=="SR"] <- "RP 1"
  Mencao_20181[Mencao_20181=="II"] <- "RP 1"
  Mencao_20181[Mencao_20181=="MI"] <- "RP 1"
  Mencao_20181[Mencao_20181=="TR"] <- "RP 1"
  Mencao_20181[Mencao_20181=="TJ"] <- "RP 1"
  Mencao_20181[Mencao_20181=="DP"] <- "RP 1"
  Mencao_20181[Mencao_20181=="RP"] <- "RP 1"
})

HE20182 <- within(HE20182, {
  Mencao_20182[Mencao_20182=="AP"] <- "AP 2"
  Mencao_20182[Mencao_20182=="CC"] <- "AP 2"
  Mencao_20182[Mencao_20182=="MM"] <- "AP 2"
  Mencao_20182[Mencao_20182=="MS"] <- "AP 2"
  Mencao_20182[Mencao_20182=="SS"] <- "AP 2"
  
  Mencao_20182[Mencao_20182=="SR"] <- "RP 2"
  Mencao_20182[Mencao_20182=="II"] <- "RP 2"
  Mencao_20182[Mencao_20182=="MI"] <- "RP 2"
  Mencao_20182[Mencao_20182=="TR"] <- "RP 2"
  Mencao_20182[Mencao_20182=="TJ"] <- "RP 2"
  Mencao_20182[Mencao_20182=="DP"] <- "RP 2"
  Mencao_20182[Mencao_20182=="RP"] <- "RP 2"
})

AA1 <- Doc %>% filter(is.na(Trancados_1) & ((`Ano Ingresso Opcao`<anobase | (`Ano Ingresso Opcao`==anobase & `Semestre Ingresso Opcao`<=1)) & `Ano Saida Opcao`>=anobase)) %>% group_by(`Nome Curso`, Sexo) %>% summarise(n=n())
md <- melt(AA1, id=c("Nome Curso", "Sexo"))
AA1 <- dcast(md, `Nome Curso` ~ Sexo)
names(AA1)[2:3] <- c("Feminino A1", "Masculino A1")

AT1 <- Doc %>% filter(Trancados_1==1 & ((`Ano Ingresso Opcao`<anobase | (`Ano Ingresso Opcao`==anobase & `Semestre Ingresso Opcao`<=1)) & `Ano Saida Opcao`>=anobase)) %>% group_by(`Nome Curso`, Sexo) %>% summarise(n=n())
md <- melt(AT1, id=c("Nome Curso", "Sexo"))
AT1 <- dcast(md, `Nome Curso` ~ Sexo)
names(AT1)[2:3] <- c("Feminino T1", "Masculino T1")

AA2 <- Doc %>% filter(is.na(Trancados_2) & (`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2)))) %>% group_by(`Nome Curso`, Sexo) %>% summarise(n=n())
md <- melt(AA2, id=c("Nome Curso", "Sexo"))
AA2 <- dcast(md, `Nome Curso` ~ Sexo)
names(AA2)[2:3] <- c("Feminino A2", "Masculino A2")

AT2 <- Doc %>% filter(Trancados_2==1 & (`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2)))) %>% group_by(`Nome Curso`, Sexo) %>% summarise(n=n())
md <- melt(AT2, id=c("Nome Curso", "Sexo"))
AT2 <- dcast(md, `Nome Curso` ~ Sexo)
names(AT2)[2:3] <- c("Feminino T2", "Masculino T2")

Tabela3.28 <- left_join(Nomes,AA1) %>% left_join(AA2) %>% left_join(AT1) %>% left_join(AT2) 
Tabela3.28[is.na(Tabela3.28)] <- 0


totais <- Tabela3.28 %>% group_by(Unidade) %>% summarise(`Feminino A1`=sum(`Feminino A1`), `Masculino A1`=sum(`Masculino A1`),
                                                         `Feminino A2`=sum(`Feminino A2`), `Masculino A2`=sum(`Masculino A2`),
                                                         `Feminino T1`=sum(`Feminino T1`), `Masculino T1`=sum(`Masculino T1`),
                                                         `Feminino T2`=sum(`Feminino T2`), `Masculino T2`=sum(`Masculino T2`))
totais <- totais %>% left_join(Label_Unidades)

Tabela3.28$Unidade <- sapply(Tabela3.28$Unidade, function(x) paste(x, "-", collapse=""))

Tabela3.28 <- bind_rows(Tabela3.28, totais) %>% arrange(Unidade, `Nome Curso`)

row.names(Tabela3.28) <- Tabela3.28$`Nome Curso`
Tabela3.28$`Nome Curso` <- NULL
Tabela3.28$Unidade <- NULL

Tabela3.28$`Total 1AS` <- rowSums(Tabela3.28[,c("Feminino A1", "Masculino A1")])
Tabela3.28$`Total 2AS` <- rowSums(Tabela3.28[,c("Feminino A2", "Masculino A2")])
Tabela3.28$`Total 1TS` <- rowSums(Tabela3.28[,c("Feminino T1", "Masculino T1")])
Tabela3.28$`Total 2TS` <- rowSums(Tabela3.28[,c("Feminino T2", "Masculino T2")])
Tabela3.28$`Total 1F` <- rowSums(Tabela3.28[,c("Feminino A1", "Feminino T1")])
Tabela3.28$`Total 1M` <- rowSums(Tabela3.28[,c("Masculino A1", "Masculino T1")])
Tabela3.28$`Total 1T` <- rowSums(Tabela3.28[,c("Total 1F", "Total 1M")])
Tabela3.28$`Total 2F` <- rowSums(Tabela3.28[,c("Feminino A2", "Feminino T2")])
Tabela3.28$`Total 2M` <- rowSums(Tabela3.28[,c("Masculino A2", "Masculino T2")])
Tabela3.28$`Total 2T` <- rowSums(Tabela3.28[,c("Total 2F", "Total 2M")])

Tabela3.28 <- Tabela3.28 %>% select(`Feminino A1`, `Masculino A1`, `Total 1AS`, `Feminino A2`, `Masculino A2`, `Total 2AS`,
                                    `Feminino T1`, `Masculino T1`, `Total 1TS`, `Feminino T2`, `Masculino T2`, `Total 2TS`,
                                    `Total 1F`, `Total 1M`, `Total 1T`, `Total 2F`, `Total 2M`, `Total 2T`)

Total_Geral <- colSums(Tabela3.28, na.rm=T)/2
Tabela3.28 <- rbind(Tabela3.28, Total_Geral) 
row.names(Tabela3.28)[98] <- "Total Geral"

rm(HE20181, HE20182, md, totais, AA1, AA2, AT1, AT2)


# Tabela 3.29 Forma de Saída ----------------------------------------------

M2 <- Doc %>% filter(`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>% group_by(`Nome Curso`) %>% summarise(n=n())

D1 <- Doc %>% filter((`Ano Ingresso Opcao`< anobase | (`Ano Ingresso Opcao`==anobase & `Semestre Ingresso Opcao`<=1))&`Ano Saida Opcao`>=anobase) %>% filter(`For. Saida Opcao`!="Está Cursando" & `For. Saida Opcao`!="Formatura Pos-Graduacao" & `For. Saida Opcao`!="Formatura com Especializacao")

D1 <- D1 %>% mutate(`For. Saida Opcao` = ifelse(`For. Saida Opcao` == "Formatura" | `For. Saida Opcao` == "Formatura Anterior a 1/88", "Formado-Graduação",
                                                ifelse(`For. Saida Opcao` == "Deslig - Não Cumpriu condicao" | `For. Saida Opcao` == "Desligamento Rendimento Academico" | `For. Saida Opcao` == "Rep 3 vezes na mesma Disc Obrig" | `For. Saida Opcao` == "Desligamento Jubilamento", "Desligado-Falta de Rendimento 1",
                                                       ifelse(`For. Saida Opcao` == "Desligamento Voluntario", "Desligamento Voluntário 1",
                                                              ifelse(`For. Saida Opcao`== "Desligamento - Abandono", "Abandono 1", "Outros 1")))))



D1 <- D1 %>% group_by(`Nome Curso`, `For. Saida Opcao`) %>% summarise(n=n())
md <- melt(D1, id=c("Nome Curso", "For. Saida Opcao"))
D1 <- dcast(md, `Nome Curso` ~ `For. Saida Opcao`)

D2 <- Doc %>% filter(`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>% filter(`For. Saida Opcao`!="Está Cursando" & `For. Saida Opcao`!="Formatura Pos-Graduacao" & `For. Saida Opcao`!="Formatura com Especializacao")

D2 <- D2 %>% mutate(`For. Saida Opcao` = ifelse(`For. Saida Opcao` == "Formatura" | `For. Saida Opcao` == "Formatura Anterior a 1/88", "Formado-Graduação 2",
                                                ifelse(`For. Saida Opcao` == "Deslig - Não Cumpriu condicao" | `For. Saida Opcao` == "Desligamento Rendimento Academico" | `For. Saida Opcao` == "Rep 3 vezes na mesma Disc Obrig" | `For. Saida Opcao` == "Desligamento Jubilamento", "Desligado-Falta de Rendimento 2",
                                                       ifelse(`For. Saida Opcao` == "Desligamento Voluntario", "Desligamento Voluntário 2",
                                                              ifelse(`For. Saida Opcao`== "Desligamento - Abandono", "Abandono 2", "Outros 2")))))


D2 <- D2 %>% group_by(`Nome Curso`, `For. Saida Opcao`) %>% summarise(n=n())
md <- melt(D2, id=c("Nome Curso", "For. Saida Opcao"))
D2 <- dcast(md, `Nome Curso` ~ `For. Saida Opcao`)


Nomes_2 <- Doc %>% group_by(`Nome Curso`, Unidade) %>% summarise(ss=n()); Nomes$ss <- NULL

Tabela3.29 <- left_join(M2,D1) 
Tabela3.29 <- left_join(Tabela3.29, D2, by = "Nome Curso")
Tabela3.29 <- left_join(Tabela3.29, Nomes_2, by = "Nome Curso")

Tabela3.29 <- Tabela3.29 %>% select(1,11,2,3,7,4,8,5,9,6,10,-12)
colnames(Tabela3.29)[3] <- "Regulares Registrados no (2º Semestre)"
Tabela3.29[is.na(Tabela3.29)] <- 0

Tabela3.29$`Total 1S` <- rowSums(Tabela3.29[,c(4,6,8,10)])
Tabela3.29$`Total 2S` <- rowSums(Tabela3.29[,c(5,7,9,11)])

totais <- Tabela3.29 %>% group_by(Unidade) %>% select(-"Nome Curso") %>%  summarize_all(sum, na.rm = TRUE) %>% left_join(Label_Unidades, by = "Unidade") 
Tabela3.29$Unidade <- sapply(Tabela3.29$Unidade, function(x) paste(x, "-", collapse=""))

Tabela3.29 <- bind_rows(Tabela3.29, totais) 
Tabela3.29 <- Tabela3.29 %>% arrange(Unidade,`Nome Curso`) %>% select(-"Unidade")

row.names(Tabela3.29) <- Tabela3.29$`Nome Curso`
Tabela3.29$`Nome Curso` <- NULL


Total_Geral <- colSums(Tabela3.29, na.rm=T)/2

Tabela3.29 <- rbind(Tabela3.29, Total_Geral)
row.names(Tabela3.29)[98] <- "Total Geral"

Tabela3.29[Tabela3.29==0] <- "-"

rm(D1, D2, M2, md, Nomes_2, totais)

# Tabela 3.30 Evolução Ingressantes -------------------------------------------

Tabela3.30 <- read.csv2("EvoIngD.csv", check.names=F) #Peguei esse csv do anuário passado
Ing2018 <- Doc %>% filter(`Ano Ingresso Opcao`== anobase) %>% group_by(`Nome Curso`, Unidade) %>% summarise(`2018`=n())


totais <- Ing2018 %>% group_by(Unidade) %>% summarise(`2018`=sum(`2018`))

totais <- totais %>% left_join(Label_Unidades)

Ing2018$Unidade <- sapply(Ing2018$Unidade, function(x) paste(x, "-", collapse=""))
Ing2018 <- bind_rows(Ing2018,totais) %>% arrange(Unidade, `Nome Curso`)
Ing2018$Unidade <- NULL
colnames(Ing2018)[1] <- "Unidade Acadêmica/Curso"



Tabela3.30 <- left_join(Tabela3.30, Ing2018)
Tabela3.30[is.na(Tabela3.30)] <- 0
row.names(Tabela3.30) <- Tabela3.30$`Unidade Acadêmica/Curso`
Tabela3.30 <- Tabela3.30 %>% select(-`Unidade Acadêmica/Curso`) 


Tabela3.30[98,5] <- sum(as.integer(Tabela3.30$`2018`))/2
Tabela3.30[98,5] <- round(Tabela3.30[98,5])

rm(Ing2018, totais, Total_Geral, "Total Geral")

# Tabela 3.31 Evolução Alunos Registrados ----------------------------------------

Tabela3.31 <- read.csv2("EvoRegD.csv", check.names=F) #Peguei esse csv do anuário passado
Reg2018 <- Doc %>% filter(`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>% group_by(`Nome Curso`, Unidade) %>% summarise(`2018`=n())


totais <- Reg2018 %>% group_by(Unidade) %>% summarise(`2018`=sum(`2018`))

totais <- totais %>% left_join(Label_Unidades)

Reg2018$Unidade <- sapply(Reg2018$Unidade, function(x) paste(x, "-", collapse=""))
Reg2018 <- bind_rows(Reg2018,totais) %>% arrange(Unidade, `Nome Curso`)
Reg2018$Unidade <- NULL
colnames(Reg2018)[1] <- "Unidade Acadêmica/Curso"

Tabela3.31 <- left_join(Tabela3.31, Reg2018)
Tabela3.31[is.na(Tabela3.31)] <- 0
row.names(Tabela3.31) <- Tabela3.31$`Unidade Acadêmica/Curso`
Tabela3.31 <- Tabela3.31 %>% select(-`Unidade Acadêmica/Curso`) 

Tabela3.31[102,5] <- sum(Tabela3.31$`2018`)/2
Tabela3.31[102,5] <- ceiling(Tabela3.31[102,5])

rm(Reg2018, totais)

# Tabela 3.32 Evolução Formados ----------------------------------------------------------------------

Tabela3.32 <- read.csv2("EvoForD.csv", check.names=F)
For2018 <- Doc %>% filter(`Ano Saida Opcao`==anobase & (`For. Saida Opcao`=="Formatura Pos-Graduacao" | `For. Saida Opcao`=="Formatura com Especializacao")) %>% group_by(`Nome Curso`, Unidade) %>% summarise(`2018`=n())

totais <- For2018 %>% group_by(Unidade) %>% summarise(`2018`=sum(`2018`))

totais <- totais %>% left_join(Label_Unidades)

For2018$Unidade <- sapply(For2018$Unidade, function(x) paste(x, "-", collapse=""))
For2018 <- bind_rows(For2018,totais) %>% arrange(Unidade, `Nome Curso`)
For2018$Unidade <- NULL
colnames(For2018)[1] <- "Unidade Acadêmica/Curso"

Tabela3.32 <- left_join(Tabela3.32, For2018)
Tabela3.32[is.na(Tabela3.32)] <- 0
row.names(Tabela3.32) <- Tabela3.32$`Unidade Acadêmica/Curso`
Tabela3.32 <- Tabela3.32 %>% select(-`Unidade Acadêmica/Curso`) 

Tabela3.32[94,5] <- sum(Tabela3.32$`2018`)/2
Tabela3.32[94,5] <- round(Tabela3.32[94,5])

rm(For2018, totais)

# Tabela 3.06 Mestrado e Doutorado por Sexo -------------------------------------------------------------

D2 <- Doc %>% filter(`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>% 
  group_by(`Nome Curso`, Sexo, Unidade) %>% 
  summarise(n=n())
md <- melt(D2, id=c("Nome Curso", "Sexo","Unidade","n"))
D2 <- dcast(md, `Nome Curso` + Unidade~ `Sexo`)
names(D2)[2:4] <- c("Unidade","Feminino D", "Masculino D")

D2$`Total D` <- rowSums(D2[,c(3,4)], na.rm = TRUE)

Mest <- Completo %>% filter(`Ano Ingresso Opcao`<=anobase & `Ano Saida Opcao`>=anobase & `Nivel Curso`=="Mestrado" & `Nome Curso`!="Agronegócios Multiinstitucional" & `Nome Curso`!="Ciências Agrárias") 

M2 <- Mest %>% filter(`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>% 
  group_by(`Nome Curso`, Sexo, Unidade) %>% summarise(n=n())
md <- melt(M2, id=c("Nome Curso", "Sexo", "Unidade","n"))
M2 <- dcast(md, `Nome Curso` + Unidade ~ `Sexo`)
names(M2)[3:4] <- c("Feminino M", "Masculino M")

M2$`Total M` <- rowSums(M2[,c(3,4)], na.rm = TRUE)

Tabela3.06 <- full_join(M2, D2, by = c("Nome Curso","Unidade"))
Tabela3.06[is.na(Tabela3.06)] <- 0

Tabela3.06$Total <- rowSums(Tabela3.06[,c(5,8)], na.rm = TRUE)

totais <- Tabela3.06 %>% group_by(Unidade) %>% select(-"Nome Curso") %>%  summarize_all(sum, na.rm = TRUE) %>% left_join(Label_Unidades, by = "Unidade")

Tabela3.06$Unidade <- sapply(Tabela3.06$Unidade, function(x) paste(x, "-", collapse=""))

Tabela3.06 <-  bind_rows(Tabela3.06, totais) %>% arrange(Unidade, `Nome Curso`)

row.names(Tabela3.06) <- Tabela3.06$`Nome Curso`
Tabela3.06 <- Tabela3.06 %>% select(-`Nome Curso`, -`Unidade`)

Total_Geral <- colSums(Tabela3.06, na.rm=T)/2

Tabela3.06 <- rbind(Tabela3.06, Total_Geral)
row.names(Tabela3.06)[122] <- "Total Geral"

Tabela3.06.2 <- totais %>% arrange(desc(Total)) %>% select(1,8)

row.names(Tabela3.06.2) <- Tabela3.06.2$Unidade
Tabela3.06.2 <- Tabela3.06.2 %>% select(-`Unidade`)


rm(D2, M2, md, totais, Total_Geral)

### exportei apenas esta tabela pois estava errada
### falta formatá-la para o arquivo final
rio::export(list(Tabela3.06, Tabela3.06.2), "Tabela3.06.xlsx", row.names = TRUE)

# Tabela 3.07 Mestrado e Doutorado por Curso ------------------------------

Dout <- Doc %>% filter(`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>% 
  group_by(Unidade) %>% summarise(Cursos_D=n_distinct(`Nome Curso`), Alunos_D=n())


Mestr <- Mest %>% filter(`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>% 
  group_by(Unidade) %>% summarise(Cursos_M=n_distinct(`Nome Curso`), Alunos_M=n())

Tabela3.07 <- left_join(Mestr, Dout, by = "Unidade")
Tabela3.07[is.na(Tabela3.07)] <- 0

Tabela3.07$`Total Alunos` <- rowSums(Tabela3.07[,c(3,5)])

Tabela3.07 <- left_join(Tabela3.07, Label_Unidades)

row.names(Tabela3.07) <- Tabela3.07$`Nome Curso`
Tabela3.07 <- Tabela3.07 %>% select(-`Nome Curso`, -`Unidade`)

Total_Geral <- round(as.numeric(colSums(Tabela3.07, na.rm=T)),0)

Tabela3.07 <- rbind(Tabela3.07, Total_Geral)
row.names(Tabela3.07)[31] <- "Total Geral"

rm(Doc, Dout, Mest, Mestr, Total_Geral)

# Excel -------------------------------------------------------------------

# REVER TODA ESSA PARTE
# ACHO QUE NÃO PRECISAMOS MAIS

library(openxlsx)
library(xlsx)

wb <- createWorkbook()

#Tabela 3.23
sheet <- createSheet(wb, sheetName="Tabela 3.23")
addDataFrame(Tabela3.23, sheet, row.names = T, startRow=6, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 3, 1, create=TRUE) 
x <- c("Tabela 3.23 - Ingresso de Alunos e Número de Teses Homologadas nos Cursos de Doutorado, por Unidade Acadêmica e Curso, UnB, 2017")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE)
x <- c("Unidade Acadêmica/Curso", "Ingresso de Alunos", rep(NA,6), "Teses Homologadas")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("1° Sem.", NA,NA,  "2° Sem.", NA,NA, "Total Ingressantes", "1° Sem.", NA,NA,  "2° Sem.", NA,NA, "Total Formados")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 6, 5, 1, 60, create=TRUE)
x <- c("Fem.", "Masc.", "Total", "Fem.", "Masc.", "Total", NA, "Fem.", "Masc.", "Total")
CB.setRowData(cb, x, 1, F)
addDataFrame(Tabela3.23.2, sheet, row.names = T, startRow=6, startColumn = 21)


#Tabela 3.24
sheet <- createSheet(wb, sheetName="Tabela 3.24")
addDataFrame(Tabela3.24, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.24 - Alunos Ingressantes e Concluintes nos Cursos de Doutorado, por Sexo e Faixa Etária, UnB, 2017")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Faixa Etária", "Ingressantes", rep(NA,4), "Concluintes")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("Fem.","%", "Masc.","%","Total", "Fem.","%", "Masc.","%","Total")
CB.setRowData(cb, x, 1, F)


#Tabela 3.25
sheet <- createSheet(wb, sheetName="Tabela 3.25")
addDataFrame(Tabela3.25, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.25 - Alunos Regulares Ativos, Matrículas e Aprovações em Disciplinas* nos Cursos de Doutorado, por Unidade Acadêmica e Curso, UnB, 2017")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Unidade Acadêmica/Curso", NA, "Alunos Ativos", NA, "Aprovado", NA, "Reprovado", NA, "% Aprovado/Matriculado", NA, "% Reprovado/Matriculado", NA)
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("1° Sem.","2° Sem.", "1° Sem.", "2° Sem.","1° Sem.", "2° Sem.","1° Sem.", "2° Sem.")
CB.setRowData(cb, x, 1, F)


#Tabela 3.26
sheet <- createSheet(wb, sheetName="Tabela 3.26")
addDataFrame(Tabela3.26, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.26 - Alunos Regulares Registrados nos Cursos de Doutorado, por Sexo e Faixa Etária, UnB, 2º/2017")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 4, 1, 60, create=TRUE) 
x <- c("Faixa Etária", "Fem.", "%", "Masc.", "%", "Total")
CB.setRowData(cb, x, 1, F)


#Tabela 3.27
sheet <- createSheet(wb, sheetName="Tabela 3.27")
addDataFrame(Tabela3.27, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 3, 1, create=TRUE) 
x <- c("Tabela 3.27 - Alunos Regulares Registrados nos Cursos de Doutorado, por Semestre, Unidade Acadêmica, Curso e Sexo, UnB, 2017")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE)
x <- c("Unidade Acadêmica/Curso", "Alunos Regulares Registrados", "1° Sem.", NA,NA,  "2° Sem.")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("Fem.", "Masc.", "Total", "Fem.", "Masc.", "Total")
CB.setRowData(cb, x, 1, F)
addDataFrame(Tabela3.27.2, sheet, row.names = T, startRow=6, startColumn = 13)


#Tabela 3.28
sheet <- createSheet(wb, sheetName="Tabela 3.28")
addDataFrame(Tabela3.28, sheet, row.names = T, startRow=6, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 3, 1, create=TRUE) 
x <- c("Tabela 3.28 - Alunos Regulares Registrados Ativos e com Trancamento Geral de Matrícula nos Cursos de Doutorado, por Semestre, Sexo, Unidade Acadêmica e Curso, UnB, 2017")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE)
x <- c("Unidade Acadêmica/Curso", "Ativos", rep(NA,5), "Com Trancamento Geral de Matrícula", rep(NA,5), "Total")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("1° Sem.",NA,NA, "2° Sem.",NA,NA, "1° Sem.",NA,NA, "2° Sem.",NA,NA, "1° Sem.",NA,NA, "2° Sem.")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 6, 5, 1, 60, create=TRUE)
x <- c("Fem.","Masc.","Total", "Fem.","Masc.","Total", "Fem.","Masc.","Total",
       "Fem.","Masc.","Total", "Fem.","Masc.","Total", "Fem.","Masc.", "Total")
CB.setRowData(cb, x, 1, F)


#Tabela 3.29
sheet <- createSheet(wb, sheetName="Tabela 3.29")
addDataFrame(Tabela3.29, sheet, row.names = T, startRow=6, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 3, 1, create=TRUE) 
x <- c("Tabela 3.29 - Desligamento de Alunos nos Cursos de Doutorado, por Forma, Semestre, Unidade Acadêmica e Curso, UnB, 2017")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE)
x <- c("Unidade Acadêmica/Curso", "Regulares Registrados (2º Semestre)", "Forma de Desligamento")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 5, 6, 1, 60, create=TRUE)
x <- c("Abandono",NA, "Desl. Voluntário",NA, "Falta de Rendimento.",NA, "Outros",NA, "Total")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 6, 5, 1, 60, create=TRUE)
x <- c(NA,"1° Sem.","2° Sem.", "1° Sem.", "2° Sem.","1° Sem.", "2° Sem.","1° Sem.", "2° Sem.","1° Sem.", "2° Sem.")
CB.setRowData(cb, x, 1, F)


#Tabela 3.30
sheet <- createSheet(wb, sheetName="Tabela 3.30")
addDataFrame(Tabela3.30, sheet, row.names = T, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.30 - Evolução do Ingresso de Alunos nos Cursos de Doutorado, por Unidade Acadêmica e Curso, UnB, 2011 a 2017")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 1, create=TRUE) 
x <- c("Unidade Acadêmica/Curso")
CB.setRowData(cb, x, 1, F)


#Tabela 3.31
sheet <- createSheet(wb, sheetName="Tabela 3.31")
addDataFrame(Tabela3.31, sheet, row.names = T, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.31 - Evolução do Número de Alunos Registrados* nos Cursos de Doutorado, por Unidade Acadêmica e Curso, UnB, 2011 a 2017")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 1, create=TRUE) 
x <- c("Unidade Acadêmica/Curso")
CB.setRowData(cb, x, 1, F)


#Tabela 3.32
sheet <- createSheet(wb, sheetName="Tabela 3.32")
addDataFrame(Tabela3.32, sheet, row.names = T, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.32 - Evolução do Número de Alunos com Teses Homologadas nos Cursos de Doutorado, por Unidade Acadêmica e Curso, UnB, 2011 a 2017")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 1, create=TRUE) 
x <- c("Unidade Acadêmica/Curso")
CB.setRowData(cb, x, 1, F)


#Tabela 3.06
sheet <- createSheet(wb, sheetName="Tabela 3.06")
addDataFrame(Tabela3.06, sheet, row.names = T, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.06 - Número de Alunos de Mestrado e Doutorado por Sexo")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 1, create=TRUE) 
x <- c("Unidade Acadêmica/Curso")
CB.setRowData(cb, x, 1, F)
addDataFrame(Tabela3.06.2, sheet, row.names = F, startRow=6, startColumn = 14)


#Tabela 3.07
sheet <- createSheet(wb, sheetName="Tabela 3.07")
addDataFrame(Tabela3.07, sheet, row.names = F, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.07 - Número de Cursos e Alunos de Mestrado e Doutorado")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 5, create=TRUE) 
x <- c("Unidade Acadêmica/Curso", "Mestrado", NA,NA, "Doutorado")
CB.setRowData(cb, x, 1, F)


saveWorkbook(wb, "dados_doutorado/Anuário Doutorado 2 UnB 2019.xlsx") # and of course you need to save it.

rm(wb,cb,sheet,x)