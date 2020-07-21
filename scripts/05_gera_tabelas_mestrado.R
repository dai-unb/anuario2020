# Setup -------------------------------------------------------------------

options(OutDec = ",")

library(tidyverse)

load("dados_identificados/Completo14042020.RData")
load("labels/Label_Unidades_pos.RData")

### define ano do CENSO
anobase <- 2019

Mest <- Completo %>% 
  filter(`Ano Ingresso Opcao`<= anobase,
         `Ano Saida Opcao`>= anobase,
         `Nivel Curso`=="Mestrado",
         `Forma Saida` != "Anulacao de Registro")

### tabela de referência dos cursos e unidades ativos
Nomes <- Mest %>% 
  count(`Nome Curso`, Unidade) %>% 
  select(-n)

# Tabela 3.08 Estrangeiros (Mestrado e Doutorado) -------------------------

Tabela3.08 <- Mest %>% filter(Paises!="Brasil" & !is.na(Paises))

Tabela3.08.2 <- Tabela3.08 %>% group_by(Paises) %>% summarise(n=n()) %>% arrange(desc(n))
Tabela3.08.2 <- Tabela3.08.2[1:10,]; Tabela3.08.2 <- as.data.frame(Tabela3.08.2, order=F)

Tabela3.08.3 <- Tabela3.08 %>% group_by(Continente) %>% summarise(n=n())
Tabela3.08.4 <- Tabela3.08 %>% group_by(Unidade) %>% summarise(n=n()) %>% arrange(desc(n))

Tabela3.08 <- Tabela3.08 %>% 
  group_by(Unidade, Continente, Paises) %>% 
  tally() %>% 
  spread(Unidade, n)

totais <- Tabela3.08 %>% 
  group_by(Continente) %>%
  select(-Paises) %>% 
  summarise_all(sum, na.rm=T)

totais$Paises <- c("Total do Continente: África", "Total do Continente: América", "Total do Continente: Ásia",
                   "Total do Continente: Europa")

totais$Continente <- sapply(totais$Continente, function(x) paste(x, "-", collapse=""))

Tabela3.08 <- bind_rows(Tabela3.08,totais) %>% arrange(Continente, Paises)
row.names(Tabela3.08) <- Tabela3.08$Paises; Tabela3.08$Paises <- NULL; Tabela3.08$Continente <- NULL

Tabela3.08$Total <- rowSums(Tabela3.08, na.rm=T)
Tabela3.08["Total Geral", ] <- colSums(Tabela3.08, na.rm=T)/2
Tabela3.08[is.na(Tabela3.08)] <- 0

rm(totais)

# Tabela 3.12 Ingressantes e Formados -------------------------------------------------------------

Ing1 <- Mest %>% 
  filter(`Ano Ingresso Opcao`==anobase & `Semestre Ingresso Opcao`==1) %>% 
  group_by(`Nome Curso`, Sexo) %>% 
  summarise(`Ingressantes 1`=n()) %>% 
  spread(Sexo, `Ingressantes 1`) %>% 
  janitor::adorn_totals("col")

names(Ing1) <- c("Nome Curso", paste0("Ing1_", names(Ing1[-1])))

Ing2 <- Mest %>% 
  filter(`Ano Ingresso Opcao`==anobase & `Semestre Ingresso Opcao`==2) %>% 
  group_by(`Nome Curso`, Sexo) %>%
  summarise(`Ingressantes 2`=n()) %>% 
  spread(Sexo, `Ingressantes 2`) %>% 
  janitor::adorn_totals("col")

names(Ing2) <- c("Nome Curso", paste0("Ing2_", names(Ing2[-1])))

For1 <- Mest %>% 
  filter(`Ano Saida Opcao`==anobase,
         `Semestre Saida Opcao`<=1,
         str_detect(`For. Saida Opcao`, "Formatura")) %>%
  group_by(`Nome Curso`, Sexo) %>% 
  summarise(`Formados 1`=n()) %>% 
  spread(Sexo, `Formados 1`) %>% 
  janitor::adorn_totals("col")

names(For1) <- c("Nome Curso", paste0("For1_", names(For1[-1])))

For2 <- Mest %>% 
  filter(`Ano Saida Opcao`==anobase,
         `Semestre Saida Opcao`==2,
         str_detect(`For. Saida Opcao`, "Formatura")) %>%
  group_by(`Nome Curso`, Sexo) %>% 
  summarise(`Formados 2`=n()) %>% 
  spread(Sexo, `Formados 2`) %>% 
  janitor::adorn_totals("col")

names(For2) <- c("Nome Curso", paste0("For2_", names(For2[-1])))

Tabela3.12 <- left_join(Nomes, Ing1, by = "Nome Curso") %>% 
  left_join(Ing2, by = "Nome Curso") %>% 
  left_join(For1, by = "Nome Curso") %>% 
  left_join(For2, by = "Nome Curso")

totais <- Tabela3.12 %>% 
  group_by(Unidade) %>% 
  select(-`Nome Curso`) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela3.12$Unidade <- sapply(Tabela3.12$Unidade, function(x) paste(x, "-", collapse=""))
Tabela3.12 <- bind_rows(Tabela3.12, totais) %>% arrange(Unidade, `Nome Curso`)
row.names(Tabela3.12) <- Tabela3.12$`Nome Curso`; Tabela3.12$`Nome Curso` <- NULL; Tabela3.12$Unidade <- NULL

Tabela3.12$`Total Ing` <- rowSums(Tabela3.12[,c("Ing1_Total", "Ing2_Total")], na.rm = TRUE)
Tabela3.12$`Total For` <- rowSums(Tabela3.12[,c("For1_Total", "For2_Total")], na.rm = TRUE)

Tabela3.12 <- Tabela3.12 %>% 
  ungroup() %>% 
  select(Ing1_Feminino, Ing1_Masculino, Ing1_Total,
         Ing2_Feminino, Ing2_Masculino, Ing2_Total, `Total Ing`,
         For1_Feminino, For1_Masculino, For1_Total,
         For2_Feminino, For2_Masculino, For2_Total, `Total For`)

Tabela3.12["Total Geral",] <- colSums(Tabela3.12, na.rm=T)/2
Tabela3.12[is.na(Tabela3.12)] <- 0

### tabela de ingressantes e concluintes consolidados
Ing <- Mest %>% filter(`Ano Ingresso Opcao`==anobase) %>% group_by(Unidade) %>% summarise(Ingressantes=n()) %>% arrange(desc(Ingressantes))
For <- Mest %>% filter(`Ano Saida Opcao`==anobase & (`For. Saida Opcao`=="Formatura Pos-Graduacao" | 
                                                       `For. Saida Opcao`=="Formatura com Especializacao")) %>% 
  group_by(Unidade) %>% summarise(Concluintes=n())
Tabela3.12.2 <- full_join(Ing, For)

rm(Ing, Ing1, Ing2, For, For1, For2, totais)

# Tabela 3.13 Ingressantes e concluintes por sexo e faixa etária -------------------------------------------------------------

Ing <- Mest %>% filter(`Ano Ingresso Opcao`==anobase)

### atentar para a data escolhida
### deve ser o início do ano base do CENSO
Ing$Idade <- difftime(as.Date("2018-01-01"), Ing$Nascimento)/365

### verifica a consistência da idade
Ing %>% arrange(Idade) %>% head(10) %>% select(Nome, Idade)

Ing$`Faixa Etária`[Ing$Idade<=24] <- "Até 24 anos"
Ing$`Faixa Etária`[Ing$Idade>24 & Ing$Idade<=29] <- "De 25 a 29 anos"
Ing$`Faixa Etária`[Ing$Idade>29 & Ing$Idade<=34] <- "De 30 a 34 anos"
Ing$`Faixa Etária`[Ing$Idade>34 & Ing$Idade<=39] <- "De 35 a 39 anos"
Ing$`Faixa Etária`[Ing$Idade>39 & Ing$Idade<=44] <- "De 40 a 44 anos"
Ing$`Faixa Etária`[Ing$Idade>44] <- "De 45 anos ou mais"

Ing <- Ing %>% 
  group_by(`Faixa Etária`, Sexo) %>% 
  summarise(n=n()) %>% 
  spread(Sexo, n) %>% 
  janitor::adorn_totals(c("row", "col"))

Ing$`% Feminino` <- paste(round(Ing$Feminino/Ing$Total*100,1), "%", sep="")
Ing$`% Masculino` <- paste(round(Ing$Masculino/Ing$Total*100,1), "%", sep="")

rownames(Ing) <- Ing$`Faixa Etária`

Ing <- Ing %>% select(Feminino, `% Feminino`, Masculino, `% Masculino`, Total)
For <- Mest %>% filter(`Ano Saida Opcao`==anobase & str_detect(`For. Saida Opcao`, "Formatura"))

### atentar para a data escolhida
### deve ser o início do ano base do CENSO
For$Idade <- difftime(as.Date("2018-01-01"), For$Nascimento)/365

### verifica idade
For %>% arrange(Idade) %>% head(10) %>% select(Nome, Idade)

For$`Faixa Etária`[For$Idade<=24] <- "Até 24 anos"
For$`Faixa Etária`[For$Idade>24 & For$Idade<=29] <- "De 25 a 29 anos"
For$`Faixa Etária`[For$Idade>29 & For$Idade<=34] <- "De 30 a 34 anos"
For$`Faixa Etária`[For$Idade>34 & For$Idade<=39] <- "De 35 a 39 anos"
For$`Faixa Etária`[For$Idade>39 & For$Idade<=44] <- "De 40 a 44 anos"
For$`Faixa Etária`[For$Idade>44] <- "De 45 anos ou mais"

For <- For %>% 
  group_by(`Faixa Etária`, Sexo) %>% 
  summarise(n=n()) %>% 
  spread(Sexo, n) %>% 
  janitor::adorn_totals(c("row", "col"))

For$`% Feminino` <- paste(round(For$Feminino/For$Total*100,1), "%", sep="")
For$`% Masculino` <- paste(round(For$Masculino/For$Total*100,1), "%", sep="")

rownames(For) <- For$`Faixa Etária`

For <- For %>% select(Feminino, `% Feminino`, Masculino, `% Masculino`, Total)

Tabela3.13 <- cbind(Ing,For); rm(Ing,For)

rm(Ing, For)

# Tabela 3.14 Alunos Regulares Ativos, Matrículas e Aprovados em Disc --------

# MUDAR NOME DAS VARIAVEIS PARA HE0 HE1 HE2

HE20180 <- read_fwf("dados_identificados/he20190.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                    col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20181", "Frequencias")), 
                    locale = locale(encoding = "latin1"))

HE20181 <- read_fwf("dados_identificados/he20191.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                    col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20181", "Frequencias")), 
                    locale = locale(encoding = "latin1"))

HE20182 <- read_fwf("dados_identificados/he20192.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                    col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20182", "Frequencias")), 
                    locale = locale(encoding = "latin1"))

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

t <- HE20182 %>% 
  group_by(MatricAluno) %>%
  mutate(dup = n())

M1 <- Mest %>% filter(is.na(Trancados_1),
                      ((`Ano Ingresso Opcao`< anobase | 
                          (`Ano Ingresso Opcao`== anobase & `Semestre Ingresso Opcao`<=1)) & `Ano Saida Opcao`>= anobase))

AA1 <- M1 %>% group_by(`Nome Curso`) %>% summarise(`Alunos Ativos 1`=n())

M1 <- inner_join(M1, HE20181, "MatricAluno")
MD1 <- M1 %>% group_by(`Nome Curso`) %>% summarise(`Matriculados em Disciplinas 1`=n())
AP1 <- M1 %>% 
  count(`Nome Curso`, Mencao_20181) %>% 
  spread(Mencao_20181, n, fill = 0)

M2 <- Mest %>% filter(is.na(Trancados_2),
                      (`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`> anobase | 
                                                           (`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`== 2))))

AA2 <- M2 %>% group_by(`Nome Curso`) %>% summarise(`Alunos Ativos 2`=n())

M2 <- inner_join(M2,HE20182, "MatricAluno")

MD2 <- M2 %>% group_by(`Nome Curso`) %>% summarise(`Matriculados em Disciplinas 2`=n())
AP2 <- M2 %>% 
  count(`Nome Curso`, Mencao_20182) %>% 
  spread(Mencao_20182, n, fill = 0)

Tabela3.14 <- left_join(Nomes, AA1) %>% 
  left_join(AA2) %>% 
  left_join(MD1) %>% 
  left_join(MD2) %>% 
  left_join(AP1) %>% 
  left_join(AP2)

totais <- Tabela3.14 %>% 
  group_by(Unidade) %>% 
  select(-`Nome Curso`) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela3.14$Unidade <- sapply(Tabela3.14$Unidade, function(x) paste(x, "-", collapse=""))
Tabela3.14 <- bind_rows(Tabela3.14,totais) %>% arrange(Unidade, `Nome Curso`)
row.names(Tabela3.14) <- Tabela3.14$`Nome Curso`; Tabela3.14$`Nome Curso` <- NULL; Tabela3.14$Unidade <- NULL

Tabela3.14 <- Tabela3.14 %>% 
  ungroup() %>% 
  select(`Alunos Ativos 1`, `Alunos Ativos 2`, `Matriculados em Disciplinas 1`, `Matriculados em Disciplinas 2`,
         `AP 1`, `AP 2`, `RP 1`, `RP 2`)

Tabela3.14["Total Geral", ] <- colSums(Tabela3.14, na.rm=T)/2
Tabela3.14[is.na(Tabela3.14)] <- 0

Tabela3.14$`RP 1` <- Tabela3.14$`AP 1`/(Tabela3.14$`RP 1` + Tabela3.14$`AP 1`)
Tabela3.14$`RP 1` <- paste(round(Tabela3.14$`RP 1`*100,1), "%", sep="")
Tabela3.14$`RP 2` <- Tabela3.14$`AP 2`/(Tabela3.14$`RP 2` + Tabela3.14$`AP 2`)
Tabela3.14$`RP 2` <- paste(round(Tabela3.14$`RP 2`*100,1), "%", sep="")

rm(totais, M1, AA1, MD1, AP1, M2, AA2, MD2, AP2)

# Tabela 3.15 Alunos Regulares por Sexo e Faixa Etária --------------------------------

Tabela3.15 <- Mest %>% filter(`Ano Ingresso Opcao`<= anobase,
                              (`Ano Saida Opcao`> anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2)))

# CONFIRMAR DATA DE CÁLCULO DA IDADE
Tabela3.15$Idade <- difftime(as.Date("2018-01-01"), Tabela3.15$Nascimento)/365

Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>18 & Tabela3.15$Idade<=24] <- "De 19 a 24 anos"
Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>24 & Tabela3.15$Idade<=29] <- "De 25 a 29 anos"
Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>29 & Tabela3.15$Idade<=34] <- "De 30 a 34 anos"
Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>34 & Tabela3.15$Idade<=39] <- "De 35 a 39 anos"
Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>39 & Tabela3.15$Idade<=44] <- "De 40 a 44 anos"
Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>44] <- "De 45 anos ou mais"

Tabela3.15 <- Tabela3.15 %>% 
  count(`Faixa Etária`, Sexo) %>% 
  spread(Sexo, n)

rownames(Tabela3.15) <- Tabela3.15$`Faixa Etária`
Tabela3.15$`Faixa Etária` <- NULL

Tabela3.15["Total", ] <- colSums(Tabela3.15, na.rm=T)
Tabela3.15$Total <- rowSums(Tabela3.15)
Tabela3.15$`% Feminino` <- paste(round(Tabela3.15$Feminino/Tabela3.15$Total*100,1), "%", sep="")
Tabela3.15$`% Masculino` <- paste(round(Tabela3.15$Masculino/Tabela3.15$Total*100,1), "%", sep="")

Tabela3.15 <- Tabela3.15 %>% select(Feminino, `% Feminino`, Masculino, `% Masculino`, Total)

# Tabela 3.16 Alunos Regulares Registrados por Sexo --------------------------------

M1 <- Mest %>% filter((`Ano Ingresso Opcao`< anobase | (`Ano Ingresso Opcao`== anobase & `Semestre Ingresso Opcao`<=1)),
                      `Ano Saida Opcao`>= anobase) %>% 
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n)

names(M1)[2:3] <- c("Feminino 1", "Masculino 1")

M2 <- Mest %>% filter(`Ano Ingresso Opcao`<= anobase,
                      (`Ano Saida Opcao`> anobase | (`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`==2))) %>% 
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n)

names(M2)[2:3] <- c("Feminino 2", "Masculino 2")

Tabela3.16 <- left_join(Nomes, M1) %>% 
  left_join(M2)

totais <- Tabela3.16 %>% 
  group_by(Unidade) %>% 
  select(-`Nome Curso`) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela3.16$Unidade <- sapply(Tabela3.16$Unidade, function(x) paste(x, "-", collapse=""))
Tabela3.16 <- bind_rows(Tabela3.16, totais) %>% arrange(Unidade, `Nome Curso`)
row.names(Tabela3.16) <- Tabela3.16$`Nome Curso`; Tabela3.16$`Nome Curso` <- NULL; Tabela3.16$Unidade <- NULL

Tabela3.16$`Total 1` <- rowSums(Tabela3.16[,c("Feminino 1", "Masculino 1")])
Tabela3.16$`Total 2` <- rowSums(Tabela3.16[,c("Feminino 2", "Masculino 2")])

Tabela3.16 <- Tabela3.16 %>% ungroup() %>% select(`Feminino 1`, `Masculino 1`, `Total 1`, `Feminino 2`, `Masculino 2`, `Total 2`)
Tabela3.16["Total Geral",] <- colSums(Tabela3.16, na.rm=T)/2

Tabela3.16.2 <- Mest %>% 
  filter(`Ano Ingresso Opcao` <= anobase & `Ano Saida Opcao` >= anobase) %>% 
  count(Unidade) %>% 
  arrange(desc(n))

rm(M1, M2, totais)

# Tabela 3.17 Alunos Ativos e Trancados por Sexo ----------------------------

AA1 <- Mest %>% filter(is.na(Trancados_1),
                       ((`Ano Ingresso Opcao`< anobase | (`Ano Ingresso Opcao`== anobase & 
                                                            `Semestre Ingresso Opcao`<=1)) & `Ano Saida Opcao`>= anobase)) %>%
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n, fill = 0)

names(AA1)[2:3] <- c("Feminino A1", "Masculino A1")

AT1 <- Mest %>% filter(Trancados_1 == 1,
                       ((`Ano Ingresso Opcao`< anobase | (`Ano Ingresso Opcao`== anobase & 
                                                            `Semestre Ingresso Opcao`<=1)) & `Ano Saida Opcao`>= anobase)) %>%
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n, fill = 0)

names(AT1)[2:3] <- c("Feminino T1", "Masculino T1")

AA2 <- Mest %>% filter(is.na(Trancados_2),
                       (`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`> anobase |
                                                            (`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`== 2)))) %>%
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n, fill = 0)

names(AA2)[2:3] <- c("Feminino A2", "Masculino A2")

AT2 <- Mest %>% filter(Trancados_2 == 1 & (`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`> anobase | (`Ano Saida Opcao`== anobase &
                                                                                                             `Semestre Saida Opcao`==2)))) %>%
  count(`Nome Curso`, Sexo) %>%
  spread(Sexo, n, fill = 0)

names(AT2)[2:3] <- c("Feminino T2", "Masculino T2")

Tabela3.17 <- left_join(Nomes, AA1) %>% 
  left_join(AA2) %>% 
  left_join(AT1) %>% 
  left_join(AT2)

totais <- Tabela3.17 %>% 
  group_by(Unidade) %>% 
  select(-`Nome Curso`) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela3.17$Unidade <- sapply(Tabela3.17$Unidade, function(x) paste(x, "-", collapse=""))
Tabela3.17 <- bind_rows(Tabela3.17, totais) %>% arrange(Unidade, `Nome Curso`)
row.names(Tabela3.17) <- Tabela3.17$`Nome Curso`; Tabela3.17$`Nome Curso` <- NULL; Tabela3.17$Unidade <- NULL

Tabela3.17[is.na(Tabela3.17)] <- 0

Tabela3.17$`Total 1AS` <- rowSums(Tabela3.17[,c("Feminino A1", "Masculino A1")])
Tabela3.17$`Total 2AS` <- rowSums(Tabela3.17[,c("Feminino A2", "Masculino A2")])
Tabela3.17$`Total 1TS` <- rowSums(Tabela3.17[,c("Feminino T1", "Masculino T1")])
Tabela3.17$`Total 2TS` <- rowSums(Tabela3.17[,c("Feminino T2", "Masculino T2")])
Tabela3.17$`Total 1F` <- rowSums(Tabela3.17[,c("Feminino A1", "Feminino T1")])
Tabela3.17$`Total 1M` <- rowSums(Tabela3.17[,c("Masculino A1", "Masculino T1")])
Tabela3.17$`Total 1T` <- rowSums(Tabela3.17[,c("Total 1F", "Total 1M")])
Tabela3.17$`Total 2F` <- rowSums(Tabela3.17[,c("Feminino A2", "Feminino T2")])
Tabela3.17$`Total 2M` <- rowSums(Tabela3.17[,c("Masculino A2", "Masculino T2")])
Tabela3.17$`Total 2T` <- rowSums(Tabela3.17[,c("Total 2F", "Total 2M")])

Tabela3.17 <- Tabela3.17 %>% 
  ungroup() %>% 
  select(`Feminino A1`, `Masculino A1`, `Total 1AS`, `Feminino A2`, `Masculino A2`, `Total 2AS`,
         `Feminino T1`, `Masculino T1`, `Total 1TS`, `Feminino T2`, `Masculino T2`, `Total 2TS`,
         `Total 1F`, `Total 1M`, `Total 1T`, `Total 2F`, `Total 2M`, `Total 2T`)

Tabela3.17["Total Geral",] <- colSums(Tabela3.17, na.rm=T)/2

rm(AA1, AA2, AT1, AT2, totais)

# Tabela 3.18 Forma de Saída ----------------------------------------------

M2 <- Mest %>% filter(`Ano Ingresso Opcao`<= anobase,
                      (`Ano Saida Opcao`> anobase | (`Ano Saida Opcao`== anobase & `Semestre Saida Opcao` == 2))) %>% 
  count(`Nome Curso`)

D1 <- Mest %>% 
  filter((`Ano Ingresso Opcao`< anobase | (`Ano Ingresso Opcao`== anobase & `Semestre Ingresso Opcao`<=1)),
         `Ano Saida Opcao`>= anobase) %>% 
  filter(`For. Saida Opcao`!="Está Cursando" & `For. Saida Opcao`!="Formatura Pos-Graduacao" &
           `For. Saida Opcao`!="Formatura com Especializacao" & `For. Saida Opcao` != "Desligamento com Especializacao")

D1 <- within(D1, {
  # `For. Saida Opcao`[`For. Saida Opcao`=="Formatura" | `For. Saida Opcao`=="Formatura Anterior a 1/88"] <- "Formado-Graduação"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Deslig - Nao Cumpriu condicao" | `For. Saida Opcao`=="Desligamento Jubilamento" |
                       `For. Saida Opcao`=="Desligamento Rendimento Academico" |
                       `For. Saida Opcao`=="Deslig - Não Cumpriu condicao" |
                       `For. Saida Opcao`=="Rep 3 vezes na mesma Disc Obrig"] <- "Desligado-Falta de Rendimento 1"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Desligamento Voluntario" | `For. Saida Opcao`=="Mudanca de Habilitacao" | 
                       `For. Saida Opcao`=="Transferencia" | `For. Saida Opcao`=="Novo Vestibular" |
                       `For. Saida Opcao`=="Mudanca de Curso" |`For. Saida Opcao`=="Mudança de Nível" |
                       `For. Saida Opcao`== "Vestibular p/outra Habilitacao" | `For. Saida Opcao`== "SISU" |
                       `For. Saida Opcao`== "Mudança de Turno"] <- "Desligamento Voluntário 1"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Falecimento" | `For. Saida Opcao`=="Desligamento Forca de Convenio" | 
                       `For. Saida Opcao`=="Desligamento Falta Documentacao" | `For. Saida Opcao`=="Anulacao de Registro" | 
                       `For. Saida Opcao`=="Desligamento Decisao Judicial" | 
                       `For. Saida Opcao`=="Expulsao Disciplinar" |
                       `For. Saida Opcao`== "Outros"] <- "Outros 1"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Desligamento - Abandono"] <- "Abandono 1"
})

D1 <- D1 %>% count(`Nome Curso`, `For. Saida Opcao`) %>% spread(`For. Saida Opcao`, n, fill = 0)

D2 <- Mest %>% 
  filter(`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`> anobase | (`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`==2))) %>% 
  filter(`For. Saida Opcao`!="Está Cursando" & `For. Saida Opcao`!="Formatura Pos-Graduacao" &
           `For. Saida Opcao`!="Formatura com Especializacao" & `For. Saida Opcao` != "Desligamento com Especializacao")
D2 <- within(D2, {
  # `For. Saida Opcao`[`For. Saida Opcao`=="Formatura" | `For. Saida Opcao`=="Formatura Anterior a 1/88"] <- "Formado-Graduação"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Deslig - Nao Cumpriu condicao" | `For. Saida Opcao`=="Desligamento Jubilamento" |
                       `For. Saida Opcao`=="Desligamento Rendimento Academico" |
                       `For. Saida Opcao`=="Deslig - Não Cumpriu condicao" |
                       `For. Saida Opcao`=="Rep 3 vezes na mesma Disc Obrig"] <- "Desligado-Falta de Rendimento 2"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Desligamento Voluntario" | `For. Saida Opcao`=="Mudanca de Habilitacao" | 
                       `For. Saida Opcao`=="Transferencia" | `For. Saida Opcao`=="Novo Vestibular" |
                       `For. Saida Opcao`=="Mudanca de Curso" |`For. Saida Opcao`=="Mudança de Nível" |
                       `For. Saida Opcao`== "Vestibular p/outra Habilitacao" | `For. Saida Opcao`== "SISU" |
                       `For. Saida Opcao`== "Mudança de Turno"] <- "Desligamento Voluntário 2"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Falecimento" | `For. Saida Opcao`=="Desligamento Forca de Convenio" | 
                       `For. Saida Opcao`=="Desligamento Falta Documentacao" | `For. Saida Opcao`=="Anulacao de Registro" | 
                       `For. Saida Opcao`=="Desligamento Decisao Judicial" | 
                       `For. Saida Opcao`=="Expulsao Disciplinar" |
                       `For. Saida Opcao`== "Outros"] <- "Outros 2"
  
  `For. Saida Opcao`[`For. Saida Opcao`=="Desligamento - Abandono"] <- "Abandono 2"
})

D2 <- D2 %>% count(`Nome Curso`, `For. Saida Opcao`) %>% spread(`For. Saida Opcao`, n, fill = 0)

Tabela3.18 <- left_join(Nomes, M2) %>% 
  left_join(D1) %>% 
  left_join(D2)

totais <- Tabela3.18 %>% 
  group_by(Unidade) %>% 
  select(-`Nome Curso`) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Tabela3.18$Unidade <- sapply(Tabela3.18$Unidade, function(x) paste(x, "-", collapse=""))
Tabela3.18 <- bind_rows(Tabela3.18, totais) %>% arrange(Unidade, `Nome Curso`)
row.names(Tabela3.18) <- Tabela3.18$`Nome Curso`; Tabela3.18$`Nome Curso` <- NULL; Tabela3.18$Unidade <- NULL

Tabela3.18$`Total 1S` <- rowSums(Tabela3.18[,c("Abandono 1", "Desligamento Voluntário 1", "Desligado-Falta de Rendimento 1", 
                                               "Outros 1")])
Tabela3.18$`Total 2S` <- rowSums(Tabela3.18[,c("Abandono 2", "Desligamento Voluntário 2", "Desligado-Falta de Rendimento 2", 
                                               "Outros 2")])

Tabela3.18 <- Tabela3.18 %>% 
  ungroup() %>% 
  select(n,`Abandono 1`, `Abandono 2`, `Desligamento Voluntário 1`, 
         `Desligamento Voluntário 2`, `Desligado-Falta de Rendimento 1`, `Desligado-Falta de Rendimento 2`,
         `Outros 1`, `Outros 2`, `Total 1S`, `Total 2S`)
Tabela3.18["Total Geral",] <- colSums(Tabela3.18, na.rm=T)/2

Tabela3.18[is.na(Tabela3.18)] <- 0

rm(M2, D1, D2, totais)

# Tabela 3.19 Evolução Ingressantes -------------------------------------------

# Pegar do último anuário e 
# verificar se há cursos novos
# se sim, atualizar no CSV
Tabela3.19 <- read.csv2("EvoIngM.csv", check.names=F, stringsAsFactors = FALSE) 

Ing <- Mest %>% 
  filter(`Ano Ingresso Opcao`== anobase) %>% 
  count(`Nome Curso`, Unidade, name = "2018") %>% 
  mutate(`Nome Curso` = ifelse(`Nome Curso` == "Propriedade Intelectual e Transferência de Tecnologia para a Inovação",
                               "Propriedade Intelectual e Transfer de Tecnol para Inovação Tecnológica",
                               `Nome Curso`))

totais <- Ing %>% 
  group_by(Unidade) %>% 
  select(-`Nome Curso`) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(Label_Unidades)

Ing$Unidade <- sapply(Ing$Unidade, function(x) paste(x, "-", collapse=""))
Ing <- bind_rows(Ing,totais) %>% arrange(Unidade, `Nome Curso`)

Ing$Unidade <- NULL

Ing["Total Geral",] <- c("Total Geral", colSums(Ing[-1], na.rm=T)/2)

Tabela3.19 <- full_join(Tabela3.19, Ing, by = c("Unidade Acadêmica/Curso" = "Nome Curso"))

Tabela3.19[Tabela3.19=="-"] <- 0
Tabela3.19[is.na(Tabela3.19)] <- 0

rm(Ing, totais)

# Tabela 3.20 Evolução Alunos Registrados ----------------------------------------

# Pegar do último anuário e 
# verificar se há cursos novos
# se sim, atualizar no CSV
Tabela3.20 <- read.csv2("EvoRegM.csv", check.names=F, stringsAsFactors = FALSE)

Reg <- Mest %>% 
  filter(`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`> anobase | (`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`==2))) %>% 
  count(`Nome Curso`, Unidade, name = "2018") %>% 
  mutate(`Nome Curso` = ifelse(`Nome Curso` == "Propriedade Intelectual e Transferência de Tecnologia para a Inovação",
                               "Propriedade Intelectual e Transfer de Tecnol para Inovação Tecnológica",
                               `Nome Curso`))


totais <- Reg %>% group_by(Unidade) %>% select(-`Nome Curso`) %>% summarise_all(sum, na.rm = TRUE) %>% left_join(Label_Unidades)

Reg$Unidade <- sapply(Reg$Unidade, function(x) paste(x, "-", collapse=""))
Reg <- bind_rows(Reg,totais) %>% arrange(Unidade, `Nome Curso`)
Reg$Unidade <- NULL

Reg["Total Geral", ] <- c("Total Geral", colSums(Reg[-1], na.rm = TRUE)/2)

Tabela3.20 <- full_join(Tabela3.20, Reg, by = c("Unidade Acadêmica/Curso" = "Nome Curso"))

### o curso Administração Profissionalizante
### não tem mais observações - retirar da tabela

Tabela3.20 <- Tabela3.20[Tabela3.20$`Unidade Acadêmica/Curso` != "Administração Profissionalizante", ]

Tabela3.20[Tabela3.20=="-"] <- 0
Tabela3.20[is.na(Tabela3.20)] <- 0

rm(Reg, totais)

# Tabela 3.21 Evolução Formados -------------------------------------------

# Pegar do último anuário e 
# verificar se há cursos novos
# se sim, atualizar no CSV
Tabela3.21 <- read.csv2("EvoForM.csv", check.names=F, stringsAsFactors = FALSE)

For <- Mest %>% filter(`Ano Saida Opcao`== anobase,
                       str_detect(`For. Saida Opcao`, "Formatura")) %>%
  count(`Nome Curso`, Unidade, name = "2018") %>% 
  mutate(`Nome Curso` = ifelse(`Nome Curso` == "Propriedade Intelectual e Transferência de Tecnologia para a Inovação",
                               "Propriedade Intelectual e Transfer de Tecnol para Inovação Tecnológica",
                               `Nome Curso`))


totais <- For %>% group_by(Unidade) %>% select(-`Nome Curso`) %>% summarise_all(sum, na.rm = TRUE) %>% left_join(Label_Unidades)

For$Unidade <- sapply(For$Unidade, function(x) paste(x, "-", collapse=""))
For <- bind_rows(For, totais) %>% arrange(Unidade, `Nome Curso`)
For$Unidade <- NULL

For["Total Geral", ] <- c("Total Geral", colSums(For[-1], na.rm = TRUE)/2)

Tabela3.21 <- full_join(Tabela3.21, For, by = c("Unidade Acadêmica/Curso" = "Nome Curso"))

### dois cursos não têm mais observações:
### Agronegócios Multi-institucional & Ciências Agrárias

Tabela3.21 <- Tabela3.21[Tabela3.21$`Unidade Acadêmica/Curso` != "Agronegócios Multi-institucional", ]
Tabela3.21 <- Tabela3.21[Tabela3.21$`Unidade Acadêmica/Curso` != "Ciências Agrárias", ]

Tabela3.21[Tabela3.21 == "-"] <- 0
Tabela3.21[Tabela3.21$`Unidade Acadêmica/Curso` == "Gestão Pública", ]$`2014` <- NA
Tabela3.21[Tabela3.21$`Unidade Acadêmica/Curso` == "Artes Cênicas", ]$`2014` <- NA
Tabela3.21[Tabela3.21$`Unidade Acadêmica/Curso` == "Ensino de Física", ]$`2014` <- NA

rm(For, totais)

# Excel -------------------------------------------------------------------

# REVER TODA ESSA PARTE
# ACHO QUE NÃO PRECISAMOS MAIS

library(openxlsx)
library(xlsx)

wb <- createWorkbook()

#Tabela 3.08

Tabela3.08 <- as.data.frame(Tabela3.08)

sheet <- createSheet(wb, sheetName="Tabela 3.08")

addDataFrame(Tabela3.08, sheet, row.names = T, startRow=5, startColumn = 5)

cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 

x <- c("Tabela 3.08: Alunos estrangeiros regulares registrados nos cursos de Pós-Graduação Stricto Sensu, por Continente, País e Unidade Acadêmica, UnB, 2o Semestre de 2018")

CB.setRowData(cb, x, 1, F)

cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 

x <- c("Continente", "País", "Unidades Acadêmicas")

CB.setRowData(cb, x, 1, F)

cb <- CellBlock(sheet, 6, 4, 75, 1, create=TRUE) 

### ESSA LINHA PRECISA SER AJUSTADA

x <- c("ÁFRICA", rep(NA,13), "AMÉRICA", rep(NA,91), "ÁSIA", rep(NA,16), "EUROPA", rep(NA,23), "OCEANIA")

CB.setColData(cb, x, 1, F)

addDataFrame(Tabela3.08.2, sheet, row.names = F, startRow=6, startColumn = 38)

addDataFrame(Tabela3.08.3, sheet, row.names = F, startRow=18, startColumn = 38)

addDataFrame(Tabela3.08.4, sheet, row.names = F, startRow=24, startColumn = 38)

#Tabela 3.12
sheet <- createSheet(wb, sheetName="Tabela 3.12")
addDataFrame(Tabela3.12, sheet, row.names = T, startRow=6, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 3, 1, create=TRUE) 
x <- c("Tabela 3.12 – Ingresso de Alunos e Número de Dissertações Homologadas nos Cursos de Mestrado, por Unidade Acadêmica e Curso, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE)
x <- c("Unidade Acadêmica/Curso", "Ingresso de Alunos", rep(NA,6), "Dissertações Homologadas")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("1° Sem.", NA,NA,  "2° Sem.", NA,NA, "Total Ingressantes", "1° Sem.", NA,NA,  "2° Sem.", NA,NA, "Total Formados")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 6, 5, 1, 60, create=TRUE)
x <- c("Fem.", "Masc.", "Total", "Fem.", "Masc.", "Total", NA, "Fem.", "Masc.", "Total", "Fem.", "Masc.", "Total")
CB.setRowData(cb, x, 1, F)
addDataFrame(Tabela3.12.2, sheet, row.names = T, startRow=6, startColumn = 21)


#Tabela 3.13
sheet <- createSheet(wb, sheetName="Tabela 3.13")
addDataFrame(Tabela3.13, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.13 – Alunos Ingressantes e Concluintes nos Cursos de Mestrado, por Sexo e Faixa Etária, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Faixa Etária", "Ingressantes", rep(NA,4), "Concluintes")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("Fem.","%", "Masc.","%","Total", "Fem.","%", "Masc.","%","Total")
CB.setRowData(cb, x, 1, F)


#Tabela 3.14
sheet <- createSheet(wb, sheetName="Tabela 3.14")
addDataFrame(Tabela3.14, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.14 – Alunos Regulares Ativos, Matrículas e Aprovações em Disciplinas* nos Cursos de Mestrado, por Unidade Acadêmica e Curso, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE) 
x <- c("Unidade Acadêmica/Curso", "Alunos Ativos", NA, "Matrículas em Disciplinas", NA,
       "Aprovados em Disciplinas", NA, "% Aprovados/Matriculados")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("1° Sem.","2° Sem.", "1° Sem.", "2° Sem.","1° Sem.", "2° Sem.","1° Sem.", "2° Sem.")
CB.setRowData(cb, x, 1, F)


#Tabela 3.15
sheet <- createSheet(wb, sheetName="Tabela 3.15")
addDataFrame(Tabela3.15, sheet, row.names = T, startRow=5, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.15 – Alunos Regulares Registrados nos Cursos de Mestrado, por Sexo e Faixa Etária, UnB, 2º/2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 5, 4, 1, 60, create=TRUE) 
x <- c("Faixa Etária", "Fem.", "%", "Masc.", "%", "Total")
CB.setRowData(cb, x, 1, F)


#Tabela 3.16
sheet <- createSheet(wb, sheetName="Tabela 3.16")
addDataFrame(Tabela3.16, sheet, row.names = T, startRow=6, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 3, 1, create=TRUE) 
x <- c("Tabela 3.16 – Alunos Regulares Registrados nos Cursos de Mestrado, por Semestre, Unidade Acadêmica, Curso e Sexo, UnB, 2018")
CB.setRowData(cb, x, 1, F)
cb <- CellBlock(sheet, 4, 4, 1, 60, create=TRUE)
x <- c("Unidade Acadêmica/Curso", "Alunos Regulares Registrados")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 5, 5, 1, 60, create=TRUE)
x <- c("1° Sem.", NA,NA,  "2° Sem.")
CB.setRowData(cb, x, 1, F) 
cb <- CellBlock(sheet, 6, 5, 1, 60, create=TRUE)
x <- c("Fem.", "Masc.", "Total", "Fem.", "Masc.", "Total")
CB.setRowData(cb, x, 1, F)
addDataFrame(Tabela3.16.2, sheet, row.names = T, startRow=6, startColumn = 12)


#Tabela 3.17
sheet <- createSheet(wb, sheetName="Tabela 3.17")
addDataFrame(Tabela3.17, sheet, row.names = T, startRow=6, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 3, 1, create=TRUE) 
x <- c("Tabela 3.17 – Alunos Regulares Registrados Ativos e com Trancamento Geral de Matrícula nos Cursos de Mestrado, por Semestre, Sexo, Unidade Acadêmica e Curso, UnB, 2018")
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


#Tabela 3.18
sheet <- createSheet(wb, sheetName="Tabela 3.18")
addDataFrame(Tabela3.18, sheet, row.names = T, startRow=6, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 3, 1, create=TRUE) 
x <- c("Tabela 3.18 – Desligamento de Alunos nos Cursos de Mestrado, por Forma, Semestre, Unidade Acadêmica e Curso, UnB, 2018")
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


#Tabela 3.19
sheet <- createSheet(wb, sheetName="Tabela 3.19")
addDataFrame(Tabela3.19, sheet, row.names = F, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.19 – Evolução do Ingresso de Alunos nos Cursos de Mestrado, por Unidade Acadêmica e Curso, UnB, 2014 a 2018")
CB.setRowData(cb, x, 1, F)
# cb <- CellBlock(sheet, 4, 4, 1, 1, create=TRUE) 
# x <- c("Unidade Acadêmica/Curso")
# CB.setRowData(cb, x, 1, F)


#Tabela 3.20
sheet <- createSheet(wb, sheetName="Tabela 3.20")
addDataFrame(Tabela3.20, sheet, row.names = F, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.20 – Evolução do Número de Alunos Registrados* nos Cursos de Mestrado, por Unidade Acadêmica e Curso, UnB, 2014 a 2018")
CB.setRowData(cb, x, 1, F)
# cb <- CellBlock(sheet, 4, 4, 1, 1, create=TRUE) 
# x <- c("Unidade Acadêmica/Curso")
# CB.setRowData(cb, x, 1, F)


#Tabela 3.21
sheet <- createSheet(wb, sheetName="Tabela 3.21")
addDataFrame(Tabela3.21, sheet, row.names = F, startRow=4, startColumn = 4)
cb <- CellBlock(sheet, 2, 4, 1, 60, create=TRUE) 
x <- c("Tabela 3.21 – Evolução do Número de Alunos com Dissertações Homologadas nos Cursos de Mestrado, por Unidade Acadêmica e Curso, UnB, 2014 a 2018")
CB.setRowData(cb, x, 1, F)
# cb <- CellBlock(sheet, 4, 4, 1, 1, create=TRUE) 
# x <- c("Unidade Acadêmica/Curso")
# CB.setRowData(cb, x, 1, F)

### salva o arquivo excel com todas as tabelas
saveWorkbook(wb, "dados_mestrado/Anuário Mestrado UnB 2019.xlsx")

rm(wb,cb,sheet,x)







