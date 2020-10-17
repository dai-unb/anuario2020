# Setup -------------------------------------------------------------------

options(OutDec = ",")

library(tidyverse)
library(lubridate)
library(data.table)
library(dataframes2xls)
library(openxlsx)
library(readxl)

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
  left_join(label_unidade) 

#_______________________________
#adicionei (Roberto - 15/10/2020) => o comando da linha 78 não estava rodando porque estávamos criando 2 colunas separadas: curso e Nome curso.

totais <- totais %>% select(10,(1:9))
colnames(totais)[colnames(totais) == "Curso"] <- "Nome Curso"
#_______________________________

Tabela3.23$Unidade <- sapply(Tabela3.23$Unidade, function(x) paste(x, "-", collapse=""))

Tabela3.23 <- bind_rows(Tabela3.23, totais) %>% arrange(Unidade, `Nome Curso`)      


#____________________________________________________
# Adicionei (Roberto - 15/10/2020 - os comandos abaixo estavam apagando o nome das colunas, inutilizando o comando anterior. Alterei para apenas deletar as colunas

#row.names(Tabela3.23) <- Tabela3.23$`Nome Curso` 
#Tabela3.23$`Nome Curso` <- NULL 
#Tabela3.23$Unidade <- NULL

Tabela3.23_rownames <- Tabela3.23$`Nome Curso`
Tabela3.23 <- Tabela3.23 %>% select(- `Nome Curso`, - Unidade)

#_____________________________________________________

Tabela3.23$`Total Ing 1` <- rowSums(Tabela3.23[,c("Ing Feminino 1","Ing Masculino 1")])
Tabela3.23$`Total Ing 2` <- rowSums(Tabela3.23[,c("Ing Feminino 2","Ing Masculino 2")])
Tabela3.23$`Total Ing` <- rowSums(Tabela3.23[,c("Total Ing 1","Total Ing 2")])
Tabela3.23$`Total For. 1` <- rowSums(Tabela3.23[,c("For. Feminino 1","For. Masculino 1")])
Tabela3.23$`Total For. 2` <- rowSums(Tabela3.23[,c("For. Feminino 2","For. Masculino 2")])
Tabela3.23$`Total For.` <- rowSums(Tabela3.23[,c("Total For. 1","Total For. 2")])


Tabela3.23 <- Tabela3.23 %>% select(`Ing Feminino 1`, `Ing Masculino 1`, `Total Ing 1`, `Ing Feminino 2`, `Ing Masculino 2`, `Total Ing 2`,
                                    `Total Ing`, `For. Feminino 1`, `For. Masculino 1`, `Total For. 1`, `For. Feminino 2`, `For. Masculino 2`,
                                    `Total For. 2`, `Total For.`)


#____________________________________________________
# Adicionei (Roberto - 15/10/2020) 

Tabela3.23_Total_Geral <- (colSums(Tabela3.23, na.rm = TRUE))/2
Tabela3.23 <- rbind(Tabela3.23, Tabela3.23_Total_Geral)
Tabela3.23_rownames <- c(Tabela3.23_rownames, "Total Geral")

row.names(Tabela3.23) <- Tabela3.23_rownames
#____________________________________________________

Ing <- Doc %>% filter(`Ano Ingresso Opcao`== anobase) %>% 
  group_by(Unidade) %>% 
  summarise(Ingressantes=n()) %>% 
  arrange(desc(Ingressantes))

For <- Doc %>% filter(`Ano Saida Opcao`== anobase & `For. Saida Opcao` == "Formatura Pos-Graduacao") %>% 
  group_by(Unidade) %>% 
  summarise(Concluintes=n())

Tabela3.23.2 <- full_join(Ing, For)
Tabela3.23.2[is.na(Tabela3.23.2)] <- 0

Tabela3.23.2_rownames <- Tabela3.23.2$Unidade
Tabela3.23.2$Unidade <- NULL
rownames(Tabela3.23.2) <- Tabela3.23.2_rownames 

rm(Ing, Ing1, Ing2, For, For1, For2, totais, Tabela3.23_rownames, Tabela3.23_Total_Geral, Tabela3.23.2_rownames)

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

For$Idade <- difftime(as.Date("2019-01-01"), For$Nascimento)/365

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

Tabela3.24$`% Masculino.x`[Tabela3.24$`% Masculino.x` == "NA%"] <- 0
Tabela3.24$`% Feminino.x`[Tabela3.24$`% Feminino.x` == "NA%"] <- 0
Tabela3.24$`% Masculino.y`[is.na(Tabela3.24$`% Masculino.y`) == T] <- 0
Tabela3.24$`% Feminino.y`[is.na(Tabela3.24$`% Feminino.y`) == T] <- 0
Tabela3.24$Feminino.x[is.na(Tabela3.24$Feminino.x) == T] <- 0
Tabela3.24$Feminino.y[is.na(Tabela3.24$Feminino.y) == T] <- 0
Tabela3.24$Masculino.x[is.na(Tabela3.24$Masculino.x) == T] <- 0
Tabela3.24$Masculino.y[is.na(Tabela3.24$Masculino.y) == T] <- 0
Tabela3.24$Total.x[is.na(Tabela3.24$Total.x) == T] <- 0
Tabela3.24$Total.y[is.na(Tabela3.24$Total.y) == T] <- 0



Tabela3.24 <- Tabela3.24 %>% select("Faixa Etária","Feminino.x","% Feminino.x","Masculino.x","% Masculino.x","Total.x","Feminino.y","% Feminino.y","Masculino.y","% Masculino.y", "Total.y")

#________________________________________________________
# Adicionei (Roberto - 15/10/2020)

tabela3.24_rownames <- Tabela3.24$`Faixa Etária`
Tabela3.24$`Faixa Etária` <-  NULL
rownames(Tabela3.24) <- tabela3.24_rownames

#rownames(Tabela3.24) <- Tabela3.24$`Faixa Etária`
#Tabela3.24$`Faixa Etária` <-  NULL

#________________________________________________________
rm(For, Ing, tabela3.24_rownames)


# Tabela 3.25 Alunos Regulares Ativos, Matrículas e Aprovados em Disc --------

# MUDAR NOME DAS VARIAVEIS PARA HE0 HE1 HE2

HE0 <- read_fwf("dados_identificados/he20190.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                    col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20191", "Frequencias")), 
                    locale = locale(encoding = "latin1")) %>% distinct(MatricAluno, .keep_all = T)

HE1 <- read_fwf("dados_identificados/he20191.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                    col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20191", "Frequencias")), 
                    locale = locale(encoding = "latin1")) %>% distinct(MatricAluno, .keep_all = T)

HE2 <- read_fwf("dados_identificados/he20192.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                    col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20192", "Frequencias")), 
                    locale = locale(encoding = "latin1")) %>% distinct(MatricAluno, .keep_all = T)


HE1 <- rbind(HE1, HE0)

HE1 <- within(HE1, {
  
  Mencao_20191[Mencao_20191=="AP"] <- "AP 1"
  Mencao_20191[Mencao_20191=="CC"] <- "AP 1"
  Mencao_20191[Mencao_20191=="MM"] <- "AP 1"
  Mencao_20191[Mencao_20191=="MS"] <- "AP 1"
  Mencao_20191[Mencao_20191=="SS"] <- "AP 1"
  
  Mencao_20191[Mencao_20191=="SR"] <- "RP 1"
  Mencao_20191[Mencao_20191=="II"] <- "RP 1"
  Mencao_20191[Mencao_20191=="MI"] <- "RP 1"
  Mencao_20191[Mencao_20191=="TR"] <- "RP 1"
  Mencao_20191[Mencao_20191=="TJ"] <- "RP 1"
  Mencao_20191[Mencao_20191=="DP"] <- "RP 1"
  Mencao_20191[Mencao_20191=="RP"] <- "RP 1"
})

HE2 <- within(HE2, {
  
  Mencao_20192[Mencao_20192=="AP"] <- "AP 2"
  Mencao_20192[Mencao_20192=="CC"] <- "AP 2"
  Mencao_20192[Mencao_20192=="MM"] <- "AP 2"
  Mencao_20192[Mencao_20192=="MS"] <- "AP 2"
  Mencao_20192[Mencao_20192=="SS"] <- "AP 2"
  
  Mencao_20192[Mencao_20192=="SR"] <- "RP 2"
  Mencao_20192[Mencao_20192=="II"] <- "RP 2"
  Mencao_20192[Mencao_20192=="MI"] <- "RP 2"
  Mencao_20192[Mencao_20192=="TR"] <- "RP 2"
  Mencao_20192[Mencao_20192=="TJ"] <- "RP 2"
  Mencao_20192[Mencao_20192=="DP"] <- "RP 2"
  Mencao_20192[Mencao_20192=="RP"] <- "RP 2"
})

#________________________________________________________
# Adicionei (Roberto - 15/10/2020) : alterei Trancados_1 e Trancados_2 por TGM20191 e TGM20192
#________________________________________________________

M1 <- Doc %>% filter(is.na(TGM20191)) %>% filter(`Ano Ingresso Opcao`< anobase | (`Ano Ingresso Opcao`== anobase & `Semestre Ingresso Opcao`<=1) & `Ano Saida Opcao`>= anobase)
AA1 <- M1 %>% group_by(`Nome Curso`) %>% summarize(`Alunos Ativos 1` = n())
M1 <- inner_join(M1, HE1, by = "MatricAluno")
MD1 <- M1 %>% group_by(`Nome Curso`) %>% summarise(`Matriculados em Disciplinas 1`=n())
AP1 <- M1 %>% group_by(`Nome Curso`, Mencao_20191) %>% summarize(n=n()) %>% spread(Mencao_20191, n, fill = 0)

M2 <- Doc %>% filter(is.na(TGM20192)) %>% filter(`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`> anobase | (`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`==2)))
AA2 <- M2 %>% group_by(`Nome Curso`) %>% summarize(`Alunos Ativos 2` = n())
M2 <- inner_join(M2, HE2, by = "MatricAluno")
MD2 <- M2 %>% group_by(`Nome Curso`) %>% summarise(`Matriculados em Disciplinas 2`=n())
AP2 <- M2 %>% group_by(`Nome Curso`, Mencao_20192) %>% summarize(n=n()) %>% spread(Mencao_20192, n, fill = 0)

Tabela3.25 <- left_join(Nomes, AA1) %>% 
  left_join(AA2) %>% 
  left_join(AP1) %>% 
  left_join(AP2) %>% 
  left_join(MD1) %>% 
  left_join(MD2)

Totais <- Tabela3.25 %>% group_by(`Unidade`) %>% 
  select(-`Nome Curso`) %>%
  summarise_all(sum, na.rm = TRUE) %>% 
  left_join(label_unidade)

#________________________________________________________
# Adicionei (Roberto - 15/10/2020) 
colnames(Totais)[colnames(Totais) == "Curso"] <- "Nome Curso"
#________________________________________________________

Tabela3.25$Unidade <- sapply(Tabela3.25$Unidade, function(x) paste(x, "-", collapse=""))
Tabela3.25 <- bind_rows(Tabela3.25, Totais) %>% arrange(`Unidade`,`Nome Curso`)

#________________________________________________________
# Adicionei (Roberto - 15/10/2020)

Tabela3.25_rownames <- Tabela3.25$`Nome Curso`
Tabela3.25_rownames <- c(Tabela3.25_rownames, "Total Geral")
Tabela3.25$`Nome Curso` <- NULL
Tabela3.25$Unidade <- NULL


tabela3.25_Total_Geral <- colSums(Tabela3.25, na.rm = TRUE)/2
Tabela3.25 <- rbind(Tabela3.25, tabela3.25_Total_Geral)


#__________________________________________________________

#Tabela3.25["Total Geral",] <- colSums(Tabela3.25, na.rm = T)

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

row.names(Tabela3.25) <- Tabela3.25_rownames

rm(HE0, HE1, HE2, M1, AA1, MD1, AP1, M2, AA2, MD2, AP2, Totais, tabela3.25_Total_Geral,Tabela3.25_rownames)

# Tabela 3.26 Alunos Regulares por Sexo e Faixa Etária --------------------------------

Tabela3.26 <- Doc %>% filter(`Ano Ingresso Opcao`<=2019 & (`Ano Saida Opcao`>2019 | (`Ano Saida Opcao`==2019 & `Semestre Saida Opcao`==2)))

Tabela3.26$Idade <- difftime(as.Date("2019-01-01"), Tabela3.26$Nascimento)/365

Tabela3.26 <- Tabela3.26 %>% mutate(`Faixa Etária` = ifelse(Idade > 18 & Idade <= 24, "De 19 a 24 anos",
                                                            ifelse(Idade > 24 & Idade <= 29, "De 25 a 29 anos",
                                                                   ifelse(Idade > 29 & Idade <= 34, "De 30 a 34 anos",
                                                                          ifelse(Idade > 34 & Idade <= 39, "De 35 a 39 anos",
                                                                                 ifelse(Idade > 39 & Idade <= 44, "De 40 a 44 anos", "De 45 anos ou mais"))))))


Tabela3.26 <- Tabela3.26 %>% group_by(`Faixa Etária`, Sexo) %>% summarise(n=n())
md <- melt(Tabela3.26, id=c("Faixa Etária", "Sexo"))
Tabela3.26 <- dcast(md, `Faixa Etária` ~ Sexo)

#__________________________________________________
# Adicionei (Roberto - 15/10/2020)
Tabela3.26_rownames <- Tabela3.26$`Faixa Etária`

#__________________________________________________

Tabela3.26 <- Tabela3.26 %>% select(-`Faixa Etária`)

Tabela3.26["Total", ] <- colSums(Tabela3.26, na.rm=T)
Tabela3.26$Total <- rowSums(Tabela3.26)

Tabela3.26$`% Feminino` <- paste(round(Tabela3.26$Feminino/Tabela3.26$Total*100,1), "%", sep="")
Tabela3.26$`% Masculino` <- paste(round(Tabela3.26$Masculino/Tabela3.26$Total*100,1), "%", sep="")

Tabela3.26 <- Tabela3.26 %>% select(Feminino, `% Feminino`, Masculino, `% Masculino`, Total)

Tabela3.26_rownames <- c(Tabela3.26_rownames, "Total Geral")
Tabela3.26_rownames[6] <- "45 anos ou mais"
row.names(Tabela3.26) <- Tabela3.26_rownames

rm(md, Tabela3.26_rownames)

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
M1.Unidade <- M1 %>% select(-`Nome Curso`) %>% group_by(Unidade) %>% summarize_all(sum, na.rm=TRUE) %>% left_join(label_unidade)

#__________________________________________________
# Adicionei (Roberto - 15/10/2020)

colnames(M1.Unidade)[5] <- "Nome Curso"

#_____________________________________________________

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
M2.Unidade <- M2 %>% select(-`Nome Curso`) %>% group_by(Unidade) %>% summarize_all(sum, na.rm=TRUE) %>% left_join(label_unidade)

#__________________________________________________
# Adicionei (Roberto - 15/10/2020)

colnames(M2.Unidade)[5] <- "Nome Curso"

#_____________________________________________________

M2 <- rbind(M2.Unidade, M2) %>%  arrange(Unidade, `Nome Curso`)
M2 <- M2 %>% select(-1) %>% select(4, 1, 2, 3)


Tabela3.27 <- left_join(M1, M2) 

Tabela3.27 <- Tabela3.27[c(1,2,3,4,6,5,10,7,8,9,12,11,15,13,14,16,18,17,20,19,22,21,24,23,26,25,28,27,29,34,30,31,32,33,35,36,37,38,44,39,40,41,42,43,45,46,47,48,50,49,58,51,52,53,54,55,56,57,59,60,63,61,62,64,66,65,68,67,69,71,70,74,72,73,78,75,76,77,79,80,81,82,83,85,84,86,87,88,90,89,92,91,93,94,95,96,97), ]


Tabela3.27_rownames <- Tabela3.27$`Nome Curso`
Tabela3.27$`Nome Curso` <- NULL

"Total Geral" <-  colSums(Tabela3.27, na.rm = TRUE)/2

Tabela3.27 <- rbind(Tabela3.27, `Total Geral`)

Tabela3.27_rownames <- c(Tabela3.27_rownames, "Total Geral")
rownames(Tabela3.27) <- Tabela3.27_rownames

rm(M1, M1.1, M1.Unidade, M2, M2.2, M2.Unidade, md, "Total Geral", Tabela3.27_rownames)



Tabela3.27.2 <- Doc %>% filter(`Ano Ingresso Opcao` <= anobase & `Ano Saida Opcao` >= anobase) %>% group_by(Unidade) %>% summarise(Registrados=n()) %>% arrange(desc(Registrados))

Tabela3.27.2_row.names <- Tabela3.27.2$Unidade
Tabela3.27.2$Unidade <- NULL
row.names(Tabela3.27.2) <- Tabela3.27.2_row.names

rm(Tabela3.27.2_row.names)

# Tabela 3.28 Alunos Ativos e Trancados por Sexo -------------------------------------------------------------

# PARECE REPETIDO - REVER: Gui, melhor alguém com mais experiência dizer se há repetição ou não


HE0 <- read_fwf("dados_identificados/he20190.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                  col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20191", "Frequencias")), 
                    locale = locale(encoding = "latin1")) %>% distinct(MatricAluno, .keep_all = T)

HE1 <- read_fwf("dados_identificados/he20191.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                  col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20191", "Frequencias")), 
                    locale = locale(encoding = "latin1")) %>% distinct(MatricAluno, .keep_all = T)

HE2 <- read_fwf("dados_identificados/he20192.txt", fwf_widths(c(9,5,6,2,3,2,3), 
                                                                  col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20192", "Frequencias")), 
                    locale = locale(encoding = "latin1")) %>% distinct(MatricAluno, .keep_all = T) %>% select(MatricAluno, Mencao=Mencao_20192)



HE1 <- rbind(HE0, HE1) %>% select(MatricAluno, Mencao=Mencao_20191); rm(HE0)


HE1 <- within(HE1, {
 Mencao[Mencao=="AP"] <- "AP 1"
 Mencao[Mencao=="CC"] <- "AP 1"
 Mencao[Mencao=="MM"] <- "AP 1"
 Mencao[Mencao=="MS"] <- "AP 1"
 Mencao[Mencao=="SS"] <- "AP 1"
  
 Mencao[Mencao=="SR"] <- "RP 1"
 Mencao[Mencao=="II"] <- "RP 1"
 Mencao[Mencao=="MI"] <- "RP 1"
 Mencao[Mencao=="TR"] <- "RP 1"
 Mencao[Mencao=="TJ"] <- "RP 1"
 Mencao[Mencao=="DP"] <- "RP 1"
 Mencao[Mencao=="RP"] <- "RP 1"
})

HE2 <- within(HE2, {
  Mencao[Mencao=="AP"] <- "AP 2"
  Mencao[Mencao=="CC"] <- "AP 2"
  Mencao[Mencao=="MM"] <- "AP 2"
  Mencao[Mencao=="MS"] <- "AP 2"
  Mencao[Mencao=="SS"] <- "AP 2"
  
  Mencao[Mencao=="SR"] <- "RP 2"
  Mencao[Mencao=="II"] <- "RP 2"
  Mencao[Mencao=="MI"] <- "RP 2"
  Mencao[Mencao=="TR"] <- "RP 2"
  Mencao[Mencao=="TJ"] <- "RP 2"
  Mencao[Mencao=="DP"] <- "RP 2"
  Mencao[Mencao=="RP"] <- "RP 2"
})

AA1 <- Doc %>% filter(is.na(TGM20191) & ((`Ano Ingresso Opcao`<anobase | (`Ano Ingresso Opcao`==anobase & `Semestre Ingresso Opcao`<=1)) & `Ano Saida Opcao`>=anobase)) %>% group_by(`Nome Curso`, Sexo) %>% summarise(n=n())
md <- melt(AA1, id=c("Nome Curso", "Sexo"))
AA1 <- dcast(md, `Nome Curso` ~ Sexo)
names(AA1)[2:3] <- c("Feminino A1", "Masculino A1")

AT1 <- Doc %>% filter(TGM20191==1 & ((`Ano Ingresso Opcao`<anobase | (`Ano Ingresso Opcao`==anobase & `Semestre Ingresso Opcao`<=1)) & `Ano Saida Opcao`>=anobase)) %>% group_by(`Nome Curso`, Sexo) %>% summarise(n=n())
md <- melt(AT1, id=c("Nome Curso", "Sexo"))
AT1 <- dcast(md, `Nome Curso` ~ Sexo)
names(AT1)[2:3] <- c("Feminino T1", "Masculino T1")

AA2 <- Doc %>% filter(is.na(TGM20192) & (`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2)))) %>% group_by(`Nome Curso`, Sexo) %>% summarise(n=n())
md <- melt(AA2, id=c("Nome Curso", "Sexo"))
AA2 <- dcast(md, `Nome Curso` ~ Sexo)
names(AA2)[2:3] <- c("Feminino A2", "Masculino A2")

AT2 <- Doc %>% filter(TGM20192==1 & (`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2)))) %>% group_by(`Nome Curso`, Sexo) %>% summarise(n=n())
md <- melt(AT2, id=c("Nome Curso", "Sexo"))
AT2 <- dcast(md, `Nome Curso` ~ Sexo)
names(AT2)[2:3] <- c("Feminino T2", "Masculino T2")

Tabela3.28 <- left_join(Nomes,AA1) %>% left_join(AA2) %>% left_join(AT1) %>% left_join(AT2) 
Tabela3.28[is.na(Tabela3.28)] <- 0


totais <- Tabela3.28 %>% group_by(Unidade) %>% summarise(`Feminino A1`=sum(`Feminino A1`), `Masculino A1`=sum(`Masculino A1`),
                                                         `Feminino A2`=sum(`Feminino A2`), `Masculino A2`=sum(`Masculino A2`),
                                                         `Feminino T1`=sum(`Feminino T1`), `Masculino T1`=sum(`Masculino T1`),
                                                         `Feminino T2`=sum(`Feminino T2`), `Masculino T2`=sum(`Masculino T2`))
totais <- totais %>% left_join(label_unidade)
colnames(totais)[10] <- "Nome Curso"

Tabela3.28$Unidade <- sapply(Tabela3.28$Unidade, function(x) paste(x, "-", collapse=""))

Tabela3.28 <- bind_rows(Tabela3.28, totais) %>% arrange(Unidade, `Nome Curso`)

Tabela3.28_rownames <- Tabela3.28$`Nome Curso`
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

Tabela3.28_rownames <-c(Tabela3.28_rownames, "Total Geral") 

row.names(Tabela3.28) <- Tabela3.28_rownames

rm(HE1, HE2, md, totais, AA1, AA2, AT1, AT2, Tabela3.28_rownames)


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

totais <- Tabela3.29 %>% group_by(Unidade) %>% select(-"Nome Curso") %>%  summarize_all(sum, na.rm = TRUE) %>% left_join(label_unidade, by = "Unidade") 
Tabela3.29$Unidade <- sapply(Tabela3.29$Unidade, function(x) paste(x, "-", collapse=""))

colnames(totais)[13] <- "Nome Curso"

Tabela3.29 <- bind_rows(Tabela3.29, totais) 
Tabela3.29 <- Tabela3.29 %>% arrange(Unidade,`Nome Curso`) %>% select(-"Unidade")

Tabela3.29_rownames <- Tabela3.29$`Nome Curso`
Tabela3.29$`Nome Curso` <- NULL


Total_Geral <- colSums(Tabela3.29, na.rm=T)/2
Tabela3.29 <- rbind(Tabela3.29, Total_Geral)

Tabela3.29_rownames <- c(Tabela3.29_rownames, " Total  Geral")

Tabela3.29 <- Tabela3.29 %>% mutate(`Regulares Registrados no (2º Semestre)` = ifelse(`Regulares Registrados no (2º Semestre)`== 0,"-", `Regulares Registrados no (2º Semestre)`))
Tabela3.29 <- Tabela3.29 %>% mutate(`Abandono 1` = ifelse(`Abandono 1`== 0,"-", `Abandono 1`))
Tabela3.29 <- Tabela3.29 %>% mutate(`Abandono 2` = ifelse(`Abandono 2`== 0,"-", `Abandono 2`))
Tabela3.29 <- Tabela3.29 %>% mutate(`Desligado-Falta de Rendimento 1` = ifelse(`Desligado-Falta de Rendimento 1`== 0,"-", `Desligado-Falta de Rendimento 1`))
Tabela3.29 <- Tabela3.29 %>% mutate(`Desligado-Falta de Rendimento 2` = ifelse(`Desligado-Falta de Rendimento 2`== 0,"-", `Desligado-Falta de Rendimento 2`))
Tabela3.29 <- Tabela3.29 %>% mutate(`Desligamento Voluntário 1` = ifelse(`Desligamento Voluntário 1`== 0,"-", `Desligamento Voluntário 1`))
Tabela3.29 <- Tabela3.29 %>% mutate(`Desligamento Voluntário 2` = ifelse(`Desligamento Voluntário 2`== 0,"-", `Desligamento Voluntário 2`))
Tabela3.29 <- Tabela3.29 %>% mutate(`Outros 1` = ifelse(`Outros 1`== 0,"-", `Outros 1`))
Tabela3.29 <- Tabela3.29 %>% mutate(`Outros 2` = ifelse(`Outros 2`== 0,"-", `Outros 2`))
Tabela3.29 <- Tabela3.29 %>% mutate(`Total 1S` = ifelse(`Total 1S`== 0,"-", `Total 1S`))
Tabela3.29 <- Tabela3.29 %>% mutate(`Total 2S` = ifelse(`Total 2S`== 0,"-", `Total 2S`))

row.names(Tabela3.29) <- Tabela3.29_rownames

rm(D1, D2, M2, md, Nomes_2, totais, Total_Geral, Tabela3.29_rownames)

# Tabela 3.30 Evolução Ingressantes -------------------------------------------
Tabela3.30 <- EvoIngD <- read_excel("dados_doutorado/EvoIngD.xlsx")

Ing2019 <- Doc %>% filter(`Ano Ingresso Opcao`== anobase) %>% group_by(`Nome Curso`, Unidade) %>% summarise(`2019`=n())

totais <- Ing2019 %>% group_by(Unidade) %>% summarise(`2019`=sum(`2019`))
totais <- totais %>% left_join(label_unidade)

colnames(totais)[3] <- "Nome Curso"


Ing2019$Unidade <- sapply(Ing2019$Unidade, function(x) paste(x, "-", collapse=""))
Ing2019 <- bind_rows(Ing2019,totais) %>% arrange(Unidade, `Nome Curso`)


colnames(Ing2019)[1] <- "Unidade Acadêmica/Curso"

Tabela3.30 <- Tabela3.30[-c(99),] #verificar nos anos posteriores a localização da linha Total Geral
Tabela3.30 <- full_join(Tabela3.30, Ing2019) 

Tabela3.30[42,6] <- "FT -"
Tabela3.30[48,6] <- "FT -"
Tabela3.30[87,6] <- "IP -"
Tabela3.30[66,6] <- "Ida -"
Tabela3.30[74,6] <- "IG -"
Tabela3.30[95,6] <- "IQ -"
Tabela3.30[98,6] <- "IREL -" #Tem que confirmar se esse curso pertence ao IREL


Tabela3.30 <- Tabela3.30 %>% arrange(Unidade, `Unidade Acadêmica/Curso`) 
Tabela3.30_rownames <- Tabela3.30$`Unidade Acadêmica/Curso`
Tabela3.30 <- Tabela3.30 %>% select(-`Unidade Acadêmica/Curso`, - Unidade)

Tabela3.30 <- Tabela3.30 %>% mutate_if(is.character,as.numeric)
Tabela3.30 <- Tabela3.30 %>% mutate_if(is.integer,as.numeric)

Tabela3.30[is.na(Tabela3.30)] <- 0


Total_2019 <- round(colSums(Tabela3.30, na.rm =T)/2)
Tabela3.30 <- bind_rows(Tabela3.30,Total_2019)
Tabela3.30_rownames <- c(Tabela3.30_rownames, "Total Geral")
row.names(Tabela3.30) <- Tabela3.30_rownames

rm(Ing2019, totais, Total_2019, Tabela3.30_rownames)



# Tabela 3.31 Evolução Alunos Registrados ----------------------------------------
Tabela3.31 <- EvoForD <- read_excel("dados_doutorado/EvoRegD.xlsx")

Reg2019 <- Doc %>% filter(`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>% group_by(`Nome Curso`, Unidade) %>% summarise(`2019`=n())

totais <- Reg2019 %>% group_by(Unidade) %>% summarise(`2019`=sum(`2019`))
totais <- totais %>% left_join(label_unidade)

colnames(totais)[3] <- "Nome Curso"


Reg2019$Unidade <- sapply(Reg2019$Unidade, function(x) paste(x, "-", collapse=""))
Reg2019 <- bind_rows(Reg2019,totais) %>% arrange(Unidade, `Nome Curso`)


colnames(Reg2019)[1] <- "Unidade Acadêmica/Curso"

Tabela3.31 <- Tabela3.31[-c(102),] 
Tabela3.31 <- full_join(Tabela3.31, Reg2019) 

Tabela3.31[87,6] <- "IP -"
Tabela3.31[88,6] <- "IP -"
Tabela3.31[89,6] <- "IP -"
Tabela3.31[66,6] <- "Ida -"
Tabela3.31[74,6] <- "IG -"
Tabela3.31[101,6] <- "IREL -" #Tem que confirmar se esse curso pertence ao IREL


Tabela3.31 <- Tabela3.31 %>% arrange(Unidade, `Unidade Acadêmica/Curso`) 
Tabela3.31_rownames <- Tabela3.31$`Unidade Acadêmica/Curso`
Tabela3.31 <- Tabela3.31 %>% select(-`Unidade Acadêmica/Curso`, - Unidade)

Tabela3.31 <- Tabela3.31 %>% mutate_if(is.character,as.numeric)
Tabela3.31 <- Tabela3.31 %>% mutate_if(is.integer,as.numeric)

Tabela3.31[is.na(Tabela3.31)] <- 0


Total_2019 <- ceiling(colSums(Tabela3.31, na.rm =T)/2)
Tabela3.31 <- bind_rows(Tabela3.31,Total_2019)

Tabela3.31_rownames <- c(Tabela3.31_rownames, "Total Geral")

row.names(Tabela3.31) <- Tabela3.31_rownames

rm(Reg2019, totais, Total_2019, Tabela3.31_rownames)

# Tabela 3.32 Evolução Formados ----------------------------------------------------------------------

Tabela3.32 <- EvoForD <- read_excel("dados_doutorado/EvoForD.xlsx")

For2019 <- Doc %>% filter(`Ano Saida Opcao`==anobase & (`For. Saida Opcao`=="Formatura Pos-Graduacao" | `For. Saida Opcao`=="Formatura com Especializacao")) %>% group_by(`Nome Curso`, Unidade) %>% summarise(`2019`=n())

totais <- For2019 %>% group_by(Unidade) %>% summarise(`2019`=sum(`2019`))
totais <- totais %>% left_join(label_unidade)

colnames(totais)[3] <- "Nome Curso"


For2019$Unidade <- sapply(For2019$Unidade, function(x) paste(x, "-", collapse=""))
For2019 <- bind_rows(For2019,totais) %>% arrange(Unidade, `Nome Curso`)


colnames(For2019)[1] <- "Unidade Acadêmica/Curso"

Tabela3.32 <- Tabela3.32[-c(94),] 
Tabela3.32 <- full_join(Tabela3.32, For2019) 

Tabela3.32[63,6] <- "Ida -"
Tabela3.32[70,6] <- "IG -"
Tabela3.32[82,6] <- "IP -"
Tabela3.32[83,6] <- "IP -"
Tabela3.32[93,6] <- "IREL -" #Tem que confirmar se esse curso pertence ao IREL

Tabela3.32 <- Tabela3.32 %>% arrange(Unidade, `Unidade Acadêmica/Curso`) 
Tabela3.32_rownames <- Tabela3.32$`Unidade Acadêmica/Curso`
Tabela3.32 <- Tabela3.32 %>%  select(-`Unidade Acadêmica/Curso`, - Unidade)

Tabela3.32 <- Tabela3.32 %>% mutate_if(is.character,as.numeric)
Tabela3.32 <- Tabela3.32 %>% mutate_if(is.integer,as.numeric)

Tabela3.32[is.na(Tabela3.32)] <- 0


Total_2019 <- round(colSums(Tabela3.32, na.rm =T)/2)
Tabela3.32 <- bind_rows(Tabela3.32,Total_2019)

Tabela3.32_rownames <- c(Tabela3.32_rownames, "Total Geral")

row.names(Tabela3.32) <- Tabela3.32_rownames

#Retirar cursos com mais de 5 anos sem dados
#Tabela3.32 <- Tabela3.32[-99,] # Não rodei essa linha, porque, embora haja mais de 5 anos sem dados de diplomados, em 2015, houve ingressante e registrados nesse curso

rm(For2019, totais, Total_2019, Tabela3.32_rownames)



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

totais <- Tabela3.06 %>% group_by(Unidade) %>% select(-"Nome Curso") %>%  summarize_all(sum, na.rm = TRUE) %>% left_join(label_unidade, by = "Unidade")
colnames(totais)[9] <- "Nome Curso"

Tabela3.06$Unidade <- sapply(Tabela3.06$Unidade, function(x) paste(x, "-", collapse=""))

Tabela3.06 <-  bind_rows(Tabela3.06, totais) %>% arrange(Unidade, `Nome Curso`)

Tabela3.06_rownames <- Tabela3.06$`Nome Curso`
Tabela3.06 <- Tabela3.06 %>% select(-`Nome Curso`, -`Unidade`)

Total_Geral <- round(colSums(Tabela3.06, na.rm=T)/2)

Tabela3.06 <- rbind(Tabela3.06, Total_Geral)
Tabela3.06_rownames <- c(Tabela3.06_rownames, "Total Geral")

row.names(Tabela3.06) <- Tabela3.06_rownames

#Tabela 3.06.2

Tabela3.06.2 <- totais %>% arrange(desc(Total)) %>% select(1,8)

Tabela3.06.2_rownames <- Tabela3.06.2$Unidade
Tabela3.06.2 <- Tabela3.06.2 %>% select(-`Unidade`)
row.names(Tabela3.06.2) <- Tabela3.06.2_rownames

rm(D2, M2, md, totais, Total_Geral, Tabela3.06_rownames, Tabela3.06.2_rownames)

### exportei apenas esta tabela pois estava errada
### falta formatá-la para o arquivo final
#rio::export(list(Tabela3.06, Tabela3.06.2), "Tabela3.06.xlsx", row.names = TRUE)

# Tabela 3.07 Mestrado e Doutorado por Curso ------------------------------

Dout <- Doc %>% filter(`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>% 
  group_by(Unidade) %>% summarise(Cursos_D=n_distinct(`Nome Curso`), Alunos_D=n())


Mestr <- Mest %>% filter(`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>% 
  group_by(Unidade) %>% summarise(Cursos_M=n_distinct(`Nome Curso`), Alunos_M=n())

Tabela3.07 <- left_join(Mestr, Dout, by = "Unidade")
Tabela3.07[is.na(Tabela3.07)] <- 0

Tabela3.07$`Total Alunos` <- rowSums(Tabela3.07[,c(3,5)])

Tabela3.07 <- left_join(Tabela3.07, label_unidade)

colnames(Tabela3.07)[7] <- "Nome Curso"

Tabela3.07_rownames <- Tabela3.07$`Nome Curso`
Tabela3.07 <- Tabela3.07 %>% select(-`Nome Curso`, -`Unidade`)

Total_Geral <- round(as.numeric(colSums(Tabela3.07, na.rm=T)),0)

Tabela3.07 <- rbind(Tabela3.07, Total_Geral)
Tabela3.07_rownames <- c(Tabela3.07_rownames, "Total Geral")

row.names(Tabela3.07) <- Tabela3.07_rownames

rm(Doc, Dout, Mest, Mestr, Total_Geral, Tabela3.07_rownames)

# Limpeza remanescente

rm(Completo, EvoForD, EvoIngD, label_unidade, Nomes, anobase)

# Excel -------------------------------------------------------------------

#Mudei para uma programação que me sinto mais confortável na hora de salvar em excel


list_of_datasets <- list("Tabela 3.06" = Tabela3.06,
                         "Tabela 3.06.2" = Tabela3.06.2,
                         "Tabela 3.07" = Tabela3.07,
                         "Tabela 3.23" = Tabela3.23,
                         "Tabela 3.23.2" = Tabela3.23.2,
                         "Tabela 3.24" = Tabela3.24,
                         "Tabela 3.25" = Tabela3.25,
                         "Tabela 3.26" = Tabela3.26,
                         "Tabela 3.27" = Tabela3.27,
                         "Tabela 3.27.2" = Tabela3.27.2,
                         "Tabela 3.28" = Tabela3.28,
                         "Tabela 3.29" = Tabela3.29,
                         "Tabela 3.30" = Tabela3.30,
                         "Tabela 3.31" = Tabela3.31,
                         "Tabela 3.32" = Tabela3.32)
                         
                         

write.xlsx(list_of_datasets, file = "Tabelas_Doutorado_Anuário_2019.xlsx", row.names=T)

