# Setup -------------------------------------------------------------------

options(OutDec = ",")

library(tidyverse)
# library(lubridate)
# library(data.table)
# library(dataframes2xls)
# library(openxlsx)
# library(readxl)

load("dados_identificados/Completo14042020.RData")
load("labels/Label_Unidades_pos.RData")

colnames(label_unidade)[2] <- "Nome Curso"
Label_Unidades <- label_unidade

rm(label_unidade)

### define ano do Anuário

anobase <- 2019


# Acerta Pais: Roberto: 22/10/2020

Completo <- within(Completo, {
  Pais[Pais==22|Pais==022|Pais==0|Pais==00|Pais==000] <-"BRA"
  Pais[Pais==8] <-"AGO"
  Pais[Pais==3] <-"ALB"
  Pais[Pais==153] <-"ARE"
  Pais[Pais==11] <-"ARG"
  Pais[Pais==225] <-"ARM"
  Pais[Pais==12] <-"AUS"
  Pais[Pais==13] <-"AUT"
  Pais[Pais==18] <-"BEL"
  Pais[Pais==164] <-"BEN"
  Pais[Pais==178] <-"BFA"
  Pais[Pais==23] <-"BGR"
  Pais[Pais==219] <-"BLR"
  Pais[Pais==20] <-"BOL"
  Pais[Pais==29] <-"CAN"
  Pais[Pais==142] <-"CHE"
  Pais[Pais==31] <-"CHL"
  Pais[Pais==32|Pais==168] <-"CHN"
  Pais[Pais==41] <-"CIV"
  Pais[Pais==27] <-"CMR"
  Pais[Pais==159] <-"COD" #Nos labels ta Zaire mas virou Rep. Dem. Congo
  Pais[Pais==226] <-"COD" 
  Pais[Pais==38] <-"COG"
  Pais[Pais==37] <-"COL"
  Pais[Pais==26] <-"CPV"
  Pais[Pais==42] <-"CRI"
  Pais[Pais==43] <-"CUB"
  Pais[Pais==214] <-"CZE"
  Pais[Pais==4 | Pais==5 | Pais == "Alemanha"] <-"DEU"
  Pais[Pais==126] <-"DOM"
  Pais[Pais==45] <-"DNK"
  Pais[Pais==10] <-"DZA"
  Pais[Pais==49] <-"ECU"
  Pais[Pais==50] <-"ESP"
  Pais[Pais==53] <-"FJI"
  Pais[Pais==55] <-"FIN"
  Pais[Pais==56] <-"FRA"
  Pais[Pais==162 | Pais==161 | Pais==175 | Pais==176] <-"GBR"
  Pais[Pais==59] <-"GHA"
  Pais[Pais==64] <-"GIN"
  Pais[Pais==65] <-"GNB"
  Pais[Pais==61] <-"GRC"
  Pais[Pais==62] <-"GTM"
  Pais[Pais==63] <-"GUY"
  Pais[Pais==194] <-"HKG"
  Pais[Pais==69] <-"HND"
  Pais[Pais==67] <-"HTI"
  Pais[Pais==70] <-"HUN"
  Pais[Pais==76] <-"IND"
  Pais[Pais==78] <-"IRN"
  Pais[Pais==79] <-"IRQ"
  Pais[Pais==83] <-"ITA"
  Pais[Pais==85] <-"JAM"
  Pais[Pais==87] <-"JOR"
  Pais[Pais==86] <-"JPN"
  Pais[Pais==223] <-"KAZ"
  Pais[Pais==124] <-"KEN"
  Pais[Pais==40] <-"KOR"
  Pais[Pais==91] <-"LBN"
  Pais[Pais==158] <-"LBY"
  Pais[Pais==101] <-"MAR"
  Pais[Pais==106] <-"MCO"
  Pais[Pais==104] <-"MEX"
  Pais[Pais==99] <-"MLI"
  Pais[Pais==105] <-"MOZ"
  Pais[Pais==96] <-"MYS"
  Pais[Pais==68] <-"NLD"
  Pais[Pais==112] <-"NGA"
  Pais[Pais==110] <-"NIC"
  Pais[Pais==115] <-"OMN"
  Pais[Pais==118] <-"PAK"
  Pais[Pais==120] <-"PER"
  Pais[Pais==54] <-"PHL"
  Pais[Pais==121] <-"POL"
  Pais[Pais==205] <-"PRI"
  Pais[Pais==39] <-"PRK"
  Pais[Pais==122] <-"PRT"
  Pais[Pais==119] <-"PRY"
  Pais[Pais==128] <-"ROU"
  Pais[Pais==206 | Pais==154] <-"RUS"
  Pais[Pais==9] <-"SAU"
  Pais[Pais==140] <-"SDN"
  Pais[Pais==133] <-"SEN"
  Pais[Pais==36] <-"SGP"
  Pais[Pais==48] <-"SLV"
  Pais[Pais==220] <-"SRB"
  Pais[Pais==132] <-"STP"
  Pais[Pais==143] <-"SUR"
  Pais[Pais==215] <-"SVK"
  Pais[Pais==141] <-"SWE"
  Pais[Pais==136] <-"SYR"
  Pais[Pais==147] <-"TGO"
  Pais[Pais==210 | Pais==211] <-"TLS"
  Pais[Pais==149] <-"TTO"
  Pais[Pais==151] <-"TUR"
  Pais[Pais==155] <-"URY"
  Pais[Pais==51] <-"USA"
  Pais[Pais==213 | Pais==218] <-"UKR"
  Pais[Pais==222] <-"UZB"
  Pais[Pais==156] <-"VEN"
  Pais[Pais==157] <-"VNM"
  Pais[Pais==2] <-"ZAF"
  Pais[Pais==174] <-"ZWE"
  Pais[Pais==107] <-"MNG"
  Pais[Pais==144] <-"THA"
  Pais[Pais==16] <-"BGD"
  Pais[Pais==47] <-"EGY"
  Pais[Pais=="Rússia"] <-"RUS"
  Pais[Pais=="Reino Unido"] <-"GBR"
})

Completo <- within(Completo, {
  Continente [Pais=="AGO"] <- "África"
  Continente [Pais=="ATA"] <- "África"
  Continente [Pais=="BFA"] <- "África"
  Continente [Pais=="BDI"] <- "África"
  Continente [Pais=="BEN"] <- "África"
  Continente [Pais=="BVT"] <- "África"
  Continente [Pais=="BWA"] <- "África"
  Continente [Pais=="COD"] <- "África"
  Continente [Pais=="CAF"] <- "África"
  Continente [Pais=="COG"] <- "África"
  Continente [Pais=="CIV"] <- "África"
  Continente [Pais=="CMR"] <- "África"
  Continente [Pais=="CPV"] <- "África"
  Continente [Pais=="DJI"] <- "África"
  Continente [Pais=="DZA"] <- "África"
  Continente [Pais=="EGY"] <- "África"
  Continente [Pais=="ESH"] <- "África"
  Continente [Pais=="ERI"] <- "África"
  Continente [Pais=="ETH"] <- "África"
  Continente [Pais=="GAB"] <- "África"
  Continente [Pais=="GHA"] <- "África"
  Continente [Pais=="GMB"] <- "África"
  Continente [Pais=="GIN"] <- "África"
  Continente [Pais=="GNQ"] <- "África"
  Continente [Pais=="SGS"] <- "África"
  Continente [Pais=="GNB"] <- "África"
  Continente [Pais=="HMD"] <- "África"
  Continente [Pais=="KEN"] <- "África"
  Continente [Pais=="COM"] <- "África"
  Continente [Pais=="LBR"] <- "África"
  Continente [Pais=="LSO"] <- "África"
  Continente [Pais=="LBY"] <- "África"
  Continente [Pais=="MAR"] <- "África"
  Continente [Pais=="MDG"] <- "África"
  Continente [Pais=="MLI"] <- "África"
  Continente [Pais=="MRT"] <- "África"
  Continente [Pais=="MUS"] <- "África"
  Continente [Pais=="MWI"] <- "África"
  Continente [Pais=="MOZ"] <- "África"
  Continente [Pais=="NAM"] <- "África"
  Continente [Pais=="NER"] <- "África"
  Continente [Pais=="NGA"] <- "África"
  Continente [Pais=="REU"] <- "África"
  Continente [Pais=="RWA"] <- "África"
  Continente [Pais=="SYC"] <- "África"
  Continente [Pais=="SDN"] <- "África"
  Continente [Pais=="SHN"] <- "África"
  Continente [Pais=="SLE"] <- "África"
  Continente [Pais=="SEN"] <- "África"
  Continente [Pais=="SOM"] <- "África"
  Continente [Pais=="SSD"] <- "África"
  Continente [Pais=="STP"] <- "África"
  Continente [Pais=="SWZ"] <- "África"
  Continente [Pais=="TCD"] <- "África"
  Continente [Pais=="ATF"] <- "África"
  Continente [Pais=="TGO"] <- "África"
  Continente [Pais=="TUN"] <- "África"
  Continente [Pais=="TZA"] <- "África"
  Continente [Pais=="UGA"] <- "África"
  Continente [Pais=="MYT"] <- "África"
  Continente [Pais=="ZAF"] <- "África"
  Continente [Pais=="ZMB"] <- "África"
  Continente [Pais=="ZWE"] <- "África"
  Continente [Pais=="ATG"] <- "América"
  Continente [Pais=="AIA"] <- "América"
  Continente [Pais=="ARG"] <- "América"
  Continente [Pais=="ABW"] <- "América"
  Continente [Pais=="BRB"] <- "América"
  Continente [Pais=="BLM"] <- "América"
  Continente [Pais=="BMU"] <- "América"
  Continente [Pais=="BOL"] <- "América"
  Continente [Pais=="BRA"] <- "América"
  Continente [Pais=="BHS"] <- "América"
  Continente [Pais=="BLZ"] <- "América"
  Continente [Pais=="CAN"] <- "América"
  Continente [Pais=="CHL"] <- "América"
  Continente [Pais=="COL"] <- "América"
  Continente [Pais=="CRI"] <- "América"
  Continente [Pais=="CUB"] <- "América"
  Continente [Pais=="DMA"] <- "América"
  Continente [Pais=="DOM"] <- "América"
  Continente [Pais=="ECU"] <- "América"
  Continente [Pais=="FLK"] <- "América"
  Continente [Pais=="GRD"] <- "América"
  Continente [Pais=="GUF"] <- "América"
  Continente [Pais=="GRL"] <- "América"
  Continente [Pais=="GLP"] <- "América"
  Continente [Pais=="GTM"] <- "América"
  Continente [Pais=="GUY"] <- "América"
  Continente [Pais=="HND"] <- "América"
  Continente [Pais=="HTI"] <- "América"
  Continente [Pais=="JAM"] <- "América"
  Continente [Pais=="KNA"] <- "América"
  Continente [Pais=="CYM"] <- "América"
  Continente [Pais=="LCA"] <- "América"
  Continente [Pais=="MAF"] <- "América"
  Continente [Pais=="MTQ"] <- "América"
  Continente [Pais=="MSR"] <- "América"
  Continente [Pais=="MEX"] <- "América"
  Continente [Pais=="NIC"] <- "América"
  Continente [Pais=="PAN"] <- "América"
  Continente [Pais=="PER"] <- "América"
  Continente [Pais=="SPM"] <- "América"
  Continente [Pais=="PRI"] <- "América"
  Continente [Pais=="PRY"] <- "América"
  Continente [Pais=="SUR"] <- "América"
  Continente [Pais=="SLV"] <- "América"
  Continente [Pais=="TCA"] <- "América"
  Continente [Pais=="TTO"] <- "América"
  Continente [Pais=="USA"] <- "América"
  Continente [Pais=="URY"] <- "América"
  Continente [Pais=="VCT"] <- "América"
  Continente [Pais=="VEN"] <- "América"
  Continente [Pais=="VGB"] <- "América"
  Continente [Pais=="VIR"] <- "América"
  Continente [Pais=="ARE"] <- "Ásia"
  Continente [Pais=="AFG"] <- "Ásia"
  Continente [Pais=="ARM"] <- "Ásia"
  Continente [Pais=="AZE"] <- "Ásia"
  Continente [Pais=="BGD"] <- "Ásia"
  Continente [Pais=="BHR"] <- "Ásia"
  Continente [Pais=="BRN"] <- "Ásia"
  Continente [Pais=="BTN"] <- "Ásia"
  Continente [Pais=="CCK"] <- "Ásia"
  Continente [Pais=="CHN"] <- "Ásia"
  Continente [Pais=="CUW"] <- "Ásia"
  Continente [Pais=="CYP"] <- "Ásia"
  Continente [Pais=="GEO"] <- "Ásia"
  Continente [Pais=="HKG"] <- "Ásia"
  Continente [Pais=="IDN"] <- "Ásia"
  Continente [Pais=="ISR"] <- "Ásia"
  Continente [Pais=="IND"] <- "Ásia"
  Continente [Pais=="IOT"] <- "Ásia"
  Continente [Pais=="IRQ"] <- "Ásia"
  Continente [Pais=="IRN"] <- "Ásia"
  Continente [Pais=="JOR"] <- "Ásia"
  Continente [Pais=="JPN"] <- "Ásia"
  Continente [Pais=="KGZ"] <- "Ásia"
  Continente [Pais=="KHM"] <- "Ásia"
  Continente [Pais=="PRK"] <- "Ásia"
  Continente [Pais=="KOR"] <- "Ásia"
  Continente [Pais=="KWT"] <- "Ásia"
  Continente [Pais=="KAZ"] <- "Ásia"
  Continente [Pais=="LAO"] <- "Ásia"
  Continente [Pais=="LBN"] <- "Ásia"
  Continente [Pais=="LKA"] <- "Ásia"
  Continente [Pais=="MMR"] <- "Ásia"
  Continente [Pais=="MNG"] <- "Ásia"
  Continente [Pais=="MAC"] <- "Ásia"
  Continente [Pais=="MDV"] <- "Ásia"
  Continente [Pais=="MYS"] <- "Ásia"
  Continente [Pais=="NPL"] <- "Ásia"
  Continente [Pais=="OMN"] <- "Ásia"
  Continente [Pais=="PHL"] <- "Ásia"
  Continente [Pais=="PAK"] <- "Ásia"
  Continente [Pais=="PSE"] <- "Ásia"
  Continente [Pais=="QAT"] <- "Ásia"
  Continente [Pais=="SAU"] <- "Ásia"
  Continente [Pais=="SGP"] <- "Ásia"
  Continente [Pais=="SYR"] <- "Ásia"
  Continente [Pais=="THA"] <- "Ásia"
  Continente [Pais=="TJK"] <- "Ásia"
  Continente [Pais=="TLS"] <- "Ásia"
  Continente [Pais=="TKM"] <- "Ásia"
  Continente [Pais=="TWN"] <- "Ásia"
  Continente [Pais=="UZB"] <- "Ásia"
  Continente [Pais=="VNM"] <- "Ásia"
  Continente [Pais=="YEM"] <- "Ásia"
  Continente [Pais=="AND"] <- "Europa"
  Continente [Pais=="ALB"] <- "Europa"
  Continente [Pais=="AUT"] <- "Europa"
  Continente [Pais=="ALA"] <- "Europa"
  Continente [Pais=="BIH"] <- "Europa"
  Continente [Pais=="BEL"] <- "Europa"
  Continente [Pais=="BGR"] <- "Europa"
  Continente [Pais=="BLR"] <- "Europa"
  Continente [Pais=="CHE"] <- "Europa"
  Continente [Pais=="CZE"] <- "Europa"
  Continente [Pais=="DEU"] <- "Europa"
  Continente [Pais=="DNK"] <- "Europa"
  Continente [Pais=="EST"] <- "Europa"
  Continente [Pais=="ESP"] <- "Europa"
  Continente [Pais=="FIN"] <- "Europa"
  Continente [Pais=="FRO"] <- "Europa"
  Continente [Pais=="FRA"] <- "Europa"
  Continente [Pais=="GBR"] <- "Europa"
  Continente [Pais=="GGY"] <- "Europa"
  Continente [Pais=="GIB"] <- "Europa"
  Continente [Pais=="GRC"] <- "Europa"
  Continente [Pais=="HRV"] <- "Europa"
  Continente [Pais=="HUN"] <- "Europa"
  Continente [Pais=="IRL"] <- "Europa"
  Continente [Pais=="IMN"] <- "Europa"
  Continente [Pais=="ISL"] <- "Europa"
  Continente [Pais=="ITA"] <- "Europa"
  Continente [Pais=="JEY"] <- "Europa"
  Continente [Pais=="LIE"] <- "Europa"
  Continente [Pais=="LTU"] <- "Europa"
  Continente [Pais=="LUX"] <- "Europa"
  Continente [Pais=="LVA"] <- "Europa"
  Continente [Pais=="MCO"] <- "Europa"
  Continente [Pais=="MDA"] <- "Europa"
  Continente [Pais=="MNE"] <- "Europa"
  Continente [Pais=="MKD"] <- "Europa"
  Continente [Pais=="MLT"] <- "Europa"
  Continente [Pais=="NLD"] <- "Europa"
  Continente [Pais=="NOR"] <- "Europa"
  Continente [Pais=="POL"] <- "Europa"
  Continente [Pais=="PRT"] <- "Europa"
  Continente [Pais=="ROU"] <- "Europa"
  Continente [Pais=="SRB"] <- "Europa"
  Continente [Pais=="RUS"] <- "Europa"
  Continente [Pais=="SWE"] <- "Europa"
  Continente [Pais=="SVN"] <- "Europa"
  Continente [Pais=="SJM"] <- "Europa"
  Continente [Pais=="SVK"] <- "Europa"
  Continente [Pais=="SMR"] <- "Europa"
  Continente [Pais=="TUR"] <- "Europa"
  Continente [Pais=="UKR"] <- "Europa"
  Continente [Pais=="VAT"] <- "Europa"
  Continente [Pais=="ASM"] <- "Oceania"
  Continente [Pais=="AUS"] <- "Oceania"
  Continente [Pais=="COK"] <- "Oceania"
  Continente [Pais=="FJI"] <- "Oceania"
  Continente [Pais=="FSM"] <- "Oceania"
  Continente [Pais=="GUM"] <- "Oceania"
  Continente [Pais=="KIR"] <- "Oceania"
  Continente [Pais=="MHL"] <- "Oceania"
  Continente [Pais=="MNP"] <- "Oceania"
  Continente [Pais=="NCL"] <- "Oceania"
  Continente [Pais=="NFK"] <- "Oceania"
  Continente [Pais=="NRU"] <- "Oceania"
  Continente [Pais=="NIU"] <- "Oceania"
  Continente [Pais=="NZL"] <- "Oceania"
  Continente [Pais=="PYF"] <- "Oceania"
  Continente [Pais=="PNG"] <- "Oceania"
  Continente [Pais=="PCN"] <- "Oceania"
  Continente [Pais=="PLW"] <- "Oceania"
  Continente [Pais=="SLB"] <- "Oceania"
  Continente [Pais=="TKL"] <- "Oceania"
  Continente [Pais=="TON"] <- "Oceania"
  Continente [Pais=="TUV"] <- "Oceania"
  Continente [Pais=="UMI"] <- "Oceania"
  Continente [Pais=="VUT"] <- "Oceania"
  Continente [Pais=="WLF"] <- "Oceania"
  Continente [Pais=="WSM"] <- "Oceania"
  Continente [Pais=="EUROPA"] <- "Europa"
  Continente [Pais=="ASIA"] <- "Ásia"
  Continente [Pais=="ÁSIA"] <- "Ásia"
})
#_________________________________________________________________________

Mest <- Completo %>% 
  filter(`Ano Ingresso Opcao`<= anobase,
         `Ano Saida Opcao`>= anobase,
         `Nivel Curso`=="Mestrado",
         `Forma Saida` != "Anulacao de Registro")

Pos <- Completo %>%  
  filter(`Ano Ingresso Opcao`<= anobase,
         `Ano Saida Opcao`>= anobase,
         `Nivel Curso`=="Mestrado"|`Nivel Curso`=="Doutorado",
         `Forma Saida` != "Anulacao de Registro")

### tabela de referência dos cursos e unidades ativos

Nomes <- Mest %>% 
  count(`Nome Curso`, Unidade) %>% 
  select(-n)

Nomes_Pos <- Pos %>% 
  count(`Nome Curso`, Unidade) %>% 
  select(-n)

# Tabela 3.08 Estrangeiros (Mestrado e Doutorado) -------------------------

#Roberto: 22/10/2020: Teoricamente: na tabela Tabela 45 do anuário, tem de estar listados Alunos estrangeiros regulares registrados 
# nos cursos de pós-graduação Stricto Sensu, por unidade acadêmica, continente e país. 
# Desse modo, adicionei os alunos de doutorado, aparentemente, não entravam na (programamção anterior mantida para consulta)

#Programação anterior:
#Tabela3.08 <- Mest %>% filter(Pais!="BRA") %>% filter (is.na(Pais)==F) 
#Tabela3.08.2 <- Tabela3.08 %>% group_by(Pais) %>% summarise(n=n()) %>% arrange(desc(n))

# Roberto: 22/10/2020: Alterei a escolha dos 10 primeiros para os 12 primeiros, 
# porque tanto o 10º, quanto o 11º e o 12º estavam empatados com o mesmo número de alunos

#Tabela3.08.2 <- Tabela3.08.2[1:10,]; Tabela3.08.2 <- as.data.frame(Tabela3.08.2, order=F)
#Tabela3.08.3 <- Tabela3.08 %>% group_by(Continente) %>% summarise(n=n())
#Tabela3.08.4 <- Tabela3.08 %>% group_by(Unidade) %>% summarise(n=n()) %>% arrange(desc(n))

#Tabela3.08 <- Tabela3.08 %>% 
#group_by(Unidade, Continente, Pais) %>% 
#tally() %>% 
#spread(Unidade, n)

#totais <- Tabela3.08 %>% 
#  group_by(Continente) %>%
# select(-Paises) %>% 
# summarise_all(sum, na.rm=T)

#totais$Paises <- c("Total do Continente: África", "Total do Continente: América", "Total do Continente: Ásia", "Total do Continente: Europa")

#totais$Continente <- sapply(totais$Continente, function(x) paste(x, "-", collapse=""))

#Tabela3.08 <- bind_rows(Tabela3.08,totais) %>% arrange(Continente, Paises)
#row.names(Tabela3.08) <- Tabela3.08$Paises; Tabela3.08$Paises <- NULL; Tabela3.08$Continente <- NULL

#Tabela3.08$Total <- rowSums(Tabela3.08, na.rm=T)
#Tabela3.08["Total Geral", ] <- colSums(Tabela3.08, na.rm=T)/2
#Tabela3.08[is.na(Tabela3.08)] <- 0

#rm(totais)

# Programação refeita

Tabela3.08 <- Pos %>% filter(Pais!="BRA") %>% filter(is.na(Pais)==F) 
Tabela3.08.2 <- Tabela3.08 %>% group_by(Pais) %>% summarise(n=n()) %>% arrange(desc(n))

# Roberto: 22/10/2020: Alterei a escolha dos 10 primeiros para os 11 primeiros, porque tanto o 10º, 
# quanto o 11º  estavam empatados com o mesmo número de alunos

Tabela3.08.2 <- Tabela3.08.2[1:11,]; Tabela3.08.2 <- as.data.frame(Tabela3.08.2, order=F)
rownames(Tabela3.08.2) <- Tabela3.08.2$Pais; Tabela3.08.2$Pais <- NULL

Tabela3.08.3 <- Tabela3.08 %>% group_by(Continente) %>% summarise(n=n())
rownames_Tabela3.08.3 <- Tabela3.08.3$Continente 
Tabela3.08.3$Continente <- NULL
row.names(Tabela3.08.3) <- rownames_Tabela3.08.3

Tabela3.08.4 <- Tabela3.08 %>% group_by(Unidade) %>% summarise(n=n()) %>% arrange(desc(n))
rownames_Tabela3.08.4 <- Tabela3.08.4$Unidade 
Tabela3.08.4$Unidade <- NULL
row.names(Tabela3.08.4) <- rownames_Tabela3.08.4

Tabela3.08 <- Tabela3.08 %>% 
  group_by(Unidade, Continente, Pais) %>% 
  tally() %>% 
  spread(Unidade, n)

totais <- Tabela3.08 %>% 
  group_by(Continente) %>%
  select(-Pais) %>% 
  summarise_all(sum, na.rm=T)

totais$Pais <- c("Total do Continente: África", "Total do Continente: América", "Total do Continente: Ásia", "Total do Continente: Europa")
totais$Continente <- sapply(totais$Continente, function(x) paste(x, "-", collapse=""))

totais <- totais %>% select(1,31,c(2:30)) 

Tabela3.08 <- bind_rows(Tabela3.08, totais) %>% arrange(Continente, Pais)

Tabela3.08_rownames <- Tabela3.08$Pais; 
Tabela3.08$Pais <- NULL; 
Tabela3.08$Continente <- NULL

Tabela3.08$Total <- rowSums(Tabela3.08, na.rm=T)
Tabela3.08_TotalGeral <- colSums(Tabela3.08, na.rm=T)/2

Tabela3.08 <- bind_rows(Tabela3.08, Tabela3.08_TotalGeral)
Tabela3.08_rownames <- c(Tabela3.08_rownames, "Total Geral")

Tabela3.08[is.na(Tabela3.08)==T] <- 0
row.names(Tabela3.08) <- Tabela3.08_rownames

rm(totais, Tabela3.08_rownames, Tabela3.08_TotalGeral, Pos, Nomes_Pos, rownames_Tabela3.08.3, rownames_Tabela3.08.4)


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
Tabela3.12_rownames <- Tabela3.12$`Nome Curso`; Tabela3.12$`Nome Curso` <- NULL; Tabela3.12$Unidade <- NULL

Tabela3.12$`Total Ing` <- rowSums(Tabela3.12[,c("Ing1_Total", "Ing2_Total")], na.rm = TRUE)
Tabela3.12$`Total For` <- rowSums(Tabela3.12[,c("For1_Total", "For2_Total")], na.rm = TRUE)

Tabela3.12 <- Tabela3.12 %>% 
  ungroup() %>% 
  select(Ing1_Feminino, Ing1_Masculino, Ing1_Total,
         Ing2_Feminino, Ing2_Masculino, Ing2_Total, `Total Ing`,
         For1_Feminino, For1_Masculino, For1_Total,
         For2_Feminino, For2_Masculino, For2_Total, `Total For`)

Tabela3.12_TotalGeral <- colSums(Tabela3.12, na.rm=T)/2
Tabela3.12[is.na(Tabela3.12)] <- 0
Tabela3.12 <- rbind(Tabela3.12, Tabela3.12_TotalGeral)

Tabela3.12_rownames <- c(Tabela3.12_rownames, "Total Geral")

row.names(Tabela3.12) <- Tabela3.12_rownames

### tabela de ingressantes e concluintes consolidados
Ing <- Mest %>% filter(`Ano Ingresso Opcao`==anobase) %>% group_by(Unidade) %>% summarise(Ingressantes=n()) %>% arrange(desc(Ingressantes))
For <- Mest %>% filter(`Ano Saida Opcao`==anobase & (`For. Saida Opcao`=="Formatura Pos-Graduacao" | 
                                                       `For. Saida Opcao`=="Formatura com Especializacao")) %>% 
  group_by(Unidade) %>% summarise(Concluintes=n())

Tabela3.12.2 <- full_join(Ing, For)
Tabela3.12.2_rownames <- Tabela3.12.2$Unidade
Tabela3.12.2$Unidade <- NULL
row.names(Tabela3.12.2) <- Tabela3.12.2_rownames

row.names(Tabela3.12.2) <- Tabela3.12.2_rownames

rm(Ing, Ing1, Ing2, For, For1, For2, totais, Tabela3.12_rownames, Tabela3.12_TotalGeral, Tabela3.12.2_rownames)


# Tabela 3.13 Ingressantes e concluintes por sexo e faixa etária -------------------------------------------------------------

Ing <- Mest %>% filter(`Ano Ingresso Opcao`==anobase)

### atentar para a data escolhida
### deve ser o início do ano base do CENSO
Ing$Idade <- difftime(as.Date("2019-01-01"), Ing$Nascimento)/365

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

Ing_rownames <- Ing$`Faixa Etária`

Ing <- Ing %>% select(Feminino, `% Feminino`, Masculino, `% Masculino`, Total)
For <- Mest %>% filter(`Ano Saida Opcao`==anobase & str_detect(`For. Saida Opcao`, "Formatura"))

### atentar para a data escolhida
### deve ser o início do ano base do CENSO
For$Idade <- difftime(as.Date("2019-01-01"), For$Nascimento)/365

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

For_rownames <- For$`Faixa Etária`

For <- For %>% select(Feminino, `% Feminino`, Masculino, `% Masculino`, Total)

Tabela3.13 <- cbind(Ing,For); rm(Ing,For)
colnames(Tabela3.13) <- c("Fem. Ing", "% Fem. Ing", "Masc. Ing", "% Masc. Ing", "Tot. Ing", "Fem. For", "% Fem. For", "Masc. For", "% Masc. For", "Tot. For" )
row.names(Tabela3.13) <- Ing_rownames

rm(Ing_rownames, For_rownames)

# Tabela 3.14 Alunos Regulares Ativos, Matrículas e Aprovados em Disc --------

# MUDAR NOME DAS VARIAVEIS PARA HE0 HE1 HE2

HE0 <- read_fwf("dados_identificados/he20190.txt", 
                fwf_widths(c(9,5,6,2,3,2,3), 
                           col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20191", "Frequencias")), 
                locale = locale(encoding = "latin1"))

HE1 <- read_fwf("dados_identificados/he20191.txt", 
                fwf_widths(c(9,5,6,2,3,2,3), 
                           col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20191", "Frequencias")), 
                locale = locale(encoding = "latin1"))

HE2 <- read_fwf("dados_identificados/he20192.txt", 
                fwf_widths(c(9,5,6,2,3,2,3), 
                           col_names = c("MatricAluno", "Periodo", "CodDisciplina", "Turma", "Credits", "Mencao_20192", "Frequencias")), 
                locale = locale(encoding = "latin1"))

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

t <- HE2 %>% 
  group_by(MatricAluno) %>%
  mutate(dup = n())

M1 <- Mest %>% filter(is.na(TGM20191),
                      ((`Ano Ingresso Opcao`< anobase | 
                          (`Ano Ingresso Opcao`== anobase & `Semestre Ingresso Opcao`<=1)) & `Ano Saida Opcao`>= anobase))

AA1 <- M1 %>% group_by(`Nome Curso`) %>% summarise(`Alunos Ativos 1`=n())

M1 <- inner_join(M1, HE1, "MatricAluno")
MD1 <- M1 %>% group_by(`Nome Curso`) %>% summarise(`Matriculados em Disciplinas 1`=n())
AP1 <- M1 %>% 
  count(`Nome Curso`, Mencao_20191) %>% 
  spread(Mencao_20191, n, fill = 0)

M2 <- Mest %>% filter(is.na(TGM20192),
                      (`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`> anobase | 
                                                           (`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`== 2))))

AA2 <- M2 %>% group_by(`Nome Curso`) %>% summarise(`Alunos Ativos 2`=n())

M2 <- inner_join(M2,HE2, "MatricAluno")

MD2 <- M2 %>% group_by(`Nome Curso`) %>% summarise(`Matriculados em Disciplinas 2`=n())
AP2 <- M2 %>% 
  count(`Nome Curso`, Mencao_20192) %>% 
  spread(Mencao_20192, n, fill = 0)

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

Tabela3.14_rownames <- Tabela3.14$`Nome Curso`; Tabela3.14$`Nome Curso` <- NULL; Tabela3.14$Unidade <- NULL

Tabela3.14 <- Tabela3.14 %>% 
  ungroup() %>% 
  select(`Alunos Ativos 1`, `Alunos Ativos 2`, `Matriculados em Disciplinas 1`, `Matriculados em Disciplinas 2`,
         `AP 1`, `AP 2`, `RP 1`, `RP 2`)

Tabela3.14_TotalGeral <- colSums(Tabela3.14, na.rm=T)/2
Tabela3.14 <- bind_rows(Tabela3.14,Tabela3.14_TotalGeral)

Tabela3.14_rownames <- c(Tabela3.14_rownames,"Total Geral")

Tabela3.14[is.na(Tabela3.14)] <- 0

Tabela3.14$`RP 1` <- Tabela3.14$`AP 1`/(Tabela3.14$`RP 1` + Tabela3.14$`AP 1`)
Tabela3.14$`RP 1` <- paste(round(Tabela3.14$`RP 1`*100,1), "%", sep="")
Tabela3.14$`RP 2` <- Tabela3.14$`AP 2`/(Tabela3.14$`RP 2` + Tabela3.14$`AP 2`)
Tabela3.14$`RP 2` <- paste(round(Tabela3.14$`RP 2`*100,1), "%", sep="")

row.names(Tabela3.14) <- Tabela3.14_rownames

rm(totais, M1, AA1, MD1, AP1, M2, AA2, MD2, AP2)

# Tabela 3.15 Alunos Regulares por Sexo e Faixa Etária --------------------------------

Tabela3.15 <- Mest %>% filter(`Ano Ingresso Opcao`<= anobase,
                              (`Ano Saida Opcao`> anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2)))

# CONFIRMAR DATA DE CÁLCULO DA IDADE
Tabela3.15$Idade <- difftime(as.Date("2019-01-01"), Tabela3.15$Nascimento)/365

Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>18 & Tabela3.15$Idade<=24] <- "De 19 a 24 anos"
Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>24 & Tabela3.15$Idade<=29] <- "De 25 a 29 anos"
Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>29 & Tabela3.15$Idade<=34] <- "De 30 a 34 anos"
Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>34 & Tabela3.15$Idade<=39] <- "De 35 a 39 anos"
Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>39 & Tabela3.15$Idade<=44] <- "De 40 a 44 anos"
Tabela3.15$`Faixa Etária`[Tabela3.15$Idade>44] <- "De 45 anos ou mais"

Tabela3.15 <- Tabela3.15 %>% 
  count(`Faixa Etária`, Sexo) %>% 
  spread(Sexo, n)

Tabela3.15_rownames <- Tabela3.15$`Faixa Etária`
Tabela3.15$`Faixa Etária` <- NULL

Tabela3.15_TotalGeral <- colSums(Tabela3.15, na.rm=T)

Tabela3.15 <- bind_rows(Tabela3.15, Tabela3.15_TotalGeral)

Tabela3.15$Total <- rowSums(Tabela3.15)
Tabela3.15$`% Feminino` <- paste(round(Tabela3.15$Feminino/Tabela3.15$Total*100,1), "%", sep="")
Tabela3.15$`% Masculino` <- paste(round(Tabela3.15$Masculino/Tabela3.15$Total*100,1), "%", sep="")

Tabela3.15 <- Tabela3.15 %>% select(Feminino, `% Feminino`, Masculino, `% Masculino`, Total)
Tabela3.15_rownames <- c(Tabela3.15_rownames, "Total Geral")

row.names(Tabela3.15) <- Tabela3.15_rownames

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

Tabela3.16_rownames <- Tabela3.16$`Nome Curso`; Tabela3.16$`Nome Curso` <- NULL; Tabela3.16$Unidade <- NULL

Tabela3.16$`Total 1` <- rowSums(Tabela3.16[,c("Feminino 1", "Masculino 1")])
Tabela3.16$`Total 2` <- rowSums(Tabela3.16[,c("Feminino 2", "Masculino 2")])

Tabela3.16 <- Tabela3.16 %>% ungroup() %>% select(`Feminino 1`, `Masculino 1`, `Total 1`, `Feminino 2`, `Masculino 2`, `Total 2`)

Tabela3.16_TotalGeral <- colSums(Tabela3.16, na.rm=T)/2
Tabela3.16 <- bind_rows(Tabela3.16, Tabela3.16_TotalGeral)

Tabela3.16_rownames <- c(Tabela3.16_rownames, "Total Geral")

Tabela3.16.2 <- Mest %>% 
  filter(`Ano Ingresso Opcao` <= anobase & `Ano Saida Opcao` >= anobase) %>% 
  count(Unidade) %>% 
  arrange(desc(n))

rownames_Tabela3.16.2 <- Tabela3.16.2$Unidade
Tabela3.16.2$Unidade <- NULL
row.names(Tabela3.16.2) <- rownames_Tabela3.16.2

Tabela3.16[is.na(Tabela3.16)==T] <- 0
row.names(Tabela3.16) <- Tabela3.16_rownames

rm(M1, M2, totais, t, totais, rownames_Tabela3.16.2, Tabela3.14_TotalGeral, Tabela3.16_rownames, Tabela3.16_TotalGeral, Tabela3.15_rownames, Tabela3.15_TotalGeral, Tabela3.14_rownames)

# Tabela 3.17 Alunos Ativos e Trancados por Sexo ----------------------------

AA1 <- Mest %>% filter(is.na(TGM20191),
                       ((`Ano Ingresso Opcao`< anobase | (`Ano Ingresso Opcao`== anobase & 
                                                            `Semestre Ingresso Opcao`<=1)) & `Ano Saida Opcao`>= anobase)) %>%
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n, fill = 0)

names(AA1)[2:3] <- c("Feminino A1", "Masculino A1")

AT1 <- Mest %>% filter(TGM20191 == 1,
                       ((`Ano Ingresso Opcao`< anobase | (`Ano Ingresso Opcao`== anobase & 
                                                            `Semestre Ingresso Opcao`<=1)) & `Ano Saida Opcao`>= anobase)) %>%
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n, fill = 0)

names(AT1)[2:3] <- c("Feminino T1", "Masculino T1")

AA2 <- Mest %>% filter(is.na(TGM20192),
                       (`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`> anobase |
                                                            (`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`== 2)))) %>%
  count(`Nome Curso`, Sexo) %>% 
  spread(Sexo, n, fill = 0)

names(AA2)[2:3] <- c("Feminino A2", "Masculino A2")

AT2 <- Mest %>% filter(TGM20192 == 1 & (`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`> anobase | (`Ano Saida Opcao`== anobase &
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

Tabela3.17_rownames <- Tabela3.17$`Nome Curso` 
Tabela3.17$`Nome Curso` <- NULL
Tabela3.17$Unidade <- NULL

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

Tabela3.17_TotalGeral <- colSums(Tabela3.17, na.rm=T)/2
Tabela3.17 <- rbind(Tabela3.17, Tabela3.17_TotalGeral)
Tabela3.17_rownames <- c(Tabela3.17_rownames, "Total Geral")

row.names(Tabela3.17) <- Tabela3.17_rownames

rm(AA1, AA2, AT1, AT2, totais, Tabela3.17_rownames, Tabela3.17_TotalGeral)

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
Tabela3.18_rownames <- Tabela3.18$`Nome Curso`
Tabela3.18$`Nome Curso` <- NULL
Tabela3.18$Unidade <- NULL

Tabela3.18$`Total 1S` <- rowSums(Tabela3.18[,c("Abandono 1", "Desligamento Voluntário 1", "Desligado-Falta de Rendimento 1", 
                                               "Outros 1")])
Tabela3.18$`Total 2S` <- rowSums(Tabela3.18[,c("Abandono 2", "Desligamento Voluntário 2", "Desligado-Falta de Rendimento 2", 
                                               "Outros 2")])

Tabela3.18 <- Tabela3.18 %>% 
  ungroup() %>% 
  select(n,`Abandono 1`, `Abandono 2`, `Desligamento Voluntário 1`, 
         `Desligamento Voluntário 2`, `Desligado-Falta de Rendimento 1`, `Desligado-Falta de Rendimento 2`,
         `Outros 1`, `Outros 2`, `Total 1S`, `Total 2S`)

Tabela3.18[is.na(Tabela3.18)] <- 0

Tabela3.18_TotalGeral <- colSums(Tabela3.18, na.rm=T)/2

Tabela3.18 <- rbind(Tabela3.18, Tabela3.18_TotalGeral)
Tabela3.18_rownames <- c(Tabela3.18_rownames, "Total Geral")

row.names(Tabela3.18) <- Tabela3.18_rownames

rm(M2, D1, D2, totais,Tabela3.18_rownames, Tabela3.18_TotalGeral)

# Tabela 3.19 Evolução Ingressantes -------------------------------------------

# Pegar do último anuário e 
# verificar se há cursos novos
# se sim, atualizar no CSV

Tabela3.19 <- read.csv2("dados_mestrado/EvoIngM.csv", check.names=F, stringsAsFactors = FALSE) 
Tabela3.19 <- Tabela3.19[-123,]

Ing <- Mest %>% 
  filter(`Ano Ingresso Opcao`== anobase) %>% 
  count(`Nome Curso`, Unidade, name = "2019") %>% 
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

Tabela3.19 <- full_join(Tabela3.19, Ing, by = c("Unidade Acadêmica/Curso" = "Nome Curso"))

Tabela3.19[8,6] <- "CET"
Tabela3.19[9,6] <- "CET -"
Tabela3.19[14,6] <- "FACE -"
Tabela3.19[82,6] <- "Ida -"
Tabela3.19[72,6] <- "IB -" # Confirmar se o curso "Ensino de Biologia em Rede Nacional"	é do IB
Tabela3.19[97,6] <- "IG -"
Tabela3.19[120,6] <- "IQ -"
Tabela3.19[112,6] <- "IP -"

Tabela3.19 <- Tabela3.19 %>% arrange(Unidade, `Unidade Acadêmica/Curso`) 

Tabela3.19_rownames <- Tabela3.19$`Unidade Acadêmica/Curso`

Tabela3.19$`Unidade Acadêmica/Curso` <- NULL
Tabela3.19$Unidade <- NULL

Tabela3.19[Tabela3.19=="-"] <- 0
Tabela3.19[is.na(Tabela3.19)] <- 0

Tabela3.19 <- Tabela3.19 %>% mutate_if(is.character,as.numeric)
Tabela3.19_TotalGeral <- colSums(Tabela3.19, na.rm=T)/2

Tabela3.19 <- rbind(Tabela3.19, Tabela3.19_TotalGeral)
Tabela3.19_rownames <- c(Tabela3.19_rownames, "Total Geral")

row.names(Tabela3.19) <- Tabela3.19_rownames

rm(Ing, totais, Tabela3.19_TotalGeral, Tabela3.19_rownames)

# Tabela 3.20 Evolução Alunos Registrados ----------------------------------------

# Pegar do último anuário e 
# verificar se há cursos novos
# se sim, atualizar no CSV

Tabela3.20 <- read.csv2("dados_mestrado/EvoRegM.csv", check.names=F, stringsAsFactors = FALSE)
Tabela3.20 <- Tabela3.20[-127,]

Reg <- Mest %>% 
  filter(`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`> anobase | (`Ano Saida Opcao`== anobase & `Semestre Saida Opcao`==2))) %>% 
  count(`Nome Curso`, Unidade, name = "2019") %>% 
  mutate(`Nome Curso` = ifelse(`Nome Curso` == "Propriedade Intelectual e Transferência de Tecnologia para a Inovação",
                               "Propriedade Intelectual e Transfer de Tecnol para Inovação Tecnológica",
                               `Nome Curso`))


totais <- Reg %>% group_by(Unidade) %>% select(-`Nome Curso`) %>% summarise_all(sum, na.rm = TRUE) %>% left_join(Label_Unidades)

# Ajustar o nome da FACE que não coincide
Tabela3.20[12,1] <- "Faculdade de Economia, Administração e Contabilidade - FACE"
totais[6,3] <- "Faculdade de Economia, Administração e Contabilidade - FACE"
#________________________________________

Reg$Unidade <- sapply(Reg$Unidade, function(x) paste(x, "-", collapse=""))

Reg <- bind_rows(Reg,totais) %>% arrange(Unidade, `Nome Curso`)

Tabela3.20 <- full_join(Tabela3.20, Reg, by = c("Unidade Acadêmica/Curso" = "Nome Curso"))

Tabela3.20[23,6] <- "FACE -"
Tabela3.20[14,6] <- "FACE -"
Tabela3.20[12,6] <- "FACE"
Tabela3.20[23,6] <- "FAV -" 
Tabela3.20[53,6] <- "FAV -" 
Tabela3.20[101,6] <- "IG -" 
Tabela3.20[116,6] <- "IP -"

Tabela3.20 <- Tabela3.20 %>% arrange(Unidade, `Unidade Acadêmica/Curso`) 

Tabela3.20$Unidade <- NULL

Tabela3.20[Tabela3.20=="-"] <- 0
Tabela3.20[is.na(Tabela3.20)] <- 0

### Apagar cursos quenão tem mais ingressantes há 5 anos

Tabela3.20 <- Tabela3.20[-c(15,23,25),]

#_________________________________________________________

Tabela3.20_rownames <- Tabela3.20$`Unidade Acadêmica/Curso`

Tabela3.20$`Unidade Acadêmica/Curso` <- NULL

Tabela3.20 <- Tabela3.20 %>% mutate_if(is.character,as.numeric)
Tabela3.20_TotalGeral <- ceiling(colSums(Tabela3.20, na.rm=T)/2)

Tabela3.20 <- rbind(Tabela3.20, Tabela3.20_TotalGeral)
Tabela3.20_rownames <- c(Tabela3.20_rownames, "Total Geral")

Tabela3.20[Tabela3.20=="-"] <- 0
Tabela3.20[is.na(Tabela3.20)] <- 0

row.names(Tabela3.20) <- Tabela3.20_rownames

rm(Reg, totais, Tabela3.20_TotalGeral, Tabela3.20_rownames)

# Tabela 3.21 Evolução Formados -------------------------------------------

# Pegar do último anuário e 
# verificar se há cursos novos
# se sim, atualizar no CSV

Tabela3.21 <- read.csv2("dados_mestrado/EvoForM.csv", check.names=F, stringsAsFactors = FALSE)
Tabela3.21 <- Tabela3.21[-123,]

For <- Mest %>% filter(`Ano Saida Opcao`== anobase,
                       str_detect(`For. Saida Opcao`, "Formatura")) %>%
  count(`Nome Curso`, Unidade, name = "2019") %>% 
  mutate(`Nome Curso` = ifelse(`Nome Curso` == "Propriedade Intelectual e Transferência de Tecnologia para a Inovação",
                               "Propriedade Intelectual e Transfer de Tecnol para Inovação Tecnológica",
                               `Nome Curso`))


totais <- For %>% group_by(Unidade) %>% select(-`Nome Curso`) %>% summarise_all(sum, na.rm = TRUE) %>% left_join(Label_Unidades)

# Ajustar o nome da FACE que não coincide
Tabela3.21[12,1] <- "Faculdade de Economia, Administração e Contabilidade - FACE"
totais[6,3] <- "Faculdade de Economia, Administração e Contabilidade - FACE"
#________________________________________

For$Unidade <- sapply(For$Unidade, function(x) paste(x, "-", collapse=""))

For <- bind_rows(For,totais) %>% arrange(Unidade, `Nome Curso`)

Tabela3.21 <- full_join(Tabela3.21, For, by = c("Unidade Acadêmica/Curso" = "Nome Curso"))

Tabela3.21[17,6] <- "FACE -"
Tabela3.21[82,6] <- "Ida -"
Tabela3.21[22,6] <- "FAV -" 
Tabela3.21[51,6] <- "FAV -" 
Tabela3.21[97,6] <- "IG -" 
Tabela3.21[112,6] <- "IP -"

Tabela3.21 <- Tabela3.21 %>% arrange(Unidade, `Unidade Acadêmica/Curso`) 

Tabela3.21$Unidade <- NULL

Tabela3.21[Tabela3.21=="-"] <- 0
Tabela3.21[is.na(Tabela3.21)] <- 0

### Apagar cursos quenão tem mais Formados há 5 anos

Tabela3.21 <- Tabela3.21[-c(17,22,24),]

#_________________________________________________________


Tabela3.21_rownames <- Tabela3.21$`Unidade Acadêmica/Curso`

Tabela3.21$`Unidade Acadêmica/Curso` <- NULL

Tabela3.21 <- Tabela3.21 %>% mutate_if(is.character,as.numeric)
Tabela3.21_TotalGeral <- ceiling(colSums(Tabela3.21, na.rm=T)/2)

Tabela3.21 <- rbind(Tabela3.21, Tabela3.21_TotalGeral)
Tabela3.21_rownames <- c(Tabela3.21_rownames, "Total Geral")

Tabela3.21[Tabela3.21=="-"] <- 0
Tabela3.21[is.na(Tabela3.21)] <- 0

row.names(Tabela3.21) <- Tabela3.21_rownames

rm(For, totais, Tabela3.21_TotalGeral, Tabela3.21_rownames)

# Limpeza final

rm(Completo, HE0, HE1, HE2, Label_Unidades, Mest, Nomes, anobase)

# Tabela para preencher os Títulos

Nomes <- c("Tabela 3.08 - Alunos estrangeiros regulares registrados nos cursos de Pós-Graduação Stricto Sensu, por Continente, País e Unidade Acadêmica, UnB, 2o Semestre de 2019",
           "Tabela 3.12 – Ingresso de Alunos e Número de Dissertações Homologadas nos Cursos de Mestrado, por Unidade Acadêmica e Curso, UnB, 2019",
           "Tabela 3.13 – Alunos Ingressantes e Concluintes nos Cursos de Mestrado, por Sexo e Faixa Etária, UnB, 2019",
           "Tabela 3.14 – Alunos Regulares Ativos, Matrículas e Aprovações em Disciplinas* nos Cursos de Mestrado, por Unidade Acadêmica e Curso, UnB, 2019",
           "Tabela 3.15 – Alunos Regulares Registrados nos Cursos de Mestrado, por Sexo e Faixa Etária, UnB, 2º/2019",
           "Tabela 3.16 – Alunos Regulares Registrados nos Cursos de Mestrado, por Semestre, Unidade Acadêmica, Curso e Sexo, UnB, 2019",
           "Tabela 3.17 – Alunos Regulares Registrados Ativos e com Trancamento Geral de Matrícula nos Cursos de Mestrado, por Semestre, Sexo, Unidade Acadêmica e Curso, UnB, 2019",
           "Tabela 3.18 – Desligamento de Alunos nos Cursos de Mestrado, por Forma, Semestre, Unidade Acadêmica e Curso, UnB, 2019",
           "Tabela 3.19 – Evolução do Ingresso de Alunos nos Cursos de Mestrado, por Unidade Acadêmica e Curso, UnB, 2015 a 2019",
           "Tabela 3.20 – Evolução do Número de Alunos Registrados* nos Cursos de Mestrado, por Unidade Acadêmica e Curso, UnB, 2014 a 2019",
           "Tabela 3.21 – Evolução do Número de Alunos com Dissertações Homologadas nos Cursos de Mestrado, por Unidade Acadêmica e Curso, UnB, 2015 a 2019")


Número <- c(1,5,7,8,9,10,11,12,13,14,15)

df_Nomes <- as.data.frame(Nomes)
df_Números <- as.data.frame(Número)

Tabela_nomes <- bind_cols(df_Nomes,df_Números)

rm(df_Nomes,df_Números, Nomes, Númreros)

# Excel -------------------------------------------------------------------


#Mudei para uma programação que me sinto mais confortável na hora de salvar em excel


list_of_datasets <- list("Tabela_nomes" = Tabela_nomes,
                         "Tabela 3.08" = Tabela3.08,
                         "Tabela 3.08.2" = Tabela3.08.2,
                         "Tabela 3.08.3" = Tabela3.08.3,
                         "Tabela 3.08.4" = Tabela3.08.4,
                         "Tabela 3.12" = Tabela3.12,
                         "Tabela 3.12.2" = Tabela3.12.2,
                         "Tabela 3.13" = Tabela3.13,
                         "Tabela 3.14" = Tabela3.14,
                         "Tabela 3.15" = Tabela3.15,
                         "Tabela 3.16" = Tabela3.16,
                         "Tabela 3.16.2" = Tabela3.16.2,
                         "Tabela 3.17" = Tabela3.17,
                         "Tabela 3.18" = Tabela3.18,
                         "Tabela 3.19" = Tabela3.19,
                         "Tabela 3.20" = Tabela3.20,
                         "Tabela 3.21" = Tabela3.21)



write.xlsx(list_of_datasets, file = "dados_mestrado/Tabelas_Mestrado_Anuário_2019.xlsx", row.names=T)






