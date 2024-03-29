library(tidyverse)
library(lubridate)
library(reshape)
library(plyr)

setwd("C:/Users/fabien/Desktop/OpenDataToulouse/administratif/Subventions_Toulouse")

toulousVilleOrganisme2012 <- read_delim("ville-de-toulouse/tiers/open-datasubv2012.csv", delim =";", locale = locale(encoding = 'UTF-8')) %>%
  dplyr::rename("code" = "Code tiers b�n�ficiaire") %>%
  dplyr::rename("nomBeneficiaire" = "Nom b�n�ficiaire") %>%
  dplyr::rename("montant" ="Montant budg�taire liquid�") %>%
  mutate(idBeneficiaire = NA) %>%
  mutate(annee = 2012) %>%
  select("nomBeneficiaire", "montant", "idBeneficiaire", "annee")


toulousVilleOrganisme2013 <- read_delim("ville-de-toulouse/tiers/subventions-versees-a-des-tiers-2013-ville-de-toulouse.csv", delim =";", locale = locale(encoding = 'UTF-8')) %>%
  dplyr::rename("code" = "Code tiers b�n�ficiaire") %>%
  dplyr::rename("nomBeneficiaire" = "Nom b�n�ficiaire") %>%
  dplyr::rename("montant" ="Montant budg�taire liquid�") %>%
  mutate(idBeneficiaire = NA) %>%
  mutate(annee = 2013) %>%
  select("nomBeneficiaire", "montant", "idBeneficiaire", "annee")

toulousVilleOrganisme2014 <- read_delim("ville-de-toulouse/tiers/subventions-versees-a-des-tiers-2014-ville-de-toulouse0.csv", delim =";", locale = locale(encoding = 'UTF-8')) %>%
  dplyr::rename("code" = "Code tiers b�n�ficiaire") %>%
  dplyr::rename("nomBeneficiaire" = "Nom b�n�ficiaire") %>%
  dplyr::rename("montant" ="Montant vers�") %>%
  mutate(idBeneficiaire = NA) %>%
  mutate(annee = 2014) %>%
  select("nomBeneficiaire", "montant", "idBeneficiaire", "annee")
    
toulousVilleOrganisme2015 <- read_delim("ville-de-toulouse/organisme/subventions-versees-a-des-organismes-2015.csv", delim =";", locale = locale(encoding = 'UTF-8')) %>%
  dplyr::rename("liquidation" = "Section liquidation") %>%
  dplyr::rename("code" = "Code tiers b�n�ficiaire") %>%
  dplyr::rename("nomBeneficiaire" = "Nom b�n�ficiaire") %>%
  dplyr::rename("montant" ="Montant vers� 2015") %>%
  mutate(idBeneficiaire = NA) %>%
  mutate(annee = 2015)  %>%
  select("nomBeneficiaire", "montant", "idBeneficiaire", "annee") %>%
  mutate(idBeneficiaire = as.character(idBeneficiaire))

toulousVilleOrganisme2016 <- read_delim("ville-de-toulouse/organisme/subventions-versees-a-des-organismes-2016-ville-de-toulouse.csv", delim =";", locale = locale(encoding = 'UTF-8')) %>%
  dplyr::rename("liquidation" = "Section liquidation") %>%
  dplyr::rename("code" = "Code tiers b�n�ficiaire") %>%
  dplyr::rename("nomBeneficiaire" = "Nom b�n�ficiaire") %>%
  dplyr::rename("montant" ="Montant vers� en 2016") %>%
  dplyr::rename("idBeneficiaire" = "N� SIRET") %>%
  dplyr::rename("structure" = "Type b�n�ficiaire") %>%
  mutate(annee = 2016) %>%
  select("nomBeneficiaire", "montant", "idBeneficiaire", "annee") %>%
  mutate(idBeneficiaire = as.character(idBeneficiaire))

toulousVilleOrganisme2017 <- read_delim("ville-de-toulouse/organisme/subventions-versees-a-des-organismes-2017-ville-de-toulouse.csv", delim =";", locale = locale(encoding = 'UTF-8')) %>%
  mutate(annee = 2017) %>%
  select("nomBeneficiaire", "montant", "idBeneficiaire", "annee") %>%
  mutate(idBeneficiaire = as.character(idBeneficiaire))

toulouseVilleOrganisme <- toulousVilleOrganisme2012 %>%
  union_all(toulousVilleOrganisme2013) %>%
  union_all(toulousVilleOrganisme2014) %>%
  union_all(toulousVilleOrganisme2015) %>%
  union_all(toulousVilleOrganisme2016) %>%
  union_all(toulousVilleOrganisme2017)

### base sirene ####
base_sirene_path = "C:/Users/fabien/Desktop/OpenDataToulouse/administratif/base-sirene.csv"

baseSirene <- read_delim(base_sirene_path, delim =";", locale = locale(encoding = 'UTF-8'))
subSirene <- baseSirene %>% 
  select("Identifiant de l'entreprise", "Num�ro interne de classement de l'�tablissement", "Libell� de l'activit� principale de l'�tablissement") %>% 
  dplyr::rename("idEntreprise" = "Identifiant de l'entreprise") %>%
  dplyr::rename("numInterne" = "Num�ro interne de classement de l'�tablissement") %>%
  dplyr::rename("type" = "Libell� de l'activit� principale de l'�tablissement") %>%
  mutate(idBeneficiaire = str_c(as.character(idEntreprise), as.character(numInterne))) %>%
  mutate(type = replace_na(type, "Non renseign�")) %>%
  select("idBeneficiaire", "type") %>%
  unique() 

toulouseVilleOrganismeEnriched <- toulouseVilleOrganisme %>%
  left_join(subSirene, by= c("idBeneficiaire" = "idBeneficiaire")) 

toulouseVilleOrganismeEnriched %>%
  filter(is.na(type)) %>%
  nrow()

corresponDanceTable <-  read_delim("corresponDanceTable.csv", delim =",", locale = locale(encoding = 'UTF-8')) 

toulouseVilleOrganismeEnriched <- toulouseVilleOrganismeEnriched %>%
  left_join(corresponDanceTable, by= c("nomBeneficiaire" = "nomBeneficiaire")) %>%
  mutate(type = case_when(
    is.na(type.x) ~ type.y,
    TRUE ~ type.x
  )) %>%
  select(-c(type.x, type.y))

toulouseVilleOrganismeEnriched %>%
  filter(is.na(type)) 
