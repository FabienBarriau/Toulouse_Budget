library(tidyverse)
library(lubridate)
library(reshape)
library(plyr)
toulousMetropoleOrganisme2012 <- read_delim("toulouse-m�tropole/organisme/subventions-versees-a-des-organismes-2012-toulouse-metropole.csv", delim =";", locale = locale(encoding = 'UTF-8'))
toulouseMetropoleTiers2012 <- read_delim("toulouse-m�tropole/tiers/subventions-versees-a-des-tiers-2012-toulouse-metropole.csv", delim =";", locale = locale(encoding = 'UTF-8'))

subventions-versees-a-des-tiers-2012-toulouse-metropole

#Est ce qu'on a la m�me info ?
toulousMetropoleOrganisme2012 %>% nrow()
toulouseMetropoleTiers2012 %>% nrow()

#Oui il semblerait
test <- toulouseMetropoleTiers2012 %>% 
  rename("montant" = "Montant fonds de concours ou subvention Ann�e 2012") %>%
  select(-c("Ann�e")) %>%
  left_join(toulousMetropoleOrganisme2012 %>% select(c("montant", "nomBeneficiaire")), by="montant")

clear_toulouseMetropoleOrganisme <- function(path){
  read_delim(path , delim =";", locale = locale(encoding = 'UTF-8')) %>%
    mutate("annee" = year(ymd(datesPeriodeVersement))) %>%
    mutate("mois" = month(ymd(datesPeriodeVersement))) %>%
    mutate(idBeneficiaire = as.character(idBeneficiaire)) %>%
    return()
}

setwd("C:/Users/fabien/Desktop/OpenDataToulouse/administratif/toulouse-m�tropole/organisme")

toulousMetropoleOrganisme2012 <- clear_toulouseMetropoleOrganisme("subventions-versees-a-des-organismes-2012-toulouse-metropole.csv")
toulousMetropoleOrganisme2013 <- clear_toulouseMetropoleOrganisme("subventions-versees-a-des-organismes-2013-toulouse-metropole.csv")
toulousMetropoleOrganisme2014 <- clear_toulouseMetropoleOrganisme("subventions-versees-a-des-organismes-2014-toulouse-metropole.csv")
toulousMetropoleOrganisme2015 <- clear_toulouseMetropoleOrganisme("subventions-versees-a-des-organismes-2015-toulouse-metropole.csv")
toulousMetropoleOrganisme2016 <- clear_toulouseMetropoleOrganisme("subventions-versees-a-des-organismes-2016-toulouse-metropole.csv")
toulousMetropoleOrganisme2017 <- clear_toulouseMetropoleOrganisme("subventions-versees-a-des-organismes-2017-toulouse-metropole.csv")

toulouseMetropoleOrganisme <- toulousMetropoleOrganisme2012 %>%
  union_all(toulousMetropoleOrganisme2013) %>%
  union_all(toulousMetropoleOrganisme2014) %>%
  union_all(toulousMetropoleOrganisme2015) %>%
  union_all(toulousMetropoleOrganisme2016) %>%
  union_all(toulousMetropoleOrganisme2017)

### base sirene ####
setwd("C:/Users/fabien/Desktop/OpenDataToulouse/administratif")

baseSirene <- read_delim("base-sirene.csv", delim =";", locale = locale(encoding = 'UTF-8'))
subSirene <- baseSirene %>% 
  select("Identifiant de l'entreprise", "Num�ro interne de classement de l'�tablissement", "Libell� de l'activit� principale de l'�tablissement") %>% 
  dplyr::rename("idEntreprise" = "Identifiant de l'entreprise") %>%
  dplyr::rename("numInterne" = "Num�ro interne de classement de l'�tablissement") %>%
  dplyr::rename("type" = "Libell� de l'activit� principale de l'�tablissement") %>%
  mutate(idBeneficiaire = str_c(as.character(idEntreprise), as.character(numInterne))) %>%
  mutate(type = replace_na(type, "Non renseign�")) %>%
  select("idBeneficiaire", "type") %>%
  unique() 
  

toulouseMetropoleOrganismeEnriched <- toulouseMetropoleOrganisme %>%
  left_join(subSirene, by= c("idBeneficiaire" = "idBeneficiaire"))

toulouseMetropoleOrganismeEnriched %>%
  filter(is.na(type)) %>%
  write_csv("toSearch.csv")

### association ####
setwd("C:/Users/fabien/Desktop/OpenDataToulouse/administratif")
association <- read_delim("associations-les-donnees-du-journal-officiel-des-associations-et-fondations-dent.csv", delim =";", locale = locale(encoding = 'UTF-8'))

association <- association %>%
  dplyr::rename("AncienTitre" = "Ancien titre") %>%
  dplyr::rename("NouveauTitre" = "Nouveau titre") %>%
  dplyr::rename("Theme" = "Theme - Libell�") %>%
  dplyr::rename("ID" = "Identifiant association") %>%
  mutate(ID = str_replace(ID, "W", "")) %>%
  select(c("Theme", "Titre", "AncienTitre", "NouveauTitre", "ID")) 

ancien <- association %>% 
  select(c("Theme", "AncienTitre")) %>%
  filter(!is.na(AncienTitre)) %>% 
  select(Theme, AncienTitre) %>%
  dplyr::rename("Titre" = "AncienTitre")

nouveau <- association %>% 
  select(c("Theme", "NouveauTitre")) %>%
  filter(!is.na(NouveauTitre)) %>% 
  select(Theme, NouveauTitre) %>%
  dplyr::rename("Titre" = "NouveauTitre")

associationName <- association %>% 
  filter(!is.na(Titre)) %>%
  select(c("Theme", "Titre")) %>%
  union_all(ancien) %>%
  union_all(nouveau) %>%
  mutate(Titre = str_replace(Titre, "\\.", ""))

associationID <- association %>%
  select(c("Theme", "ID"))

test2 <- test %>%
  left_join(associationName, by = c("nomBeneficiaire" = "Titre")) %>%
  select(c("idBeneficiaire", "nomBeneficiaire", "Theme")) %>%
  filter(!is.na(Theme)) %>%
  nrow()


test2 <- test %>%
  mutate(idBeneficiaire = as.character(idBeneficiaire)) %>%
  left_join(associationID, by = c("idBeneficiaire" = "ID")) %>% 
  select(c("idBeneficiaire", "nomBeneficiaire", "Theme")) %>%
  filter(!is.na(Theme))
  



