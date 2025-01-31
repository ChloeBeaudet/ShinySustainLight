#################################################################
#                                                               #
#                                                               #
#                    Creation dataframes                        # 
#                       For shiny app                           #
#                                                               #
#################################################################
rm(list=ls())
library(terra)
library(tidyterra)
library(dplyr)
library(sf)
library(rmapshaper)
library(png)
library(grid)

# ################################################################# #
#### Light pollution indicators                                  ####
# ################################################################# #
### A - Level of upward emission

upward_emission0 <- as.polygons(rast("Data/RAW/indicateurs_pollum/2022_MOSAIC_TOT_georef_L93_AE_binaire.tif")) %>%
  rename("level" = "2022_MOSAIC_TOT_georef_L93_AE_binaire") %>%
  mutate(indice = 0, 
         level = "Low")

# crs(upward_emission0) <- '+proj=longlat +datum=WGS84'

upward_emission0_sf <- st_as_sf(upward_emission0)
upward_emission0_sf <- st_transform(upward_emission0_sf, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')


upward_emission25 <- as.polygons(rast("Data/RAW/indicateurs_pollum/2022_MOSAIC_TOT_georef_L93_AE_binaire25.tif")) %>%
  rename("level" = "2022_MOSAIC_TOT_georef_L93_AE_binaire25") %>%
  mutate(indice = 1, 
         level = "Moderate")

#crs(upward_emission25) <- '+proj=longlat +datum=WGS84'

upward_emission25_sf <- st_as_sf(upward_emission25)
upward_emission25_sf <- st_transform(upward_emission25_sf, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')



upward_emission50 <- as.polygons(rast("Data/RAW/indicateurs_pollum/2022_MOSAIC_TOT_georef_L93_AE_binaire50.tif")) %>%
  rename("level" = "2022_MOSAIC_TOT_georef_L93_AE_binaire50") %>%
  mutate(indice = 2, 
         level = "High")

#crs(upward_emission50) <- '+proj=longlat +datum=WGS84'

upward_emission50_sf <- st_as_sf(upward_emission50)
upward_emission50_sf <- st_transform(upward_emission50_sf, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')
upward_emission50_simplify <- ms_simplify(upward_emission50_sf, keep = 0.01) 

upward_emission25_2 <- st_difference(upward_emission25_sf, upward_emission50_sf, dimension = "polygon")
upward_emission0_2 <- st_difference(upward_emission0_sf, upward_emission25_sf, dimension = "polygon")


upward_emission25_2_simplify <- ms_simplify(upward_emission25_2, keep = 0.01) 
upward_emission0_2_simplify <- ms_simplify(upward_emission0_2, keep = 0.01) 

upward_emission <- bind_rows(upward_emission0_2_simplify, 
                         upward_emission25_2_simplify,
                         upward_emission50_simplify) %>%
  mutate(level = factor(level, 
                        level = c("Low", "Moderate", "High")), 
         type = "upward_emission") %>%
  select(-level.1, - indice.1)



# # B - Visibility 
# # Chiro
# 
# visibility_chiro <- as.polygons(rast("Data/RAW/indicateurs_pollum/Visibility_chiro_6_6_100_moy_5m.tif")) %>%
#   mutate(indice = ifelse(Visibility_chiro_6_6_100_moy_5m == 0, 0, 
#                          ifelse(Visibility_chiro_6_6_100_moy_5m < 5, 1, 
#                                 ifelse(Visibility_chiro_6_6_100_moy_5m < 10, 2, 
#                                        ifelse(Visibility_chiro_6_6_100_moy_5m < 25, 3, 
#                                               ifelse(Visibility_chiro_6_6_100_moy_5m < 50, 4, 
#                                                      ifelse(Visibility_chiro_6_6_100_moy_5m < 100, 5, NA)))))), 
#          
#          level = ifelse(indice == 0, "0", 
#                         ifelse(indice == 1, "1-5", 
#                                ifelse(indice == 2, "5-10", 
#                                       ifelse(indice == 3, "10-25", 
#                                              ifelse(indice == 4, "25-50", 
#                                                     ifelse(indice == 5, "50-100", NA)))))), 
#          type = "visibility_chiro") %>%
#   aggregate(by =  c("indice", "level", "type")) %>% 
#   filter(level != "0")
# 
# # crs(visibility_chiro) <- '+proj=longlat +datum=WGS84'
# 
# visibility_chiro_sf <- st_as_sf(visibility_chiro)
# visibility_chiro_sf <- st_transform(visibility_chiro_sf, 2154) %>% 
#   st_transform('+proj=longlat +datum=WGS84')
# visibility_chiro_simplify <- ms_simplify(visibility_chiro_sf, keep = 0.01) 
# 
# # Insect
# 
# visibility_insects <- as.polygons(rast("Data/RAW/indicateurs_pollum/Visibility_insectes_6_6_500_moy_5m.tif")) %>%
#   mutate(indice = ifelse(Visibility_insectes_6_6_500_moy_5m == 0, 0, 
#                          ifelse(Visibility_insectes_6_6_500_moy_5m < 10, 1, 
#                                 ifelse(Visibility_insectes_6_6_500_moy_5m < 25, 2, 
#                                        ifelse(Visibility_insectes_6_6_500_moy_5m < 50, 3, 
#                                               ifelse(Visibility_insectes_6_6_500_moy_5m < 100, 4, 
#                                                      ifelse(Visibility_insectes_6_6_500_moy_5m < 200, 5, 6)))))), 
#          
#          level = ifelse(indice == 0, "0", 
#                         ifelse(indice == 1, "1-10", 
#                                ifelse(indice == 2, "10-25", 
#                                       ifelse(indice == 3, "25-50", 
#                                              ifelse(indice == 4, "50-100", 
#                                                     ifelse(indice == 5, "100-200", "> 200")))))), 
#          type = "visibility_insects") %>%
#   aggregate(by =  c("indice", "level", "type")) %>% 
#   filter(level != "0")
# 
# visibility_insects_sf <- st_as_sf(visibility_insects)
# visibility_insects_sf <- st_transform(visibility_insects_sf, 2154) %>% 
#   st_transform('+proj=longlat +datum=WGS84')
# visibility_insects_simplify <- ms_simplify(visibility_insects_sf, keep = 0.01) 
# 
# 
# #crs(visibility_insects) <- '+proj=longlat +datum=WGS84'
# 
df_pollum <- bind_rows(upward_emission)#,
                       # visibility_chiro_simplify,
                       # visibility_insects_simplify)

# df_pollum_simplify_sf <- ms_simplify(st_as_sf(df_pollum), keep = 0.01,
#                                         keep_shapes = TRUE)

# ################################################################# #
#### Ecological indicators                                       ####
# ################################################################# #

### A - Loss of functionality

# Nightjar
nightjar_LF_2 <- as.polygons(
  rast("Data/RAW/species/Engoulevent/priorites/Prio2_engoulevent_3class_superimpose.tif")) %>%
  rename(functionality_lose = Prio2_engoulevent_3class_superimpose) %>%
  mutate(priority = 2, 
         species = "Nightjar")

# crs(nightjar_LF_2) <- '+proj=longlat +datum=WGS84'

nightjar_LF_2_sf <- st_as_sf(nightjar_LF_2)
nightjar_LF_2_simplify <- ms_simplify(nightjar_LF_2_sf, keep = 0.01) 

nightjar_LF_2_simplify <- st_transform(nightjar_LF_2_simplify, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')


nightjar_LF_3 <- as.polygons(rast("Data/RAW/species/Engoulevent/priorites/Prio3_engoulevent_3classes.tif")) %>%
  rename(functionality_lose = Prio3_engoulevent_3classes) %>%
  mutate(priority = 3, 
         species = "Nightjar")

# crs(nightjar_LF_3) <- '+proj=longlat +datum=WGS84'

nightjar_LF_3_sf <- st_as_sf(nightjar_LF_3)
nightjar_LF_3_simplify <- ms_simplify(nightjar_LF_3_sf, keep = 0.01) 

nightjar_LF_3_simplify <- st_transform(nightjar_LF_3_simplify, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')

# Insects
insect_LF_2 <- as.polygons(rast("Data/RAW/species/Insectes ZH/priorites/Prio2_insecteszh_3classes.tif")) %>%
  rename(functionality_lose = Prio2_insecteszh_3classes) %>%
  mutate(priority = 2, 
         species = "Insects")

# crs(insect_LF_2) <- '+proj=longlat +datum=WGS84'

insect_LF_2_sf <- st_as_sf(insect_LF_2)
insect_LF_2_simplify <- ms_simplify(insect_LF_2_sf, keep = 0.01) 

insect_LF_2_simplify <- st_transform(insect_LF_2_simplify) %>% 
  st_transform('+proj=longlat +datum=WGS84')

insect_LF_3 <- as.polygons(rast("Data/RAW/species/Insectes ZH/priorites/Prio3_insecteszh_3classes.tif")) %>%
  rename(functionality_lose = Prio3_insecteszh_3classes) %>%
  mutate(priority = 3, 
         species = "Insects")

# crs(insects_LF_3) <- '+proj=longlat +datum=WGS84'

insect_LF_3_sf <- st_as_sf(insect_LF_3)
insect_LF_3_simplify <- ms_simplify(insect_LF_3_sf, keep = 0.01) 
insect_LF_3_simplify <- st_transform(insect_LF_3_simplify) %>% 
  st_transform('+proj=longlat +datum=WGS84')

# Lampyridae
lampyridae_LF_2 <- as.polygons(rast("Data/RAW/species/Lampyridae/priorites/Prio2_lampyridae_3classes.tif")) %>%
  rename(functionality_lose = Prio2_lampyridae_3classes) %>%
  mutate(priority = 2, 
         species = "Lampyridae")

# crs(lampyridae_LF_2) <- '+proj=longlat +datum=WGS84'

lampyridae_LF_2_sf <- st_as_sf(lampyridae_LF_2)
lampyridae_LF_2_simplify <- ms_simplify(lampyridae_LF_2_sf, keep = 0.01) 

lampyridae_LF_2_simplify <- st_transform(lampyridae_LF_2_simplify) %>% 
  st_transform('+proj=longlat +datum=WGS84')

lampyridae_LF_3 <- as.polygons(rast("Data/RAW/species/Lampyridae/priorites/Prio3_lampyridae_3classes.tif")) %>%
  rename(functionality_lose = Prio3_lampyridae_3classes) %>%
  mutate(priority = 3, 
         species = "Lampyridae")

# crs(lampyridae_LF_3) <- '+proj=longlat +datum=WGS84'

lampyridae_LF_3_sf <- st_as_sf(lampyridae_LF_3)
lampyridae_LF_3_simplify <- ms_simplify(lampyridae_LF_3_sf, keep = 0.01) 

lampyridae_LF_3_simplify <- st_transform(lampyridae_LF_3_simplify) %>% 
  st_transform('+proj=longlat +datum=WGS84')

# Murine
murine_LF_2 <- as.polygons(rast("Data/RAW/species/Murins/priorites/Prio2_murins_3classes.tif")) %>%
  rename(functionality_lose = Prio2_murins_3classes) %>%
  mutate(priority = 2, 
         species = "Murine")

# crs(murine_LF_2) <- '+proj=longlat +datum=WGS84'

murine_LF_2_sf <- st_as_sf(murine_LF_2)
murine_LF_2_simplify <- ms_simplify(murine_LF_2_sf, keep = 0.01) 
murine_LF_2_simplify <- st_transform(murine_LF_2_simplify) %>% 
  st_transform('+proj=longlat +datum=WGS84')

murine_LF_3 <- as.polygons(rast("Data/RAW/species/Murins/priorites/Prio3_murins_3classes.tif")) %>%
  rename(functionality_lose = Prio3_murins_3classes) %>%
  mutate(priority = 3, 
         species = "Murine")

# crs(murine_LF_3) <- '+proj=longlat +datum=WGS84'

murine_LF_3_sf <- st_as_sf(murine_LF_3)
murine_LF_3_simplify <- ms_simplify(murine_LF_3_sf, keep = 0.01) 
murine_LF_3_simplify <- st_transform(murine_LF_3_simplify) %>% 
  st_transform('+proj=longlat +datum=WGS84')

# Rhinolophus
rhinolophus_LF_2 <- as.polygons(rast("Data/RAW/species/rhinolophes/priorites/Prio2_rhinolophe_3classes.tif")) %>%
  rename(functionality_lose = Prio2_rhinolophe_3classes) %>%
  mutate(priority = 2, 
         species = "Rhinolophus")

# crs(rhinolophus_LF_2) <- '+proj=longlat +datum=WGS84'

rhinolophus_LF_2_sf <- st_as_sf(rhinolophus_LF_2)
rhinolophus_LF_2_simplify <- ms_simplify(rhinolophus_LF_2_sf, keep = 0.01) 
rhinolophus_LF_2_simplify <- st_transform(rhinolophus_LF_2_simplify) %>% 
  st_transform('+proj=longlat +datum=WGS84')


rhinolophus_LF_3 <- as.polygons(rast("Data/RAW/species/rhinolophes/priorites/Prio3_rhinolophe_3classes.tif")) %>%
  rename(functionality_lose = Prio3_rhinolophe_3classes) %>%
  mutate(priority = 3, 
         species = "Rhinolophus")

# crs(rhinolophus_LF_3) <- '+proj=longlat +datum=WGS84'

rhinolophus_LF_3_sf <- st_as_sf(rhinolophus_LF_3)
rhinolophus_LF_3_simplify <- ms_simplify(rhinolophus_LF_3_sf, keep = 0.01) 
rhinolophus_LF_3_simplify <- st_transform(rhinolophus_LF_3_simplify) %>% 
  st_transform('+proj=longlat +datum=WGS84')

### B - Biodiversity reservoirs

# Nightjar
nightjar_rb_priority1 <- as.polygons(rast("Data/RAW/species/Engoulevent/priorites/engoulevent_RB_prio1.tif")) %>%
  rename(indicator = engoulevent_rb_prio1) %>%
  mutate(priority = 1, 
         species = "Nightjar")

# crs(nightjar_rb_priority1) <- '+proj=longlat +datum=WGS84'

nightjar_rb_priority2 <- as.polygons(rast("Data/RAW/species/Engoulevent/priorites/engoulevent_RB_prio2.tif")) %>%
  rename(indicator = engoulevent_rb_prio2) %>%
  mutate(priority = 2, 
         species = "Nightjar")

# crs(nightjar_rb_priority2) <- '+proj=longlat +datum=WGS84'

# Amphibian
amphibien_rb_priority1 <- as.polygons(rast("Data/RAW/species/amphibiens/priorites/Amphibien_RB_prio1.tif")) %>%
  rename(indicator = Amphibien_RB_prio1) %>%
  mutate(priority = 1,
         species = "Amphibian")

# crs(amphibien_rb_priority1) <- '+proj=longlat +datum=WGS84'


amphibien_rb_priority2 <- as.polygons(rast("Data/RAW/species/amphibiens/priorites/Amphibien_RB_prio2.tif")) %>%
  rename(indicator = Amphibien_RB_prio2) %>%
  mutate(priority = 2, 
         species = "Amphibian")

# crs(amphibien_rb_priority2) <- '+proj=longlat +datum=WGS84'


# Insect
insect_rb_priority1 <- as.polygons(rast("Data/RAW/species/Insectes ZH/priorites/ZH_rb_prio1.tif")) %>%
  rename(indicator = ZH_rb_prio1) %>%
  mutate(priority = 1, 
         species = "Insects")

# crs(insect_rb_priority1) <- '+proj=longlat +datum=WGS84'


insect_rb_priority2 <- as.polygons(rast("Data/RAW/species/Insectes ZH/priorites/ZH_rb_prio2.tif")) %>%
  rename(indicator = ZH_rb_prio2) %>%
  mutate(priority = 2, 
         species = "Insects")

# crs(insect_rb_priority2) <- '+proj=longlat +datum=WGS84'


# Lampyridae
lampyridae_rb_priority1 <- as.polygons(rast("Data/RAW/species/Lampyridae/priorites/lampyridae_RB_prio1.tif")) %>%
  rename(indicator = lampyridae_RB_prio1) %>%
  mutate(priority = 1, 
         species = "Lampyridae")

# crs(lampyridae_rb_priority1) <- '+proj=longlat +datum=WGS84'


lampyridae_rb_priority2 <- as.polygons(rast("Data/RAW/species/Lampyridae/priorites/lampyridae_RB_prio2.tif")) %>%
  rename(indicator = lampyridae_RB_prio2) %>%
  mutate(priority = 2, 
         species = "Lampyridae")

# crs(lampyridae_rb_priority2) <- '+proj=longlat +datum=WGS84'


### C - Global grades
# Overall score
global_prio1 <- as.polygons(rast("Data/RAW/global/Prio1_global_note.tif"))  %>%
  mutate(Prio1_global_note  = ifelse(Prio1_global_note == 12, 102, Prio1_global_note),
         Prio1_global_note  = ifelse(Prio1_global_note == 11, 101, Prio1_global_note)) %>%
  rename(indicator = Prio1_global_note) %>%
  mutate(priority = 1) 

# crs(global_prio1) <- '+proj=longlat +datum=WGS84'


global_prio23 <- as.polygons(rast("Data/RAW/global/prio2_3_global_note.tif")) %>%
  rename(indicator = prio2_3_global_note) %>%
  mutate(priority = ifelse(substr(indicator, 1, 1) == 2, 2, 3))

# crs(global_prio23) <- '+proj=longlat +datum=WGS84'


global_prio4 <- as.polygons(rast("Data/RAW/global/prio4_global_note.tif"))  %>%
  rename(indicator = Prio4_global_note) %>%
  mutate(priority = 4) 

# crs(global_prio4) <- '+proj=longlat +datum=WGS84'

df_global <- bind_spat_rows(global_prio1,
                            global_prio23, 
                            global_prio4) %>%
  mutate(lose_functionality = ifelse(substr(indicator, 2, 2) == 3, "Low", 
                                     ifelse(substr(indicator, 2, 2) == 2, "Moderate", "Strong")),
         impacted_species = ifelse(substr(indicator, 3, 3) == 1, "3 to 6", "1 to 2"), 
         type = "note", 
         species = "global") %>%
  aggregate(by = c("indicator", "priority", "lose_functionality", "impacted_species", 
                  "type", "species"))

df_global_sf <- st_as_sf(df_global)
df_global_simplify <- ms_simplify(df_global_sf, keep = 0.01) 

df_global_simplify <- st_transform(df_global_simplify, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')

# Score lighting
df_global_lighted <-  vect("Data/RAW/global/prio_2_3_zones_eclairees_enjeu_max.gpkg") %>%
  rename(indicator = Note_globale) %>%
  filter(indicator %in% c(211, 212, 221, 222, 311, 312, 321, 322)) %>%
  mutate(priority = ifelse(substr(indicator, 1, 1) == 2, 2, 3), 
         lose_functionality = ifelse(substr(indicator, 2, 2) == 2, "Moderate", "Strong"), 
         impacted_species = ifelse(substr(indicator, 3, 3) == 1, "3 to 6", "1 to 2"),
         type = "prio2_lighted_note", 
         species = "global") %>%
  aggregate(by = c("indicator", "priority", "lose_functionality", "impacted_species", 
                   "type", "species")) 

# crs(df_global_lighted) <- '+proj=longlat +datum=WGS84'

df_global_lighted_sf <- st_as_sf(df_global_lighted)
df_global_lighted_simplify <- ms_simplify(df_global_lighted_sf, keep = 0.01)
#crs(df_global_lighted_simplify) <- '+proj=longlat +datum=WGS84'

df_global_lighted_simplify <- st_transform(df_global_lighted_simplify, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')


### D - Bind data

df_functionality <- rbind(nightjar_LF_2_simplify, nightjar_LF_3_simplify,
                          insect_LF_2_simplify, insect_LF_3_simplify, 
                          lampyridae_LF_2_simplify, lampyridae_LF_3_simplify, 
                         murine_LF_2_simplify, murine_LF_3_simplify, 
                          rhinolophus_LF_2_simplify, rhinolophus_LF_3_simplify) %>%
  filter(functionality_lose %in% c(1, 2)) %>%
  mutate(legend = factor(ifelse(functionality_lose == 2, "Moderate loss of connectivity", 
                                "Strong loss of connectivity"), 
                                         levels = c("Strong loss of connectivity", 
                                                    "Moderate loss of connectivity")), 
         indicator = as.numeric(paste0(priority, functionality_lose)), 
         type = "priority23") %>%
  select("functionality_lose", "priority", "species", "legend", "indicator", "type")
  # group_by("functionality_lose", "priority", "species", "legend", "indicator", "type") %>%
  # summarize(geometry = st_union(geometry))
 # aggregate(by =  c("functionality_lose", "priority", "species", "legend", "indicator", "type"))

# df_functionality_sf <- st_as_sf(df_functionality)
#df_functionality_simplify <- ms_simplify(df_functionality_sf, keep = 0.01) # C'est ici que ça bug


df_rb <- bind_spat_rows(amphibien_rb_priority1, amphibien_rb_priority2, 
                        nightjar_rb_priority1, nightjar_rb_priority2, 
                        insect_rb_priority1, insect_rb_priority2, 
                        lampyridae_rb_priority1, lampyridae_rb_priority2) %>%
  mutate(legend = factor(ifelse(priority == 1, "Unaffected by light pollution", 
                                         "Affected by light pollution"), 
                                  levels = c("Unaffected by light pollution", 
                                             "Affected by light pollution")), 
         type = "rb", 
         indicator = priority) %>%
  aggregate(by =  c("indicator", "priority", "species", "legend", "type"))

df_rb_sf <- st_as_sf(df_rb)
df_rb_simplify <- ms_simplify(df_rb_sf, keep = 0.01)

df_rb_simplify <- st_transform(df_rb_simplify, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')



df_map_species <-  bind_rows(df_functionality, 
                         df_rb_simplify, 
                         df_global_simplify, 
                         df_global_lighted_simplify)



# df_map_species_simplify <- ms_simplify(st_as_sf(df_map_species), keep = 0.01,
#                                      keep_shapes = TRUE)

# ################################################################# #
#### Socio-economic indicators                                  ####
# ################################################################# #

df_wtp <- vect("Data/RAW/wtp.gpkg") %>%
  mutate(acceptabilite_extinction1 = factor(ifelse(wtp_extinction1 < -20, "Very opposed", 
                                                   ifelse(wtp_extinction1 < -5, "Opposed", 
                                                          ifelse(wtp_extinction1 <  5, "Little concerned", 
                                                                 ifelse(wtp_extinction1 <  15, "Rather favourable",
                                                                        ifelse(wtp_extinction1 <  30, "Favourable", "Very favourable") )))),
                                            levels = c("Very opposed", "Opposed", "Little concerned",
                                                       "Rather favourable", "Favourable", "Very favourable")), 
         
         acceptabilite_extinction2 = factor(ifelse(wtp_extinction2 < -20, "Very opposed", 
                                                   ifelse(wtp_extinction2 < -5, "Opposed", 
                                                          ifelse(wtp_extinction2 <  5, "Little concerned", 
                                                                 ifelse(wtp_extinction2 <  15, "Rather favourable",
                                                                        ifelse(wtp_extinction2 <  30, "Favourable", "Very favourable") )))),
                                            levels = c("Very opposed", "Opposed", "Little concerned",
                                                       "Rather favourable", "Favourable", "Very favourable")),
         acceptabilite_extinction1 = factor(acceptabilite_extinction1, 
                                            levels = c("Very opposed", "Opposed", "Little concerned",
                                                       "Rather favourable", "Favourable", "Very favourable")),
         acceptabilite_extinction2 = factor(acceptabilite_extinction2, 
                                            levels = c("Very opposed", "Opposed", "Little concerned",
                                                       "Rather favourable", "Favourable", "Very favourable")))






# ################################################################# #
#### Bivariate maps                                             ####
# ################################################################# #

# WTP dataframe
df_wtp_biv <- df_wtp %>%
  mutate(wtp_extinction1_class4 = ifelse(wtp_extinction1 < -5, 1, 
                                         ifelse(wtp_extinction1 <5, 2, 
                                                ifelse(wtp_extinction1 <  30, 3, 4))),
         acceptabilite_extinction1 = ifelse(wtp_extinction1 < -5, "Very opposed", 
                                            ifelse(wtp_extinction1 <5, "Little concerned", 
                                                   ifelse(wtp_extinction1 <  30, "Favourable", "Very favourable"))),
         wtp_extinction2_class4 = ifelse(wtp_extinction2 < -5, 1, 
                                         ifelse(wtp_extinction2 < 5, 2, 
                                                ifelse(wtp_extinction2 <  30, 3, 4))), 
         
         acceptabilite_extinction2 = ifelse(wtp_extinction2 < -5, "Very opposed", 
                                            ifelse(wtp_extinction2 <5, "Little concerned", 
                                                   ifelse(wtp_extinction2 <  30, "Favourable", "Very favourable")))) 

# Ecological indicator dataframe
prio_2_3_global_note <-  df_global_lighted %>%
  filter(lose_functionality != "Low")

prio_2 <- prio_2_3_global_note %>%
  filter(indicator < 300) %>%
  mutate(indice_class4 = as.factor(ifelse(indicator == 211, 4, 
                                          ifelse(indicator == 212, 3, 
                                                 ifelse(indicator == 221, 2, 1)))))

prio_3 <- prio_2_3_global_note %>%
  filter(indicator > 300) %>%
  mutate(indice_class4 = as.factor(ifelse(indicator == 311, 4, 
                                          ifelse(indicator == 312, 3, 
                                                 ifelse(indicator == 321, 2, 1)))))


df_wtp_biv <- project(df_wtp_biv, prio_2)#'+proj=longlat +datum=WGS84'

# Intersection for priority2 
intersection_prio2 <- terra::intersect(df_wtp_biv, prio_2) %>%
  mutate(indicator_extinction1_44 =  paste0(indice_class4, wtp_extinction1_class4),
         indicator_extinction2_44 =  paste0(indice_class4, wtp_extinction2_class4)
         
  )

bivariate_prio2 <- simplifyGeom(intersection_prio2, 0.2) %>%
  mutate(LIBCOM = iconv(LIBCOM,from="UTF-8",to="ASCII//TRANSLIT"),
         LIBIRIS = iconv(LIBIRIS,from="UTF-8",to="ASCII//TRANSLIT")) %>%
  select(-indicator)


# Intersection for priority3
intersection_prio3 <- terra::intersect(df_wtp_biv, prio_3) %>%
  mutate(indicator_extinction1_44 =  paste0(indice_class4, wtp_extinction1_class4),
         indicator_extinction2_44 =  paste0(indice_class4, wtp_extinction2_class4)
  )

bivariate_prio3 <- simplifyGeom(intersection_prio3, 0.2) %>%
  mutate(LIBCOM = iconv(LIBCOM,from="UTF-8",to="ASCII//TRANSLIT"),
         LIBIRIS = iconv(LIBIRIS,from="UTF-8",to="ASCII//TRANSLIT")) %>%
  select(-indicator)


### plot Bivariate map Prio 3, extinction3
df_mma_shp  <- read_sf(dsn = paste0("./Data/"), layer = "MMM_MMM_Limites")
legend_img <- readPNG("./WWW/legend_map_4x4.png")  
legend_grob <- rasterGrob(legend_img, interpolate = TRUE)
bbox <- st_bbox(bivariate_prio3)  # Extracts xmin, xmax, ymin, ymax

ggplot() + 
  geom_sf(data = bivariate_prio3, aes(fill = indicator_extinction2_44), color = NA) +
  scale_fill_manual(values = c(  "#d3d3d3",  
                                 "#d5ce9c", "#95b89e", "#53a19f", "#0285a1", 
                                 "#d7c659", "#99b35a", "#529c5a", "#03815c",
                                 "#d9be01", "#9aab00", "#539600", "#007b00")) + 
  geom_sf(data = df_mma_shp, alpha = 0, size = 15, color = "black") + 
  theme_void() +
  theme(
    legend.position = "none"  # Remove default legend
  ) +
  coord_sf(expand = TRUE, 
           xlim = c(757280.3 - 5000, 783800.3 + 15000), 
           ylim = c(6269857 - 10000, 6294955 + 5000)) +
  annotation_custom(legend_grob, 
                    xmin = 783800.3- 5000, xmax = 783800.3 + 15000, 
                    ymin = 6269857 - 5000, ymax = 6269857 + 10000)

#ggsave("./Results/map_onestep_wtp_extinction2.png", height = 10, width = 7)

ggsave("./bivariate_map_low_eco_stake.svg", height = 10, width = 7, dpi = 700)
ggsave("./bivariate_map_low_eco_stake.png", height = 10, width = 7, dpi = 700)

### plot Bivariate map Prio 2, extinction3
df_mma_shp  <- read_sf(dsn = paste0("./Data/"), layer = "MMM_MMM_Limites")
legend_img <- readPNG("./WWW/legend_map_4x4.png")  
legend_grob <- rasterGrob(legend_img, interpolate = TRUE)
bbox <- st_bbox(bivariate_prio2)  # Extracts xmin, xmax, ymin, ymax

 ggplot() + 
  geom_sf(data = bivariate_prio2, aes(fill = indicator_extinction2_44), color = NA) +
  scale_fill_manual(values = c( "#d3d3d3", "#94bdd4", "#51a5d6", "#0088d9", 
                                "#53a19f", "#0285a1", 
                                "#d7c659", "#99b35a", "#529c5a", "#03815c", 
                                "#d9be01", "#9aab00", "#539600", "#007b00")) + 
  geom_sf(data = df_mma_shp, alpha = 0, size = 15, color = "black") + 
  theme_void() +
  theme(
    legend.position = "none"  # Remove default legend
  ) +
coord_sf(expand = TRUE, 
           xlim = c(757280.3 - 5000, 783800.3 + 15000), 
           ylim = c(6269857 - 10000, 6294955 + 5000)) +
  annotation_custom(legend_grob, 
                    xmin = 783800.3- 5000, xmax = 783800.3 + 15000, 
                    ymin = 6269857 - 5000, ymax = 6269857 + 10000)

#ggsave("./Results/map_onestep_wtp_extinction2.png", height = 10, width = 7)

ggsave("./bivariate_map_high_eco_stake.svg", height = 10, width = 7, dpi = 700)
ggsave("./bivariate_map_high_eco_stake.png", height = 10, width = 7, dpi = 700)

# ################################################################# #
#### Base map                                                    ####
# ################################################################# #

# base mmm fond de carte
df_mmm_shp  <- vect("./Data/MMM_MMM_Limites.shp")
#crs(df_mmm_shp) <- '+proj=longlat +datum=WGS84'

# ################################################################# #
#### Save session                                                ####
# ################################################################# #
# saveRDS(df_pollum, file = "Data/df_pollum.rds")
# saveRDS(df_map_species, file = "Data/df_map_species.rds")
# saveRDS(df_wtp, file = "Data/df_wtp.rds")
# saveRDS(bivariate_prio2, file = "Data/bivariate_prio2.rds")
# saveRDS(bivariate_prio3, file = "Data/bivariate_prio3.rds")
# saveRDS(df_mmm_shp, file = "Data/df_mmm_shp.rds")
# 
# df_pollum_wrap <- wrap(df_pollum)
# save(df_mmm_shp_wrap, file = "Data2.RData")
# 
# load("Data.RData", verbose = T)


df_pollum_sf <- st_as_sf(df_pollum)
df_map_species_sf <- df_map_species
df_wtp_sf <- st_as_sf(df_wtp)
bivariate_prio2_sf <- st_as_sf(bivariate_prio2)
bivariate_prio3_sf <- st_as_sf(bivariate_prio3)
df_mmm_shp <- st_as_sf(df_mmm_shp)


df_pollum_simplify <- df_pollum
df_map_species_simplify <- df_map_species_sf
df_wtp_simplify <- ms_simplify(df_wtp_sf, keep = 0.1) %>%
  select(-LIBCOM, -LIBIRIS) %>%
  left_join(as.data.frame(df_wtp_sf) %>%
              select(IRIS, LIBCOM, LIBIRIS))

bivariate_prio2_simplify <- ms_simplify(bivariate_prio2_sf, keep = 0.1)
bivariate_prio3_simplify <- ms_simplify(bivariate_prio3_sf, keep = 0.1)

# df_pollum <- st_transform(df_pollum_simplify , 2154) %>% 
#   st_transform('+proj=longlat +datum=WGS84')

# df_map_species <- st_transform(df_map_species_simplify, 2154) %>% 
#   st_transform('+proj=longlat +datum=WGS84')

df_wtp <- st_transform(df_wtp_simplify, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')

bivariate_prio2 <- st_transform(bivariate_prio2_simplify, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')

bivariate_prio3 <- st_transform(bivariate_prio3_simplify, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')

df_mmm_shp <- st_transform(df_mmm_shp, 2154) %>% 
  st_transform('+proj=longlat +datum=WGS84')

df_pollum <- df_pollum_simplify

save(df_pollum, df_map_species, df_wtp, bivariate_prio2, bivariate_prio3, df_mmm_shp, file = "Data_vf.RData")
load("Data_vf.RData")

# df_pollum_wrap <- wrap(df_pollum)
# df_map_species_wrap  <- wrap(df_map_species)
# df_wtp_wrap  <- wrap(df_wtp)
# bivariate_prio2_wrap  <- wrap(bivariate_prio2)
# bivariate_prio3_wrap  <- wrap(bivariate_prio3)
# df_mmm_shp_wrap  <- wrap(df_mmm_shp)
# 
# 
# save(df_pollum_wrap, 
#      df_map_species_wrap, 
#      df_wtp_wrap, 
#      bivariate_prio2_wrap, 
#      bivariate_prio3_wrap, 
#      df_mmm_shp_wrap, 
#      file = "Data.RData")
# 
# load("Data.RData")