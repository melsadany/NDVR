################################################################################
#                              build NDVR database                             #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/NDVR/samples-mapping"
setwd(project.dir)
################################################################################
################################################################################
# build the dataset for all them
dset.all <- data.frame(NDVR_id = NA,
                       te_id=NA,
                       wetlab_id = NA,
                       devGenes_id = NA,
                       initials = NA,
                       sex = NA,
                       DOB = NA,
                       saliva_barcode = NA)
################################################################################
################################################################################
ndvr <- readxl::read_xlsx("data/raw/NDVR_Samples.xlsx", sheet = 1)
dna <- readxl::read_xlsx("data/raw/2E_RPOE_DNA_extraction.xlsx", sheet = 1)
rpoe <- readxl::read_xlsx("data/raw/RPOE_participants_metadata.xlsx", sheet = 1)[-1,]
################################################################################
# clean ndvr
ndvr.2 <- ndvr %>%
  mutate(NDVR_id = sub("_.*", "", sample_ID),
         name = ifelse(is.na(name), sample_name, name)) %>%
  select(NDVR_id, saliva_barcode, Name = name) %>%
  mutate(saliva_barcode = ifelse(saliva_barcode=="NA", NA, saliva_barcode)) %>%
  distinct() %>%
  drop_na(saliva_barcode) %>%
  mutate(NDVR_id = paste0("NDVR_", str_pad(parse_number(NDVR_id), 6, pad = "0")))
################################################################################
# clean dna extraction
dna.2 <- dna %>%
  select(saliva_barcode = barcode_ID,
         wetlab_id = wetlab_ID,
         devGenes_id = 3)
################################################################################
# clean RPOE
rpoe.2 <- rpoe %>%
  select(Name, initials = Initials, devGenes_id, te_MRI_id = te_id, sex, DOB) %>%
  drop_na(devGenes_id)
################################################################################
dset.all.2 <- full_join(dna.2, rpoe.2)
dset.3 <- full_join(dset.all.2, ndvr.2) %>%
  mutate(saliva_barcode = paste0("SB_", saliva_barcode)) %>%
  mutate(devGenes_id = sub("_1", "", devGenes_id))
################################################################################
# devgenes dob
dev <- read_csv("data/raw/DevGenesDatabases-DevGenesinfomapping_DATA_2024-02-29_0955.csv")
# parent
dev.1 <- dev %>%
  mutate(dob = as.Date(dob),
         first_name = ifelse(is.na(first_name), first_name_821150, first_name),
         last_name = ifelse(is.na(last_name), last_name_528742, last_name),
         sex = ifelse(is.na(sex_56f64a), sex_56f64a_v2, sex_56f64a),
         sex = ifelse(sex == 1, "F", "M")) %>%
  select(devGenes_id = 1, dob, sex, first_name, last_name) %>%
  full_join(dev %>% 
              mutate(dob = as.Date(dob_v2)) %>% 
              select(devGenes_id = 1, dob) %>%
              drop_na()) %>%
  group_by(devGenes_id) %>%
  summarize(
    dob = na.omit(dob)[1],  # Extract the first non-NA dob value
    sex = na.omit(sex)[1],  # Extract the first non-NA sex value
    first_name = na.omit(first_name)[1],  # Extract the first non-NA first_name value
    last_name = na.omit(last_name)[1]  # Extract the first non-NA last_name value
  )
# child 1
dev.2 <- dev %>%
  mutate(dob = as.Date(dob_child_1),
         first_name = child1_name,
         last_name = ifelse(is.na(last_name), last_name_528742, last_name),
         sex = ifelse(is.na(child1_sex), child1_sex_v2, child1_sex),
         sex = ifelse(sex == 1, "F", "M")) %>%
  select(devGenes_id = 1, dob, sex, first_name, last_name) %>%
  full_join(dev %>% 
              mutate(dob = as.Date(dob_child_1_v2)) %>% 
              select(devGenes_id = 1, dob) %>%
              drop_na()) %>%
  group_by(devGenes_id) %>%
  summarize(dob = na.omit(dob)[1],  # Extract the first non-NA dob value
    sex = na.omit(sex)[1],  # Extract the first non-NA sex value
    first_name = na.omit(first_name)[1],  # Extract the first non-NA first_name value
    last_name = na.omit(last_name)[1]) %>%  # Extract the first non-NA last_name value
  drop_na(first_name) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_2"))
# child 2
dev.3 <- dev %>%
  mutate(dob = as.Date(dob_child_2),
         first_name = child2_name,
         last_name = ifelse(is.na(last_name), last_name_528742, last_name),
         sex = ifelse(is.na(child2_sex), child2_sex_v2, child2_sex),
         sex = ifelse(sex == 1, "F", "M")) %>%
  select(devGenes_id = 1, dob, sex, first_name, last_name) %>%
  full_join(dev %>% 
              mutate(dob = as.Date(dob_child_2_v2)) %>% 
              select(devGenes_id = 1, dob) %>%
              drop_na()) %>%
  group_by(devGenes_id) %>%
  summarize(dob = na.omit(dob)[1],  # Extract the first non-NA dob value
            sex = na.omit(sex)[1],  # Extract the first non-NA sex value
            first_name = na.omit(first_name)[1],  # Extract the first non-NA first_name value
            last_name = na.omit(last_name)[1]) %>%  # Extract the first non-NA last_name value
  drop_na(first_name) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_3"))
# child 3
dev.4 <- dev %>%
  mutate(dob = as.Date(dob_child_3),
         first_name = child3_name,
         last_name = ifelse(is.na(last_name), last_name_528742, last_name),
         sex = ifelse(is.na(child3_sex), child3_sex_v2, child3_sex),
         sex = ifelse(sex == 1, "F", "M")) %>%
  select(devGenes_id = 1, dob, sex, first_name, last_name) %>%
  full_join(dev %>% 
              mutate(dob = as.Date(dob_child_3_v2)) %>% 
              select(devGenes_id = 1, dob) %>%
              drop_na()) %>%
  group_by(devGenes_id) %>%
  summarize(dob = na.omit(dob)[1],  # Extract the first non-NA dob value
            sex = na.omit(sex)[1],  # Extract the first non-NA sex value
            first_name = na.omit(first_name)[1],  # Extract the first non-NA first_name value
            last_name = na.omit(last_name)[1]) %>%  # Extract the first non-NA last_name value
  drop_na(first_name) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_4"))
# child 4
dev.5 <- dev %>%
  mutate(dob = as.Date(dob_child_4),
         first_name = child4_name,
         last_name = ifelse(is.na(last_name), last_name_528742, last_name),
         sex = ifelse(is.na(child4_sex), child4_sex_v2, child4_sex),
         sex = ifelse(sex == 1, "F", "M")) %>%
  select(devGenes_id = 1, dob, sex, first_name, last_name) %>%
  full_join(dev %>% 
              mutate(dob = as.Date(dob_child_4_v2)) %>% 
              select(devGenes_id = 1, dob) %>%
              drop_na()) %>%
  group_by(devGenes_id) %>%
  summarize(dob = na.omit(dob)[1],  # Extract the first non-NA dob value
            sex = na.omit(sex)[1],  # Extract the first non-NA sex value
            first_name = na.omit(first_name)[1],  # Extract the first non-NA first_name value
            last_name = na.omit(last_name)[1]) %>%  # Extract the first non-NA last_name value
  drop_na(first_name) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_5"))
######### combine all dev
all.dev <- rbind(dev.1, dev.2, dev.3, dev.4, dev.5)
################################################################################
dset.4 <- full_join(dset.3 %>% select(-c(DOB, sex)) %>%
                      separate(Name, sep = " ", extra = "merge", 
                               into = c("first_name2", "last_name2")), 
                    all.dev) %>%
  mutate(first_name = ifelse(is.na(first_name), first_name2, first_name),
         last_name = ifelse(is.na(last_name), last_name2, last_name),
         initials = ifelse(is.na(initials)&!is.na(first_name)&!is.na(last_name), 
                           paste0(str_sub(first_name, start = 1, end = 1), 
                                  str_sub(last_name, start = 1, end = 1)), 
                           initials)) %>%
  select(-ends_with("name2")) %>%
  arrange(NDVR_id)
dset.4.ndvr <- dset.4 %>% filter(!is.na(NDVR_id))
dset.4.all <- dset.4 %>% filter(is.na(NDVR_id)) %>%
  mutate(NDVR_id = c(nrow(dset.4.ndvr)+1:(nrow(dset.4)-nrow(dset.4.ndvr))),
         NDVR_id = str_pad(NDVR_id, width = 6, side = "left", pad = "0"),
         NDVR_id = paste0("NDVR_", NDVR_id))

dset.clean <- rbind(dset.4.ndvr, dset.4.all)
################################################################################
write_delim(dset.clean, "data/derivatives/merged-master-table.tsv", delim = "\t")
