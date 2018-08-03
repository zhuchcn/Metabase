# load("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/data/hdl_structure_and_function.Rdata")
#
# edata = conc_table(as.matrix(lipidome$edata))
# pdata = sample_table(lipidome$pdata)
# fdata = feature_data(lipidome$fdata)
# experiment = MetabolomicsExperimentData(
#     institute                = "West Coast Metabolomics Center",
#     instrument               = "Agilent 6530",
#     resuspension_volumn_ul   = 110,
#     injection_volumn_ul      = 1.7,
#     data_processing_software = "MS-Dial version 2.54"
# )
# mset = MetabolomicsSet(conc_table = edata,
#                        sample_table = pdata,
#                        feature_data = fdata,
#                        experiment_data = experiment)
#
# study_design = new(
#     "StudyDesign",
#     institute = "UC Davis Department of Nutrition",
#     organism = "Human",
#     title = "Short-term effect of fast food vs Mediterranean diet to human HDL lipidome",
#     abstract = NULL
# )
#
# mset_sum = summarize_samples(mset, c("TX", "Day"))
#
# design = model.matrix(data = as(sample_table(mset), "data.frame"), ~TX*Day + Subj + 1)
# ################################################################################
# file = "data-raw/302870_Zhu_CSH-QTOF_lipidomics.xlsx"
# mset.raw = import_wcmc_excel(
#     file = file, sheet = "Submit",
#     conc_range = "I10:BA613",
#     sample_range = "H1:BA9",
#     feature_range = "A9:H613",
#     InChIKey = "InChI Key",
#     experiment_type = "Lipidomics"
# )
# mset = subset_features(mset.raw, !is.na(feature_data(mset.raw)$InChIKey))
# mset = collapse_QC(mset, qc_names = paste0("Biorec00", 1:5))
# mset = subset_features(
#     mset, apply(conc_table(mset), 1, function(x) sum(is.na(x)) < 8) )
# mset = transform_by_feature(
#     mset, function(x) ifelse(is.na(x), min(x, na.rm = TRUE)/2, x)
# )
# feature_data(mset)$class = assign_lipid_class(feature_data(mset)$Annotation)
# feature_data(mset)$ESI = ifelse(grepl("\\+$", feature_data(mset)$Species),
#                                 "pos", "neg")
# internal_standards = read.csv("data-raw/wcmc_lipidomics_standards.csv")
#
# experiment_data = experiment_data(mset)
# experiment_data(mset)$institute = "West Coast Metabolomics Center"
# experiment_data(mset)$sample_volumn_ul = 20
# experiment_data(mset)$internal_standards = internal_standards
#
# mset = calibrate_lipidomics_wcmc(mset, cid = "InChIKey", class = "class", ESI = "ESI")
# mset = filter_by_cv(mset, cv = "qc_cv", cid = "InChIKey")
# mset$feature_data$Annotation = lipid_name_formater(mset$feature_data$Annotation)
# ################################################################################
# file = "/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/raw_data/diet_data/Diet Data.7.3.18xlsx.xlsx"
# diet_data = read_excel(path = file, sheet = "Sheet1", range = "A1:Q81")
# diet_data = as.data.frame(diet_data)
# rownames(diet_data) = with(diet_data, paste0("Egg", `Study ID`, `Visit`))
# conc_table = conc_table(t(diet_data[,-(1:5)]))
# sample_table = sample_table(diet_data[,1:5])
# multiSet = MultiSet(conc_table = conc_table, sample_table = sample_table)
# ################################################################################
# file = "/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/analysis/raw_data/biogenic_amines/mx 349859_Zhu_HILIC-QTOF MSMS_11-2017_submit.xlsx"
# mset = import_wcmc_excel(
#     file = file,
#     sheet = "Submit",
#     conc_range = "I8:CS1296",
#     sample_range = "H1:CS7",
#     feature_range = "A7:H1296",
#     InChIKey = "InChI Key",
#     experiment_type = "Biogenic Amines"
# )
# mset = subset_features(mset, !is.na(feature_data(mset)$InChIKey) | !is.na(feature_data(mset)$Annotation))
#
# cas_ids = cts_convert(mset$feature_data$InChIKey[!is.na(mset$feature_data$InChIKey)], from = "inchikey", to = "CAS")
# names(cas_ids) = mset$feature_data$InChIKey[!is.na(mset$feature_data$InChIKey)]
#
# inchis = cs_convert(mset$feature_data$InChIKey[!is.na(mset$feature_data$InChIKey)],
#                     from = "inchikey", to = "inchi")
# names(inchis) = mset$feature_data$InChIKey[!is.na(mset$feature_data$InChIKey)]
################################################################################
wcmc_adduct = read.delim("data-raw/adduct.tsv", stringsAsFactors = FALSE)
# wcmc_adduct = mutate(
#     wcmc_adduct,
#     multiply = str_split_fixed(Ion.mass, " [+-] ", n=2)[,1],
#     multiply = ifelse(grepl("M\\/\\d{1}", multiply),
#                       1/as.integer(str_split_fixed(multiply, "\\/", n=2)[,2]),
#                       multiply),
#     multiply = gsub("(\\d{1})M", "\\1", multiply),
#     multiply = gsub("M", "1", multiply),
#     multiply = as.numeric(multiply),
#     plus = as.numeric(str_split_fixed(Ion.mass, " [+-] ", n=2)[,2]),
#     plus = ifelse(grepl("\\+", Ion.mass),
#                   plus, -plus)
# )

`M-H2O+H` = data.frame(
    Ion.name = "M-H2O+H",
    Ion.mass = "M - 17.00384",
    Charge = "1+",
    Mult = 1,
    Mass = -17.00384
)
wcmc_adduct = rbind(wcmc_adduct, `M-H2O+H`) %>%
    column_to_rownames("Ion.name")

