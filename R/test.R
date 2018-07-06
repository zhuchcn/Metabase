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
# file = "/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/data/lipidomics/raw_data/mx 302870_Zhu_CSH-QTOF MS lipidomics_03-2017_submit.xlsx"
# mset.raw = import_wcmc_excel_raw(
#     file = file, sheet = "Submit",  conc_range = "I8:BA611",
#     sample_range = "H1:BA7", feature_range = "A7:H611"
# )
# mset = subset_features(mset.raw, !is.na(feature_data(mset.raw)$Annotation))
# mset = collapse_QC(mset, qc_names = paste0("Biorec00", 1:5))
# mset = subset_features(
#     mset, apply(conc_table(mset), 1, function(x) sum(is.na(x)) < 8) )
# mset = transform_by_samples(
#     mset, function(x) ifelse(is.na(x), min(x, na.rm = TRUE)/2, x)
# )
# feature_data(mset)$class = assign_lipid_class(feature_data(mset)$Annotation)
#
#
#
# as(feature_data(mset), "data.frame") %>%
#     rownames_to_column("feature_id") %>%
#     group_by(Annotation) %>%
#     mutate(qc_cv == max(qc_cv))
#
# mset = subset_features(
#     mset,
#     grepl("iSTD$", feature_data(mset)$Annotation) |
# )


# feature_data(mset)$ESI = ifelse(grepl("\\+$", feature_data(mset)$Species),
#                                 "pos", "neg")
# feature_data(mset)$`InChI Key` = gsub("\\?$", "", feature_data(mset)$`InChI Key`)
# feature_data(mset)$`InChI Key` = gsub(" or .*", "", feature_data(mset)$`InChI Key`)
# feature_data(mset)$`InChI Key`[!grepl("N$", feature_data(mset)$`InChI Key`)] = paste0(
#     feature_data(mset)$`InChI Key`[!grepl("N$", feature_data(mset)$`InChI Key`)],
#     "N"
# )




