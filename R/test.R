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
