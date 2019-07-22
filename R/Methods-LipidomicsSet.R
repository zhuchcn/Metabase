################################################################################
#' @title Lipid annotation formater for lipidblast
#' @description A formater for lipid names for wcmc lipidblast annotations.
#' Lipid names are forced to be in a style of "PC 36:2 o", as lipid class
#' abbreviations followed by fatty acid length and number of double bonds. An
#' additional "d" in the end means a SM or Ceramide, and a "p" or "o" means a
#' plasmonyl-lipid.
#' @param x character vector of lipid names.
#' @return a character vector
#' @export
lipid_name_formater = function(x){
    # 2 names connected with a 'or'. Seriously wcmc, why?
    x = gsub("(.*)or.*","\\1",x) %>% str_trim(side = "both")
    x = str_split(x, "; ", simplify = T)[,1]
    # if fatty acids are listed separated
    pat = "\\d{1,2}:\\d{1}\\/{1}\\d{1,2}:\\d{1}"
    for(i in 1:length(x)){
        if(grepl(pat, x[i])){
            x[i] = gsub(
                pat,
                str_extract(x[i],pat) %>%
                    str_split("\\/", simplify = T) %>%
                    str_split("\\:", simplify = T) %>%
                    apply(2, function(xx) sum(as.integer(xx))) %>%
                    paste(collapse =":"),
                x[i])
        }
    }

    # remove Z, H, and E
    x = gsub("\\(\\d{1,2}[ZHE]\\)","",x)
    # CE
    x = gsub("\\({,1}(\\d{1,2}\\:\\d{1})\\){,1}\\s*Cholesteryl ester", "CE \\1", x)
    x = gsub("\\({,1}(\\d{1,2}\\:\\d{1})\\){,1}\\s*CE", "CE \\1", x)
    x = gsub("Cholesteryl ester\\s*\\({,1}(\\d{1,2}\\:\\d{1})\\){,1}.*", "CE \\1", x)
    x = gsub("CE\\s*\\(*(\\d{2}\\:\\d{1})\\)*.*", "CE \\1", x)
    # DG
    x = gsub('DAG', 'DG', x)
    x = gsub('Diacylglycerol', 'DG', x)
    x = gsub("DG\\s*\\({,1}(\\d{1,2}\\:\\d{1})\\){,1}", "DG \\1", x)
    # MG
    x = gsub('MAG', "MG", x)
    x = sub("MG\\s*\\({,1}(\\d{1,2}\\:\\d{1})\\){,1}", "MG \\1", x)
    # FA
    x = gsub("FA\\s*\\({,1}(\\d{1,2}\\:\\d{1})\\){,1}.*", "FA \\1", x)
    # DCER
    x = gsub('DCER\\(([0-9]{1,2}:[0-9]{1})\\)', 'Dihydroceramide \\1', x)
    # HCER
    x = gsub('HCER\\(([0-9]{1,2}:[0-9]{1})\\)', 'Hexosylceramide \\1', x)

    # Gal-Gal-Cer
    x = gsub("LCER", 'Lactosylceramide', x)
    x = gsub("^Gal-Gal-Cer\\s*\\({0,1}d(\\d{1,2}:\\d{1})\\){,1}.*", "Gal-Gal-Cer \\1 d",x)
    x = gsub("^Lactosylceramide\\s*\\(*d*(\\d{1,2}:\\d{1})\\)*.*", "Gal-Gal-Cer \\1 d",x)
    # GlcCer
    x = gsub("GlcCer\\s*\\(d(\\d{1,2}:\\d{1}).*", "GlcCer \\1 d", x)
    # Ceramide
    x = gsub("CERAMIDE", 'Ceramide', x)
    x = gsub('CER', 'Ceramide', x)
    x = gsub("^Ceramide\\s*\\(*d*(\\d{1,2}:\\d{1})\\)*.*", "Ceramide \\1 d",x)
    x = gsub("^Cer{,1}\\({,1}d(\\d{2}\\:\\d{1})\\){,1}.*", "Ceramide \\1 d",x)
    # LPC, LPE, PC, and PE
    x = gsub("(L{0,1}P[CEASI])\\s*\\(*(\\d{1,2}:\\d{1})\\)*.*", "\\1 \\2", x)
    x = gsub("(L{0,1}P[CEASI])\\s*\\(*([opOP]{1})-(\\d{1,2}:\\d{1})\\)*.*", "\\1 \\3 \\2", x)
    x = gsub("Plasmenyl-(L{0,1}P[CEASI])\\s*\\(*(\\d{1,2}:\\d{1})\\)*.*", "\\1 \\2 p", x)
    # OxPC
    x = gsub("OxPC\\s*\\(*(\\d{2}:\\d{1})\\)*.*", "PC \\1 ox", x)
    # SM
    x = gsub("SM\\s*\\(*d*(\\d{1,2}:\\d{1})\\)*.*","SM \\1 d", x)
    # TG
    x = gsub("TG\\s*\\(*(\\d{1,2}:\\d{1})\\)*.*","TG \\1", x)
    x = gsub("TAG\\s*\\(*(\\d{1,2}:\\d{1})\\)*.*","TG \\1", x)
    return(x)
}

################################################################################
#' @title Extract the carbon chain length from lipidome annotation
#' @description This function returns the total number of carbons from a given
#' lipid annotation name. For example, the carbon chain length of lipid PC 34:2
#' is 34. The lipid annotation names input must be the ouput of
#' \code{\link{lipid_name_formater}} function.
#' @param x character. The lipid annotation name. Must be formatted by the
#' \code{\link{lipid_name_formater}} function.
#' @export
#' @author Chenghao Zhu
nCarbons = function(x){
    ifelse(
        grepl("Cholesterol", x), 0,
        as.integer(str_split_fixed(str_split_fixed(x, " ", n = 2)[,2], ":", 2)[,1])
    )
}
################################################################################
#' @title Get the number of fatty acyls of a given lipid annotation.
#' @description This function returns the number of fatty acyls of a given
#' lipid annotation name. For example, the number of fatty acyl of lipid PC 34:2
#' is 2. The lipid annotation names input must be the ouput of
#' \code{\link{lipid_name_formater}} function.
#' @param x character. The lipid annotation name. Must be formatted by the
#' \code{\link{lipid_name_formater}} function.
#' @export
#' @author Chenghao Zhu
nFattyAcyls = function(x){
    class = str_split_fixed(x, " ", 2)[,1]
    return(ifelse(
        class == 'Cholesterol', 0,
        ifelse(
            class == 'TG', 3,
            ifelse(
                class %in% c('LPC', "LPE", "LPG", "LPI", "LPA", "MG", "FA"),
                1, 2
            )
        )
    ))
}
################################################################################
#' @title Get the number of double bonds
#' @description This function returns the number of double bonds from a given
#' lipid annotation name. For example, the number of double bonds of lipid PC 34:2
#' is 34. The lipid annotation names input must be the ouput of
#' \code{\link{lipid_name_formater}} function.
#' @param x character. The lipid annotation name. Must be formatted by the
#' \code{\link{lipid_name_formater}} function.
#' @export
#' @author Chenghao Zhu
nDoubleBonds = function(x){
    ifelse(
        grepl("Cholesterol", x), 0,
        as.integer(str_split_fixed(str_split_fixed(x, " ", 3)[,2], ":", 2)[,2])
    )
}
################################################################################
#' @title Calculate the average carbon chain length
#' @description Calculate the average carbon chain length of each lipid class.
#' The input must be a \code{\link{LipidomicsSet-class}} object. The object
#' must be contain a column in the feature_data slot with the annotation names
#' of each feature. The feature name must be formated using the
#' \code{\link{lipid_name_formater}} function.
#'
#' The average chain length is calculated using the equation below:
#'
#' \deqn{ACL = (\sum conc_{i,j} x nc_{i,j}) / (\sum con_{i,j} x nfa_{i,j})}
#'
#' The conc represents for the mol concentration, nc for number of carbon,
#' and nfa for number of fatty acyls. The i stands for the ith sample, while j
#' stands for the jth feature.
#'
#' @param object a \code{\link{LipidomicsSet-class}} object
#' @param name character. The name of the feature variable that contains the
#' annotation name. The annotation name must be formatted by the
#' \code{\link{lipid_name_formater}} function
#' @param class character. The name of the feature variable that contains the
#' lipid class.
#' @export
#' @author Chenghao Zhu
summarize_ACL = function(object, name, class){
    if(!isClass(object, Class = "LipidomicsSet"))
        stop("[ Metabase ] The object must be a LipidomcisSet-class object",
             call. = FALSE)
    if(!name %in% colnames(object@feature_data))
        stop("[ Metabase ] The lipid name variable " %+% name %+% " is not found. Please varify.", call. = FALSE)
    if(!class %in% colnames(object@feature_data))
        stop("[ Metabase ] The lipid class variable " %+% class %+% " is not found. Please varify.", call. = FALSE)

    # calculated using the equation below:
    # $ACL = \\frac{\\sum conc_{i,j} \\times nc_i}{\\sum con_{i,j} \\times nfa_i}$

    nCB = nCarbons(object@feature_data[, name])
    nFA = nFattyAcyls(object@feature_data[, name])
    class = object@feature_data[, class]

    mat1 = apply(object@conc_table * nCB, 2, function(col){
        tapply(col, class, function(x) sum(x, na.rm = TRUE))
    })
    mat2 = apply(object@conc_table * nFA, 2, function(col){
        tapply(col, class, function(x) sum(x, na.rm = TRUE))
    })
    mat = mat1 / mat2
    mat = mat[rownames(mat) != "Cholesterol",]
    Overall = apply(object@conc_table * nCB, 2, function(x) sum(x, na.rm = TRUE)) /
        apply(object@conc_table * nFA, 2, function(x) sum(x, na.rm = TRUE))
    mat = rbind(Overall, mat)
    LipidomicsSet(conc_table = conc_table(mat),
                  sample_table = sample_table(object))
}
################################################################################
#' @title Calculate the Equivalent Double Bond per 18 carbons
#' @description Calculate the equivalen double bonds per 18 carbons of each
#' lipid class. The input must be a \code{\link{LipidomicsSet-class}} object.
#' The object must be contain a column in the feature_data slot with the
#' annotation names of each feature. The feature name must be formated using the
#' \code{\link{lipid_name_formater}} function.
#'
#' The equivalent of double bond is calculated using the equation below:
#'
#' \deqn{EOD_{18} = (\sum conc_{i,j} x ndb_{i,j}) / (\sum conc_{i,j} x nc_{i,j}) x 18 }
#'
#' The conc represents for the mol concentration, ndb for number of double
#' bonds, and nc for number of carbon. The i stands for the ith sample, while j
#' stands for the jth feature.
#'
#' @param object a \code{\link{LipidomicsSet-class}} object
#' @param name character. The name of the feature variable that contains the
#' annotation name. The annotation name must be formatted by the
#' \code{\link{lipid_name_formater}} function
#' @param class character. The name of the feature variable that contains the
#' lipid class.
#' @export
#' @author Chenghao Zhu
summarize_EOD = function(object, name, class){
    if(!isClass(object, Class = "LipidomicsSet"))
        stop("[ Metabase ] The object must be a LipidomcisSet-class object",
             call. = FALSE)
    if(!name %in% colnames(object@feature_data))
        stop("[ Metabase ] The lipid name variable " %+% name %+% " is not found. Please varify.", call. = FALSE)
    if(!class %in% colnames(object@feature_data))
        stop("[ Metabase ] The lipid class variable " %+% class %+% " is not found. Please varify.", call. = FALSE)

    # calculated using the equation below:
    # $EOD_{18} = \\frac{ \\sum conc_{i,j} \\times ndb_{i,j} }{ \\sum conc_{i,j} \\times nc_{i,j}} \\times 18 $

    nDB = nDoubleBonds(object@feature_data[, name])
    nCB = nCarbons(object@feature_data[, name])
    class =object@feature_data[, class]

    mat1 = apply(object@conc_table * nDB, 2, function(col){
        tapply(col, class, function(x) sum(x, na.rm = TRUE))
    })
    mat2 = apply(object@conc_table * nCB, 2, function(col){
        tapply(col, class, function(x) sum(x, na.rm = TRUE))
    })
    mat = mat1 / mat2 * 18
    mat = mat[rownames(mat) != "Cholesterol",]
    Overall = apply(object@conc_table * nDB, 2, function(x) sum(x, na.rm = TRUE))/
        apply(object@conc_table * nCB, 2, function(x) sum(x, na.rm = TRUE)) * 18
    mat = rbind(Overall, mat)
    LipidomicsSet(conc_table = conc_table(mat),
                  sample_table = sample_table(object))
}
################################################################################
#' @title Calculate odd chain lipids abundance
#' @description Calculate the abundance of odd chain lipids of each
#' lipid class. The input must be a \code{\link{LipidomicsSet-class}} object.
#' @param object a \code{\link{LipidomicsSet-class}} object
#' @param name character. The name of the feature variable that contains the
#' annotation name. The annotation name must be formatted by the
#' \code{\link{lipid_name_formater}} function
#' @param class character. The name of the feature variable that contains the
#' lipid class.
#' @export
#' @author Chenghao Zhu
summarize_odd_chain = function(object, name, class){
    if(!isClass(object, Class = "LipidomicsSet"))
        stop("[ Metabase ] The object must be a LipidomcisSet-class object",
             call. = FALSE)
    if(!name %in% colnames(object@feature_data))
        stop("[ Metabase ] The lipid name variable " %+% name %+% " is not found. Please varify.", call. = FALSE)
    if(!class %in% colnames(object@feature_data))
        stop("[ Metabase ] The lipid class variable " %+% class %+% " is not found. Please varify.", call. = FALSE)

    nCB = nCarbons(object@feature_data[, name])

    object = subset_features(object, nCB %% 2 == 1)
    Overall = apply(object@conc_table, 2, function(x) sum(x, na.rm = TRUE))
    object = summarize_features(object, class)

    conc_table = rbind(Overall, object@conc_table) %>%
        conc_table()
    sample_table = sample_table(object)

    LipidomicsSet(conc_table = conc_table, sample_table = sample_table)
}
################################################################################
#' @title Calculate lipid classes ratios
#' @description Calculate ratios between different lipid classes. The input
#' must be a \code{\link{LipidomicsSet-class}} object. The lipid class ratios
#' calculated includes PC/LPC, PE/LPE, CE/free cholesterol, TG/DG, SM / Cer,
#' CE/TG, surface lipid / core lipid, PC / surface lipid, and SM / surface lipid.
#' @param object a \code{\link{LipidomicsSet-class}} object
#' @param name character. The name of the feature variable that contains the
#' annotation name. The annotation name must be formatted by the
#' \code{\link{lipid_name_formater}} function
#' @param class character. The name of the feature variable that contains the
#' lipid class.
#' @export
#' @author Chenghao Zhu
summarize_lipid_ratios = function(object, name, class){
    if(!isClass(object, Class = "LipidomicsSet"))
        stop("[ Metabase ] The object must be a LipidomcisSet-class object",
             call. = FALSE)
    if(!name %in% colnames(object@feature_data))
        stop("[ Metabase ] The lipid name variable " %+% name %+% " is not found. Please varify.", call. = FALSE)
    if(!class %in% colnames(object@feature_data))
        stop("[ Metabase ] The lipid class variable " %+% class %+% " is not found. Please varify.", call. = FALSE)

    object = summarize_features(object, class)
    featureNames(object) = tolower(featureNames(object))
    edata = object@conc_table

    keys = list(
        c('ce', "cholesterol"),
        c("pc", "lpc"),
        c("pe", "lpe"),
        c("pg", 'lpg'),
        c("pi", "lpi"),
        c("pa", "lpa"),
        c("tg", "dg"),
        c("tag", "dag"),
        c("sm", "cer"),
        c("sm", "ceremide"),
        c("tg", "ce"),
        c("tag", "ce")
    )

    ratios = NULL
    for(key in keys){
        if( key[1] %in% rownames(edata) & key[2] %in% rownames(edata)){
            ratios = rbind(ratios, edata[key[1],]/edata[key[2],])
            rownames(ratios)[nrow(ratios)] = key[1] %+% "/" %+% key[2]
        }
    }

    surface_lipids = c(
        "pc", "pe", "ps", "pi", "pg", "pa", "lpc", "lpe", "lps", "lpi", "lpa",
        "dg", "dag", "sm", "cer", "ceramide", "mg", "mag", "cholesterol",
        "dcer", 'ffa', 'hcer', 'lcer'
    )
    core_lipids = c("ce", "tg", 'tag')
    pl = c("pc", "pe", "ps", "pi", "pg", "pa", "lpc", "lpe", "lps", "lpi",
           "lpa", "sm")

    surface = colSums(edata[rownames(edata) %in% surface_lipids,])
    core = colSums(edata[rownames(edata) %in% core_lipids,])
    pl = colSums(edata[rownames(edata) %in% pl,])
    ratios = rbind(edata["pc",]/pl, ratios)
    ratios = rbind(edata["sm",]/pl, ratios)
    ratios = rbind(edata["pc",]/surface, ratios)
    ratios = rbind(edata["sm",]/surface, ratios)
    ratios = rbind(surface / core, ratios)
    rownames(ratios)[1:5] = c("surface/core", "sm/surface", "pc/surface", "sm/pl", "pc/pl")

    LipidomicsSet(conc_table = conc_table(ratios),
                  sample_table = sample_table(object))
}
################################################################################
#' @title calculate molecular weight from m/z
#' @description This function calculate the molecular weight according to the
#' m/z and adduct ion species.
#' @param species character value of the adduct ion species
#' @param mz numeric value of the m/z
#' @export
mz2molwt = function(species, mz){
    data("wcmc_adduct")
    multiply = wcmc_adduct[species,]$Mult
    plus = wcmc_adduct[species,]$Mass
    (mz - plus) / multiply
}
