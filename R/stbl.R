# stbl, stbl2sam


#' Synthetic Table
#'
#' Extract part of the series of the two synthetic tables, the TEE
#' (Tableau Economique d'ensemble) and the TES (Tableau Entrees
#' Sorties)
#'
#' Many arguments enable to make very customized querries in the data
#' base.
#' 
#' @name stbl
#' @importFrom stats setNames
#' @param an une annee
#' @param tableau the table
#' @param unit monetary unit, `K` for thousands of euros, `M` for
#'     millions of euros, `G` for billions of euros (the default)
#' @param format `large`for a table with one line for a product /
#'     sector for the TES, one line for a variable for the TEE. `long`
#'     for a table with one line for each value. `large` for a table
#'     which returns the production and the intermediate consumptions
#'     for sectors or by products
#' @param sgn `pos` to select variables that are ressources and `neg`
#'     to select variables that are expenses and `diff` (only for the
#'     TEE) for the difference between ressources and expenses
#' @param label a character vector that indicates labels that should
#'     be printed in the table
#' @param variables the variables to be extracted (by default, all the
#'     variables are returned)
#' @param naf the activity nomenclature
#' @param isbl a boolean, if `FALSE`, the default, ISBL are merged
#'     with households,
#' @param net_acc a boolean, if `TRUE`, the default, investment and
#'     stock variations are merged
#' @param sep_ent a boolean, if `TRUE`, financial and non financial
#'     societies are separated,
#' @param net_tax a boolean, if `TRUE`, the default, taxes on
#'     production and products are computed as the difference between
#'     taxes and subventions
#' @param simpl_dist a boolean, if `TRUE` (the default is `FALSE`),
#'     the distribution operations are simplified. All taxes and
#'     social cotisations are received by the State, all social
#'     prestations are paid by the State and received by households,
#'     income tax is only paid by enterprises and households
#' @param net_cap a boolean, if `TRUE`, capital income is splited in
#'     EBE, en revenu mixte brut and properties income
#' @param agents a subset of economic agents for the TEE
#' @param comptes a subset of accounts for the TEE
#' @param x an object of class `stbl` for the `stbl2sam` function
#' @format a `stbl` object, which inherits from data frame
#'     containing :
#' - for the long format: each line contains a variable
#' - for the large format:
#'   - for production sectors and products, each line contains a
#'     combination of years and sectors / products and each column is
#'     a variable
#'   - for the TEE: each line is a variable, each column is a
#'     combination of an agent and either ressource or expense
#' @examples
#' # by default, the products table
#' stbl(2023)
#' # with the nomenclature with 4 sectors for the years 2021, 2022,
#' # 2023 with only the ressources
#' stbl(an = 2021:2023, sgn = "pos", naf = "A4")
#' # the products table for 2020 with the 4 sectors nomenclature and a
#' # large format
#' stbl(2023, naf = "A4", format = "large")
#' # idem, but separate: investment and stock variations, ISBL and
#' # household, taxes and subventions
#' stbl(2023, naf = "A4", format = "large",
#'        net_acc = FALSE, net_tax = FALSE, isbl = TRUE)
#' # the large format is used with several years
#' stbl(2021:2023, naf = "A4", format = "large")
#' # sectors table, two variables (intermediate consumptions and wages
#' # are selected)
#' stbl(2020, tableau = "branches", naf = "A4", variables = c("ci.br", "l"))
#' # production and exploitation accounts with a large format
#' stbl(2020, tableau = "branches", naf = "A4", format = "large")
#' # margin format, the production and the intermediate consumptions
#' # of productive sectors
#' stbl(2020, tableau = "branches", naf = "A4", format = "marges")
#' # default TEE: financial and non financial societes are merged in a
#' # unique agent (`ent`), ISBL are merged with households fusionnees
#' stbl(2021, tableau = "tee")
#' # idem with ISBl, financial and non-financial enterprises,
#' # separation of taxes and subventions
#' stbl(2021, tableau = "tee", sep_ent = FALSE, isbl = TRUE, net_tax = FALSE)
#' # selection des menages et de l'Etat, pour les comptes de
#' # production et d'utilisation du revenu
#' stbl(2021, tableau = "tee", agents = c("men", "etat"),
#'        comptes = c("prod", "util"))
#' # idem, mais selection uniquement des ressources
#' stbl(2021, tableau = "tee", agents = c("men", "etat"),
#'        comptes = c("prod", "util"), sgn = "pos")
#' # a complete TEE with a large format
#' stbl(2021, tableau = "tee", format = "large")
#' # property income
#' stbl(2021, tableau = "tee", variables = "rp")
#' # net (ressources minus expenses) of property income
#' stbl(2021, tableau = "tee", variables = "rp", sgn = "diff")
#' @export
stbl <- function(an = 2023,
                 tableau = c("produits", "branches", "tei", "matprod", "tee"),
                 unit = c("G", "M", "K", "E"),
                 format = c("long", "marges", "large"),
                 sgn = NULL, 
                 label = NULL,
                 variables = NULL,
                 net_tax = TRUE,
                 isbl = FALSE,                   
                 naf = NULL,
                 net_acc = TRUE,
                   sep_ent = FALSE,
                 net_cap = TRUE,
                 simpl_dist = FALSE, 
                 agents = NULL,
                 comptes = NULL
                 ){
    
    ##########
    # 1. Setup
    ##########

    # sauvegarde des arguments
    .format <- match.arg(format)
    .tableau <- match.arg(tableau)
    .variables <- variables
    no_var_select <- is.null(.variables)
    .agents <- agents
    .unit <- match.arg(unit)
    .an <- as.character(an)
    .label <- label
    .sgn <- sgn
    .comptes <- comptes
    .naf <- naf
    is_tes <- .tableau %in% c("branches", "produits", "matprod", "tei")
    is_tee <- .tableau  == "tee"
    is_mat <- .tableau %in% c("matprod", "tei")
    is_brpr <- .tableau %in% c("branches", "produits")
    to_sum <- to_diff <- FALSE
    # vecteur indiquant les elements uniques correspondant des
    # colonnes qui seront supprimes a la fin
    uniques <- rep(FALSE, 5)
    names(uniques) <- c("an", "variables", "naf", "comptes", "sgn")
    uniques["an"] <- length(.an) == 1
    uniques["variables"] <-
        ! is.null(.variables) && length(.variables) == 1
    uniques["naf"] <- ! is.null(.naf) && .naf == "A1"
    uniques["sgn"] <- ! is.null(.sgn) && (length(.sgn) == 1)
    uniques["comptes"] <- ! is.null(.comptes) && (length(.comptes) == 1)

    if (is.null(.sgn)) .sgn <- c("neg", "pos")
    if (! is.null(.variables) & is_mat)
        stop("lorsque le tableau tei ou matprod est selectionne, pas de selection de variable")
    if (! uniques["an"] & is_mat)
        stop("pour tei et matprod, une seule annee doit etre selectionnee")
    
    # selection du tableau et des annees
    TBL$an <- factor(TBL$an)
    class(TBL) <- "data.frame"

    x <- TBL[TBL$tableau == .tableau & TBL$an %in% .an, - which(names(TBL) == "tableau")]
    x$an <- x$an[drop = TRUE]
    multi_naf <- (any(.an %in% 1949:1977) & any(.an %in% 1978:2023)) & is_tes
    if (multi_naf){
        x_A17 <- x[x$an %in% 1949:1977, ]
        x_A38 <- x[x$an %in% 1978:2023, ]
        .nomenc <- unique(nomenc_naf[, c("A38", "A17")]) |>
            setNames(c("naf", "nomenc"))
        x_A38 <- merge(x_A38, .nomenc, by.x = "compte", by.y = "naf", all.x = TRUE)
        x_A38 <- x_A38[- which(names(x_A38) == "compte")]
        x_A38 <- condsummary(x_A38, c("an", "nomenc", "variable", "sgn"), sum)
        x_A38 <- setaname(x_A38, "nomenc", "compte")
        x_A38$compte2 <- "none"
        x <- rbind(x_A17, x_A38)
    }
    
    if (is_tes){
        # message d'erreurs si la naf demandee est indisponible
        nafs <- c("A38", "A17", "A10", "Sections", "A4", "A1")
        base_naf <- ifelse(any(an < 1978), "A17", "A38") 
        if (any(.an < 1949)) stop("TES available from 1949")
        if (is.null(naf)) .naf <- base_naf else .naf <- naf
        if (! .naf %in% nafs) stop("unknown nomenclature")
        if (.naf %in% c("A38", "Sections") & any(an < 1978))
            stop("nomenclature not available for year < 1978")
    }

    if (is_tee){
        # comptes du TEE et soldes correspondants
        cptes <- c("prod", "expl", "affect", "dist", "util")
        soldes <- c("vab", "ebe", "rpb", "rdb", "s")
        # liste exhaustive des agents
        agts <- c("snf", "sf", "ent", "etat", "men", "isbl", "rdm")
        if (! is.null(.comptes) & ! is.null(.variables))
            stop("il n'est pas possible de selectionner a la fois des variables et des comptes dans un tee")
        # soit diff soit pos et/ou neg pour le tee
        if (is.null(.sgn)){
            .sgn <- c("pos", "neg")
            to_diff <- FALSE
        } else {
            if (! any(.sgn %in% c("pos", "neg", "diff"))){
                stop("sgn should be either pos, neg, diff or NULL, the default")
            } else {
                if ("diff" %in% .sgn){
                    if (any(c("pos", "neg") %in% .sgn))
                        stop("diff cannot be selected in sgn along with pos or/and neg")
                    .sgn <- c("pos", "neg")
                    to_diff <- TRUE
                }
            }
            # ajout du RMB a l'EBE pour les menages
            rmb <- x[x$variable == "rmb" & x$sgn == "pos", "valeur", drop = TRUE]
            x[x$variable == "ebe" & x$compte == "men" & x$sgn == "pos", "valeur"] <-
                x[x$variable == "ebe" & x$compte == "men" & x$sgn == "pos", "valeur"] + rmb
            x[x$variable == "ebe" & x$compte == "men" & x$sgn == "neg", "valeur"] <-
                x[x$variable == "ebe" & x$compte == "men" & x$sgn == "neg", "valeur"] + rmb
        }
        # ensemble des agents (contient ou non isbl, et ent ou sf et snf)
        all_agents <- unique(x$compte)
        if (! isbl) all_agents <- setdiff(all_agents, "isbl")
        if (! sep_ent) all_agents <- c(setdiff(all_agents, c("sf", "snf")), "ent")
        if (is.null(.comptes)) .comptes <- cptes
    }

    # selection des ressources et/ou des emplois

    if (! is_mat) x <- x[x$sgn %in% .sgn, ]

    # gestion des colonnes compte et compte2 : compte2 n'existe que
    # pour les tableaux matriciels (tei et matprod), dans ce cas, ils
    # sont renommes en branche et produit ; autrement, compte est
    # renomme en agent (tee), naf (branches et produits)
    if (is_mat){
        x <- setaname(x, "compte", "produit")
        x <- setaname(x, "compte2", "branche")
        ## names(x)[which(names(x) == "compte")] <- "produit"
        ## names(x)[which(names(x) == "compte2")] <- "branche"
    } else {
        x <- x[, - which(names(x) == "compte2"), drop = FALSE]
        ## if (is_tee) names(x)[which(names(x) == "compte")] <- "agent"
        ## if (is_brpr) names(x)[which(names(x) == "compte")] <- "naf"
        if (is_tee) x <- setaname(x, "compte", "agent")
        if (is_brpr) x <- setaname(x, "compte", "naf")
    }
    
    # selection des variables
    all_variables <- unique(x$variable)
    if (! is.null(.variables)){
        unknown_vars <- setdiff(.variables, all_variables)
        if (length(unknown_vars)){
            err_msg <- paste("variable(s)",
                             paste(unknown_vars, collapse = ", "),
                             "inconnue(s)")
            stop(err_msg)
        }
    } else .variables <- all_variables
    x <- x[x$variable %in% .variables, ]

    # selection et eventuelle fusion des agents (tee uniquement)
    if (is_tee){
       # eventuelle fusion des menages et des isbl, ainsi que des
       # societes financieres et non-financieres
        if (! isbl | ! sep_ent){
            to_sum <- TRUE
            if (! isbl)
                x$agent <- ifelse(x$agent == "isbl", "men", x$agent)
            if (! sep_ent)
                x$agent <- ifelse(x$agent %in% c("sf", "snf"), "ent", x$agent)
        }
        if (! is.null(.agents)){
            unknown_agts <- setdiff(.agents, all_agents)
            if (length(unknown_agts)){
                err_msg <- paste("agent(s)",
                             paste(unknown_agts, collapse = ", "),
                             "inconnue(s)")
                stop(err_msg)
            }
            x <- x[x$agent %in% .agents, ]
        }
    }

    # agregation de certaines variables
    if ("t.pr" %in% .variables & net_tax){
        x$variable <- ifelse(x$variable == "s.pr", "t.pr", x$variable)
        to_sum <- TRUE
    }
    if ("t.br" %in% .variables & net_tax){
        x$variable <- ifelse(x$variable == "s.br", "t.br", x$variable)
        to_sum <- TRUE
    }

    # pour le tableau des produits, eventuelle fusion des menages et
    # des isbl, ainsi que de la fbcf et de la variation des stocks
    if (.tableau == "produits"){
        if (net_acc){
            x$variable <- ifelse(x$variable == "ds", "fbcf", x$variable)
            to_sum <- TRUE
        }
        if (! isbl){
            x$variable <- ifelse(x$variable == "cfisbl", "cf", x$variable)
            to_sum <- TRUE
        }
    }
    # pour le TEE, association aux comptes et affectation des soldes a
    # ceux-ci, puis selection eventuelle
    if (.tableau == "tee"){
        x <- merge(x, codes_tee[, c("cpte", "variable")],
                   by = "variable", all.x = TRUE, sort = FALSE)
        class(x) <- c("tbl", "tbl_df", "data.frame")
        x$cpte[x$variable == "rmb"] <- "expl"
        for (i in 1:length(cptes)){
            .pre <- ifelse(i >= 2, soldes[i - 1], NA)
            .post <- soldes[i]
            if (! is.na(.pre))
                x[x$variable == .pre & x$sgn == "pos", "cpte"] <- cptes[i]
            x[x$variable == .post & x$sgn == "neg", "cpte"] <- cptes[i]
        }
    }
    # aggregation variables / agents
    if (is_brpr)
        ## x <- base_summary(x, c("an", "naf", "variable", "sgn"),
        ##                   function(x) data.frame(valeur = sum(x$valeur)))
        x <- condsummary(x, c("an", "naf", "variable", "sgn"), sum)
    
    if (is_tee){
        ## x <- base_summary(x, c("an", "cpte", "variable", "agent", "sgn"),
        ##                   function(x) data.frame(valeur = sum(x$valeur)))
        x <- condsummary(x, c("an", "cpte", "variable", "agent", "sgn"), sum)
        if (to_diff){
            x$valeur <- ifelse(x$sgn == "neg", - x$valeur, x$valeur)
            x <- condsummary(x, c("an", "cpte", "variable", "agent"), sum)
            ## x <- base_summary(x, c("an", "cpte", "variable", "agent"),
            ##                   function(x) data.frame(valeur = sum(x$valeur)))
        }
        x$variable <- factor(x$variable, levels = all_variables)
        x$cpte <- factor(x$cpte, levels = cptes)
        x <- x[order(x$cpte, x$variable), , drop = FALSE]
        x$variable <- as.character(x$variable)
        x$cpte <- as.character(x$cpte)
        x <- x[x$cpte %in% .comptes, , drop = FALSE]
    }
    
    # aggregation dans la nomenclature choisie (TES uniquement)

    if (is_tes){
        if (.naf != base_naf){
            .nomenc <- unique(nomenc_naf[, c(base_naf, .naf)]) |>
                setNames(c("naf", "nomenc"))
            if (.tableau %in% c("produits", "branches")){
                x <- merge(x, .nomenc, by = "naf", all.x = TRUE)
                x <- x[- which(names(x) == "naf")]
                ## x <- base_summary(x, c("an", "nomenc", "variable", "sgn"),
                ##                   function(x) data.frame(valeur = sum(x$valeur)))
                x <- condsummary(x, c("an", "nomenc", "variable", "sgn"), sum)
#                names(x)[which(names(x) == "nomenc")] <- "naf"
                x <- setaname(x, "nomenc", "naf")
            }
            if (.tableau %in% c("tei", "matprod")){
                x <- merge(x, .nomenc, by.x = "produit", by.y = "naf", all.x = TRUE)
                x <- x[, - which(names(x) == "produit")]
#                names(x)[which(names(x) == "nomenc")] <- "produit"
                x <- setaname(x, "nomenc", "produit")
                x <- merge(x, .nomenc, by.x = "branche", by.y = "naf", all.x = TRUE)
                x <- x[, - which(names(x) == "branche")]
#                names(x)[which(names(x) == "nomenc")] <- "branche"
                x <- setaname(x, "nomenc", "branche")
                ## x <- base_summary(x, c("an", "produit", "branche"),
                ##                   function(x) data.frame(valeur = sum(x$valeur)))
                x <- condsummary(x, c("an", "produit", "branche"), sum)
            }
        }
    }

    # Changement d'unite
    if (.unit != "G"){
        if (.unit == "G") E10 <- 0
        if (.unit == "K") E10 <- - 6
        if (.unit == "M") E10 <- - 3
        if (.unit == "E") E10 <- - 9
        x$valeur <- x$valeur / 10 ^ E10
    }

    if (uniques["an"]) x <- x[, - which(names(x) == "an")]
    if (is_brpr & uniques["naf"]) x <- x[, - which(names(x) == "naf")]
    if (uniques["variables"]) x <- x[, - which(names(x) == "variable")]
    if (uniques["sgn"] & ! to_diff) x <- x[, - which(names(x) == "sgn")]

    structure(x, an = .an, tableau = .tableau, naf = .naf,
              net_acc = net_acc, net_cap = net_cap,
              simpl_dist = simpl_dist,
              class = c("tbl_df", "tbl", "data.frame"))
}

#' @rdname stbl
#' @export
stbl2sam <- function(x){
    class(x) <- "data.frame"
    .tableau <- attr(x, "tableau")
    .an <- attr(x, "an")
    .naf <- attr(x, "naf")
    net_acc <- attr(x, "net_acc")
    net_cap <- attr(x, "net_cap")
    simpl_dist <- attr(x, "simpl_dist")
    if (.tableau %in% c("branches", "produits", "tei", "matprod"))
        if (.naf == "A1")
            x$naf <- "A"
    if (.tableau == "branches"){
        x <- x[! x$variable %in% c("ci.br", "y.br"), ]
        x$row_1[x$variable %in% c("t.br", "s.br")] <- "tax"
        x$row_1[x$variable %in% c("l", "k")] <- "fact"
        ## names(x)[which(names(x) == "variable")] <- "row_2"
        ## names(x)[which(names(x) == "naf")] <- "col_2"
        x <- setaname(x, "variable", "row_2")
        x <- setaname(x, "naf", "col_2")
        x$col_1 <- "br"
        x <- x[- which(names(x) == "sgn")]
    }
    if (.tableau == "produits"){
        if (net_acc){
            acc <- x[x$variable == "fbcf", ]
            acc$variable <- "acc"
        } else {
            acc <- x[x$variable %in% c("ds", "fbcf"), ]
            acc$variable <- "acc"
            ## acc <- base_summary(acc, c("naf", "variable", "sgn"),
            ##                     function(x) data.frame(valeur = sum(x$valeur)))
            acc <- condsummary(acc, c("naf", "variable", "sgn"), sum)
        }
        x <- rbind(x, acc)

        emplois <- x[x$sgn == "neg" & x$variable != "ci.pr", ]
        emplois <- emplois[- which(names(x) == "sgn")]
        emplois$variable <- as.character(emplois$variable)
        emplois$variable[emplois$variable == "cf"] <- "men"
        emplois$variable[emplois$variable == "g"] <- "etat"
        emplois$variable[emplois$variable == "x"] <- "rdm"
        emplois$variable[emplois$variable == "cfisbl"] <- "isbl"
        emplois$col_1 <- NA
        emplois$col_1[emplois$variable %in% c("men", "etat", "rdm", "isbl")] <- "agt"
        emplois$col_1[emplois$variable %in% c("fbcf", "ds", "acc")] <- "acc"
        emplois$row_1 <- "pr"
        names(emplois)[which(names(emplois) == "naf")] <- "row_2"
        names(emplois)[which(names(emplois) == "variable")] <- "col_2"
        ressources <- x[x$sgn == "pos" & x$variable != "y.pr", ]
        ressources <- ressources[- which(names(x) == "sgn")]
        ressources$variable <- as.character(ressources$variable)
        ressources$variable[ressources$variable == "m"] <- "rdm"
        ressources$row_1 <- NA
        ressources$row_1[ressources$variable == "rdm"] <- "agt"
        ressources$row_1[ressources$variable %in% c("mc", "mt")] <- "mg"
        ressources$row_1[ressources$variable %in% c("t.pr", "s.pr", "tva")] <- "tax"
        names(ressources)[which(names(ressources) == "variable")] <- "row_2"
        ressources$col_1 <- "pr"
        names(ressources)[which(names(ressources) == "naf")] <- "col_2"


        x <- rbind(emplois, ressources)
    }
    if (.tableau == "matprod"){
        x <- setaname(x, "branche", "row_2")
        x <- setaname(x, "produit", "col_2")
        ## names(x)[which(names(x) == "branche")] <- "row_2"
        ## names(x)[which(names(x) == "produit")] <- "col_2"
        x$row_1 <- "br"
        x$col_1 <- "pr"
    }
    if (.tableau == "tei"){
        x <- setaname(x, "produit", "row_2")
        x <- setaname(x, "branche", "col_2")
        ## names(x)[which(names(x) == "produit")] <- "row_2"
        ## names(x)[which(names(x) == "branche")] <- "col_2"
        x$row_1 <- "pr"
        x$col_1 <- "br"
    }
    if (.tableau == "tee"){
        tva <- sum(TBL$valeur[TBL$variable == "tva" & TBL$an == .an])
        tax_ress <- x[x$sgn == "pos" & x$variable %in% c("t.pr.ag", "s.pr.ag", "t.br.ag", "s.br.ag"), ]
        tax_ress <- tax_ress[- which(names(x) %in% c("cpte", "sgn"))]
        t.pr_etat <- x[x$agent == "etat" & x$variable == "t.pr.ag", "valeur", drop = TRUE]
        tax_ress[tax_ress$variable == "t.pr.ag" & tax_ress$agent == "etat", "valeur"] <- t.pr_etat - tva
        tax_ress <- rbind(tax_ress, data.frame(variable = "tva", agent = "etat", valeur = tva))
        tax_ress$row_1 <- "agt"
        tax_ress$col_1 <- "tax"
        names(tax_ress)[which(names(tax_ress) == "agent")] <- "row_2"
        names(tax_ress)[which(names(tax_ress) == "variable")] <- "col_2"
        if (simpl_dist){
            tax_ress$row_2 <- "etat"
            ## tax_ress <- base_summary(tax_ress, c("row_1", "row_2", "col_1", "col_2"),
            ##                          function(x) data.frame(valeur = sum(x$valeur)))
            tax_ress <- condsummary(tax_ress, c("row_1", "row_2", "col_1", "col_2"), sum)
        }
        tot_fact <- TBL[TBL$variable %in% c("l", "k") & TBL$an == .an & TBL$tableau == "branches", ]
        ## tot_fact <- base_summary(tot_fact, "variable",
        ##                          function(x) data.frame(valeur = sum(x$valeur)))
        tot_fact <- condsummary(tot_fact, "variable", sum)
        tot_l <- tot_fact[tot_fact$variable == "l", "valeur", drop = TRUE]
        tot_k <- tot_fact[tot_fact$variable == "k", "valeur", drop = TRUE]
        l_rdm_ress <- x[x$variable == "l.ag" & x$agent == "rdm" & x$sgn == "pos", "valeur", drop = TRUE]
        l_rdm_emp <- x[x$variable == "l.ag" & x$agent == "rdm" & x$sgn == "neg", "valeur", drop = TRUE]
        tot_l <- tot_l + l_rdm_emp
        travail <- data.frame(row_1 = character(0), row_2 = character(0),
                              col_1 = character(0), col_2 = character(0),
                              valeur = numeric(0)) |> 
            rbind(data.frame(row_1 = "agt", row_2 = "rdm", col_1 = "fact", col_2 = "l", valeur = l_rdm_ress)) |>
            rbind(data.frame(row_1 = "fact", row_2 = "l", col_1 = "agt", col_2 = "rdm", valeur = l_rdm_emp)) |>
            rbind(data.frame(row_1 = "agt", row_2 = "men", col_1 = "fact", col_2 = "l",
                             valeur = tot_l - l_rdm_ress))
        EBE <- x[x$variable == "ebe" & x$sgn == "pos", ]
        EBE$valeur <- EBE$valeur / sum(EBE$valeur) * tot_k
        EBE <- EBE[- which(names(EBE) %in% c("cpte", "sgn", "variable"))]
        EBE$variable <- "ebe"

        RP <- x[x$variable == "rp", ]
        RP$valeur <- ifelse(RP$sgn == "neg", - RP$valeur, RP$valeur)
        ## RP <- base_summary(RP, "agent",
        ##                    function(x) data.frame(valeur = sum(x$valeur)))
        RP <- condsummary(RP, "agent", sum)
        RP$variable <- "rp"

        RMB <- x[x$variable == "rmb" & x$sgn == "neg", c("agent", "valeur")]
        RMB$variable <- "rmb"
        capital <- rbind(rbind(EBE, RP), RMB)
        capital <- reshape(capital, idvar = "agent", timevar = "variable", direction = "wide")
        for (i in 2:length(capital)) capital[[i]] <- ifelse(is.na(capital[[i]]), 0, capital[[i]])
        names(capital)[- 1] <- substr(names(capital)[- 1], 8, 100)
        capital$k <- capital$ebe + capital$rp
        capital$ebe <- capital$ebe - capital$rmb
        names(capital)[- 1] <- paste("valeur", names(capital)[- 1], sep = ".")
        capital <- reshape(capital, varying = c("valeur.ebe", "valeur.rp", "valeur.rmb", "valeur.k"),
                           direction = "long")
        capital <- capital[- which(names(capital) == "id")]
        names(capital)[which(names(capital) == "time")] <- "variable"
        capital$row_1 <- "agt"
        names(capital)[which(names(capital) == "agent")] <- "row_2"
        capital$col_1 <- "fact"
        names(capital)[which(names(capital) == "variable")] <- "col_2"
        capital <- capital[capital$valeur != 0, ]
        if (net_cap)
            capital <- capital[capital$col_2 == "k", ]
        facteurs <- rbind(travail, capital)
        ress_dist <- x[x$variable %in% c("atc", "cot", "ir", "prest") & x$sgn == "pos", ]
        emp_dist <- x[x$variable %in% c("atc", "cot", "ir", "prest") & x$sgn == "neg", ]
        ress_dist <- ress_dist[- which(names(ress_dist) %in% c("cpte", "sgn"))]
        ress_dist$row_1 <- "agt"
        ress_dist$col_1 <- "dist"
        names(ress_dist)[names(ress_dist) == "agent"] <- "row_2"
        names(ress_dist)[names(ress_dist) == "variable"] <- "col_2"

        emp_dist <- emp_dist[- which(names(emp_dist) %in% c("cpte", "sgn"))]
        emp_dist$row_1 <- "dist"
        emp_dist$col_1 <- "agt"
        names(emp_dist)[names(emp_dist) == "agent"] <- "col_2"
        names(emp_dist)[names(emp_dist) == "variable"] <- "row_2"

        if (simpl_dist){
            ress_dist$row_2[ress_dist$col_2 == "prest"] <- "men"
            ress_dist$row_2[ress_dist$col_2 == "cot"] <- "etat"
            ## ress_dist <- base_summary(ress_dist, c("row_1", "row_2", "col_1", "col_2"),
            ##                           function(x) data.frame(valeur = sum(x$valeur)))
            ress_dist <- condsummary(ress_dist, c("row_1", "row_2", "col_1", "col_2"), sum)
            emp_dist$col_2[emp_dist$row_2 == "prest"] <- "etat"
            emp_dist$col_2[emp_dist$row_2 == "cot"] <- "men"
            ## emp_dist <- base_summary(emp_dist, c("row_1", "row_2", "col_1", "col_2"),
            ##                          function(x) data.frame(valeur = sum(x$valeur)))
            emp_dist <- condsummary(emp_dist, c("row_1", "row_2", "col_1", "col_2"), sum)
            emp_dist <- emp_dist[! (emp_dist$row_2 == "ir" & emp_dist$col_2 %in% c("etat", "rdm")), ]
            IR <- sum(emp_dist[emp_dist$row_2 == "ir" & emp_dist$col_2 %in% c("ent", "men"), "valeur"])
            ress_dist[ress_dist$row_2 == "etat" & ress_dist$col_2 == "ir", "valeur"] <- IR
        }
        x <- rbind(rbind(rbind(tax_ress, facteurs), ress_dist), emp_dist)
    }
    x[c("row_1", "row_2", "col_1", "col_2", "valeur")]
}


