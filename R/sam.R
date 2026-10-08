# sam

#' Social Account Matrix
#'
#' Generate a SAM for France, many arguments enable the user to
#' customize the result
#'
#' `sam` returns a data frame in long format (one line for each
#' value), with a `"sam"` class. The `as.matrix` method transforms the
#' SAM in a large format, each row and column being an account.
#'
#' @name sam
#' @param an a year
#' @param object,x an object of class `sam`,
#' @param tot_gen a boolean, if `TRUE`, a general total is added for
#'     each account
#' @param naf,isbl,net_acc,sep_ent,net_tax,simpl_dist,net_cap see
#'     `stbl`
#' @param rev_int a boolean, if `TRUE`, intermediary incomes (primary
#'     income and disposable income are added to the SAM)
#' @param trace a boolean, if `TRUE` (the default is `FALSE`), some
#'     information messages are printed while computing the SAM
#' @param unit monetary unit, `K` for thousands of euros, `M` for
#'     millions of euros, `G` for billions of euros (the default) and
#'     `PIB` for a 100 base for the PIB,
#' @param ... further arguments for the `summary` and the `as.matrix`
#'     method (currently unused)
#' @return an object of class `mcs` which inherits the `data.frame`
#'     class
#' @importFrom stats na.omit ave reshape
#' @export
sam <- function(an = NULL,
                naf = c("A17", "A38", "A1", "A4", "A10", "Sections"),
                tot_gen = TRUE,
                isbl = FALSE,
                sep_ent = FALSE,
                net_acc = TRUE,
                net_tax = TRUE,
                net_cap = TRUE,
                rev_int = FALSE,
                simpl_dist = FALSE,
                trace = FALSE,
                unit = c("G", "M", "K", "PIB")){

    # ------------------------------------
    # 1. Sets and nomenclature definitions
    # ------------------------------------
    
    .unit <- match.arg(unit)
    .an <- an
    if (is.null(.an)) stop("the an argument is mandatory")
    .tol <- sqrt(.Machine$double.eps)
    .nomenc <- match.arg(naf)
    agents <- c("men", "etat", "rdm")
    if (isbl) agents <- c(agents, "isbl")
    if (sep_ent){
        agents <- c(agents, "sf", "snf")
    } else {
        agents <- c(agents, "ent")
    }
    
    facteurs <- c("l", "k")
    impots <- c("t.br", "t.pr", "tva")
    if (! net_tax) impots <- c(impots, "s.pr", "s.br")
    marges <- c("mc", "mt")
    .var_int <- c("ebe", "rmb", "rp", "RPB", "RDB", "fbcf", "ds")

    # --------------------------------------
    # 2. Reading and formating of the series 
    # --------------------------------------

    PRODUITS <- stbl(.an, tableau = "produits",
                       net_tax = net_tax, isbl = isbl,
                       net_acc = net_acc, naf = .nomenc) |>
        stbl2sam()

    BRANCHES <- stbl(.an, tableau = "branches",
                       net_tax = net_tax,  naf = .nomenc) |>
        stbl2sam()

    MATPROD <- stbl(.an, , tableau = "matprod",
                      naf = .nomenc) |>
        stbl2sam()

    TEI <- stbl(.an, tableau = "tei",
                  naf = .nomenc) |>
        stbl2sam()

    TEE <- stbl(.an, tableau = "tee",
                  net_tax = net_tax,
                  sep_ent = sep_ent,
                  net_cap = net_cap,
                  isbl = isbl,
                  simpl_dist = simpl_dist) |>
        stbl2sam()

    mcs_dom <- PRODUITS |>
        rbind(BRANCHES) |>
        rbind(MATPROD) |>
        rbind(TEI) |>
        rbind(TEE)
    class(mcs_dom) <- "data.frame"

    # -------------------------
    # 3. Computation of the PIB
    # -------------------------

    TES_pib <- stbl(an = .an, net_tax = FALSE, net_acc = FALSE,
                      isbl = TRUE, tableau = "produits") |>
        rbind(stbl(an = .an, net_tax = FALSE,
                   tableau = "branches"))
    class(TES_pib) <- "data.frame"
    TES_pib <- TES_pib[- which(names(TES_pib) == "sgn")]
    TES_pib <- reshape(TES_pib, direction = "wide", idvar = "naf", timevar = "variable")
    p <- lapply(TES_pib[- 1], function(x) sum(x, na.rm = TRUE)) |> as.data.frame()
    names(p) <- substr(names(p), 8, 200)
    pib_prod <- p$y.br - p$ci.br + p$tva + p$t.pr + p$s.pr
    pib_dem <- p$cf + p$g + p$cfisbl + p$fbcf + p$ds + p$x - p$m
    pib <- data.frame(pib_prod = pib_prod, pib_dem = pib_dem)
    if (abs(pib_dem - pib_prod) > 1E-01)
        stop("problem in the computation of the PIB")


    entete <- function (x){
        z <- nchar(x)
        cat(paste("  ", paste(rep("-", z), collapse = ""), "\n", 
                  sep = ""))
        cat(paste("  ", x, "\n", sep = ""))
        cat(paste("  ", paste(rep("-", z), collapse = ""), "\n", 
                  sep = ""))
    }

    if (trace){
        entete("Computation of the PIB")
        cat(paste("      Final demands : ", round(pib$pib_dem,  2), "\n", sep = ""))
        cat(paste("      Added values  : ", round(pib$pib_prod, 2), "\n", sep = ""))
    }

    # --------------------------------
    # 4. Savings, computated as a sold
    # --------------------------------
    class(mcs_dom) <- "data.frame"


    if (rev_int){
        # Factor (capital and labor) incomes
        .revint <- mcs_dom[mcs_dom$row_1 == "agt" &
                           mcs_dom$col_1 == "fact" &
                           mcs_dom$col_2 %in% c("l", "k"), ]
        ## .revint <- base_summary(.revint, c("row_1", "row_2"),
        ##                         function(x) data.frame(valeur = sum(x$valeur)))
        .revint <- condsummary(.revint, c("row_1", "row_2"), sum)
        # For the State, production and product taxes ressources are
        # added"
        .tax_pr_br <- sum(mcs_dom[mcs_dom$row_1 == "agt" &
                                  mcs_dom$row_2 == "etat" &
                                  mcs_dom$col_1 == "tax"  &
                                  mcs_dom$col_2 %in% impots, "valeur"])
        .revint[.revint$row_2 == "etat", "valeur"] <- .revint[.revint$row_2 == "etat", "valeur"] +
            .tax_pr_br
        .revint$col_1 <- "RPB"
        .revint$col_2 <- "RPB"
        mcs_dom <- rbind(mcs_dom, .revint)

        .RPB <- mcs_dom[mcs_dom$col_1 == "RPB", c("row_2", "valeur")]
        names(.RPB)[which(names(.RPB) == "row_2")] <- "agt"
        .ress <- mcs_dom[mcs_dom$col_1 == "dist", c("row_2", "valeur")]
        names(.ress)[which(names(.ress) == "row_2")] <- "agt"
        .emp <- mcs_dom[mcs_dom$row_1 == "dist", c("col_2", "valeur")]
        names(.emp)[which(names(.emp) == "col_2")] <- "agt"
        .emp$valeur <- - .emp$valeur
        .RDB <- rbind(rbind(.RPB, .ress), .emp)
        ## .RDB <- base_summary(.RDB, "agt",
        ##                      function(x) data.frame(valeur = sum(x$valeur)))
        .RDB <- condsummary(.RDB, "agt", sum)
        .RDB$row_1 <- "agt"
        .RDB$col_1 <- "RDB"
        .RDB$col_2 <- "RDB"
        names(.RDB)[which(names(.RDB) == "agt")] <- "row_2"
        mcs_dom <- rbind(mcs_dom, .RDB)
    }

    ress_agt <- mcs_dom[mcs_dom$row_1 == "agt" &
                        ! mcs_dom$col_2 %in% .var_int, ]
    ## ress_agt <- base_summary(ress_agt, "row_2",
    ##                          function(x) data.frame(ress = sum(x$valeur)))
    ress_agt <- condsummary(ress_agt, "row_2", sum) |>
        setaname("valeur", "ress")

    emp_agt <- mcs_dom[mcs_dom$col_1 == "agt" &
                       ! mcs_dom$row_2 %in% .var_int, ]

    ## emp_agt <- base_summary(emp_agt, "col_2",
    ##                         function(x) data.frame(emp = sum(x$valeur)))
    emp_agt <- condsummary(emp_agt, "col_2", sum) |>
        setaname("valeur", "emp")
    epargne <- merge(ress_agt, emp_agt, by.x = "row_2", by.y = "col_2")
    epargne$epargne <- epargne$ress - epargne$emp

    # savings are added to the accumulation account

    for (i in agents){
        s_i <- epargne[epargne$row_2 == i, "epargne", drop = TRUE]
        mcs_dom <- mcs_dom |>
            rbind(data.frame(row_1 = "acc", row_2 = "acc",
                    col_1 = "agt", col_2 = i, valeur = s_i))
    }

    # -------------------------------------------
    # 5. Check of the equilibrium of the accounts
    # -------------------------------------------

    class(mcs_dom) <- c("sam", class(mcs_dom))

    .verif <- verif_eq(mcs_dom)

    .max_abs_diff <- max(.verif$abs_diff)
    .max_rel_diff <- max(.verif$rel_diff)
    
    if (trace){        
        entete("Check of the equilibrium of the accounts")
        cat(paste("      Maximal absolute difference : ",
                  round(.max_abs_diff, 8), "\n", sep = ""))
        cat(paste("      Maximal relative difference : ",
                  round(.max_rel_diff, 8), "\n", sep = ""))
    }

    
    # -----------------
    # 13. Account total
    # -----------------
    if (tot_gen){
        rows <- mcs_dom[! mcs_dom$col_2 %in% .var_int, ]
        ## rows <- base_summary(rows, c("row_1", "row_2"),
        ##                      function(x) data.frame(valeur = sum(x$valeur, na.rm = TRUE)))
        rows <- condsummary(rows, c("row_1", "row_2"), sum)
        rows$col_1 <- rows$col_2 <- "total"

        cols <- mcs_dom[! mcs_dom$row_2 %in% .var_int, ]
        ## cols <- base_summary(cols, c("col_1", "col_2"),
        ##                      function(x) data.frame(valeur = sum(x$valeur, na.rm = TRUE)))
        cols <- condsummary(cols, c("col_1", "col_2"), sum)
        cols$row_1 <- cols$row_2 <- "total"

        mcs_dom <- mcs_dom |>
            rbind(cols) |> 
            rbind(rows) |>
            rbind(data.frame(row_1 = "total", row_2 = "total",
                    col_1 = "total", col_2 = "total",
                    valeur = sum(rows$valeur)))
    }
    # Monetary unit
    if (.unit == "G") deflator <- 1
    if (.unit == "K") deflator <- 1E-06
    if (.unit == "M") deflator <- 1E-03
    if (.unit == "PIB") deflator ~ pib$pib_dem / 100

    mcs_dom$valeur[abs(mcs_dom$valeur) < .tol] <- 0
    mcs_dom$valeur <- mcs_dom$valeur / deflator

    .pib <- round(pib$pib_prod / deflator, 2)
    attr(mcs_dom, "description") <- list(an = .an, naf = .nomenc,
                                         unit = .unit, pib = .pib)
    attr(mcs_dom, "check") <- data.frame(pib_dem  = pib$pib_dem   / deflator,
                                         pib_prod = pib$pib_prod  / deflator,
                                         max_abs  = .max_abs_diff / deflator,
                                         max_rel  = .max_rel_diff)
    rownames(mcs_dom) <- NULL
    class(mcs_dom) <- c("sam", "tbl_df", "tbl", "data.frame")
    mcs_dom
}

#source("./sam/R/stbl.R");source("./sam/R/sam.R");source("./sam/R/xtbl.R");source("./sam/R/methodes.R");load("./sam/R/sysdata.rda");library(tinytable);library(tinyplot)
