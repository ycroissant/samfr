read_yx <- function(x){
    lhs <- paste(deparse(x[[2]]))
    lhs <- strsplit(lhs, split = " \\+ ")[[1]]
    rhs <- attr(terms(x), "term.labels")
    list(lhs = lhs, rhs = rhs)
}

guess_naf <- function(x){
    thenaf <- unique(x$naf)
    nafA38 <- unique(nomenc_naf$A38)
    nafA17 <- unique(nomenc_naf$A17)
    nafA10 <- unique(nomenc_naf$A10)
    nafSections <- unique(nomenc_naf$Sections)
    nafA4 <- unique(nomenc_naf$A4)
    nafA1 <- unique(nomenc_naf$A1)
    nafs <- unique(c(nafA1, nafA4, nafSections, nafA10, nafA17, nafA38))
    if (length(setdiff(thenaf, nafA4)) == 0) .naf <- "A4"
    if (length(setdiff(thenaf, nafA38)) == 0) .naf <- "A38"
    if (length(setdiff(thenaf, nafA17)) == 0) .naf <- "A17"
    if (length(setdiff(thenaf, nafSections)) == 0) .naf <- "Sections"
            if (length(setdiff(thenaf, nafA10)) == 0) .naf <- "A10"
    .naf
}


#' Transformation of the raw table
#'
#' The raw table obtained using `"stbl"` is in a long format and
#' contains several characters and one numerical colomn. The raw table
#' can be nicely formated by pivoting it wider and creating groups of
#' rows and of columns
#'
#' @name xtbl
#' @param x a data frame,
#' @param formula a formula indicating the lines of the resulting
#'     table on the left-hand side and the columns on the right-hand
#'     side
#' @param shares a character vector indicating the variables for which
#'     the proportions should be computed
#' @param labels a character vector indicating variables for which
#'     labels will be used in the table
#' @return a data frame
#' @importFrom stats terms
#' @export
xtbl <-  function (x, formula, shares = NULL,
                   labels = NULL){

    if(is.null(labels)) .labels <- character(0) else .labels <- labels
    
    if (is.null(shares)){
        .shares <- NULL
    } else {
        .shares <- shares
        if (! is.character(.shares)) stop("shares should be a character vector")
        if (! all(.shares %in% names(x))) stop("unknown variable in the share argument")
    }

    levs <- rbind(lib_codes,
                  data.frame(code = as.character(1949:2023),
                             lib = as.character(1949:2023),
                             stringsAsFactors = FALSE))
    if (any(names(x) == "naf")){
        .naf <- guess_naf(x)
        lib_naf <- lib_naf[lib_naf$naf == .naf, c("code", "lib")]
        levs <- rbind(levs, lib_naf)
    }

    levs_lib <- levs$lib
    names(levs_lib) <- levs$code

    if ("variable" %in% names(x)) 
        .variable <- unique(x$variable)
    else .variable <- NA
    oclass <- class(x)
    x <- as.data.frame(x)

    .formula <- read_yx(formula)
    .rows <- .formula$lhs
    lgt_rows <- length(.rows)
    .cols <- .formula$rhs
    lgt_cols <- length(.cols)
    if (length(.rows) > 2) 
        stop("au maximum 2 variables pour les lignes")
    if (length(.cols) > 2) 
        stop("au maximum 2 variables pour les colonnes")
    x <- x[intersect(names(x), c(.rows, .cols, "valeur"))]
    
    # arrange the data set (by .rows first and then by .cols and
    # locate the .rows variables first
    x <- x[do.call(order, x[c(.rows, .cols)]), ]
    x <- x[, c(.rows, setdiff(names(x), .rows))]

    # locate the columns in the good order while reshaping; use the
    # levels of the variables
    levs_cols_1 <- levels(factor(x[[.cols[1]]], levels = levs$code)[drop = TRUE])
    # substitute the labels to the code if required
    if (.cols[1] %in% .labels){
        levs_cols_1 <- unname(levs_lib[levs_cols_1])
        x[[.cols[1]]] <- levs_lib[x[[.cols[1]]]]
    }
    if (lgt_cols > 1L){
        levs_cols_2 <- levels(factor(x[[.cols[2]]], levels = levs$code)[drop = TRUE])
        # substitute the labels to the code if required
        if (.cols[2] %in% .labels){
            levs_cols_2 <- unname(levs_lib[levs_cols_2])
            x[[.cols[2]]] <- levs_lib[x[[.cols[2]]]]
        }
        cols_ordered <- outer(levs_cols_1, levs_cols_2,
                              FUN = function(x, y) paste(x, y, sep = "|")) |>
            t() |> as.character()
    }
    else cols_ordered <- levs_cols_1
    
    # compute the percentages if required
    if (! is.null(.shares))
        x$valeur <- x$valeur / condsummary(x, .shares, sum, dim = "table")
#        x$valeur <- x$valeur / base_mutate(x, "valeur", .shares, FUN = sum) * 100
    
   # create a unique rows and cols column before reshaping
    if (lgt_rows > 1L) x$rows <- paste(x[[.rows[1]]], x[[.rows[2]]], sep = "|")
    else x <- setaname(x, .rows, "rows")
#    else names(x)[names(x) == .rows] <- "rows"
    if (lgt_cols > 1L){
        x$cols <- paste(x[[.cols[1]]], x[[.cols[2]]], sep = "|")
    } else {
        x <- setaname(x, .cols, "cols")
#        names(x)[names(x) == .cols] <- "cols"
    }
    # and remove the individual colums
    cols_rows_pos <- which(names(x) %in% c(.cols, .rows))
    if (length(cols_rows_pos) > 0L)
        x <- x[- which(names(x) %in% c(.cols, .rows))]

    # reshape
    x <- reshape(x, idvar = "rows", timevar = "cols", direction = "wide")

    # re-create the initial rows variables, with the initial names
    if (lgt_rows > 1L){
        rows <- as.data.frame(do.call(rbind, strsplit(x$rows, "|", fixed = TRUE)))
        names(rows) <- .rows
        x <- cbind(rows, x[setdiff(names(x), "rows")])
    } else {
        x <- setaname(x, "rows", .rows)
#        names(x)[names(x) == "rows"] <- .rows
    }
    
    # arrange the lines coercing the variable to factors
    levs_rows_1 <- levels(factor(x[[1]], levels = levs$code)[drop = TRUE])
    x[[1]] <- factor(x[[1]], levels = levs_rows_1)
    if (names(x)[[1]] %in% labels){
        levs_rows_1_lib <- unname(levs_lib[levs_rows_1])
        x[[1]] <- factor(x[[1]], levels = levs_rows_1, labels = levs_rows_1_lib)

    }
    if (lgt_rows > 1L){
        levs_rows_2 <- levels(factor(x[[2]], levels = levs$code)[drop = TRUE])
        x[[2]] <- factor(x[[2]], levels = levs_rows_2)
        if (names(x)[[2]] %in% labels){
            levs_rows_2_lib <- unname(levs_lib[levs_rows_2])
            x[[2]] <- factor(x[[2]], levels = levs_rows_2, labels = levs_rows_2_lib) 
        }
    }
    x <- x[do.call(order, x[.rows]), ]

    # back to characters
    x[[1]] <- as.character(x[[1]])
    if (lgt_rows > 1L)
        x[[2]] <- as.character(x[[2]])

#    for (i in names(x)) x[[i]][is.na(x[[i]])] <- 0

    names(x)[(lgt_rows + 1):length(x)] <- substr(names(x)[(lgt_rows + 1):length(x)], 8, 200)
    x <- x[c(names(x)[1:lgt_rows], intersect(cols_ordered, names(x)))]
    x
}

#' A data frame formated as a markdown table using tinytable
#'
#' This function is particulary usefull for a table in "wide"
#' format. It deals with lines and columns groups.
#' 
#' @name xtbl2tt
#' @param x a data frame
#' @param height the height of the lines
#' @param theme a boolean, if `TRUE` the `theme_sam` function is
#'     applied
#' @param ... supplementary arguments passed to `tinytable`
#' @importFrom tinytable tt group_tt style_tt format_tt
#' @export
xtbl2tt <- function(x, height = 1, theme = TRUE, ...){
    col_nms <- names(x)
    # identification des colonnes non numeriques
    num_cols <- sapply(x, is.numeric)
    char_cols <- names(x)[! num_cols]
    
    # detection des colonnes a noms composes
    cols_multi <- grepl(col_nms, pattern = "\\|")
    nb_multi <- sum(cols_multi)
    # boolein pour les colonnes multiples
    multi_columns <- any(cols_multi)
    
    # la detection de groupes de ligne se fait sur la presence de plus
    # d'une colonne non multi
    K <- length(x)
    nb_uni <- K - nb_multi
    rows_group <- nb_uni > 1

    # creation de variables pour le multi-colonne
    if (multi_columns){
        uni_cols <- col_nms[! cols_multi]
        multi_cols <- col_nms[cols_multi]
        nms_multi <- strsplit(multi_cols, "\\|")
        nms_multi <- t(as.matrix(as.data.frame(nms_multi)))
        dimnames(nms_multi) <- NULL
        nms_group <- nms_multi[, 1]
        nms_var <- nms_multi[, 2]
        x <- setNames(x, c(uni_cols, nms_var))
        tbl_group <- table(nms_group)
        nms_unique_var <- unique(nms_var)
        nms_unique_group <- unique(nms_group)
        K <- length(nms_var)
        deb <- nb_uni - rows_group + c(1, cumsum(tbl_group)[- K] + 1)
        names(deb) <- unique(nms_group)
        fin <- nb_uni - rows_group + cumsum(tbl_group)
        cols <- lapply(unique(nms_group), function(x) seq(deb[x], fin[x]))
        names(cols) <- nms_unique_group
    }
    if (rows_group){
        rgps <- x[[1]]
        nms_x <- names(x)[- 1]
        x <- x[, - 1, drop = FALSE]
        names(x) <- nms_x
    }
    x <- tt(x, escape = TRUE, height = height, ...) |>
        format_tt(replace = TRUE)
    if (rows_group) x <- x |> group_tt(i = rgps)
    if (multi_columns){
        x <- x |> group_tt(j = cols)
    }
    if (theme){
        x <- x |> theme_sam()
    }
    x
}

    
                        
