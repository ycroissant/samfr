#' Reshape a data frame obtained using `stbl`
#'
#' Up to two variables should be indicated as "time-invariant" and two
#' variables as "time-varying", the cells are filled with the `value`
#' column
#'
#' @name wider
#' @param x a data frame obtained using `stbl`
#' @param rows a character with one or two variables
#' @param cols a character with one or two variables
#' @format a data frame
#' @export
wider <- function(x, rows = NULL, cols = NULL){
    .rows <- rows
    .cols <- cols
    lgt_rows <- length(.rows)
    lgt_cols <- length(.cols)
    oclass <- class(x)
    x <- as.data.frame(x)
    x <- x[which(names(x) %in% c("valeur", .rows, .cols))]
    if (lgt_rows > 1){
        .rows_1 <- .rows[1]
        .rows_2 <- .rows[2]
        x$rows <- paste(x[[.rows_1]], x[[.rows_2]], sep ="|")
    }  else {
        x <- setaname(x, .rows, "rows")
    }
    if (lgt_cols > 1){
        .cols_1 <- .cols[1]
        .cols_2 <- .cols[2]
        x$cols <- paste(x[[.cols_1]], x[[.cols_2]], sep = "|")
    } else {
        x <- setaname(x, .cols, "cols")
    }
    x <- x[c("rows", "cols", "valeur")]
    x <- reshape(x, idvar = "rows", timevar = "cols", direction = "wide")
    if (lgt_rows > 1){
        rows <- as.data.frame(do.call(rbind, strsplit(x$rows, "|", fixed = TRUE)))
        names(rows) <- .rows
        x <- cbind(rows, x[setdiff(names(x), "rows")])
    } else {
        x <- setaname(x, "rows", .rows)
    }
    names(x)[(lgt_rows + 1):length(x)] <- substr(names(x)[(lgt_rows + 1):length(x)], 8, 200)
    structure(x, class = oclass)
}
    
#' Summarise using a function and a set of factors
#'
#' The `valeur` column of the table is transformed according to a
#' function, for all the combinations of a set of factors
#'
#' @name condsummary
#' @param x a data frame obtained using `stbl`
#' @param gps a list of factors
#' @param FUN the function
#' @param dim with the default (`"levels"`) returns a data frame with
#'     all the combinations of the factors and a `valeur` column
#'     containing the result, if `"table"` returns a vector that can
#'     be added to the original data frame
#' @param ... further arguments passed to `FUN` (for example `na.rm`)
#' @format a series or a data frame
#' @export
condsummary <- function(x, gps, FUN, dim = c("levels", "table"), ...){
    .dim <- match.arg(dim)
    oclass <- class(x)
    lgps <- lapply(gps, function(i) x[[i]])
    CONDS <- x[gps]
    x <- by(x, lgps, FUN = function(w) data.frame(valeur = FUN(w$valeur, ...)), simplify = FALSE)
    lgnt1 <- length(gps) == 1
    if (lgnt1){
        d <- data.frame(d = names(x))
    } else {
        d <- expand.grid(dimnames(x), stringsAsFactors = FALSE)
    }
    names(d) <- gps
    nd <- sapply(x, function(w) is.null(w))
    x <- x[! nd]
    d <- d[! nd, , drop = FALSE]
    x <- cbind(d, do.call(rbind, x))
    class(x) <- oclass
    if (.dim == "table"){
        x <- merge(cbind(CONDS, rk = 1:nrow(CONDS)), x)
        x <- x[order(x$rk), ]
        x <- x$valeur
    }
    x
}

#' Replace acronyms by labels for selected columns
#'
#' @name add_label
#' @param x a data frame obtained using `stbl`
#' @param vars a character vector indicating the columns for which
#'     short names should be replaced by complete names
#' @format a data frame
#' @export
add_label <- function(x, vars){
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
    for (i in vars){
        x[[i]] <- levs_lib[x[[i]]]
    }
    x
}
    
#' Set the name of one variable and return the data frame
#'
#' @name setaname
#' @param x a data frame
#' @param old the old name, a character
#' @param new the new name, a character
#' @format a data frame
#' @export
setaname <- function(x, old, new){
    names(x)[which(names(x) == old)] <- new
    x
}

#' Check the equilibrium of the SAM
#' 
#' The margins of lines (ressources) and columns (expenses) of the SAM
#' are computed and their equality are checked
#'
#' @name check_eq
#' @param x an object of class `sam`
#' @param diff the difference either absolute (`"abs"`) or relative
#'     (`"rel"`)
#' @param n the number of lines to print
#' @return a data frame with a line for each account, lines and
#'     columns' margins and the relative and absolute differences
#' @export
verif_eq <- function(x, diff = c("abs", "rel"), n = NULL){
    .var_int <- c("total", "fbcf", "ds", "ebe", "rmb", "rp", "RPB", "RDB")
    .diff <- match.arg(diff)
    if (! inherits(x, "sam"))
        stop("the argument should be a sam object")
    x <- x[! x$row_2 %in% .var_int & ! x$col_2 %in% .var_int, ]
    ## rows <- base_summary(x, c("row_1", "row_2"),
    ##                      function(x) data.frame(ress = sum(x$valeur)))
    ## cols <- base_summary(x, c("col_1", "col_2"),
    ##                      function(x) data.frame(emp = sum(x$valeur)))
    rows <- condsummary(x, c("row_1", "row_2"), sum)
    rows <- setaname(x, "valeur", "emp")
    cols <- condsummary(x, c("col_1", "col_2"), sum)
    cols <- setaname(x, "valeur", "ress")
    .tol <- sqrt(.Machine$double.eps)
    mgs <- merge(rows, cols, by.x = c("row_1", "row_2"), by.y = c("col_1", "col_2"))
    names(mgs)[which(names(mgs) == "row_1")] <- "cpte_1"
    names(mgs)[which(names(mgs) == "row_2")] <- "cpte_2"
    mgs$ress[is.na(mgs$ress) | abs(mgs$ress) < .tol] <- 0
    mgs$emp[is.na(mgs$emp) | abs(mgs$emp) < .tol] <- 0
    mgs$abs_diff <- abs(mgs$ress - mgs$emp)
    mgs$rel_diff <- 2 * mgs$abs_diff / (
        ifelse(abs(mgs$ress) + abs(mgs$emp) != 0,
               abs(mgs$ress) + abs(mgs$emp), 1))
    if (! is.null(n)){
        if (! is.numeric(n)) stop("n sould be a numeric")
        if (.diff == "abs") mgs <- mgs[order(- mgs$abs_diff), ][1:n, ]
        if (.diff == "rel") mgs <- mgs[order(- mgs$rel_diff), ][1:n, ]
    }
    mgs
}

#' Population
#'
#' Extract the population (in millions inhabitants) for requested
#' years or add a pop column in a table returned by `syntbl`
#'
#' @name pop
#' @param x a numerical vector containing years or a data frame
#' @param deflate if `TRUE` numerical values of the data frame are
#'     divided by the population, otherwise, the population is added
#'     to the data frame
#' @return a numerical vector containing the population for the
#'     requested years or the `x` data frame augmented by a `pop`
#'     column
#' @examples
#' pop(2010:2015)
#' stbl(2020:2022, tableau = "produits",
#'      variable = c("cf", "fbcf"), naf = "A1") |>
#'      pop()
#' stbl(2020:2022, tableau = "produits",
#'      variable = c("cf", "fbcf"), naf = "A1") |>
#'      pop(deflate = FALSE)
#' @export
pop <- function(x, deflate = TRUE){
    if (is.numeric(x)){
        if (any(! x %in% pop_ipc$an)){
            stop("unknown years")
        }
        x <- pop_ipc[pop_ipc$an %in% x, c("an", "pop_metro")]
        x <- x$pop_metro
    }
    if (is.data.frame(x)){
        if (! "an" %in% names(x)){
            stop("the table must contain a column called year")
        }
        oclass <- class(x)
        yrs <- x$an
        .pop <- pop_ipc$pop_metro
        names(.pop) <- pop_ipc$an
        .pop <- .pop[yrs]
        if (deflate){
            num_col <- which(sapply(x, is.numeric))
            for (i in num_col) x[[i]] <- x[[i]] / .pop
        } else {
            x <- cbind(x, pop = .pop)
        }
        class(x) <- oclass
    }
    x
}

#' Consumer Price Index
#'
#' Extract the cpi for requested years and add a `ipc` column in a
#' table returned by `syntbl` or deflate the `valeur` column by the
#' `ipc`
#'
#' @name ipc
#' @param x a numerical vector containing years or a data frame
#' @param base the base, ie the year for which the index is equal to 1
#' @param deflate if `TRUE` numerical values of the data frame are
#'     divided by the ipc, otherwise, the ipc is added to the data
#'     frame
#' @return a numerical vector containing the ipc for the requested
#'     years or the `x` data frame augmented by a `ipc` column
#' @examples
#' ipc(2010:2015)
#' stbl(2020:2022, tableau = "produits",
#'      variable = c("cf", "fbcf"), naf = "A1") |>
#'      ipc(base = 2020)
#' stbl(2020:2022, tableau = "produits",
#'      variable = c("cf", "fbcf"), naf = "A1") |>
#'      ipc(base = 2020, deflate = FALSE)
#' @export
ipc <- function(x, base = 2023, deflate = TRUE){
    x.base <- pop_ipc[pop_ipc$an == base, "ipc", drop = TRUE]
    if (is.numeric(x)){
        if (any(! x %in% pop_ipc$an)){
            stop("unknown years")
        }
        x <- pop_ipc[pop_ipc$an %in% x, c("an", "ipc")]
        x <- x$ipc / x.base
    }
    if (is.data.frame(x)){
        if (! "an" %in% names(x)){
            stop("the table must contain a column called year")
        }
        oclass <- class(x)
        yrs <- x$an
        .ipc <- pop_ipc$ipc
        names(.ipc) <- pop_ipc$an
        .ipc <- .ipc[yrs]
        if (deflate){
            num_col <- which(sapply(x, is.numeric))
            for (i in num_col) x[[i]] <- x[[i]] / .ipc
        } else {
            x <- cbind(x, ipc = .ipc)
        }
        class(x) <- oclass
    }
    x
}
