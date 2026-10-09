# summary.sam, as.matrix.sam, verif_eq, pop, ipc, entete, sam2tt

#' @rdname sam
#' @method summary sam
#' @export
summary.sam <- function(object, ...){
    desc <- attr(object, "description")
    .nomenc <- desc["naf"]
    .unit <- desc["unit"]
    .pib <- desc["pib"]
    if (.unit == "K") .unit <- "milliers d'euros"
    if (.unit == "M") .unit <- "millions d'euros"
    if (.unit == "G") .unit <- "milliards d'euros"
    if (.unit == "PIB") .unit <- "base 100 pour le PIB"

    cat(paste("MCS pour ", "la France", "\n", sep = ""))
    cat(paste("Nomenclature ", .nomenc, "\n", sep = ""))
    cat(paste("Unite : ", .unit, "\n", sep = ""))
    cat(paste("PIB : ", .pib, "\n", sep = ""))
}

#' @rdname sam
#' @method as.matrix sam
#' @export
as.matrix.sam <- function(x, ...){
    levs_1 <- c("br", "pr", "fact", "mg", "tax", "RPB", "dist", "RDB", "agt", "acc", "total")
    levs_naf <- as.character(unique(x[x$row_1 == "pr", "row_2", drop = TRUE]))
    K <- length(levs_naf)
    levs_2 <- c(levs_naf,
                levs_naf,
                "l", "ebe", "rmb", "rp", "k",
                "mc", "mt",
                "t.br", "s.br", "t.pr", "s.pr", "tva",
                "RPB",
                "ir", "cot", "prest", "atc",
                "RDB",
                "snf", "sf", "ent", "etat", "men", "isbl", "rdm",
                "fbcf", "ds", "acc", "total")
    levs_1 <- c(rep("br", K), rep("pr", K), rep("fact", 5),
                rep("mg", 2), rep("tax", 5), "RPB",
                rep("dist", 4), "RDB", rep("agt", 7),
                rep("acc", 3), "total")
    levs <- paste(levs_1, levs_2, sep = "_")
    class(x) <- "data.frame"
    col <- paste(x$col_1, x$col_2, sep = "_")
    row <- paste(x$row_1, x$row_2, sep = "_")
    x <- data.frame(row = row, col = col, valeur = x$valeur)
    nms <- list(row = intersect(levs, unique(x$row)),
                col = intersect(levs, unique(x$col)))
    x <- reshape(x, idvar = "row", timevar = "col", direction = "wide")
    names(x) <- substr(names(x), 8, 100)
    rn <- x[[1]]
    x <- x[, - 1]
    x <- as.matrix(x)
    rownames(x) <- rn
    x <- x[nms$row, nms$col]
    x
}

#' SAM formated as a markdown table
#'
#' The tinytable package is used and functions of this package can be
#' used to customize the result. The table can then be coerced in
#' several format, especially LaTeX and html
#'
#' @name sam2tt
#' @param x a `sam` object
#' @param height the height of the lines
#' @param theme a boolean, if `TRUE` the `theme_sam` function is
#'     applied
#' @importFrom tinytable tt group_tt style_tt format_tt
#' @export
sam2tt <- function(x, height = 1, theme = TRUE){
    x <- as.matrix(x)
#    class(x) <- c("matrix", "array")
#    if (rm_zero) x[x == 0] <- NA
    row_nms <- rownames(x)
    col_nms <- colnames(x)

    row_nms_mat <- strsplit(row_nms, "_") |> sapply(function(x) x[[2]])
    col_nms_mat <- strsplit(col_nms, "_") |> sapply(function(x) x[[2]])
    dimnames(x) <- list(row_nms_mat, col_nms_mat)

    x <- cbind(" " = row_nms_mat, as.data.frame(x))
    get_index <- function(x, type = c("rows", "cols")){
        .type <- match.arg(type)
        x <- strsplit(x, "_")
        un <- sapply(x, function(x) x[1])
        deux <- sapply(x, function(x) x[2])
        x <- data.frame(un = un, deux = deux)

        x$rg <- 1:nrow(x)
        x$max <- ave(x$rg, x$un, FUN = max)
        x$min <- ave(x$rg, x$un, FUN = min)
        x <- unique(x[, - which(names(x) %in% c("rg", "deux"))])
        if (.type == "rows"){
            vals <- as.list(x$min)
        }
        if (.type == "cols"){
            vals <- lapply(1:nrow(x), function(i) (x$min[i]:x$max[i]) + 1)
        }
        names(vals) <- x$un
        vals
    }
    gps_i <- get_index(row_nms, "rows")

    names(gps_i) <- c("branches", "produits", "facteurs", "marges", "taxes",
                      "transferts", "agents", "accumulation", "total")
    gps_j <- get_index(col_nms, "cols")
    j_rpb <- which(names(x) ==  "RPB")
    j_rdb <- which(names(x) == "RDB")
    n_agts <- gps_i$accumulation - gps_i$agents
    i_agts <- gps_i$agents + 7
    i_acc <- gps_i$accumulation + 8
    x <- tt(x, escape = TRUE, height = height) |>
        format_tt(replace = TRUE) |> 
        group_tt(j = gps_j) |>
        group_tt(i = gps_i)# |>
    if (theme) x <- x |> theme_sam()
    x
}

#' Theme for the samfr package
#'
#' Default customization for tables constructed using either `stbl` or
#' `sam`
#'
#' @name theme_sam
#' @param x a data frame
#' @param colors a list of colors for "groupes" and "comptes" %>%
#' @param fontsize font size
#' @param digits number of digits
#' @param align the alignement (by default center)
#' @param grid if `TRUE` (the default), grid is added
#' @format a `tinytable` object
#' @importFrom tinytable theme_grid
#' @export
theme_sam <- function(x,
                      colors = list(groupes = "#878787",
                                   comptes = "#D3D3D3"),
                      fontsize = 1,
                      digits = 1,
                      align = "c",
                      grid = TRUE){
    if (grid) x <- x |> theme_grid()
    x <- x |> 
        style_tt(j = 1, align = "l", background = colors$comptes) |> 
        style_tt(i = 0, background = colors$comptes) |> 
        style_tt("groupj", background = colors$groupes) |>
        style_tt("groupi", background = colors$groupes, align = align) |>
        style_tt(fontsize = fontsize) |>
        style_tt(align = align) |>
        style_tt(j = 1, align = "l") |> 
        style_tt("groupj", background = colors$groupes) |>
        style_tt("groupi", background = colors$groupes, align = align) |>
        format_tt(digits = digits, replace = TRUE)
    x
}
