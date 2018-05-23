#' Print Comparison of BenchDesign objects
#'
#' Simple comparison of two BenchDesign objects based on
#' methods meta data and data MD5 hashs.
#'
#' @param x a BenchDesign or SummarizedBenchmark object
#' @param y a BenchDesign or SummarizedBenchmark object
#'
#' @return
#' logical value indicating whether the two objects have
#' methods producing the same meta data and data with the
#' same MD5 hashes.
#'
#' @importFrom crayon red yellow green bold
#' @export
#' @author Patrick Kimes
printUpdateBench <- function(sb, bd, version = FALSE) {
    res <- compareBenchDesigns(sb, bd)

    metres <- res$methods$res
    metres <- dplyr::mutate(metres, rerun = !f | !meta | !params | !post)
    if (version) {
        metres <- dplyr::mutate(metres, rerun = rerun | !version)
    }
    metres <- dplyr::mutate(metres, rerun = rerun | overlap == "yOnly")
    metres <- dplyr::mutate(metres, oldfn = overlap != "yOnly")

    ## need to rerun all if data is new
    if (!isTRUE(res$data$data)) {
        metres <- dplyr::mutate(metres, rerun = TRUE)
    }
    
    sbdat <- BDData(sb)
    bddat <- BDData(bd)
    
    cat(stringr::str_pad(crayon::bold("Update SummarizedBenchmark (dryrun) "), 70, "right", "-"), "\n")
    if (isTRUE(res$data$data)) {
        if (!isFALSE(res$data$type) && res$data$type == "md5hash") {
            cat(crayon::yellow$bold("  benchmark data:"), crayon::yellow("unchanged (full data missing)\n"))
            cat("    MD5 hash: ") 
            cat(sbdat@data, "\n")
            cat("    names: ??\n")
        } else {
            cat(crayon::green$bold("  benchmark data:"), crayon::green("unchanged\n"))
            cat("    MD5 hash: ") 
            cat(hashBDData(sbdat)@data, "\n")
            if (sbdat@type == "data")
                cat("    names:", names(sbdat@data), "\n")
            else
                cat("    names:", names(bddat@data), "\n")
        }
    } else {
        cat(crayon::red$bold("  benchmark data:"), crayon::red("changed "))
        if (bddat@type == "md5hash") {
            cat(crayon::red("(full data missing - can't run)\n"))
            cat("    BenchDesign\n")
            cat("      MD5 hash: ", bddat@data, "\n")
            cat("      names: ??\n")
        } else {
            cat("\n")
            cat("    BenchDesign\n")
            cat("      MD5 hash: ", hashBDData(bddat)@data, "\n")
            cat("      names:", names(bddat@data), "\n")
        }
        cat("    SummarizedBenchmark\n")
        cat("      MD5 hash: ", hashBDData(sbdat)@data, "\n")
        if (sbdat@type == "md5hash")
            cat("      names: ??\n")
        else
            cat("      names:", names(sbdat@data), "\n")
    }

    metres <- dplyr::mutate_if(metres, is.logical, `!`)
    metres <- dplyr::mutate(metres, rerun = !rerun)
    metres <- dplyr::mutate_if(metres, is.logical, dplyr::funs(ifelse(., "Y", "N")))
    metres$rerun[metres$overlap == "xOnly"] <- "Drop"
    
    cat(crayon::bold("  benchmark methods:\n"))
    if (nrow(metres)) {
        header1 <- paste0(stringr::str_pad("    |", 18, "right"), stringr::str_pad("| Need to", 10), "  |",
                          stringr::str_pad("Outdated", 31, "both"), "|\n")
        header2 <- .methodrow(c("Method", "(Re)Run", "Func", "Param", "Meta", "Post", "Vers"))
        cat(crayon::bold(header1))
        cat(crayon::bold(header2))
        for (i in seq_len(nrow(metres))) {
            if (metres$overlap[i] == "Both") {
                istr <- metres[i, c("label", "rerun", "f", "params", "meta", "post", "version"), drop = TRUE]
                istr <- .methodrow(istr)
                ## if (metres$rerun[i] == "Y") 
                ##     istr <- crayon::red(istr)
                cat(istr)
            } else {
                istr <- c(metres[i, c("label", "rerun"), drop = TRUE], rep("-", 5))
                istr <- .methodrow(istr)
                ## if (metres$overlap[i] == "yOnly") {
                ##     cat(crayon::red(istr))
                ## } else if (metres$overlap[i] == "xOnly") {
                ##     cat(crayon::silver(istr))
                ## }
                cat(istr)
            }
        }
    } else {
        cat("    none\n")
    }
    
}

## standard row format
.methodrow <- function(x, n) {
    method_label <- stringr::str_pad(stringr::str_trunc(x[1], 12), 12, "right")
    if (x[2] == "Y") {
        method_label <- crayon::red(method_label)
    } else if (x[2] == "N") {
        method_label <- crayon::green(method_label)
    }
    paste0("    | ", method_label, " |", .loglab(x[2], 8), "  |",
           .loglab(x[3], 6), .loglab(x[4], 6), .loglab(x[5], 6), .loglab(x[6], 6),
           .loglab(x[7], 6), " |\n")
}

## standard column format
.loglab <- function(x, n) {
    xp <- stringr::str_pad(x, n, "left")
    if (x == "Y") 
        crayon::red(xp)
    else if (x == "N")
        crayon::green(xp)
    else
        xp
}

