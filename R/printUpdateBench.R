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

    ## need to rerun all
    if (!isTRUE(res$data$data)) {
        metres <- dplyr::mutate(metres, rerun = TRUE)
    }
    
    sbdat <- BDData(sb)
    bddat <- BDData(bd)
    
    cat(stringr::str_pad(crayon::bold("Update SummarizedBenchmark (dryrun) "), 60, "right", "-"), "\n")
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

    metres <- dplyr::mutate_if(metres, is.logical, dplyr::funs(ifelse(., "Y", "N")))
    metres$rerun[metres$overlap == "xOnly"] <- "X"
    
    cat(crayon::bold("  benchmark methods:\n"))
    if (nrow(metres)) {
        header <- .methodrow(c("Method", "Run", "Ran", "fn", "pm", "me", "po", "ve"))
        cat(crayon::bold(header))
        for (i in seq_len(nrow(metres))) {
            if (metres$overlap[i] == "Both") {
                istr <- metres[i, c("label", "rerun", "oldfn", "f", "params", "meta", "post", "version"), drop = TRUE]
                istr <- .methodrow(istr)
                if (metres$rerun[i] == "Y") 
                    istr <- crayon::red(istr)
                cat(istr)
            } else {
                istr <- c(metres[i, c("label", "rerun", "oldfn"), drop = TRUE], rep("-", 5))
                istr <- .methodrow(istr)
                if (metres$overlap[i] == "yOnly") {
                    cat(crayon::red(istr))
                } else if (metres$overlap[i] == "xOnly") {
                    cat(crayon::silver(istr))
                }
            }
        }
    } else {
        cat("    none\n")
    }
    
}

## standard row format
.methodrow <- function(x, n) {
    paste0("    ", stringr::str_pad(stringr::str_trunc(x[1], 11), 12, "right"),
           .loglab(x[2], 5), .loglab(x[3], 5), "  |", .loglab(x[4], 4), .loglab(x[5], 4),
           .loglab(x[6], 4), .loglab(x[7], 4), .loglab(x[8], 4),"\n")
}

## standard column format
.loglab <- function(x, n) {
    stringr::str_pad(x, n, "left")
}
