.libPaths(c("/home/users/bak47/R/x86_64-pc-linux-gnu-library/3.6", "/auto/pkg/local/lib/R/site-library", "/usr/lib/R/site-library", "/usr/lib/R/library"))
message("[slurmR info] Loading variables and functions... ", appendLF = FALSE)
Slurm_env <- function (x = "SLURM_ARRAY_TASK_ID") 
{
    y <- Sys.getenv(x)
    if ((x == "SLURM_ARRAY_TASK_ID") && y == "") {
        return(1)
    }
    y
}
ARRAY_ID  <- as.integer(Slurm_env("SLURM_ARRAY_TASK_ID"))

# The -snames- function creates the write names for I/O of files as a 
# function of the ARRAY_ID
snames    <- function (type, array_id = NULL, tmp_path = NULL, job_name = NULL) 
{
    if (length(array_id) && length(array_id) > 1) 
        return(sapply(array_id, snames, type = type, tmp_path = tmp_path, 
            job_name = job_name))
    type <- switch(type, r = "00-rscript.r", sh = "01-bash.sh", 
        out = "02-output-%A-%a.out", rds = if (missing(array_id)) "03-answer-%03i.rds" else sprintf("03-answer-%03i.rds", 
            array_id), job = "job.rds", stop("Invalid type, the only valid types are `r`, `sh`, `out`, and `rds`.", 
            call. = FALSE))
    sprintf("%s/%s/%s", tmp_path, job_name, type)
}
TMP_PATH  <- "/home/users/bak47/projects/parlrdev/cluster_jobs"
JOB_NAME  <- "slurmr-job-74f32655a8fd"

# The -tcq- function is a wrapper of tryCatch that on error tries to recover
# the message and saves the outcome so that slurmR can return OK.
tcq <- function (...) 
{
    ans <- tryCatch(..., error = function(e) e)
    if (inherits(ans, "error")) {
        ARRAY_ID. <- get("ARRAY_ID", envir = .GlobalEnv)
        msg <- paste("An error has ocurred while evualting the expression:\n", 
            paste(deparse(match.call()[[2]]), collapse = "\n"), 
            "\n in ", "ARRAY_ID # ", ARRAY_ID.)
        warning(msg, immediate. = TRUE, call. = FALSE)
        ans$message <- paste(ans$message, msg)
        saveRDS(ans, snames("rds", tmp_path = get("TMP_PATH", 
            envir = .GlobalEnv), job_name = get("JOB_NAME", envir = .GlobalEnv), 
            array_id = ARRAY_ID.))
        q("no")
    }
    invisible(ans)
}
message("done loading variables and functions.")
message("[slurmR info] Loading packages ... ")
tcq({
  library(parlrdev, lib.loc = "/home/users/bak47/projects")
})
message("[slurmR info] done loading packages.")
tcq({
  INDICES <- readRDS("/home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-74f32655a8fd/INDICES.rds")
})
tcq({
  X <- readRDS(sprintf("/home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-74f32655a8fd/X_%04d.rds", ARRAY_ID))
})
tcq({
  FUN <- readRDS("/home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-74f32655a8fd/FUN.rds")
})
tcq({
  mc.cores <- readRDS("/home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-74f32655a8fd/mc.cores.rds")
})
tcq({
  seeds <- readRDS("/home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-74f32655a8fd/seeds.rds")
})
set.seed(seeds[ARRAY_ID], kind = NULL, normal.kind = NULL)
tcq({
  ans <- parallel::mclapply(
    X                = X,
    FUN              = FUN,
    mc.cores         = mc.cores
)
})
saveRDS(ans, sprintf("/home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-74f32655a8fd/03-answer-%03i.rds", ARRAY_ID), compress = TRUE)
