# Fix for pearson.msm function with interaction terms

pearson_msm <- function (x, transitions = NULL, timegroups = 3, intervalgroups = 3, 
    covgroups = 3, groups = NULL, boot = FALSE, B = 500, next.obstime = NULL, 
    N = 100, indep.cens = TRUE, maxtimes = NULL, pval = TRUE) 
{
    dat <- x$data$mf
    if (!inherits(x, "msm")) 
        stop("expected \"x\" t to be a msm model")
    if (x$hmodel$hidden && !x$emodel$misc) 
        stop("only HMMs handled are misclassification models specified using \"ematrix\"")
    if (any(dat$"(obstype)" == 2)) 
        stop("exact transition times are not supported, only panel-observed data")
    if (!is.null(transitions) && !is.numeric(transitions)) 
        stop("expected \"transitions\" to be numeric")
    if (!is.numeric(timegroups) || length(timegroups) != 1) 
        stop("expected \"timegroups\" to be a single number")
    if (!is.numeric(intervalgroups) || length(intervalgroups) != 
        1) 
        stop("expected \"intervalgroups\" to be a single number")
    if (!is.numeric(covgroups) || length(covgroups) != 1) 
        stop("expected \"covgroups\" to be a single number")
    if (!is.numeric(B) || length(B) != 1) 
        stop("expected \"B\" to be a single number")
    if (!is.numeric(N) || length(N) != 1) 
        stop("expected \"N\" to be a single number")
    if (!is.null(x$pci) && length(grep("timeperiod\\[([0-9]+|Inf),([0-9]+|Inf)\\)", 
        x$qcmodel$covlabels)) == x$qcmodel$ncovs) 
        covgroups <- 1
    nst <- x$qmodel$nstates
    exact.death <- any(dat$"(obstype)" == 3)
    dstates <- if (exact.death) 
        absorbing.msm(x)
    else NULL
    ndstates <- if (exact.death) 
        transient.msm(x)
    else 1:nst
    nndstates <- length(ndstates)
    od <- dat[, c("(subject)", "(time)", "(state)", "(obstype)")]
    names(od) <- c("subject", "time", "state", "obstype")
    od$cov <- dat[, attr(dat, "covnames.q")[!grepl(":", attr(dat, "covnames.q"))], drop = FALSE]
    if (x$emodel$misc) 
        od$misccov <- dat[, attr(dat, "covnames.e")]
    ncovs <- x$qcmodel$ncovs
    od$state <- factor(od$state, levels = sort(unique(od$state)))
    n <- nrow(od)
    lastobs <- c(od$subject[1:(n - 1)] != od$subject[2:n], TRUE)
    cens.notend <- (od$state %in% x$cmodel$censor) & (!lastobs)
    if (any(cens.notend)) {
        od <- od[!cens.notend, ]
        warning("Omitting censored states not at the end of individual series")
    }
    od <- od[!od$subject %in% unique(od$subject)[table(od$subject) == 
        1], ]
    if (length(x$cmodel$censor) >= 2) {
        ind <- NULL
        for (i in 1:length(x$cmodel$censor)) {
            if (identical(x$cmodel$states[x$cmodel$index[i]:(x$cmodel$index[i + 
                1] - 1)], as.numeric(transient.msm(x)))) {
                ind <- i
                break
            }
        }
        if (is.null(ind)) {
            warning("Omitting all censored states")
            cens.drop <- od$state %in% x$cmodel$censor
        }
        else {
            cens.drop <- od$state %in% x$cmodel$censor[-ind]
            warning("Omitting censored states of types other than ", 
                x$cmodel$censor[ind])
        }
        od <- od[!cens.drop, ]
    }
    n <- nrow(od)
    nstcens <- length(unique(od$state))
    cens <- nstcens > nst
    od$prevstate <- factor(c(NA, od$state[1:(n - 1)]), levels = 1:nndstates)
    od$prevtime <- c(NA, od$time[1:(n - 1)])
    od$ind <- 1:n
    od$firstobs <- rep(tapply(1:n, od$subject, min)[as.character(unique(od$subject))], 
        table(od$subject)[as.character(unique(od$subject))])
    od$obsno <- 1:n - od$firstobs + 1
    od$subj.num <- match(od$subject, unique(od$subject))
    if (!is.null(next.obstime) && (!is.numeric(next.obstime) || 
        length(next.obstime) != n)) 
        stop(paste("expected \"next.obstime\" to be a numeric vector length", 
            n))
    od$timeinterval <- if (is.null(next.obstime)) 
        (od$obsno > 1) * (od$time - od$time[c(1, 1:(n - 1))])
    else next.obstime
    if (!is.null(maxtimes) && (!is.numeric(maxtimes) || !(length(maxtimes) %in% 
        c(1, n)))) 
        stop(paste("expected \"maxtimes\" to be a numeric vector length 1 or", 
            n))
    if (!is.null(groups) && (!(length(groups) == n))) 
        stop(paste("expected \"groups\" to be a vector length", 
            n))
    od$maxtimes <- if (is.null(maxtimes)) 
        ifelse(od$state %in% dstates, max(od$timeinterval) + 
            1, od$timeinterval)
    else rep(maxtimes, length = n)
    od$usergroup <- factor(if (!is.null(groups)) 
        groups
    else rep(1, n))
    trans <- list()
    trans$allowed <- msm:::intervaltrans.msm(x, exclude.absabs = TRUE, 
        censor = cens)
    trans$labsall <- paste(trans$allowed[, 1], trans$allowed[, 
        2], sep = "-")
    trans$allowed[trans$allowed[, 2] > nst, 2] <- nst + 1
    trans$na <- nrow(trans$allowed)
    trans$use <- if (is.null(transitions)) 
        1:trans$na
    else transitions
    if (length(trans$use) != trans$na) 
        stop("Supplied ", length(trans$use), " transition indices, expected ", 
            trans$na)
    else if (!x$emodel$misc && !all(tapply(trans$allowed[, 1], 
        trans$use, function(u) length(unique(u))) == 1)) 
        stop("Only transitions from the same origin can be grouped")
    trans$labsagg <- tapply(trans$labsall, trans$use, function(u) paste(u, 
        collapse = ","))
    trans$from <- trans$allowed[, 1][!duplicated(trans$use)]
    trans$to <- trans$allowed[, 2][!duplicated(trans$use)]
    trans$ngroups <- length(unique(trans$use))
    if (ncovs > 0) {
        uniq <- unique(od$cov)
        nouniq <- dim(uniq)[1]
        pastedu <- do.call("paste", uniq)
        pastedc <- do.call("paste", od$cov)
        qmatindex <- match(pastedc, pastedu)
        qmat <- array(0, dim = c(nst, nst, nouniq))
        for (i in 1:nouniq) qmat[, , i] <- qmatrix.msm(x, covariates = as.list(uniq[i, 
            ]), ci = "none")
    }
    else {
        qmatindex <- rep(1, n)
        nouniq <- 1
        qmat <- array(qmatrix.msm(x, ci = "none"), dim = c(nst, 
            nst, 1))
    }
    qmatmaster <- qmat
    qmat <- qmat[, , qmatindex]
    od$rates <- apply(qmat, 3, function(u) sum(diag(u)))
    md <- od[od$obsno > 1, ]
    qmat <- qmat[, , od$obsno > 1]
    qmatindex <- qmatindex[od$obsno > 1]
    ntrans <- nrow(md)
    nfromstates <- length(unique(md$prevstate))
    timegroups.use <- min(length(unique(md$time)), timegroups)
    md$timegroup <- msm:::qcut(md$time, timegroups.use)
    intervalq <- tapply(md$timeinterval[md$state %in% ndstates], 
        md$timegroup[md$state %in% ndstates], function(x) quantile(x, 
            probs = seq(0, 1, 1/intervalgroups)))
    md$intervalgroup <- rep(1, ntrans)
    for (i in levels(md$timegroup)) md$intervalgroup[md$timegroup == 
        i] <- unclass(msm:::qcut(md$timeinterval[md$timegroup == i], 
        qu = intervalq[[i]]))
    md$intervalgroup <- factor(md$intervalgroup)
    covgroups.use <- min(length(unique(md$rates)), covgroups)
    md$covgroup <- if (ncovs > 0) 
        msm:::qcut(md$rates, covgroups.use)
    else factor(rep(1, ntrans))
    groupdims <- c(length(levels(md$timegroup)), length(levels(md$intervalgroup)), 
        length(levels(md$covgroup)), length(levels(md$usergroup)))
    groupdimnames <- list(levels(md$timegroup), levels(md$intervalgroup), 
        levels(md$covgroup), levels(md$usergroup))
    md$obtype <- rep(0, ntrans)
    if (exact.death) 
        md$obtype[md$state %in% dstates] <- 1
    md$obtype[md$state %in% x$cmodel$censor] <- 2
    md$cens <- factor(as.numeric(md$obtype == 2), levels = c(0, 
        1))
    ndeath <- sum(md$obtype == 1)
    if (exact.death && is.null(next.obstime)) {
        cat("Imputing sampling times after deaths...\n")
        incl <- if (indep.cens) 
            (0:2)
        else (0:1)
        empiricaldist <- empiricaldists(md$timeinterval[md$obtype %in% 
            incl], md$state[md$obtype %in% incl], as.numeric(md$timegroup[md$obtype %in% 
            incl]), timegroups, ndstates)
        imputation <- array(0, c(ndeath, N, 4))
        dimnames(imputation) <- list(NULL, NULL, c("times", "cens", 
            "intervalgroup", "timeqmatindex"))
        deathindex <- which(md$obtype == 1)
        for (i in 1:ndeath) {
            mintime <- md$timeinterval[deathindex[i]]
            centime <- md$maxtimes[deathindex[i]]
            tg <- md$timegroup[deathindex[i]]
            st <- sampletimes(mintime, centime, empiricaldist[, 
                tg, empiricaldist["time", tg, ] > 0, drop = FALSE], 
                N, tg, intervalq)
            for (j in c("times", "cens", "intervalgroup")) imputation[i, 
                , j] <- st[, j]
        }
    }
    else imputation <- deathindex <- NULL
    ndeathindex <- setdiff(1:ntrans, deathindex)
    timeint <- c(md$timeinterval[md$obtype != 1], c(imputation[, 
        , "times"]))
    qmatint <- c(qmatindex[md$obtype != 1], rep(qmatindex[deathindex], 
        N))
    timeqmat <- paste(timeint, qmatint, sep = "-")
    timeqmata <- data.frame(timeint, qmatint)[!duplicated(timeqmat), 
        ]
    pastedu <- unique(timeqmat)
    timeqmatindex <- match(timeqmat, pastedu)
    npmats <- length(pastedu)
    pmi <- array(0, dim = c(nst, nst, npmats))
    for (i in unique(timeqmata[, 2])) pmi[, , timeqmata[, 2] == 
        i] <- MatrixExp(qmatmaster[, , i], timeqmata[timeqmata[, 
        2] == i, 1], method = "pade")
    md$timeqmatindex <- rep(0, ntrans)
    md$timeqmatindex[md$obtype != 1] <- timeqmatindex[1:(ntrans - 
        ndeath)]
    if (exact.death && is.null(next.obstime)) 
        imputation[, , "timeqmatindex"] <- timeqmatindex[(ntrans - 
            ndeath + 1):length(timeqmatindex)]
    prob <- array(0, dim = c(nst, ntrans))
    if (x$emodel$misc) {
        misccov <- if (x$ecmodel$ncovs > 0) 
            od$misccov[od$obsno > 1, , drop = FALSE]
        else NULL
        p.true <- array(dim = c(nst, ntrans))
        initp <- x$hmodel$initpmat
        if (x$ecmodel$ncovs > 0) {
            uniqmisc <- unique(misccov)
            ematindex <- match(do.call("paste", misccov), do.call("paste", 
                uniqmisc))
            emat <- array(0, dim = c(nst, nst, nrow(uniqmisc)))
            for (i in 1:nrow(uniqmisc)) emat[, , i] <- ematrix.msm(x, 
                covariates = as.list(uniqmisc[i, ]), ci = "none")
        }
        else emat <- ematrix.msm(x, ci = "none")
        for (i in 1:ntrans) {
            ematrix <- if (x$ecmodel$ncovs > 0) 
                emat[, , ematindex[i]]
            else emat
            if (md$state[i] %in% ndstates) {
                T <- pmi[, , md$timeqmatindex[i]] * matrix(ematrix[, 
                  md$state[i]], nrow = nst, ncol = nst, byrow = TRUE)
                p.true[, i] <- if (md$obsno[i] == 2) 
                  t(initp[md$subj.num[i], ]) %*% T
                else t(p.true[, i - 1]) %*% T
                p.true[, i] <- p.true[, i]/sum(p.true[, i])
            }
            if (!(md$state[i] %in% dstates)) {
                prob[, i] <- if (md$obsno[i] == 2) 
                  initp[md$subj.num[i], ] %*% pmi[, , md$timeqmatindex[i]] %*% 
                    ematrix
                else p.true[, i - 1] %*% pmi[, , md$timeqmatindex[i]] %*% 
                  ematrix
            }
        }
    }
    else prob[cbind(rep(1:nst, ntrans - ndeath), rep((1:ntrans)[ndeathindex], 
        each = nst))] <- pmi[cbind(rep(md$prevstate[(1:ntrans)[ndeathindex]], 
        each = nst), rep(1:nst, ntrans - ndeath), rep(md$timeqmatindex[(1:ntrans)[ndeathindex]], 
        each = nst))]
    if (exact.death && is.null(next.obstime)) {
        stat.sim <- rep(0, N)
        obs.rep <- exp.rep <- dev.rep <- array(0, dim = c(groupdims, 
            trans$ngroups, N))
        dimnames(obs.rep) <- dimnames(exp.rep) <- dimnames(dev.rep) <- c(groupdimnames, 
            list(trans$labsagg), list(1:N))
        cat("Calculating replicates of test statistics for imputations...\n")
        for (i in 1:N) {
            if (x$emodel$misc) {
                for (j in 1:ndeath) {
                  k <- deathindex[j]
                  ematrix <- if (x$ecmodel$ncovs > 0) 
                    emat[, , ematindex[k]]
                  else emat
                  prob[, k] <- if (md$obsno[k] == 2) 
                    initp[md$subj.num[k], ] %*% pmi[, , imputation[j, 
                      i, "timeqmatindex"]] %*% ematrix
                  else p.true[, k - 1] %*% pmi[, , imputation[j, 
                    i, "timeqmatindex"]] %*% ematrix
                }
            }
            else prob[cbind(rep(1:nst, ndeath), rep(deathindex, 
                each = nst))] <- pmi[cbind(rep(md$prevstate[deathindex], 
                each = nst), rep(1:nst, ndeath), rep(imputation[, 
                i, "timeqmatindex"], each = nst))]
            md$intervalgroup[deathindex] <- factor(imputation[, 
                i, "intervalgroup"], labels = levels(md$intervalgroup[deathindex])[sort(unique(imputation[, 
                i, "intervalgroup"]))])
            md$cens[deathindex] <- as.numeric(imputation[, i, 
                "cens"])
            obs.rep.i <- table(md$state, md$prevstate, md$timegroup, 
                md$intervalgroup, md$covgroup, md$usergroup)
            exp.cens <- array(0, dim = c(nst, nndstates, groupdims, 
                2))
            for (j in 1:nst) exp.cens[j, , , , , , ] <- tapply(prob[j, 
                ], list(md$prevstate, md$timegroup, md$intervalgroup, 
                md$covgroup, md$usergroup, md$cens), sum)
            exp.cens <- replace(exp.cens, is.na(exp.cens), 0)
            exp.unadj <- array(0, dim = c(nstcens, nndstates, 
                groupdims))
            exp.unadj[1:nst, , , , , ] <- exp.cens[, , , , , 
                , 1]
            for (j in dstates) exp.unadj[j, , , , , ] <- exp.cens[j, 
                , , , , , 1] + exp.cens[j, , , , , , 2]
            if (cens) 
                exp.unadj[nst + 1, , , , , ] <- apply(exp.cens[1:nndstates, 
                  , , , , , 2, drop = FALSE], 2:6, sum)
            exp.unadj <- replace(exp.unadj, (exp.unadj < sqrt(.Machine$double.eps)), 
                0)
            exp.rep.i <- msm:::adjust.expected.cens(exp.unadj, obs.rep.i, 
                nst, ndstates, dstates, groupdims, N, cens, md, 
                nstcens)
            agg <- msm:::agg.tables(obs.rep.i, exp.rep.i, groupdims, 
                groupdimnames, trans)
            obs.rep.i <- agg$obs
            exp.rep.i <- agg$exp
            dev.rep.i <- (obs.rep.i - exp.rep.i)^2/(exp.rep.i + 
                (exp.rep.i == 0))
            obs.rep[, , , , , i] <- obs.rep.i
            exp.rep[, , , , , i] <- exp.rep.i
            dev.rep[, , , , , i] <- dev.rep.i
            stat.sim[i] <- sum(dev.rep.i)
        }
        obstable <- apply(obs.rep, 1:5, mean)
        exptable <- apply(exp.rep, 1:5, mean)
        exptable <- replace(exptable, (exptable < sqrt(.Machine$double.eps)), 
            0)
        devtable <- apply(dev.rep, 1:5, mean)
        stat <- mean(stat.sim)
    }
    else {
        obstable <- exptable <- array(0, dim = c(nstcens, nndstates, 
            groupdims))
        dimnames(obstable) <- dimnames(exptable) <- c(list(1:nstcens, 
            1:nndstates), groupdimnames)
        obstable <- table(md$state, md$prevstate, md$timegroup, 
            md$intervalgroup, md$covgroup, md$usergroup)
        exp.cens <- array(0, dim = c(nst, nndstates, groupdims, 
            2))
        for (j in 1:nst) exp.cens[j, , , , , , ] <- tapply(prob[j, 
            ], list(md$prevstate, md$timegroup, md$intervalgroup, 
            md$covgroup, md$usergroup, md$cens), sum)
        exp.cens <- replace(exp.cens, is.na(exp.cens), 0)
        exptable <- array(0, dim = c(nstcens, nndstates, groupdims))
        exptable[1:nst, , , , , ] <- exp.cens[, , , , , , 1]
        exptable[nst, , , , , ] <- exp.cens[nst, , , , , , 1] + 
            exp.cens[nst, , , , , , 2]
        if (cens) 
            exptable[nst + 1, , , , , ] <- apply(exp.cens[1:nndstates, 
                , , , , , 2, drop = FALSE], 2:6, sum)
        exptable <- replace(exptable, (exptable < sqrt(.Machine$double.eps)), 
            0)
        if (cens) 
            exptable <- msm:::adjust.expected.cens(exptable, obstable, 
                nst, ndstates, dstates, groupdims, N, cens, md, 
                nstcens)
        agg <- msm:::agg.tables(obstable, exptable, groupdims, groupdimnames, 
            trans)
        obstable <- agg$obs
        exptable <- agg$exp
        devtable <- (obstable - exptable)^2/(exptable + (exptable == 
            0))
        stat <- sum(devtable)
    }
    n.indep.trans <- length(trans$from[trans$to <= nst]) - length(unique(trans$from[trans$to <= 
        nst]))
    n.from <- apply(obstable, 1:4, function(u) tapply(u, trans$from, 
        sum))
    n.from <- array(n.from, dim = c(length(unique(trans$from)), 
        dim(obstable)[1:4]))
    ndf <- as.numeric(table(trans$from) - 1)
    n.zero <- sum((n.from == 0) * ndf)
    df.upper <- prod(dim(obstable)[1:4]) * n.indep.trans - n.zero
    df.lower <- df.upper - length(x$opt$par)
    acc.p <- FALSE
    if (pval && (x$cmodel$ncens == 0) && !exact.death && !x$emodel$misc) {
        acc.p <- TRUE
        md$grouplab <- interaction(md$timegroup, md$intervalgroup, 
            md$covgroup, md$usergroup, factor(md$prevstate))
        md$group <- match(md$grouplab, sort(unique(md$grouplab)))
        C <- length(unique(md$group))
        R <- nst
        ncat <- prod(groupdims)
        from <- trans$from
        to <- trans$to
        nfrom <- length(unique(from))
        Parr <- array(0, dim = c(R, ncat, nfrom))
        for (i in 1:nfrom) {
            fi <- unique(from)[i]
            et <- 1/sqrt(exptable[, , , , from == fi])
            Parr[to[from == fi], , i] <- t(array(et, dim = c(ncat, 
                length(to[from == fi]))))
        }
        Pmat <- array(Parr, dim = c(R, ncat * nfrom))
        Pmat <- Pmat[, is.finite(colSums(Pmat))]
        if (ncol(Pmat) != C) 
            stop("Remove any absorbing-absorbing transitions from data and refit model")
        P <- diag(as.vector(Pmat))
        pr <- matrix(0, nrow = ntrans, ncol = nst)
        for (i in 1:ntrans) pr[i, ] <- pmatrix.msm(x, t = md$timeinterval[i], 
            t1 = md$time[i], covariates = if (ncovs > 0) 
                as.list(md$cov[i, ])
            else 0)[md$prevstate[i], ]
        Sigma <- matrix(0, nrow = C * nst, ncol = C * nst)
        for (c in 1:C) {
            block <- matrix(0, nrow = nst, ncol = nst)
            prc <- pr[md$group == c, , drop = FALSE]
            for (r in 1:nst) block[r, -r] <- -colSums(prc[, r] * 
                prc[, -r, drop = FALSE])
            diag(block) <- colSums(prc * (1 - prc))
            rows <- cols <- (c - 1) * nst + 1:nst
            Sigma[rows, cols] <- block
        }
        PSigmaPT <- P %*% Sigma %*% t(P)
        dp <- msm:::Ccall.msm(x$paramdata$opt$par, do.what = "dpmat", 
            msm:::expand.data(x), x$qmodel, x$qcmodel, x$cmodel, x$hmodel, 
            x$paramdata)
        Psiarr <- apply(dp, c(2, 3), function(x) tapply(x, md$group, 
            sum))
        npars <- x$paramdata$nopt
        Psi <- t(array(aperm(Psiarr, c(2, 1, 3)), dim = c(R * 
            C, npars)))
        EI <- 0.5 * x$paramdata$info
        Omega <- rbind(cbind(EI, Psi %*% P), cbind(t(P) %*% t(Psi), 
            PSigmaPT))
        Barr <- Psiarr
        fromgroups <- md$prevstate[!duplicated(md$group)][order(unique(md$group))]
        for (i in 1:nfrom) {
            fi <- unique(from)[i]
            rows <- fromgroups == fi
            et <- exptable[, , , , from == fi]
            Barr[rows, to[from == fi], ] <- -Barr[rows, to[from == 
                fi], ]/as.numeric(sqrt(et[et > 0]))
        }
        Bmat <- array(aperm(Barr, c(2, 1, 3)), dim = c(R * C, 
            npars))
        A <- cbind(Bmat %*% solve(EI), diag(R * C))
        V <- A %*% Omega %*% t(A)
        lambda <- eigen(V, only.values = TRUE)$values
        psi <- function(u) prod((1 - (0+2i) * lambda * u)^(-0.5))
        fn <- function(u) {
            res <- numeric(length(u))
            for (i in seq_along(u)) res[i] <- Im(psi(u[i]) * 
                exp(-(0+1i) * u[i] * stat)/(2 * pi * u[i]))
            res
        }
        int <- try(integrate(fn, -Inf, Inf))
        if (inherits(int, "try-error")) {
            message("Unable to calculate more accurate p-value")
            p.acc <- p.err <- NULL
        }
        else {
            p.acc <- 0.5 + int$value
            p.err <- int$abs.error
            if (p.acc - 2 * p.err < 0) 
                p.acc <- 0
            if (p.acc + 2 * p.err > 1) 
                p.acc <- 1
        }
    }
    test <- data.frame(stat = stat)
    if (acc.p) 
        test$p <- p.acc
    test$df.lower <- if (exact.death && is.null(next.obstime)) 
        NA
    else df.lower
    test$p.lower <- if (exact.death && is.null(next.obstime)) 
        NA
    else 1 - pchisq(stat, df.lower)
    test$df.upper <- df.upper
    test$p.upper <- 1 - pchisq(stat, df.upper)
    rownames(test) <- ""
    if (!is.null(imputation)) {
        imp.times <- matrix(rep(x$data$mf$"(time)", N), nrow = length(x$data$mf$"(time)"), 
            ncol = N)
        prevtime <- imp.times[which(x$data$mf$"(state)" %in% 
            dstates) - 1, ]
        imp.times[x$data$mf$"(state)" %in% dstates, ] <- prevtime + 
            imputation[, , "times"]
    }
    else imp.times <- NULL
    if (boot) {
        if (!is.null(groups)) 
            stop("Bootstrapping not valid with user-specified groups")
        cat("Starting bootstrap refitting...\n")
        boot.stats <- msm:::pearson.boot.msm(x, imp.times = imp.times, 
            transitions = transitions, timegroups = timegroups, 
            intervalgroups = intervalgroups, covgroups = covgroups, 
            groups = groups, B = B)
        test$p.boot <- sum(boot.stats > stat)/B
    }
    pearson <- list(observed = obstable, expected = exptable, 
        deviance = devtable * sign(obstable - exptable), test = test, 
        intervalq = intervalq)
    names(pearson) <- c("Observed", "Expected", "Deviance*sign(O-E)", 
        "test", "intervalq")
    if (exact.death && is.null(next.obstime)) 
        pearson$sim <- list(observed = obs.rep, expected = exp.rep, 
            deviances = dev.rep, stat = stat.sim, imputation = imputation)
    if (boot) 
        pearson$boot <- boot.stats
    if (acc.p) 
        pearson$lambda <- lambda
    pearson <- msm:::reformat.pearson.msm(pearson)
    class(pearson) <- "pearson.msm"
    pearson
}
