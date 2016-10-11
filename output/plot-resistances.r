###
# produces a plot of resistances to all the treatments
###

source("common.r")

plot.resistances <- function(outdir=".", x.axis=FALSE, y.axis=FALSE) {
    cat("  resistances\n")
    # get all the cage names
    pattern = ".*_.*_resistance.csv$"
    resistance.filenames = list.files(path=outdir, pattern=pattern)
    cage.names = c("farm")
    if (grepl("mosaic", outdir)) {
        if (SINGLE.PLOTS) {
            cage.names = c()
            for (filename in resistance.filenames) {
                all = str_locate_all(filename, "_")
                start = all[[1]][1,1] + 1
                end = all[[1]][2,1] -1
                cage.name = substring(filename, start, end)
                if (!grepl("farm", cage.name)) {
                    cage.names = append(cage.name, cage.names)
                }
            }
        } else {
            # only interested in one cage
            cage.names = c("cage-1")
        }
        
                                        # make a unique list
        cage.names = sort(unique(cage.names))
    } 
    # plot the resistances
    for (cage.name in cage.names) {
        plot.cage.resistances(outdir, cage.name, FALSE, FALSE)
        y.axis = FALSE
    }
}

plot.cage.resistances <- function(outdir, cage="farm", x.axis, y.axis) {
    cat("    ")
    cat(cage)
    cat("\n")

                                        # create the output chart file
    # open.chart.file(paste(outdir, "/", outdir, "-", cage, "-resistances.pdf", sep=""))
    
    max.x = cycles.length*cycles.num
    max.y = 1

    if (!SINGLE.PLOTS) {
        par(mar=c(0.3, 0.3, 2, 0.3))
        plot(0, type="n",
             xlim=c(1, max.x), ylim=c(0, max.y),
             xaxt="n", xlab="", yaxt="n", ylab="")
        chart.title = substring(outdir, 3)
        chart.title = paste(toupper(substr(chart.title,1,1)), substring(chart.title, 2), sep="")
        if (grepl("mosaic", outdir)) {
            cage.str = paste(toupper(substring(cage, 1, 1)), substring(cage, 2, 4), sep="")
            cage.str = paste(cage.str, substring(cage, 6))
            chart.title = paste(chart.title, "%: ", cage.str, sep="")
        }
        mtext(chart.title, side=3, line=0.2)

    } else {
        par(mar=c(5, 5, 0, 0), lwd=3)
        plot(0, type="n",
             xlim=c(1, max.x), ylim=c(0, max.y),
             xaxt="n", xlab="", yaxt="n", ylab="")
        
        axis(1, seq(cycles.length/2, max.x-cycles.length/2, cycles.length),
             seq(1, cycles.num, 1), tick=FALSE)
        axis(1, seq(0, max.x, cycles.length), labels=FALSE)
        mtext("Cycle", side=1, line=3, cex=label.size)
        
        axis(2, seq(0, max.y, max.y/10), seq(0, max.y, max.y/10))
        mtext("Proportion of alleles", side=2, line=2.5, cex=label.size)
    }

    # list of all the data
    all.data = list()

    # get the filenames of the farm-wide resistance files
    pattern = paste(".*_", cage, "_resistance.csv$", sep="")
    resistance.filenames = list.files(path=outdir, pattern=pattern)
    for (filename in resistance.filenames) {
        res.type = substring(filename, 1, nchar(filename)-nchar("__resistance.csv")-nchar(cage))
        if (res.type != "CONTROL") {
            if (res.type == "T1") { res.type = "C1" }
            if (res.type == "T2") { res.type = "C2" }
            cat(paste("     ", res.type, "\n"))

            data = read.csv.file(outdir, filename)
            all.data[[res.type]] = process.data(data)
        }
    }
    #cat("names: ")
    #cat(names(all.data))
    #cat("\n")
    if (!SINGLE.PLOTS) {
                                        # draw all the confidence intervals
        for (data.name in names(all.data)) {
            draw.CI(all.data[[data.name]])
        }
    }

    # draw all the average lines
    # draw.average.lines(all.data, leg.loc=c(max.x/6, 5*max.y/6), type="treatment")
    if (outdir == "./responsive" || SINGLE.PLOTS) {
        leg.loc = c(535, 0.9)
    } else {
        leg.loc = "none"
    }
    draw.average.lines(all.data, leg.loc, type="treatment")

    if (!SINGLE.PLOTS) {
                                        # draw a grid
        abline(v=(seq(0, max.x, cycles.length/2)), col="lightgray", lty="dotted")
        abline(h=(seq(0, max.y, 0.1)), col="lightgray", lty="dotted")

                                        # close the charting
                                        # dev.off()
    }
}
