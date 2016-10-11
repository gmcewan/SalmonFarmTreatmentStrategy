source("plot-resistances.r")
source("plot-licecounts.r")
source("plot-treatments.r")

# get all the output directories
output.dirs = list.dirs(path=".", recursive=FALSE)

multi.dir.list = c("./responsive",
    "./mosaic-30",
    "./rotation",
    "./periodic-longer",
    "./periodic-shorter",
    "./combination")


do.resistance = TRUE
do.treatment = TRUE
do.count = TRUE
do.infection.pressure = TRUE

start.multi <- function(filename, max.x, max.y) {
                                        # do all the resistances in a single figure
        open.chart.file(filename)
                                        # par(mfrow=c(3,3)) #, mar=c(0,0,0,0), oma=c(5,5,0,0), xpd=NA)

                                        # set up the layout
        layout(matrix(c(1, 6, 7, 8, 2, 9, 10, 11, 0, 3, 4, 5), 3, 4, byrow=TRUE),
               c(1,6,6,6), c(4,4,1), TRUE)

                                        # do the y-axes down the left
        par(mar=c(0.3,5,2,0))
        for (i in seq(1,2,1)) {
            plot(0, type="n", xaxt="n", yaxt="n",
                 xlab="", ylab="", axes=0,
                 xlim=c(0, max.x), ylim=c(0, max.y))
            mtext("Proportion of alleles", side=2, line=3)
            axis(2, seq(0, max.y, max.y/10), seq(0, max.y, max.y/10))
        }

                                        # do the x-axes along the bottom
        par(mar=c(5,0.3,0,0))
        for (i in seq(1,3,1)) {
            plot(0, type="n", xaxt="n", yaxt="n",
                 xlab="", ylab="", axes=0,
                 xlim=c(0, max.x), ylim=c(0, max.y))
            mtext("Cycles", side=1, line=3)
            axis(1, seq(cycles.length/2, max.x-cycles.length/2, cycles.length),
                 seq(1, cycles.num, 1), tick=FALSE)
            axis(1, seq(0, max.x, cycles.length), labels=FALSE)  
        }

}

if (do.resistance) {
    cat(paste("---","Resistances","\n"))
    if (!SINGLE.PLOTS) {
        max.x = cycles.length*cycles.num
        max.y = 1
        start.multi("resistances.pdf", max.x, max.y)
        
        plot.count = 1
        for (outdir in multi.dir.list) {
            cat(paste(outdir,"\n"))
            x.axis = (plot.count > 4)
            y.axis = (plot.count %in% c(1,3,5))
            plot.resistances(outdir, x.axis, y.axis)

            plot.count = plot.count + 1
        }
        dev.off()
    } else {
        for (outdir in output.dirs[nchar(output.dirs)>2]) {
            if (substring(outdir,3,3) != ".") {
                cat(paste(outdir,"\n"))
                open.chart.file(paste(outdir,"/resistance.pdf",sep=""), figure.height=5)
                plot.resistances(outdir)
                dev.off()
            }
        }
    }

}

if (do.count) {
    cat(paste("---","lice counts","\n"))

    if (!SINGLE.PLOTS) {
        max.x = cycles.length*cycles.num
        max.y = 10
        start.multi("licecounts.pdf", max.x, max.y)
        
        for (outdir in multi.dir.list) {
            cat(paste(outdir,"\n"))
            plot.licecounts(outdir)
        }
        dev.off()
    } else {
        for (outdir in output.dirs[nchar(output.dirs)>2]) {
            if (substring(outdir,3,3) != ".") {
                cat(paste(outdir,"\n"))
                open.chart.file(paste(outdir,"/licecounts.pdf",sep=""), figure.height=5)
                plot.licecounts(outdir)
                dev.off()
            }
        }
    }

}

if (do.infection.pressure) {
                                        # create all the infection pressure files
    cat(paste("---","infection pressure","\n"))
    for (outdir in output.dirs[nchar(output.dirs)>2]) {
        if (substring(outdir,3,3) != ".") {

            cat(paste(outdir,"\n"))
            write.pressure.file(outdir)
        }
    }
}

if (do.treatment) {
    cat(paste("---","treatment counts","\n"))

    if (!SINGLE.PLOTS) {
        max.x = cycles.num
        max.y = 20

        open.chart.file("treatments.pdf")
                                        # par(mfrow=c(3,3)) #, mar=c(0,0,0,0), oma=c(5,5,0,0), xpd=NA)

                                        # set up the layout
        layout(matrix(c(1, 6, 7, 8, 2, 9, 10, 11, 0, 3, 4, 5), 3, 4, byrow=TRUE),
               c(1,6,6,6), c(4,4,1), TRUE)

                                        # do the y-axes down the left
        par(mar=c(0.3,5,2,0))
        for (i in seq(1,2,1)) {
            plot(0, type="n", xaxt="n", yaxt="n",
                 xlab="", ylab="", axes=0,
                 xlim=c(0, max.x), ylim=c(0, max.y))
            mtext("Proportion of alleles", side=2, line=3)
            axis(2, seq(0, max.y, 2), seq(0, max.y, 2))
        }

                                        # do the x-axes along the bottom
        par(mar=c(5,0.3,0,0))
        for (i in seq(1,3,1)) {
            plot(0, type="n", xaxt="n", yaxt="n",
                 xlab="", ylab="", axes=0,
                 xlim=c(1, max.x), ylim=c(0, max.y))
            mtext("Cycles", side=1, line=3)
            axis(1, seq(1, cycles.num, 1), seq(1, cycles.num, 1))
        }

        for (outdir in multi.dir.list) {
            cat(paste(outdir,"\n"))
            plot.treatments(outdir)
        }
        dev.off()
    } else {
        for (outdir in output.dirs[nchar(output.dirs)>2]) {
            if (substring(outdir,3,3) != ".") {
                cat(paste(outdir,"\n"))
                open.chart.file(paste(outdir,"/treatments.pdf",sep=""), figure.height=5)
                plot.treatments(outdir)
                dev.off()
            }
        }
    }

}
