source("common.r")

print.infection.pressure <- function(outdir, ip.data) {
    ip.filename = paste(outdir, "/", outdir, "-infection_pressure.txt", sep="")

    # print the headers
    header.string = sprintf("  %-13s", "Stage")
    for (cycle in seq(1, cycles.num)) {
        header.string = paste(header.string, sprintf("   Cycle %-8d", cycle))
    }
    header.string = paste(header.string, "\n")
    cat(header.string, file=ip.filename, append=F)

    # print by stage
    stage.order = c("Chalimus", "PreAdults", "AdultMales", "AdultFemales", "Gravids")

    for (stage in stage.order) {
        cat(paste("   ", stage,"\n"))
        ip.string = sprintf("%-13s", stage)
        # print(ip.data[[stage]])
        for (i in seq(1,length(ip.data[[stage]]$means))) {
            new.value = sprintf("%8.2f (\u00B1%6.2f)", ip.data[[stage]]$means[i], ip.data[[stage]]$se[i]*1.96)
            ip.string = paste(ip.string, new.value)
        }
        # print(paste(ip.string,"\n"))
        ip.string = paste(ip.string, "\n")
        cat(ip.string, file=ip.filename, append=T)
        # print(ip.data[[stage]])
    }
}

write.pressure.file <- function(outdir) {
    cat("  pressure\n")
    # get the filenames of the resistance files
    pattern = "^licecount_.*.csv$"
    licecount.filenames = list.files(path=outdir, pattern=pattern)

    infection.pressure = list()
    # ip.filename = paste(outdir, "/", outdir, "infection_pressure.txt", sep="")
    # cat(sprintf("  %-13s %-9s (%-8s -  %-7s)\n", "Stage", "Mean", "Low CI", "High CI"), file=ip.filename, append=F)

    for (filename in licecount.filenames) {
        stagename = substring(filename, nchar("licecount_")+1, nchar(filename)-nchar(".csv"))
                                        # only chart all adults
        
                                        # get the infection pressure
        data = read.csv.file(outdir, filename)
        infection.pressure[[stagename]] = process.data(data, compress=T)
        # infection.pressure = get.infection.pressure(outdir, filename)

                                        # write to a file
        # ip.string = sprintf("%-13s %9.3f  (%9.3f - %9.3f)\n", stagename, infection.pressure$mean, infection.pressure$lowCI, infection.pressure$highCI)
        # cat(ip.string, file=ip.filename, append=T)
    }
    print.infection.pressure(outdir, infection.pressure)
}
    
plot.licecounts <- function(outdir) {
    cat("  licecounts\n")
    # get the filenames of the resistance files
    pattern = "^licecount_.*.csv$"
    licecount.filenames = list.files(path=outdir, pattern=pattern)

    for (filename in licecount.filenames) {
        stagename = substring(filename, nchar("licecount_")+1, nchar(filename)-nchar(".csv"))
        if (grepl("AllAdults", stagename)) {
            # only chart all adults
            cat(paste("   ", stagename,"\n"))
                                        # cat(paste("   ", res.type, "\n"))

            data = read.csv.file(outdir, filename)
            all.data = process.data(data)

                                        # create the output chart file
                                        # open.chart.file(paste(outdir, "/", outdir, "-", stagename, "-licecount.pdf", sep=""))

            max.x = cycles.length*cycles.num
            max.y = 10

            if (!SINGLE.PLOTS) {
                par(mar=c(0.3, 0.3, 2, 0.3))
                plot(0, type="n",
                     xlim=c(1, max.x), ylim=c(0, max.y),
                     xaxt="n", xlab="", yaxt="n", ylab="")
                chart.title = substring(outdir, 3)
                chart.title = paste(toupper(substr(chart.title,1,1)),
                    substring(chart.title, 2), sep="")
                mtext(chart.title, side=3, line=0.2)

            } else {
                par(mar=c(5, 5, 0, 0), lwd=2)
                plot(0, type="n",
                     xlim=c(1, max.x), ylim=c(0, max.y),
                     xaxt="n", xlab="", yaxt="n", ylab="")
                 
                axis(1, seq(cycles.length/2, max.x-cycles.length/2, cycles.length),
                     seq(1, cycles.num, 1), tick=FALSE)
                axis(1, seq(0, max.x, cycles.length), labels=FALSE)
                mtext("Cycle", side=1, line=3, cex=label.size)
                
                axis(2, seq(0, max.y, max.y/10), seq(0, max.y, max.y/10))
                mtext("Lice per Salmon", side=2, line=2.5, cex=label.size)
            }


            # x axis
            # 
            # axis(1, seq(cycles.length/2, max.x-cycles.length/2, cycles.length), seq(1, cycles.num, 1), tick=FALSE)
            # axis(1, seq(0, max.x, cycles.length), labels=FALSE)

            # y axis
            # 
            # axis(2, seq(0, max.y, max.y/10), seq(0, max.y, max.y/10))


            draw.CI(all.data)
                                        # draw all the average lines
            data.list = list()
            data.list[[stagename]] = all.data
            draw.average.lines(data.list, leg.loc="none")

            if (!SINGLE.PLOTS) {
                                        # draw a grid
                abline(v=(seq(0, max.x, cycles.length/2)), col="lightgray", lty="dotted")
                abline(h=(seq(0, max.y, 1)), col="lightgray", lty="dotted")

                                        # close the charting
                                        # dev.off()
            }
        }
    }
    
} 
