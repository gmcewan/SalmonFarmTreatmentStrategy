source("common.r")

treatment.event.counts <- function(outdir=".") {
    # list of all the data
    all.data = list()

    # get the filenames of the treatment files
    pattern = ".*_treatments.csv$"
    treatment.filenames = list.files(path=outdir, pattern=pattern)
    # print("---")
    for (filename in treatment.filenames) {
        trt.type = substring(filename, 1, nchar(filename)-nchar("_treatments.csv"))
        if (!grepl("control", tolower(filename))) {
            # print("not control")
            data = read.csv.file(outdir, filename)
            data[data<0] = 0
            data[is.na(data)] = 0
            if (length(all.data) == 0) {
                # print(paste("first", outdir, filename, length(data)))
                # first one, fill in the data
                all.data = data
            } else if (grepl("periodic", tolower(outdir)) ||
                       grepl("responsive", tolower(outdir)) ||
                       grepl("rotation", tolower(outdir))) {
                # print(paste("adding", outdir, filename, length(data)))
                # have to add the treatments
                all.data = all.data + data
            }
        }
    }

    # print("---")
    # print(outdir)
    # print(all.data)

    all.data = process.data(all.data, compress=TRUE)

    # print(all.data)
    
    treatment.event.counts = all.data

}

plot.treatment.counts <- function(outdir) {
    # create a chart showing number of treatments per cycle
    # create the output chart file
    #tiff(filename=paste(outdir,"/treatments.tiff",sep=""), family="ArialMT", pointsize=10,
    #     width=19, height=12, units="cm", res=300, compression="lzw", type="cairo")
    # open.chart.file(paste(outdir, "/", outdir, "-treatment_counts.pdf", sep=""))

    max.x = cycles.num
    max.y = 20
    if (!SINGLE.PLOTS) {
        par(mar=c(0.3, 0.3, 2, 0.3))
        plot(0, type="n",
             xlim=c(1, max.x), ylim=c(0, max.y),
             xaxt="n", xlab="", yaxt="n", ylab="")
        chart.title = substring(outdir, 3)
        chart.title = paste(toupper(substr(chart.title,1,1)),
            substring(chart.title, 2), sep="")
        mtext(chart.title, side=3, line=0.2)

        all.data = list()
        all.data$all = treatment.event.counts(outdir)

        print(all.data)

                                        # draw all the lines with error bars
        create.line.plot(all.data, show.legend=F)


                                        # draw a grid
        abline(v=(seq(0, max.x, 1)), col="lightgray", lty="dotted")
        abline(h=(seq(0, max.y, 1)), col="lightgray", lty="dotted")

                                        # close the charting
                                        # dev.off()

    } else {
        plot.treatment.times(outdir)
       # par(mar=c(5, 5, 0, 0), lwd=2)
       # plot(0, type="n",
       #      xlim=c(1, max.x), ylim=c(0, max.y),
       #      xaxt="n", xlab="", yaxt="n", ylab="")
        
       # axis(1, seq(1, cycles.num, 1), seq(1, cycles.num, 1))
       # mtext("Cycle", side=1, line=3, cex=label.size)
        
       # axis(2, seq(0, max.y, 2), seq(0, max.y, 2))
       # mtext("Number of Treatment Events", side=2, line=2.5, cex=label.size)
    }
}

plot.treatment.times <- function(outdir=".") {

    # create a chart showing the treatment times
    open.chart.file(paste(outdir, "/", outdir, "-treatment_times.pdf", sep=""))

    max.x = cycles.num*cycles.length
    max.y = 1
    plot(0, type="n", ylab="Proportion of Simulation Trials with a Treatment Event",
         xlim=c(1, max.x), ylim=c(0,max.y),
         xaxt="n", xlab="Cycle")
    axis(1, seq(0, max.x, cycles.length/2), seq(0, cycles.num*2, 1))

                                        # list of all the data
    all.data = list()

                                        # get the filenames of the treatment files
    pattern = ".*_treatments.csv$"
    treatment.filenames = list.files(path=outdir, pattern=pattern)

    block.size = treatment.min.time
    block.nums = as.integer(max.x/block.size)

    for (filename in treatment.filenames) {
        print(filename)
        trt.type = substring(filename, 1, nchar(filename)-nchar("_treatments.csv"))

        read.data = read.csv(paste(outdir, "/", filename, sep=""), header=F)

                                        # <0 means a missing value
        read.data[read.data<0] = NA
        
                                        # get number of treatments in each block
        compressed.matrix = matrix(0, 1, block.nums)

        total.possible.treatments = (as.integer(block.size/treatment.min.time))*length(read.data[,1])
        
        max.treatments = 0
        for (block in seq(1, block.nums)) {
            start = (block-1)*treatment.min.time + 1
            end = block*treatment.min.time
            if (end > length(read.data[1,])) { end = length(read.data[1,]) }
            num.treatments = sum(read.data[,seq(start, end)], na.rm=T)
            if (num.treatments > max.treatments) { max.treatments = num.treatments }
            compressed.matrix[,block] = num.treatments/total.possible.treatments
            ##f (trt.type != "CONTROL" && num.treatments>total.possible.treatments) {
            ##   cat("---\n")
            ##   print(read.data[,seq(start,end)])
            ##   print(start)
            ##   print(end)
            ##   print(num.treatments)
            ##   print(total.possible.treatments)
            ##   print(num.treatments/total.possible.treatments)
            ##   cat("---\n")
            ##
            
        }
        # print(max.treatments)
        all.data[[trt.type]] = compressed.matrix
    }

                                        # now draw the data
    offset = 0
    col.num = 1
    for (trt.times in all.data) {
                                        # print(offset)
        x.seq = seq(0+offset, max.x, block.size)
        start.y.seq = matrix(0, 1, length(x.seq))
        end.y.seq = trt.times
        
        segments(x.seq, start.y.seq, x.seq, end.y.seq, treatment.cols[col.num])

        offset = offset + as.integer(block.size/(length(names(all.data))-1))
        col.num = col.num + 1
    }


                                        # close the treatment count charting
    dev.off()
    
    
    # warnings()
}

plot.treatments <- function(outdir=".") {
    cat("  treatments\n")
    cat("    counts\n")
    plot.treatment.counts(outdir)
    # cat("    times\n")
    # plot.treatment.times(outdir)
}
