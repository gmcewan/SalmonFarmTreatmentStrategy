###
# Common functions and data that get reused lots
###
library(stringr)
require(Hmisc)

# true if doincg individual plots
SINGLE.PLOTS = FALSE

label.size = 3

cycles.num = 6
farm.time = 656
fallow.time = 730 - farm.time
cycles.length = farm.time + fallow.time

treatment.min.time = 14

lice.cols = heat.colors(7)

treatment.cols = c("black", "purple", "chartreuse3")
ci.color = "gray90"

#symbol.codes = list(
#    "H2O2"=1,
#    "EMB"=2,
#    "AZA"=3)
colour.codes = list(
    "CONTROL"=treatment.cols[1],
    "T1"=treatment.cols[2],
    "C1"=treatment.cols[2],    
    "T2"=treatment.cols[3],
    "C2"=treatment.cols[3],
    "Chalimus"=lice.cols[7],
    "PreAdults"=lice.cols[6],
    "AdultMales"=lice.cols[5],
    "AdultFemales"=lice.cols[4],
    "Gravids"=lice.cols[3],
    "Mobiles"=lice.cols[2],
    "AllAdults"=lice.cols[1])

open.chart.file <- function(filename, figure.height=5) {
                                        # open up the output image file for the chart
    #tiff(filename=paste(outdir,"/resistances.tiff",sep=""), family="ArialMT", pointsize=10,
    #     width=19, height=12, units="cm", res=300, compression="lzw", type="cairo")
    pdf(file=filename, height=9, width=19, pointsize=12)
    par(bg="white")
    par(mar=c(4.1, 4.1, 4.1, 4.1))
}   

error.bar <- function(x, y, upper, lower=upper, length=0.05,...) {
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
        stop("vectors must be same length")
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

sem <- function(x) sd(x, na.rm=T)/sqrt(length(na.omit(x)))

data.confidence <- function(x, percentage) {
    sorted.tmp = sort(x, na.last=NA)
    if (length(sorted.tmp > 0)) {
        data.index = round(percentage*length(sorted.tmp))
        data.confidence = sorted.tmp[data.index]
    } else {
        data.confidence = NA
    }
}
data.confidence.low <- function(x) data.confidence(x, 0.1)
data.confidence.high <- function(x) data.confidence(x, 0.9)

read.csv.file <- function(dir, filename) {
    csv.data = read.csv(paste(dir, "/", filename, sep=""), header=F)

                                        # <0 means a missing value
    csv.data[csv.data<0] = NA

    read.csv.file = csv.data
}

process.data <- function(r1, compress=FALSE) {
                                        # 'compress' means add up all the values in a cycle
    # print("---")
    # print(r1)
    if (compress) {
        compressed.matrix = matrix(0, length(r1[,1]), cycles.num)
        # print("---")
        for (cycle in seq(1, cycles.num)) {
            start = (cycle-1)*cycles.length + 1
            end = cycle*cycles.length
            
            if (end > length(r1[1,])) { end = length(r1[1,]) }
            # print(paste(start, end, sep = " - "))
            compressed.matrix[,cycle] = apply(r1[,seq(start, end)], 1, sum, na.rm=T)
            # print(compressed.matrix)
        }
        r1 = compressed.matrix
    } 

                                        # get means and standard error and data based confidence
    r1.means = apply(r1, 2, mean, na.rm=T)
    r1.se = apply(r1, 2, sem)
    r1.low = apply(r1, 2, data.confidence.low)
    r1.high = apply(r1, 2, data.confidence.high)

    process.data = list("means"=r1.means, "se"=r1.se, "low"=r1.low, "high"=r1.high)
}

get.infection.pressure <- function(dir, filename) {
    r1 = read.csv(paste(dir, "/", filename, sep=""), header=F)

                                        # <0 means a missing value
    r1[r1<0] = NA

    # sum all values in each simulation
    r1 = apply(r1, 1, sum, na.rm=T)

    # get the mean and CIs
    r1.mean = mean(r1)
    r1.low = r1.mean - 1.96*sem(r1)
    r1.high = r1.mean + 1.96*sem(r1)

    get.infection.pressure = list("mean"=r1.mean, "lowCI"=r1.low, "highCI"=r1.high)
}

draw.CI <- function(all.data) {
    m.length = length(all.data$means)
    v1 = seq(1,m.length,1)

    # assume normal - distrubution around mean
    v2 = all.data$means - 1.96*all.data$se
    v3 = all.data$means + 1.96*all.data$se

    # cat(paste(data.means$means[m.length]," (",v2[m.length],",",v3[m.length],")\n",sep=""))

    for (x.pos in v1) {
        segments(x.pos, v3[x.pos], x.pos, v2[x.pos], col=ci.color)
    }
}    

draw.average.lines <- function(all.data, leg.loc="none", type="lice") {
    curves = list()
    labels = vector()
    colours = vector()

    sorted.names = sort(names(all.data), decreasing=TRUE)
    count = 1
    
    for (key in sorted.names) {
        print(paste("key",key))
        # if (type == "lice") {
        colour = colour.codes[[key]]
        #} else {
        #    colour = treatment.cols[count]
        #}
        line.xy = draw.line(all.data[[key]], colour)
        colours = append(colours, colour)
        curves[[key]] = line.xy
        # print(key)
        # print(all.data[[key]])
        # print(sapply(all.data[[key]], mode))
        labels = append(labels, key)
        count = count + 1
    }

    # print(curves)
    if (leg.loc!="none") {
        print("koala")
        labcurve(curves, keys='lines', keyloc=leg.loc, labels, col=colours, type='l')
    }
}

draw.line <- function(data.means, colour) {
    # print(data.means$means)
    if (sum(data.means$means,na.rm=T) > 0) {
        v1 = seq(1,length(data.means$means),1)
        lines(v1, data.means$means, col=colour)

        draw.line = list(x=v1, y=data.means$means)
    }
}

create.line.plot <- function(all.data, show.legend=T) {
    # make sure we always get them in the same order
    sorted.names = sort(names(all.data))
    count = 1

    # draw the error bars first
    for (trt.type in sorted.names) {
        error.bar(1:cycles.num, all.data[[trt.type]]$means,
                  all.data[[trt.type]]$se, col="grey70")
    }

    # draw the lines
    colour.scheme = vector()
    symbol.types = vector()
    label.strings = vector()
    for (trt.type in sorted.names) {
        symbol.types = append(symbol.types, count)  # unlist(symbol.codes[[trt.type]])
        colour.scheme = append(colour.scheme, treatment.cols[count])  # unlist(colour.codes[[trt.type]]))
        label.strings = append(label.strings, trt.type)
        par(pch=count)  # symbol.codes[[trt.type]])
        lines(1:cycles.num, all.data[[trt.type]]$means,
               type="b", col=treatment.cols[count])  # colour.codes[[trt.type]])
        count = count + 1
    }

    par(cex=1)
    if (show.legend) {
        legend("topleft", legend=label.strings,
               col=colour.scheme, bg="white",
               pch=symbol.types, horiz=F, cex=5)
    }

}
