#!/bin/bash

LRD='\033[1;31m'
LGN='\033[1;32m'
LCY='\033[1;36m'
YW='\033[1;33m'
LPR='\033[1;35m'
N='\033[0m'

module load bio3 #delete this when pushing

echo '--------'
echo 'Versions'
echo '--------'
python --version
deeptools --version
computeMatrix --version

bedFile=goldstd.bed
bw1File=SRX1070680_NT2_DRIP-seq_RNaseA.hg38.bw
bw2File=SRX3581339_drip_HeLa_IP_1_1.hg38.bw
bwFile=$bw1File,$bw2File
b=5000
a=5000
type=scale_regions
skipZeros=--skipZeros

outName=$bwFile,$bedFile,b$b,a$a,type$type

outMatrixFile=$outName.matrix.gz
outSortedFile=$outName.sorted.bed
outsvgFile=$outName.svg
outpngFile=$outName.png

mkdir -p matrix
mkdir -p sortedRegions
mkdir -p png
mkdir -p svg

echo '-------------'
echo '0. Parameters'
echo '-------------'
echo -e bedFile ${LCY}bed/$bedFile${N}
echo -e bwFile ${LCY}bw/$bw1File bw/$bw2File${N}
echo -e outMatrixFile ${LCY}matrix/$outName.matrix.gz${N}
echo -e outSortedFile ${LCY}sortedRegions$outSortedFile${N}
echo -e b ${LCY}$b${N}
echo -e a ${LCY}$a${N}
echo -e type ${LCY}$type${N}
echo -e skipZeros ${LCY}$skipZeros${N}
echo '   '

echo '-----------------'
echo '1. computeMatrix'
echo '-----------------'
echo -e computeMatrix scale-regions${N} -b ${LCY}10000${N} -a ${LCY}10000${N} -R ${LCY}bed/$bedFile${N} -S ${LCY}bw/$bw1File bw/$bw2File $skipZeros${N} -o ${LCY}matrix/$outMatrixFile${N} --outFileSortedRegions ${LCY}sortedRegions/$outSortedFile${N}
computeMatrix scale-regions -b 10000 -a 10000 -R bed/$bedFile -S bw/$bw1File bw/$bw2File $skipZeros -o matrix/$outMatrixFile --outFileSortedRegions sortedRegions/$outSortedFile

echo '   '
echo '-------------'
echo '2A. Making SVG'
echo '-------------'
echo -e plotProfile${N} -m ${LCY}$outMatrixFile${N} -o ${LCY}$outsvgFile${N} --perGroup ${LCY}--colors green red blue${N} --plotTitle ${LCY}$outName${N}

plotProfile -m matrix/$outMatrixFile -o svg/$outsvgFile --perGroup --colors green red blue --plotTitle $outName

echo '   '
echo '-------------'
echo '2B. Making PNG'
echo '-------------'
echo -e plotProfile${N} -m ${LCY}$outMatrixFile${N} -o ${LCY}$outpngFile${N} --perGroup ${LCY}--colors green red blue${N} --plotTitle ${LCY}$outName${N}

plotProfile -m matrix/$outMatrixFile -o png/$outpngFile --perGroup --colors green red blue --plotTitle $outName

echo '   '

# b = before (-10k of TSS)
# a = after (+10k of TES)
# --referencePoints [TSS,TES,center] or --scale-regions for +/- a/b bp off gene
