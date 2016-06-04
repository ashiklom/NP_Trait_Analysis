#!/bin/sh

#$ -l h_rt=48:00:00
#$ -N try.run.multi
#$ -V
#$ -j y
#$ -pe omp 8 -q "geo*"
#$ -v OMP_NUM_THREADS=8

Rscript 01.run.models.R uni=FALSE uni.group=FALSE multi=TRUE multi.group=TRUE hier=FALSE with.na=FALSE n.chains=5
