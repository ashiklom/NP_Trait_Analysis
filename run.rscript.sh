#!/bin/bash
#$ -l h_rt=120:00:00
#$ -V
#$ -j y
#$ -o logs/
#$ -q "geo*"

Rscript $@
