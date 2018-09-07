#!/bin/bash

while read line; do
    doi2bibtex $line
done < scripts/ref_dois
