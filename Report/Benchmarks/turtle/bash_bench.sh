#!/bin/bash
for datfile in turtle_inputs/*.dat; do
	echo processing $datfile
	python benchmarker.py $datfile
done
mv turtle_inputs/*.svg turtle_figures/
