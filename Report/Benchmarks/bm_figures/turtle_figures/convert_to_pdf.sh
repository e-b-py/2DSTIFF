#!/usr/bin/env bash
for file in *.svg; do
   filename_with_extension=$(basename $file)
   filename=${filename_with_extension%.*}
   outname=$filename$".pdf"
   inkscape $filename_with_extension --export-pdf=$outname
done

