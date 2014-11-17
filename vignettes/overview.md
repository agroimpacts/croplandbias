---
title: "Analysis of cropland biases"
author: "Lyndon Estes"
date: "15 November 2014"
output: html_document
---

# Introduction

## Sidebar
I am just learning how to use Rmarkdown with Rmd files to document analyses, primarily analyses that are more interactive in nature and notrepeated many times. Code that will be re-used multiple times will go into a library. 

One things I am noticing is that the knit process for compiling the notebook does not find functions within code chunks unless the libraries are called within the code chunks themselves. It seems like each code chunk is its own environment.  The solution to this is to do something like `raster::raster()` when you want a function, or run this where chunks are set to {r, eval = FALSE}, and just run the code chunk as if you were in a normal R script working interactively.  I have already set up the library for SAcropland to call the packages its needs so they are loaded up in the global environment. 
