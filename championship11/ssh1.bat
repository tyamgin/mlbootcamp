@echo off

set cmd=%6
set cmd=%cmd:\"lol\"=Rscript%

ssh %1 %2 %3 %4 %5 %cmd%