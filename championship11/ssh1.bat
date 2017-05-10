@echo off

::echo %1
::echo %2
::echo %3
::echo %4
::echo %5
::echo %6

set cmd=%6
set cmd=%cmd:\"lol\"=Rscript%

::echo %cmd%
ssh %1 %2 %3 %4 %5 %cmd%