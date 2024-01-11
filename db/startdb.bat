@echo off 

set DLC=C:\Progress\OpenEdge
set PATH=%DLC%\bin;%PATH%
cd C:\progress_education\openedge\IntroPASForOpenEdge\exercise
call proserve sports2020 -H localhost -S 9000

