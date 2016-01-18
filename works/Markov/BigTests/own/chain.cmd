@echo off
setlocal ENABLEDELAYEDEXPANSION

SET /A TOT=(7-2+1)*5*10
SET /A GTOT=TOT*7
SET /A GIT=0
for /f %%A in ('copy /Z "%~dpf0" nul') do set "CR=%%A"

<nul set/p="" > out.csv

FOR %%F IN (6640.txt 26046.txt 39213.txt 75166.txt 112451.txt 404461.txt 492732.txt) do (
	SET /A IT=0
	FOR /L %%K IN (2,1,7) do ( rem (2,1,7)
		FOR %%M IN (100 1000 10000 100000 1000000) do ( rem (10000 100000 1000000 10000000 100000000)
			FOR /L %%I IN (1,1,10) do ( rem (1,1,10)
				markovw.exe %%K %%M !GIT! < %%F >> out.csv
				SET /A IT=IT+1
				SET /A GIT=GIT+1
				<nul set/p="[!IT! / !TOT!] of %%F (!GIT!/!GTOT!)!CR!"
			)
		)
	)
	echo.
)

<nul set/p="Done"


rem echo 6640.txt
rem markov.exe START < 6640.txt > out.csv

rem echo 26046.txt
rem markov.exe < 26046.txt >> out.csv

rem echo 39213.txt
rem markov.exe < 39213.txt >> out.csv

rem echo 75166.txt
rem markov.exe < 75166.txt >> out.csv

rem echo 112451.txt
rem markov.exe < 112451.txt >> out.csv

rem echo 404461.txt
rem markov.exe < 404461.txt >> out.csv

rem echo 492732.txt
rem markov.exe < 492732.txt >> out.csv