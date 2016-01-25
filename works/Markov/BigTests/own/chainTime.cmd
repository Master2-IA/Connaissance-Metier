@echo off
setlocal ENABLEDELAYEDEXPANSION

SET /A TOT=(7-2+1)*5*10
SET /A GTOT=TOT*7
SET /A GIT=0
for /f %%A in ('copy /Z "%~dpf0" nul') do set "CR=%%A"

gcc -w -o markovwo.exe markov-windows.c

<nul set/p="" > outSerialStochTestRnd.csv

FOR %%F IN (6640.txt) do (
	SET /A IT=0
	FOR %%K IN (3) do ( rem (2,1,7)
		FOR %%M IN (100000) do ( rem (10000 100000 1000000 10000000 100000000)
			FOR /L %%I IN (1,1,100) do ( rem (1,1,10)
				rem start /b cmd /c markovwo.exe %%K %%M !GIT! < %%F
				markovwo.exe %%K %%M !GIT! < %%F >> outSerialStochTestRnd.csv
				rem >> outTimeParal.csv &
				SET /A IT=IT+1
				SET /A GIT=GIT+1
				rem <nul set/p="[!IT! / !TOT!] of %%F (!GIT!/!GTOT!)!CR!"
			)
		)
	)
	rem echo.
)

rem <nul set/p="Done"


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