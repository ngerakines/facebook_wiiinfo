all: code template

code: clean
	erl -make

clean:
	rm -fv ebin/*.beam *.rel *.script *.boot erl_crash.dump *.log *.access

template:
	erl -noshell -eval '[erltl:compile(F, [{outdir, "ebin"}, report_errors, report_warnings, nowarn_unused_vars]) || F <- filelib:wildcard("templates/*.et")].' -pa ebin -s init stop

start:
	erl -setcookie wiiinfopass -name wiiinfoapp@`hostname` -pa ebin -yaws embedded true -boot start_sasl -eval '[application:start(X) || X <- [inets, yaws, wifbfe]].' -detached 

start-ctl:
	erl -setcookie wiiinfopass -name wiiinfoctp@`hostname` -pa ebin -remsh wiiinfoapp@`hostname`
